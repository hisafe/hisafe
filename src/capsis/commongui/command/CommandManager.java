/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2010  Francois de Coligny, Samuel Dufour
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A 
 * PARTICULAR PURPOSE. See the GNU Lesser General Public 
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package capsis.commongui.command;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JFrame;

import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.ListenedTo;
import jeeb.lib.util.Listener;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.projectmanager.Current;
import capsis.gui.MainFrame;
import capsis.kernel.Engine;
import capsis.kernel.Project;
import capsis.kernel.Step;


/**	A manager for commands.
*	All the Commands implement ActionCommand: i.e. Action AND Command.
*	The commands can be retrieved from everywhere.
*	<pre>
*	CommandManager cmdMan = CommandManager.getInstance ();
*	...
*	// add the About command in the Help menu
*	JMenu helpMenu = new JMenu (Translator.swap ("JFrame.helpMenu"));
*	menuBar.add (helpMenu);
*	helpMenu.add (cmdMan.getCommand ("About"));
*	...
*	// execute the About command
*	Command cmd = cmdMan.getCommand ("About");
*	cmd.execute ();
*	</pre>
*	@author F. de Coligny - august 2009, january 2010
*/
public class CommandManager implements Listener {

	static {
		Translator.addBundle("capsis.commongui.command.CommandManager");
	}
	private static CommandManager instance;	// Almost Singleton pattern

	// The command with a PROJECT level will be disabled when no project exist.
	// e.g. CloseProject will be disabled if no project in the Session. 
	static public enum Level {
		PROJECT, BASIC
	}
	private JFrame frame;
	private String frameTitle;
	private String applicationName;
	private Map<String,ActionCommand> commands;
	private List<ActionCommand> projectCommands;
	
	
	/**	Almost Singleton pattern.
	*	Constructor must be called once, then use getInstance ().
	*/
	public static CommandManager getInstance () {
		return instance;
	}
	
	/**	Almost Singleton pattern
	*	Constructor must be called once, then use getInstance ().
	*	The title of the frame should be set before this constructor is used.
	*/
	public CommandManager (JFrame frame, String applicationName) {
		if (instance != null) {
			Log.println (Log.ERROR, "CommandManager.c ()", 
					"This constructor cannot be called twice, design error");
			System.out.println ("This constructor cannot be called twice, design error, aborting");
			System.exit (-1);
		}
		instance = this;
		this.frame = frame;
		this.frameTitle = frame.getTitle ();
		projectCommands = new ArrayList<ActionCommand> ();
		createBasicCommands ();
		
		// We will be told when current step / project changes
		// Needed to enable / disable project commands
		Current.getInstance ().addListener (this);
		
		setCommandsEnabled (Level.PROJECT, false);
		
	}
	
	/**	Updates the frame title according to the session state and name.	
	 */
	public void updateFrameTitle () {
		Step s = Current.getInstance().getStep ();
		if (s != null) {
			// Update the frame title
			frame.setTitle (frameTitle /*+ Engine.getVersion ()*/ + " - [" + s.getCaption() + "]");
			
//		}
//		if (Engine.getInstance ().getSession () != null) {
//			frame.setTitle (frameTitle+" - ["+Engine.getInstance ().getSession ().getName ()+"]");
		
		} else {
			frame.setTitle (frameTitle);
		}

	}
	
	/**	Returns the name of the application	
	 */
	public String getApplicationName () {
		return applicationName;
	}
	
	/**	Create all the basic commands here, they will be returned when 
	*	needed with getCommand () to put them into menus or to execute () them.
	*/
	private void createBasicCommands () {
		
		// Main commands to manage a capsis.kernel.Session and its projects
		addCommand (new NewProject (frame), Level.BASIC);
		addCommand (new OpenProject (frame), Level.BASIC);
		addCommand (new CloseProject (frame), Level.PROJECT);
		addCommand (new SaveProject (frame), Level.PROJECT);
		addCommand (new SaveAsProject (frame), Level.PROJECT);
		addCommand (new ReinitialiseProject (frame), Level.PROJECT);
		addCommand (new ConfigureProject (frame), Level.PROJECT);
		
		addCommand (new NewSession (frame), Level.BASIC);
		addCommand (new OpenSession (frame), Level.BASIC);
		addCommand (new CloseSession (frame), Level.PROJECT);
		addCommand (new SaveSession (frame), Level.PROJECT);
		addCommand (new SaveAsSession (frame), Level.PROJECT);

		addCommand (new Quit (frame), Level.BASIC);
		
		// Step menu commands
		addCommand (new Evolution (frame), Level.PROJECT);
		addCommand (new Intervention (frame), Level.PROJECT);
		addCommand (new DeleteStep (frame), Level.PROJECT);
		
		// fc-3.1.2017
		addCommand (new ChangeStepColor(frame), Level.PROJECT);
		
		addCommand ("ShowStep1", new ShowStep (frame, Translator.swap ("ShowStep.showAllSteps"), 1), Level.PROJECT);
		addCommand ("ShowStep5", new ShowStep (frame, Translator.swap ("ShowStep.showStep5"), 5), Level.PROJECT);
		addCommand ("ShowStep10", new ShowStep (frame, Translator.swap ("ShowStep.showStep10"), 10), Level.PROJECT);
		addCommand ("ShowStep20", new ShowStep (frame, Translator.swap ("ShowStep.showStep20"), 20), Level.PROJECT);
		
		// Generic commands
		addCommand (new Logs (frame), Level.BASIC);
		
	}

	
	/**	The -preferred- method to add a Command to the CommandManager.
	 *	Can be used to add specific commands from Capsis, Simeo, Xplo (...), 
	 *	e.g. commandManager.addCommand (new Export (...), true);
	 *	Project commands can be disabled when the session contains no project.
	 *	See getCommand (String commandName). 
	 */
	public void addCommand (ActionCommand command, Level level) {
		addCommand ( command.getClass().getSimpleName(), command, level );
	}

	/**	The -preferred- method to get a command: from its class.
	 *	See addCommand(ActionCommand, boolean).
	 */
	public ActionCommand getCommand (Class commandClass) {
		return commands.get( commandClass.getSimpleName() );  // returns null if not found
	}

	
	
	/**	Generally, addCommand (ActionCommand, boolean) is preferred. 
	 *	A way to add a command with a name, convenient if several instances 
	 *	may be added. See example below: 	
	 *	addCommand ("ShowStep1", new ShowStep (frame, Translator.swap ("ShowStep.showAllSteps"), 1), true);
	 *	addCommand ("ShowStep5", new ShowStep (frame, Translator.swap ("ShowStep.showStep5"), 5), true);
	 */
	public void addCommand (String commandName, ActionCommand newCommand, Level level) {
		if (commands == null) {commands = new HashMap<String,ActionCommand> ();}
		commands.put (commandName, newCommand);

		if (level == Level.PROJECT) {
			projectCommands.add (newCommand);
		}
	}

	/**	Generally, getCommand(Class) is preferred. 
	 *	Returns the Command with the given name. Can be used when several 
	 *	instances of the same Command are created (e.g. ShowStep1, ShowStep5...).
	 *	See addCommand(String, ActionCommand, boolean).
	 */
	public ActionCommand getCommand (String commandName) {
		return commands.get (commandName);  // returns null if not found
	}

	
	/**	Called by ListenedTo when something happens.
	*/
	public void somethingHappened (ListenedTo l, Object param) {
		Current current = Current.getInstance ();
		if (l.equals (current)) {
			
			updateFrameTitle();
			
			// Moved to new / open / close project
//			// Manage Project commands enabling / disabling
//			Project project = current.getProject ();
//			boolean enable = project != null;
//			setCommandsEnabled (Level.PROJECT, enable);
			
		}
	}
	
	/**	Enables / disables the project commands. When there is 
	*	no project in the session, some commands must be disabled 
	*	(e.g. "CloseProject"...).
	*/
	public void setCommandsEnabled (Level level, boolean yep) {
		
		if (level == Level.PROJECT) {
			for (ActionCommand c : projectCommands) {c.setEnabled(yep);}
		}
			
	}
	
	
}
