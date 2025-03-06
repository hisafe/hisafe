package capsis.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import capsis.util.group.GroupableElement;
import capsis.util.group.GroupableType;
import capsis.util.group.PossiblyGroupableScene;
import jeeb.lib.util.Translator;

/**
 * A helper class for fish models, answers all questions related to grouping.
 * The fish scene classes implementing PossiblyGroupableScene may redirect the
 * methods expected by PossiblyGroupableScene to these ones, passing itself in
 * argument.
 *
 * @author F. de Coligny - July 2019
 */
public class FishGroupHelper {

	static {
		Translator.addBundle("capsis.util.Labels");
	}

	// fc-1.7.2019 GroupableElement element type keys
	public static final GroupableType GROUP_FISH = new GroupableType("GROUP_FISH");
	public static final GroupableType GROUP_RIVER = new GroupableType("GROUP_RIVER"); // mb+al-19.4.2021
	public static final GroupableType GROUP_REACH = new GroupableType("GROUP_REACH");
	public static final GroupableType GROUP_WEIR = new GroupableType("GROUP_WEIR");

	/**
	 * Returns the types of the elements that can be grouped in the given scene.
	 * Returns null if !isGroupable ().
	 */
	static public Collection<GroupableType> getGroupableTypes(PossiblyGroupableScene scene) {

		if (scene == null || !scene.isGroupable())
			return null;

		// fc-1.7.2019 Add types depending on scene class
		List<GroupableType> types = new ArrayList<>();

		if (scene instanceof FishComposite)
			types.add(GROUP_FISH);
		if (scene instanceof RiverComposite) // mb+al-19.4.2021
			types.add(GROUP_RIVER);
		if (scene instanceof ReachComposite)
			types.add(GROUP_REACH);
		if (scene instanceof WeirComposite)
			types.add(GROUP_WEIR);

		return types;

	}

	/**
	 * Returns the collection of elements in the given groupable scene matching
	 * the given type. Returns null if !isGroupable ().
	 */
	static public Collection<GroupableElement> getGroupableElements(PossiblyGroupableScene scene, GroupableType type) {

		// fc-5.7.2019 added type argument, was missing

		if (scene == null || !scene.isGroupable() || type == null)
			return null;

		if (type.equals(GROUP_FISH) && scene instanceof FishComposite)
			return ((FishComposite) scene).getFishes();

		if (type.equals(GROUP_RIVER) && scene instanceof RiverComposite) // mb+al-19.4.2021
			return ((RiverComposite) scene).getRiverMap().values();

		if (type.equals(GROUP_REACH) && scene instanceof ReachComposite)
			return ((ReachComposite) scene).getReachMap().values();

		if (type.equals(GROUP_WEIR) && scene instanceof WeirComposite)
			return ((WeirComposite) scene).getWeirMap().values();

		return null;

	}

}
