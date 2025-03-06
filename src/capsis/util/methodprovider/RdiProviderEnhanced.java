package capsis.util.methodprovider;

import capsis.defaulttype.Speciable;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;

/**
 * The RdiProviderEnhanced interface redefines the getRDI method. The method is now entirely dependent of the stand and no longer on the model.
 * A nested class implements a static method that makes it possible to choose the appropriate getRDI method based on the instantiation of the
 * MethodProvider
 * @author Mathieu Fortin - March 2011
 */
public interface RdiProviderEnhanced extends RDIProvider {
	
	/**
	 * This class embeds a static method that makes it possible to choose the appropriate method for the rdi calculation. 
	 * @author Mathieu Fortin - March 2011
	 */
	public static class RdiTool {
		/**
		 * This private method returns the rdi. The rdi calculation is based on the instantiation of the method provider.
		 * @param Object an object instance
		 * @param model a GModel reference
		 * @param nbTreesHa a double
		 * @param mdq a double
		 * @return the relative density index (rdi) or -1 if an exception is thrown
		 */
		public static double getRdiDependingOnMethodProviderInstance(GScene stand, GModel model, double nbTreesHa, double mdq) {
			try {
				double partialRdi;
				MethodProvider mp = model.getMethodProvider();
				if (mp instanceof RdiProviderEnhanced) {
					if (stand instanceof Speciable) {
						partialRdi = ((RdiProviderEnhanced) mp).getRDI((Speciable) stand, nbTreesHa, mdq);
					} else {
						throw new Exception("Incompatible stand and method provider : the stand should be Speciable");
					}
				} else {
					partialRdi = ((RDIProvider) mp).getRDI(model, nbTreesHa, mdq, null);
				}
				return partialRdi;
			} catch (Exception e) {
				return -1d;
			}
		}
	}
	
	
	/**
	 * This Rdi provider does not rely on the model as does the original Rdi provider. Instead a Speciable object 
	 * makes it possible to specify the species.
	 * @param speciable a Speciable instance
	 * @param Nha the number of stems per hectare
	 * @param Dg the mean quadratic diameter
	 * @return the rdi (a double)
	 */
	public double getRDI(Speciable speciable, double Nha, double Dg); 

	
}
