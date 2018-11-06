package safe.stics;

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import com.sun.jna.Structure;

/**
 * SafeSticsReinit - Objet to store SafeSticsCommun data to restart a project 
 * 
 * 
 * @author Isabelle Lecomte - March 2017
 */

public class SafeSticsReinit extends Structure implements Serializable {
	
	  //SafeSticsCommon data
	
	   public float tcultveille;  
	   public float tairveille;    
	   public float Chumt;       
	   public float Chuma; 
	   public float Chumi; 
	   public float Nhumt; 
	   public float Nhuma; 
	   public float Nhumi; 
	   
	   public float Cr;
	   public float Nr;
	   public float Cb;
	   public float Nb;
	   
	   public float Cmulchnd;
	   public float Nmulchnd;
	   public float Cmulchdec;
	   public float Nmulchdec;
	   public float Cbmulch;
	   public float Nbmulch;
		
	   public float kres[];
	   public float hres[];
	   public float Wb[];
	   
	   public float Cnondec[]; 
	   public float Nnondec[]; 
	   
	   public float Chum[]; 
	   public float Nhum[];
	   public float Cres[];
	   public float Nres[];
	   public float Cbio[];
	   public float Nbio[];
	   
	   public float HUR[];
	   public float sat[];
	   public float tsolveille[];
	   
	   //SafeSticsSoil data
	   
	   public float HR[];	
	   public float AZnit[];
	   public float AZamm[];
	   public float nit[];
	   public float amm[];
	   


		
		public SafeSticsReinit (SafeSticsCommun sc, SafeSticsSoil soil) {
			
			this.tcultveille = sc.tcultveille;
			this.tairveille = sc.tairveille;
			
			this.Chumt = sc.Chumt;
			this.Chuma = sc.Chuma;
			this.Chumi = sc.Chumi;
			this.Nhumt = sc.Nhumt;
			this.Nhuma = sc.Nhuma;
			this.Nhumi = sc.Nhumi;
			this.Cr = sc.Cr;
			this.Nr = sc.Nr;
			this.Cb = sc.Cb;
			this.Nb = sc.Nb;
			
			this.Cmulchnd = sc.Cmulchnd;
			this.Nmulchnd = sc.Nmulchnd;
			this.Cmulchdec = sc.Cmulchdec;
			this.Nmulchdec = sc.Nmulchdec;
			this.Cbmulch = sc.Cbmulch;
			this.Nbmulch = sc.Nbmulch;
			
			this.kres = new float[21];
			this.hres = new float[21];
			this.Wb = new float[21];
			this.Cnondec = new float[10];
			this.Nnondec = new float[10];			
			this.Chum = new float[1000];
			this.Nhum = new float[1000];
			this.Cres = new float[21000];
			this.Nres = new float[21000];
			this.Cbio = new float[21000];
			this.Nbio = new float[21000];
			this.HUR = new float[1000];
			this.sat = new float[1000];
			this.tsolveille = new float[1000];
			
			this.HR = new float[5];
			this.AZnit = new float[5];
			this.AZamm = new float[5];
			this.nit = new float[1001];
			this.amm = new float[1000];
			
			
			System.arraycopy(sc.kres , 0, this.kres , 0, 21);
			System.arraycopy(sc.hres , 0, this.hres , 0, 21);
			System.arraycopy(sc.Wb , 0, this.Wb , 0, 21);			
			System.arraycopy(sc.Cnondec , 0, this.Cnondec , 0, 10);
			System.arraycopy(sc.Nnondec , 0, this.Nnondec , 0, 10);
			System.arraycopy(sc.Chum 	, 0, this.Chum 	, 0, 	1000);
			System.arraycopy(sc.Nhum 	, 0, this.Nhum 	, 0, 	1000);			
			System.arraycopy(sc.Cres 	, 0, this.Cres 	, 0, 	21000);
			System.arraycopy(sc.Nres 	, 0, this.Nres 	, 0, 	21000);
			System.arraycopy(sc.Cbio 	, 0, this.Cbio 	, 0, 	21000);
			System.arraycopy(sc.Nbio 	, 0, this.Nbio 	, 0, 	21000);			
			System.arraycopy(sc.HUR 	, 0, this.HUR 	, 0, 	1000);
			System.arraycopy(sc.sat 	, 0, this.sat 	, 0, 	1000);
			System.arraycopy(sc.tsolveille , 0, this.tsolveille , 0, 1000);

			System.arraycopy(soil.HR 	, 0, this.HR 	, 0, 5);
			System.arraycopy(soil.AZnit , 0, this.AZnit , 0, 5);
			System.arraycopy(soil.AZamm , 0, this.AZamm , 0, 5);
			System.arraycopy(soil.nit 	, 0, this.nit 	, 0, 	1001);
			System.arraycopy(soil.amm 	, 0, this.amm 	, 0, 	1000);
		
		}
		
		
		 @Override
			protected List getFieldOrder() {
				return Arrays.asList(new String[] { "tcultveille", "tairveille","Chumt","Chuma","Chumi",
						"Nhumt", "Nhuma", "Nhumi",  "Cr", "Nr", "Cb", "Nb",
						"Cmulchnd", "Nmulchnd", "Cmulchdec", "Nmulchdec", "Cbmulch", "Nbmulch", 
						"kres", "hres", "Wb", "Cnondec", "Nnondec",
						"Chum", "Nhum", "Cres", "Nres", "Cbio", "Nbio", 
						"HUR", "sat", "tsolveille", "HR", "AZnit", "AZamm", "nit", "amm"
						
				});
			}
			    


			
}
