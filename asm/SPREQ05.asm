*          DATA SET SPREQ05    AT LEVEL 189 AS OF 10/14/16                      
*PHASE T20805A,+0                                                               
         TITLE 'SPREQ05 - REQUEST - SOON RESTRICION CHECKS'                     
* --------------------------------------------------------------------*         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT ITMF-2462   01/21/16 RELAX SOON RESTIRCTIONS FOR AGENCY ON     *         
* SMUR SPEC-6743   10/14/16 RELAX SOON RESTIRCTIONS FOR AGENCY OO     *         
* --------------------------------------------------------------------*         
T20805   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 000,T20805,RR=R9                                                 
         USING T20805,RB,RA                                                     
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
                                                                                
         LA    RA,2048(RB)         NOTE RB,RA AS BASE REGISTERS                 
         LA    RA,2048(RA)                                                      
                                                                                
         L     R9,0(R1)                                                         
         USING REQTEMP,R9                    R9=A(W/S)                          
                                                                                
                                                                                
         MVC   FERN,=AL2(FF)       SET OK FLAG                                  
                                                                                
         L     R3,ASAVE                                                         
         USING T208FFD,R3                                                       
         CLC   =C'SOON',BVROUT     IS IT SOON REQUEST                           
         BNE   XIT                                                              
                                                                                
******   L     R3,ASAVE            DDS WILL HAVE 1 YEAR RESTRICTION             
******   USING TWAD,R3                                                          
******   CLI   DDS,1               IS IT DDS TERMINAL                           
******   BE    XIT                                                              
                                                                                
         BAS   RE,SOONCHK          CHECK RESTRICTIONS                           
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
SOONCHK  NTR1                                                                   
*                                                                               
         L     R3,ASAVE            DDS WILL HAVE 1 YEAR RESTRICTION             
         USING TWAD,R3                                                          
         CLI   DDS,1                                                            
         BE    MKTXX                                                            
*                                                                               
         CLC   RNUM,=C'C1'         C1 REPORT HAS NO SOON RESTRICTIONS           
         BE    XIT                                                              
         CLC   RNUM,=C'CE'         CE REPORT HAS NO SOON RESTRICTIONS           
         BE    XIT                                                              
         CLC   RNUM,=C'CR'         CR REPORT HAS NO SOON RESTRICTIONS           
         BE    XIT                                                              
         CLC   RNUM,=C'LT'         LT REPORT HAS NO SOON RESTRICTIONS           
         BE    XIT                 LABATT                                       
         CLC   RNUM,=C'CI'         CI REPORT HAS NO SOON RESTRICTIONS           
         BE    XIT                 CONTINENTAL AIRLINES                         
         CLC   RNUM,=C'IN'         IN REPORT HAS NO SOON RESTRICTIONS           
         BE    XIT                 NISSAN                                       
         CLC   RNUM,=C'SE'         SE REPORT HAS NO SOON RESTRICTIONS           
         BE    XIT                 SPRINT                                       
         CLC   RNUM,=C'PH'         PH REPORT HAS NO SOON RESTRICTIONS           
         BE    XIT                 PHILIP MORRIS                                
         CLC   RNUM,=C'AI'         AI REPORT HAS NO SOON RESTRICTIONS           
         BE    XIT                 AT&T INTERFACE                               
         CLC   RNUM,=C'74'         74 REPORT PROFILE LIST                       
         BE    XIT                                                              
         CLC   RNUM,=C'LO'         LO REPORT HAS NO SOON RESTRICTIONS           
         BE    XIT                                                              
         CLC   RNUM,=C'TD'         TD REPORT HAS NO SOON RESTRICTIONS           
         BE    XIT                                                              
         CLC   RNUM,=C'WB'         WB REPORT HAS NO SOON RESTRICTIONS           
         BE    XIT                                                              
         CLC   RNUM,=C'ZB'         ZB REPORT HAS NO SOON RESTRICTIONS           
         BE    XIT                                                              
                                                                                
                                                                                
* - SOON RESTRICTIONS BY SPECIFIC AGENCY UPFRONT                                
* - ALTHOUGH THIS LEADS TO DUPLICATION OF CODE IT IS ONLY                       
* - SANE WAY I COULD THINK OF TO KEEP TRACK OF VARIOUS OPTIONS/EX-              
* - EMPTIONS FOR AGENCIES AND CLIENTS                                           
*                                                                               
         BAS   RE,CHKID            CHECK FOR SPECIFIC ACCESS                    
         USING CTDSCD,R4                                                        
         CLC   =C'BESAT',CTDSC      FOR BESAT                                   
         BNE   SONXX                                                            
         DROP  R4                                                               
         CLC   RNUM,=C'RW'         RW REPORT                                    
         BE    SON10                                                            
         CLC   RNUM,=C'W2'         W2 -> M2  FUDGED FOR BESAT                   
         BE    SON10                                                            
         CLC   RNUM,=C'W9'         W9 -> M9                                     
         BE    SON10                                                            
         CLC   RNUM,=C'W4'         W4 -> M9                                     
         BNE   SONXX                                                            
SON10    TM    CLISAVE,X'20'       ONE CLIENT                                   
         BNZ   CLIERR                                                           
         TM    CLISAVE,X'04'       ONE CLIENT                                   
         BZ    CLIERR                                                           
         MVC   HALF,=H'100'        14 WEEKS                                     
         BAS   RE,DATECHK                                                       
         BE    XIT                                                              
         B     STENDERR                                                         
                                                                                
SONXX    EQU   *                                                                
         EJECT                                                                  
* CLIENT RESTRICTIONS                                                           
                                                                                
* FOR NETWORK                                                                   
         CLI   RMED,C'N'                                                        
         BNE   CLI20                                                            
         CLC   RNUM,=C'W2'         W2 1 CLIENT                                  
         BE    CLI100                                                           
         CLC   RNUM,=C'OR'         OR 1 CLIENT                                  
         BE    CLI100                                                           
         B     CLIXX               REST HAVE NO CLIENT RESTRICTIONS             
                                                                                
                                                                                
* FOR SPOT                                                                      
CLI20    DS    0H                                                               
         CLC   RNUM,=C'Z5'         NO RESTRICTIONS ON Z5                        
         BE    CLIXX                                                            
         CLC   RNUM,=C'Z7'         NO RESTRICTIONS ON Z7                        
         BE    CLIXX                                                            
         CLC   RNUM,=C'SS'         NO RESTRICTIONS ON SS                        
         BE    CLIXX                                                            
*                                                                               
         CLC   RAGY,=C'H9'         FOR MVNY AND M2 ALLOW CLIENT GROUP           
         BNE   CLI100              REST HAVE CLIENT RESTRICTIONS                
         CLC   RNUM,=C'M2'                                                      
         BNE   CLI100                                                           
         TM    CLISAVE,X'40'       IS IT CLIENT GROUP?                          
         BNO   CLI100                                                           
         TM    MKTSAVE,X'08'       YES, IS IT SINGLE MKT REQUEST ?              
         BO    CLIXX               YES, ALLOW IT                                
*                                                                               
CLI100   TM    CLISAVE,X'20'       SOON MUST = ONE CLIENT                       
         BO    CLIERR                                                           
         TM    CLISAVE,X'04'       SOON MUST = ONE CLIENT                       
         BO    CLIXX               OK                                           
CLIERR   MVI   ROUTNUM,CLINUM      ERROR / POSITION CURSOR                      
         B     SOONERR                                                          
*                                                                               
CLIXX    EQU   *                                                                
*                                                                               
* D4->W4,DC->WC,M2->W2,M9->W9,M4->m4  special requests for WI agencies          
* no restrictions except for client check                     i                 
         CLC   =C'WI',RAGY                                                      
         BNE   CLIXXX                                                           
         CLC   =C'W4',RNUM                                                      
         BE    XIT                                                              
         CLC   =C'WC',RNUM                                                      
         BE    XIT                                                              
         CLC   =C'W2',RNUM                                                      
         BE    XIT                                                              
         CLC   =C'W9',RNUM                                                      
         BE    XIT                                                              
         CLC   =C'm4',RNUM                                                      
         BE    XIT                                                              
                                                                                
CLIXXX   CLC   RNUM,=C'GT'         FOR GT NO RESTR OTHER THEN CLT               
         BE    XIT                                                              
         CLC   RNUM,=C'41'                                                      
         BE    XIT                                                              
*                                                                               
         EJECT                                                                  
PROD     EQU   *                                                                
* PRODUCT RESTRICTIONS                                                          
                                                                                
* NO RESTRICTIONS FOR THESE REPORTS                                             
         CLC   RNUM,=C'M4'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'm4'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'DN'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'D5'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'N5'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'RS'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'RY'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'A2'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'AX'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'AB'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'N2'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'I2'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'Z5'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'Z7'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'M8'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'RN'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'RX'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'D1'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'B1'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'BU'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'DU'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'K5'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'KL'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'D2'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'K4'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'SS'                                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'K1'                                                      
         BE    PRODXX                                                           
                                                                                
PROD00   EQU   *                                                                
         CLC   RNUM,=C'D3'         ..D3                                         
         BE    *+14                                                             
         CLC   RNUM,=C'J6'         ..J6                                         
         BNE   PROD00A                                                          
         TM    PROSAVE,X'08'       ..POL OK                                     
         BO    PRODXX                                                           
                                                                                
                                                                                
* - AGENCIES                                                                    
PROD00A  EQU   *                                                                
                                                                                
         CLC   RAGY,=C'ON'         AGENCY ON?                                   
         BNE   PRODONX             NO                                           
         CLC   RNUM,=C'D4'         D4                                           
         BE    PRODXX              YES - OK                                     
         CLC   RNUM,=C'D8'         D4                                           
         BE    PRODXX              YES - OK                                     
PRODONX  EQU   *                                                                
                                                                                
* AGY=OO                                                                        
         CLC   RAGY,=C'OO'         ,,FOR OO                                     
         BNE   PRODOOX                                                          
         CLC   RNUM,=C'M2'         ,,FOR M2 REPORT                              
         BE    PRODXX                                                           
PRODOOX  EQU   *                                                                
                                                                                
* AGY=H9                                                                        
         CLC   RAGY,=C'H9'         ,,FOR H9                                     
         BE    *+14                                                             
         CLC   RAGY,=C'O0'         ,,FOR O0                                     
         BNE   PRODH9X                                                          
         CLC   RNUM,=C'M9'         ,,FOR M9 REPORT                              
         BE    PRODXX                                                           
         CLC   RNUM,=C'D4'         ,,FOR D4 REPORT                              
         BE    PRODXX                                                           
         CLC   RNUM,=C'M2'         ,,FOR M2 REPORT                              
         BE    PRODXX                                                           
         CLC   RNUM,=C'D1'         ,,FOR D1 REPORT                              
         BE    PRODXX                                                           
PRODH9X  EQU   *                                                                
                                                                                
* AGY=G7                                                                        
         CLC   RAGY,=C'G7'         ,,FOR G7 (GSTX)                              
         BNE   PROD01                                                           
         CLC   RNUM,=C'DU'         ,,FOR DU REPORT                              
         BE    PRODXX                                                           
         CLC   RNUM,=C'D1'         ,,FOR D1 REPORT                              
         BE    PRODXX                                                           
                                                                                
* AGY=MI                                                                        
PROD01   CLC   RAGY,=C'MI'         ,,FOR MI                                     
         BNE   PROD04                                                           
         CLC   RNUM,=C'D4'         ,,D4                                         
         BE    PRODXX                                                           
         CLC   RNUM,=C'RN'         ,,RN                                         
         BE    PRODXX                                                           
         CLC   RNUM,=C'RX'         ,,RX                                         
         BE    PRODXX                                                           
         CLC   RNUM,=C'RY'         ,,RY                                         
         BE    PRODXX                                                           
         CLC   RNUM,=C'M9'         ,,M9                                         
         BE    PRODXX              ,,NO RESTRICTIONS                            
                                                                                
         CLC   RNUM,=C'D2'         ..D2                                         
         BNE   PROD05                                                           
         TM    PROSAVE,X'08'       ..POL OK                                     
         BO    PRODXX                                                           
                                                                                
* AGY=OU                                                                        
PROD04   DS    0H                                                               
         CLC   RAGY,=C'OU'         ,,FOR OU                                     
         BNE   PROD05                                                           
         CLC   RNUM,=C'RN'         ,,RN                                         
         BE    PRODXX                                                           
         CLC   RNUM,=C'D1'         ,,D1                                         
         BE    PRODXX                                                           
         CLC   RNUM,=C'RX'         ,,RX                                         
         BE    PRODXX                                                           
         CLC   RNUM,=C'RY'         ,,RY                                         
         BE    PRODXX                                                           
         CLC   RNUM,=C'D8'         ..D6                                         
         BE    PRODXX                                                           
         CLC   RNUM,=C'D2'         ..D2                                         
         BNE   PROD05                                                           
         B     PRODXX                                                           
                                                                                
                                                                                
* AGENCY=WI                                                                     
PROD05   CLC   RAGY,=C'WI'         ..FOR AGY WI                                 
         BNE   PROD12                                                           
         CLC   RNUM,=C'D6'         ..D6                                         
         BE    PRODXX              ..NO RESTRICTIONS                            
         CLC   RNUM,=C'J6'         ..J6                                         
         BE    PRODXX              ..NO RESTRICTIONS                            
         CLC   RNUM,=C'D4'         ..D4                                         
         BE    PRODXX              ..NO RESTRICTIONS                            
         CLC   RNUM,=C'D7'         ..D7                                         
         BE    PRODXX              ..NO RESTRICTIONS                            
*                                                                               
PROD10   CLC   =C'WDW',RCLI        ..CLIENT WDW                                 
         BNE   PROD12                                                           
         CLC   RNUM,=C'M2'         ..M2                                         
         BE    PRODXX              ..NO RESTRICTIONS                            
                                                                                
                                                                                
PROD12   EQU   *                                                                
         CLC   RAGY,=C'S4'         FOR WAVA                                     
         BNE   PROD13                                                           
         CLC   RNUM,=C'D6'         AND FOR D6 REPORT                            
         BE    *+14                                                             
         CLC   RNUM,=C'J6'         AND FOR J6 REPORT                            
         BNE   PROD13                                                           
         TM    PROSAVE,X'08'       ..POL OK IF..                                
         BNO   PROD13                                                           
         TM    STASAVE,X'04'       SINGLE STATION                               
         BNO   PROD13                                                           
         TM    ESTSAVE,X'04'       AND SINGLE EST REQUEST                       
         BO    PRODXX                                                           
* AGENCY=M1                                                                     
PROD13   CLC   RAGY,=C'M1'         ,,FOR AGY M1                                 
         BNE   PROD14                                                           
         CLC   RNUM,=C'D7'         ,,D7                                         
         BE    PRODXX              ,,NO RESTRICTIONS                            
                                                                                
                                                                                
PROD14   CLC   RAGY,=C'KA'            ..FOR AGY KA                              
         BE    PROD14A                                                          
         CLC   RAGY,=C'U#'            ..FOR AGY M2 POL OK FOR M2                
         BE    PROD14B                                                          
         CLC   RAGY,=C'NT'            ..FOR AGY NT POL OK FOR M2                
         BE    PROD14B                                                          
         CLC   RAGY,=C'H0'            ..FOR AGY H0 POL OK FOR D8                
         BE    PROD14C                                                          
         CLC   RAGY,=C'HY'            ..FOR AGY HY POL OK FOR D8                
         BE    PROD14C                                                          
         B     PROD16                                                           
*                                                                               
PROD14A  CLC   RNUM,=C'D2'            ..D2                                      
         BNE   PROD14B                                                          
         TM    PROSAVE,X'08'          ..POL OK                                  
         BO    PRODXX                                                           
PROD14B  CLC   RNUM,=C'M2'            ..M2                                      
         BE    PROD14E                                                          
PROD14C  CLC   RNUM,=C'D8'         OKAY FOR D8                                  
         BNE   PROD16                                                           
PROD14E  TM    PROSAVE,X'08'          .POL OK                                   
         BO    PRODXX                                                           
                                                                                
                                                                                
* AGENCY=WD AND DA                                                              
PROD16   CLC   RAGY,=C'WD'            .FOR AGY WD                               
         BE    *+14                                                             
         CLC   RAGY,=C'DA'            .FOR AGY DA                               
         BNE   PROD18                                                           
*                                                                               
         CLC   RNUM,=C'M2'            .M2                                       
         BNE   PROD18                                                           
         TM    PROSAVE,X'08'          .POL OK                                   
         BO    PRODXX                                                           
                                                                                
                                                                                
* AGENCY=TY                                                                     
PROD18   CLC   RAGY,=C'TY'         AGENCY TY                                    
         BNE   PROD19              HAS NO PROD RESTRICTIONS FOR                 
         CLC   RNUM,=C'M8'         M8                                           
         BE    PRODXX                                                           
         CLC   RNUM,=C'M9'         M9                                           
         BE    PRODXX                                                           
         CLC   RNUM,=C'D2'         D2                                           
         BE    PRODXX                                                           
         CLC   RNUM,=C'D3'         D3                                           
         BE    PRODXX                                                           
         CLC   RNUM,=C'D4'         D4                                           
         BE    PRODXX                                                           
         CLC   RNUM,=C'D5'         D5                                           
         BE    PRODXX                                                           
         CLC   RNUM,=C'D7'         D7                                           
         BE    PRODXX                                                           
         CLC   RNUM,=C'DX'         DX                                           
         BE    PRODXX                                                           
         CLC   RNUM,=C'D8'         D8                                           
         BE    PRODXX                                                           
         CLC   RNUM,=C'M2'         M2                                           
         BE    PRODXX                                                           
         CLC   RNUM,=C'M3'         M3                                           
         BE    PRODXX                                                           
         CLC   RNUM,=C'M4'         M4                                           
         BE    PRODXX                                                           
         CLC   RNUM,=C'm4'         m4                                           
         BE    PRODXX                                                           
                                                                                
* AGENCY=EM                                                                     
PROD19   CLC   RAGY,=C'EM'         AGENCY EM                                    
         BNE   PROD20              HAS NO PROD RESTRICTIONS FOR                 
         CLC   RNUM,=C'D4'         D4                                           
         BE    PRODXX                                                           
         CLC   RNUM,=C'D8'         D8                                           
         BE    PRODXX                                                           
*                                                                               
* AGENCY=SS                                                                     
PROD20   CLC   RAGY,=C'SS'         FOR AGENCY SVVA                              
         BNE   PROD21                                                           
         CLC   RNUM,=C'M2'          M2                                          
         BE    PRODXX                                                           
*                                                                               
PROD21   CLC   RAGY,=C'TB'         FOR AGENCY ZOTO                              
         BNE   PROD22                                                           
         CLC   RNUM,=C'M2'          M2                                          
         BE    PRODXX                                                           
         CLC   RNUM,=C'M9'          M9                                          
         BE    PRODXX                                                           
         CLC   RNUM,=C'D4'          D4                                          
         BE    PRODXX                                                           
         CLC   RNUM,=C'D8'          D8                                          
         BE    PRODXX                                                           
                                                                                
*                                                                               
PROD22   CLC   RAGY,=C'YR'         FOR AGENCY YRTMOD                            
         BNE   PROD22A                                                          
         CLC   RNUM,=C'M9'         FOR M9                                       
         BE    PRODXX                                                           
         CLC   RNUM,=C'M2'         FOR M2                                       
         BE    PRODXX                                                           
         CLC   RNUM,=C'D4'         FOR D4                                       
         BE    PRODXX                                                           
*                                                                               
PROD22A  CLC   RAGY,=C'MI'         FOR AGENCI MKMO (MI)                         
         BNE   PROD22B                                                          
         CLC   RNUM,=C'D8'          FOR D8                                      
         BE    PRODXX                                                           
*                                                                               
PROD22B  CLC   RAGY,=C'BA'         FOR AGENCY BATO                              
         BNE   PROD22C                                                          
         CLC   RNUM,=C'M9'          FOR M9                                      
         BE    PRODXX                                                           
*                                                                               
PROD22C  CLC   RAGY,=C'PT'         FOR AGENCY PTTOD                             
         BNE   PROD22D                                                          
         BE    PRODXX              NO PRODUCT RESTRICTIONS                      
*                                                                               
PROD22D  CLC   RAGY,=C'DN'         FOR 'DN', (AGY KMVA,PJVA ETC)                
         BNE   PROD22E                                                          
         BE    PRODXX              NO PRODUCT RESTRICTIONS                      
*                                                                               
PROD22E  CLC   RAGY,=C'TH'         FOR ZENITH                                   
         BNE   PROD22Z                                                          
         CLC   RNUM,=C'M2'          FOR M2                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'M9'          FOR M9                                      
         BE    PRODXX                                                           
         CLC   RNUM,=C'D8'          FOR D8                                      
         BE    PRODXX                                                           
*                                                                               
*                                                                               
PROD22Z  EQU   *                                                                
*                                                                               
                                                                                
                                                                                
* SOME RESTRICTIONS FOR THESE REPORTS                                           
         CLC   RNUM,=C'D4'            .D4                                       
         BNE   *+12                                                             
         TM    PROSAVE,X'08'          .POL OK                                   
         BO    PRODXX                                                           
                                                                                
         CLC   RNUM,=C'D8'            .D8                                       
         BNE   PROD24                                                           
         TM    PROSAVE,X'0A'          .POL OK                                   
         BZ    PRODXX                                                           
         TM    MKTSAVE,X'08'          .IF ONE MARKET                            
         BO    PRODXX                                                           
         B     PRODERR                                                          
                                                                                
PROD24   EQU   *                                                                
*                                                                               
PROD100  DS    0H                                                               
         TM    PROSAVE,X'04'       FOR SOON ONLY 1 PROD                         
         BNO   PRODERR                                                          
         TM    PROSAVE,X'30'       PROD=NO/FILTER NOT OK                        
         BZ    PRODXX                                                           
                                                                                
PRODERR  MVI   ROUTNUM,PRODNUM                                                  
         B     SOONERR                                                          
                                                                                
PRODXX   EQU   *                                                                
         EJECT                                                                  
* ESTIMATE RESTRICTIONS                                                         
                                                                                
         CLC   RNUM,=C'I2'         I2 NO RESTIRICTIONS                          
         BE    ESTXX                                                            
         CLC   RNUM,=C'N2'         N2 NO RESTIRICTIONS                          
         BE    ESTXX                                                            
         CLC   RNUM,=C'RX'         RX NO RESTIRICTIONS                          
         BE    ESTXX                                                            
         CLC   RNUM,=C'RY'         RY NO RESTIRICTIONS                          
         BE    ESTXX                                                            
         CLC   RNUM,=C'D1'         D1 NO RESTIRICTIONS                          
         BE    ESTXX                                                            
         CLC   RNUM,=C'B1'         B1 NO RESTIRICTIONS                          
         BE    ESTXX                                                            
         CLC   RNUM,=C'BU'         BU NO RESTIRICTIONS                          
         BE    ESTXX                                                            
         CLC   RNUM,=C'DU'         DU NO RESTIRICTIONS                          
         BE    ESTXX                                                            
         CLC   RNUM,=C'SS'         SS NO RESTIRICTIONS                          
         BE    ESTXX                                                            
                                                                                
* AGENCY RESTRICTIONS                                                           
                                                                                
                                                                                
* AGY=OO                                                                        
         CLC   RAGY,=C'OO'         ,,FOR OO                                     
         BNE   OOESTX                                                           
         CLC   RNUM,=C'M2'         ,,FOR M2 REPORT                              
         BE    ESTXX                                                            
OOESTX   EQU   *                                                                
                                                                                
* AGENCY=H9                        .H9 POWER CODE                               
         CLC   RAGY,=C'H9'                                                      
         BE    *+14                                                             
         CLC   RAGY,=C'O0'                                                      
         BNE   H9ESTX                                                           
         CLC   RNUM,=C'M8'         .M8 HAS NO SOON RESTICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'M9'         .M9 HAS NO SOON RESTICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'D2'         .D2 HAS NO SOON RESTICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'D4'         .D4 HAS NO SOON RESTICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'D5'         .D5 HAS NO SOON RESTICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'RS'         .RS HAS NO SOON RESTICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'N5'         .N5 HAS NO SOON RESTICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'RN'         .RN HAS NO SOON RESTICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'RX'         .RX HAS NO SOON RESTICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'M2'         .M2 HAS NO SOON RESTICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'D1'         .D1 HAS NO SOON RESTICTIONS                  
         BE    ESTXX                                                            
H9ESTX   EQU   *                                                                
                                                                                
* AGENCY=GSTX                      .G7 POWER CODE                               
         CLC   RAGY,=C'G7'                                                      
         BNE   G7ESTX                                                           
         CLC   RNUM,=C'DU'         .DU HAS NO SOON RESTICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'D1'         .D1 HAS NO SOON RESTICTIONS                  
         BE    ESTXX                                                            
G7ESTX   EQU   *                                                                
                                                                                
* - AGENCY=DA                                                                   
         CLC   RAGY,=C'DA'         .DA POWER CODE                               
         BNE   EST10                                                            
         CLC   RNUM,=C'RS'         .RS HAS NO SOON RESTRICTIONS                 
         BE    ESTXX                                                            
         CLC   RNUM,=C'RY'         .RY HAS NO SOON RESTRICTIONS                 
         BE    ESTXX                                                            
         CLC   RNUM,=C'D5'         .D5 HAS NO SOON RESTRICTIONS                 
         BE    ESTXX                                                            
         CLC   RNUM,=C'D4'         .D4 HAS NO SOON RESTRICTIONS                 
         BE    ESTXX                                                            
         CLC   RNUM,=C'RN'         .RN HAS NO SOON RESTRICTIONS                 
         BE    ESTXX                                                            
         CLC   RNUM,=C'RX'         .RX HAS NO SOON RESTRICTIONS                 
         BE    ESTXX                                                            
         CLC   RNUM,=C'M2'         .M2 HAS NO SOON RESTRICTIONS                 
         BE    ESTXX                                                            
         CLC   RNUM,=C'D2'         D2 NO RESTIRICTIONS                          
         BE    ESTXX                                                            
                                                                                
* - AGENCY=WI                                                                   
EST10    CLC   RAGY,=C'WI'         WI POWER CODE                                
         BNE   EST12                                                            
         CLC   RNUM,=C'K1'                                                      
         BE    EST12               K1 RESTRICTIONS APPLY!                       
         CLC   RNUM,=C'D4'         OTHERWISE, ONLY 1 EST FOR FOLLOWING          
         BE    EST11               REPORTS                                      
         CLC   RNUM,=C'D6'                                                      
         BE    EST11                                                            
         CLC   RNUM,=C'J6'                                                      
         BE    EST11                                                            
         CLC   RNUM,=C'D7'                                                      
         BNE   ESTXX               THE REST HAVE NO RESTRICTIONS                
EST11    TM    MKTSAVE,X'08'       IS IT FOR ONE MARKET                         
         BO    ESTXX               YES, NO RESTRICTIONS                         
                                                                                
* - AGENCY=BA                                                                   
EST12    CLC   RAGY,=C'BA'         ...BATO                                      
         BNE   EST13                                                            
         CLC   RNUM,=C'M8'         ...M8 NO SOON RESTIRICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'M2'         ...M2 NO SOON RESTRICTIONS                   
         BE    ESTXX                                                            
         CLC   RNUM,=C'M9'         ...M9 NO SOON RESTRICTIONS                   
         BE    ESTXX                                                            
                                                                                
* - AGENCY=TB                                                                   
EST13    CLC   RAGY,=C'TB'         ...ZOTO                                      
         BNE   EST14                                                            
         CLC   RNUM,=C'M2'         ...M2 NO SOON RESTIRICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'M8'         ...M8 NO SOON RESTIRICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'M9'         ...M9 NO SOON RESTIRICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'D2'         ...D2 NO SOON RESTIRICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'D4'         ...D4 NO SOON RESTIRICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'D5'         ...D5 NO SOON RESTIRICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'D8'         ...D8 NO SOON RESTIRICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'DN'         ...DN NO SOON RESTIRICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'N5'         ...N5 NO SOON RESTIRICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'RS'         ...RS NO SOON RESTIRICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'RN'         ...RN NO SOON RESTIRICTIONS                  
         BE    ESTXX                                                            
*                                                                               
* - AGENCY=TY                                                                   
EST14    CLC   RAGY,=C'TY'         ...TY                                        
         BNE   EST15                                                            
         CLC   RNUM,=C'M8'         ...M8 NO SOON RESTIRICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'M9'         ...M9 NO SOON RESTRICTIONS                   
         BE    ESTXX                                                            
         CLC   RNUM,=C'D2'         ...D2 NO SOON RESTRICTIONS                   
         BE    ESTXX                                                            
         CLC   RNUM,=C'D3'         ...D3 NO SOON RESTRICTIONS                   
         BE    ESTXX                                                            
         CLC   RNUM,=C'D4'         ...D4 NO SOON RESTRICTIONS                   
         BE    ESTXX                                                            
         CLC   RNUM,=C'D5'         ...D5 NO SOON RESTRICTIONS                   
         BE    ESTXX                                                            
         CLC   RNUM,=C'D7'         ...D7 NO SOON RESTRICTIONS                   
         BE    ESTXX                                                            
         CLC   RNUM,=C'DX'         ...DX NO SOON RESTRICTIONS                   
         BE    ESTXX                                                            
         CLC   RNUM,=C'D8'         ...D8 NO SOON RESTRICTIONS                   
         BE    ESTXX                                                            
         CLC   RNUM,=C'M2'         ...M2 NO SOON RESTRICTIONS                   
         BE    ESTXX                                                            
         CLC   RNUM,=C'M3'         ...M3 NO SOON RESTRICTIONS                   
         BE    ESTXX                                                            
         CLC   RNUM,=C'M4'         ...M4 NO SOON RESTRICTIONS                   
         BE    ESTXX                                                            
         CLC   RNUM,=C'm4'         ...m4 NO SOON RESTRICTIONS                   
         BE    ESTXX                                                            
*                                                                               
* - AGENCY=EM                                                                   
EST15    CLC   RAGY,=C'EM'         ...EM                                        
         BNE   EST16                                                            
         CLC   RNUM,=C'D2'         ...D2 NO SOON RESTIRICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'D4'         ...D4 NO SOON RESTRICTIONS                   
         BE    ESTXX                                                            
         CLC   RNUM,=C'D5'         ...D5 NO SOON RESTIRICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'D8'         ...D8 NO SOON RESTRICTIONS                   
         BE    ESTXX                                                            
         CLC   RNUM,=C'RS'         ...RS NO SOON RESTIRICTIONS                  
         BE    ESTXX                                                            
         CLC   RNUM,=C'RN'         ...RN NO SOON RESTRICTIONS                   
         BE    ESTXX                                                            
*                                                                               
* AGENCY=MKTO                                                                   
EST16    CLC   RAGY,=C'MI'            MKTO                                      
         BNE   EST17                                                            
         CLC   RNUM,=C'RY'            RY HAS NO SOON RESTRICTIONS               
         BE    ESTXX                                                            
         CLC   RNUM,=C'RX'            RX HAS NO SOON RESTRICTIONS               
         BE    ESTXX                                                            
         CLC   RNUM,=C'RS'            RS HAS NO SOON RESTRICTIONS               
         BE    ESTXX                                                            
         CLC   RNUM,=C'RN'            RN HAS NO SOON RESTRICTIONS               
         BE    ESTXX                                                            
         CLC   RNUM,=C'D8'            D8 HAS NO SOON RESTRICTIONS               
         BE    ESTXX                                                            
         CLC   RNUM,=C'D2'            D2 HAS NO SOON RESTRICTIONS               
         BE    ESTXX                                                            
         CLC   RNUM,=C'D1'            D1 HAS NO SOON RESTRICTIONS               
         BE    ESTXX                                                            
         TM    ESTSAVE,X'80'          ALL OTHERS ALLOW EST FILTERS              
         BO    ESTXX                                                            
                                                                                
* AGENCY=OMDTO                                                                  
EST17    CLC   RAGY,=C'OU'            OMDTO                                     
         BNE   EST18                                                            
         CLC   RNUM,=C'RY'            RY HAS NO SOON RESTRICTIONS               
         BE    ESTXX                                                            
         CLC   RNUM,=C'RX'            RX HAS NO SOON RESTRICTIONS               
         BE    ESTXX                                                            
         CLC   RNUM,=C'RS'            RS HAS NO SOON RESTRICTIONS               
         BE    ESTXX                                                            
         CLC   RNUM,=C'RN'            RN HAS NO SOON RESTRICTIONS               
         BE    ESTXX                                                            
         CLC   RNUM,=C'D8'            D6 HAS NO SOON RESTRICTIONS               
         BE    ESTXX                                                            
         CLC   RNUM,=C'D2'            D2 HAS NO SOON RESTRICTIONS               
         BE    ESTXX                                                            
         CLC   RNUM,=C'D1'            D1 HAS NO SOON RESTRICTIONS               
         BE    ESTXX                                                            
         TM    ESTSAVE,X'80'          ALL OTHERS ALLOW EST FILTERS              
         BO    ESTXX                                                            
                                                                                
* AGENCY=WT                                                                     
EST18    CLC   RAGY,=C'WT'            WT                                        
         BNE   EST20                                                            
         TM    ESTSAVE,X'80'          ALLOWS EST FILTERS                        
         BO    ESTXX                                                            
                                                                                
EST20    CLC   RAGY,=C'MW'         MWTTO                                        
         BNE   EST22                                                            
         CLC   RNUM,=C'RY'         RY NO ESTIMATE RESTIRICTIONS                 
         BE    ESTXX                                                            
                                                                                
EST22    CLC   RAGY,=C'YR'         YRTMOD                                       
         BNE   EST24                                                            
         CLC   RNUM,=C'M2'         FOR M2                                       
         BE    ESTXX               NO RESTRICTIONS                              
         CLC   RNUM,=C'M9'         FOR M9                                       
         BE    ESTXX               NO RESTRICTIONS                              
         CLC   RNUM,=C'RN'         FOR RN                                       
         BE    ESTXX               NO RESTRICTIONS                              
         CLC   RNUM,=C'D4'         FOR D4                                       
         BE    ESTXX               NO RESTRICTIONS                              
         CLC   RNUM,=C'RS'         FOR RS                                       
         BE    ESTXX               NO RESTRICTIONS                              
                                                                                
                                                                                
EST24    CLC   RAGY,=C'TY'         TBSNY                                        
         BNE   EST26                                                            
         CLC   RNUM,=C'D1'         FOR D1                                       
         BE    ESTXX               NO RESTRICTIONS                              
         CLC   RNUM,=C'DU'         FOR DU                                       
         BE    ESTXX               NO RESTRICTIONS                              
                                                                                
EST26    EQU   *                                                                
* - AGENCY=WR                                                                   
         CLC   RAGY,=C'WR'         WR POWER CODE                                
         BNE   EST28                                                            
         B     ESTXX               NO RESTRICTIONS                              
*                                                                               
EST28    EQU   *                                                                
* - AGENCY=MI                                                                   
         CLC   RAGY,=C'MI'         MI POWER CODE                                
         BNE   EST30                                                            
         CLI   RMED,C'T'           FOR MEDIA T                                  
         BNE   EST30                                                            
         CLC   RNUM,=C'D4'          D4                                          
         BE    ESTXX                                                            
         CLC   RNUM,=C'D5'          D5                                          
         BE    ESTXX                                                            
*                                                                               
EST30    EQU   *                                                                
         CLC   RAGY,=C'PT'         FOR AGENCY PTTOD                             
         BNE   EST32                                                            
         BE    ESTXX               NO EST RESTRICTIONS                          
                                                                                
*                                                                               
EST32    EQU   *                                                                
         CLC   RAGY,=C'DN'         FOR DN POWER CODE (KMVA,PJVA)                
         BNE   EST33                                                            
         BE    ESTXX               NO EST RESTRICTIONS                          
*                                                                               
EST33    EQU   *                                                                
         CLC   RAGY,=C'DV'         FOR DV MOROCH                                
         BNE   EST34                                                            
         BE    ESTXX               NO EST RESTRICTIONS                          
*                                                                               
EST34    EQU   *                                                                
         CLC   RAGY,=C'PB'         FOR PB POWER CODE                            
         BNE   EST36                                                            
         CLC   RNUM,=C'D5'         FOR D5 REPORT                                
         BNE   EST100                                                           
         BE    ESTXX               NO EST RESTRICTIONS                          
                                                                                
* - AGENCY=OU                                                                   
EST36    DS    0H                                                               
         CLC   RAGY,=C'OU'         OU POWER CODE                                
         BNE   EST37                                                            
         CLI   RMED,C'T'           FOR MEDIA T                                  
         BNE   EST38                                                            
         CLC   RNUM,=C'D5'          D5                                          
         BE    ESTXX                                                            
                                                                                
* - AGENCY=ON                                                                   
EST37    DS    0H                                                               
         CLC   RAGY,=C'ON'         ON POWER CODE                                
         BNE   EST38                                                            
         CLC   RNUM,=C'D2'          D2                                          
         BE    ESTXX                                                            
         CLC   RNUM,=C'D4'          D2                                          
         BE    ESTXX                                                            
         CLC   RNUM,=C'D5'          D5                                          
         BE    ESTXX                                                            
         CLC   RNUM,=C'D8'          D8                                          
         BE    ESTXX                                                            
         CLC   RNUM,=C'DN'          DN                                          
         BE    ESTXX                                                            
         CLC   RNUM,=C'N5'          N5                                          
         BE    ESTXX                                                            
         CLC   RNUM,=C'RN'          RN                                          
         BE    ESTXX                                                            
         CLC   RNUM,=C'RS'          RS                                          
         BE    ESTXX                                                            
                                                                                
* - AGENCY=JE                                                                   
EST38    DS    0H                                                               
         CLC   RAGY,=C'JT'         JT POWER CODE                                
         BE    EST39                                                            
         CLC   RAGY,=C'HY'         HY POWER CODE                                
         BE    *+14                                                             
         CLC   RAGY,=C'H0'         H0 POWER CODE                                
         BNE   EST100                                                           
         CLC   RNUM,=C'D8'         DISALLOW EST NO FOR H0'S D8                  
         BNE   *+12                                                             
         TM    ESTSAVE,X'10'       EST NO?                                      
         BO    ESTERR                                                           
*                                                                               
         CLC   RNUM,=C'M8'         M8                                           
         BE    ESTXX                                                            
EST39    CLC   RNUM,=C'D5'          D5                                          
         BE    ESTXX                                                            
         CLC   RNUM,=C'RN'          RN                                          
         BE    ESTXX                                                            
         CLC   RNUM,=C'RS'          RS                                          
         BE    ESTXX                                                            
                                                                                
                                                                                
* ESTIMATE = ALL                                                                
EST100   TM    ESTSAVE,X'02'          ALL INVALID FOR SOON REQUESTS             
         BO    ESTERR                                                           
                                                                                
                                                                                
* ESTIMATE FILTERS                                                              
                                                                                
* - FOLLOWING REPORTS HAVE NO FILTER RESTRICTIONS                               
         CLC   RNUM,=C'A2'                                                      
         BE    ESTXX                                                            
         CLC   RNUM,=C'AX'                                                      
         BE    ESTXX                                                            
         CLC   RNUM,=C'AB'                                                      
         BE    ESTXX                                                            
         CLC   RNUM,=C'Z5'                                                      
         BE    ESTXX                                                            
         CLC   RNUM,=C'Z7'                                                      
         BE    ESTXX                                                            
         CLC   RNUM,=C'K4'                                                      
         BE    ESTXX                                                            
         CLC   RNUM,=C'K5'                                                      
         BE    ESTXX                                                            
         CLC   RNUM,=C'KL'                                                      
         BE    ESTXX                                                            
                                                                                
                                                                                
* INDIVIDUAL REPORTS                                                            
         CLC   RNUM,=C'D1'            ..D1                                      
         BNE   EST101                                                           
         TM    ESTSAVE,X'80'          ..ALLOWS EST FILTERS                      
         BO    ESTXX                                                            
         CLI   ESTSAVE,X'21'          ..OR EST+NON-SPECIFIC PROD                
         BE    ESTXX                                                            
*                                                                               
EST101   DS    0H   **NOTE** THERE'S NO RESTR FOR PRD ON K1, SEE PROD00         
*ST101   CLC   RNUM,=C'K1'            K1 PRD VAL NOT SET STD PRDSAVE!           
*        BNE   EST110                                                           
*        CLI   ESTSAVE,X'21'          ..NNN=EST+NON-SPECIFIC PROD               
*        BNE   ESTERR                 (ONLY SINGLE EST ALLOWED)                 
                                                                                
                                                                                
EST110   EQU   *                                                                
                                                                                
* FOR ALL OTHERS ESTIMATE FILTERS ARE INVALID                                   
****     TM    ESTSAVE,X'F0'          FILTERS INVALID                           
****     BO    ESTERR                                                           
                                                                                
* - ESTIMATE RANGE                                                              
         TM    ESTSAVE,X'08'          IS IT RANGE                               
         BNO   ESTXX                                                            
***      CLC   RNUM,=C'M9'            FOR M9 INVALID                            
***      BE    ESTERR                                                           
***      CLC   RNUM,=C'D5'            FOR D5 INVALID                            
***      BE    ESTERR                                                           
***      CLC   RNUM,=C'RS'            FOR RS INVALID                            
***      BE    ESTERR                                                           
         CLC   RNUM,=C'DC'            FOR DC INVALID                            
         BE    ESTERR                                                           
         CLC   RNUM,=C'RZ'            FOR RZ INVALID                            
         BE    ESTERR                                                           
         CLC   RNUM,=C'K1'            FOR K1 INVALID                            
         BE    ESTERR                                                           
                                                                                
* FOR ALL OTHER REPORTS                                                         
* GET RS PROFILE FOR ESTIMATE SERIES RESTRICTIONS                               
         MVC   HALF,=C'SR'         GET SOON PROF                                
         XC    TEMP,TEMP                                                        
         MVC   TEMP+30(2),=C'SO'                                                
         MVC   TEMP+32(2),HALF                                                  
         MVC   TEMP+34(6),RAGY     AGY/MED/CLT                                  
         MVI   TEMP+40,C'*'                                                     
         MVC   TEMP+41(1),CLIOFFC                                               
*                                                                               
EST120   DS    0H                                                               
         GOTO1 GETPROF,DMCB,(0,TEMP+30),TEMP,DATAMGR                            
         CLI   TEMP,C'Y'                                                        
         BE    ESTXX                                                            
                                                                                
ESTERR   MVI   ROUTNUM,ESTNUM            SOON ERROR                             
         B     SOONERR                                                          
                                                                                
ESTXX    EQU   *                                                                
         EJECT                                                                  
*                                                                               
* MARKET RESTRICTIONS                                                           
                                                                                
         CLI   RMED,C'N'           FOR NETWORK                                  
         BE    MKTXX               NO RESTRICTIONS                              
                                                                                
* THESE REPORTS HAVE NO MARKET RESTRICTIONS                                     
         CLC   RNUM,=C'PF'         . PF                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'PM'         . PM                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'PR'         . PR                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'AB'         . AB                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'M8'         . M8                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'M9'         . M9                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'M4'         . M4                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'm4'         . m4                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'K4'         . K4                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'K5'         . K5                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'KL'         . KL                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'I2'         . I2                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'N2'         . N2                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'N5'         . N5                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'DN'         . DN                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'D2'         . D2                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'M2'         . M2                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'A2'         . A2                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'RN'         . RN                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'RX'         . RX                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'RS'         . RS                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'RY'         . RY                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'T9'         . T9                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'D6'         . D6                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'J6'         . J6                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'D1'         . D1                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'B1'         . B1                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'D5'         . D5                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'SS'         . SS                                         
         BE    MKTXX                                                            
         CLC   RNUM,=C'K1'         . K1                                         
         BE    MKTXX                                                            
                                                                                
                                                                                
                                                                                
                                                                                
*  - AGENCIES                                                                   
                                                                                
MKTAGYS  DS    0H                                                               
                                                                                
* AGENCY=DA,M2,NT,TB,EM,ON                                                      
         CLC   RAGY,=C'DA'         FOR AGENCY DSMO                              
         BE    MKT01C                                                           
         CLC   RAGY,=C'NT'         FOR AGENCY MMCTO                             
         BE    MKT01C                                                           
         CLC   RAGY,=C'TB'         FOR AGENCY ZOTO                              
         BE    MKT01C                                                           
         CLC   RAGY,=C'EM'         FOR AGENCY EM                                
         BE    MKT01B                                                           
         CLC   RAGY,=C'U#'         FOR AGENCY M2TO                              
         BE    MKT01B                                                           
         CLC   RAGY,=C'HY'         .AND HY                                      
         BE    MKT01B                                                           
         CLC   RAGY,=C'H0'         ... AND H0                                   
         BNE   MKT01E                                                           
MKT01B   CLC   RNUM,=C'D8'         D8 NO RESTRICTIONS                           
         BE    MKTXX                                                            
*                                                                               
MKT01C   CLC   RNUM,=C'D4'         D4 NO RESTRICTIONS                           
         BE    MKTXX                                                            
*                                                                               
* AGENCY=M1                                                                     
MKT01E   CLC   RAGY,=C'M1'         FOR AGENCY M1                                
         BNE   MKT03                                                            
         CLC   RNUM,=C'M3'         M3 NO RESTRICTIONS                           
         BE    MKTXX                                                            
*                                                                               
* AGENCY=KA                                                                     
MKT03    CLC   RAGY,=C'KA'         FOR AGY KA (KETCHUM)                         
         BNE   MKT05                                                            
         CLC   RNUM,=C'D4'         D4                                           
         BNE   MKT05                                                            
         CLC   RPRO,=C'POL'        IF POL                                       
         BE    MKT100              LIMITED TO ONE MKT                           
         CLC   RPRO,=C'ALL'        IF ALL                                       
         BE    MKT100              LIMITED TO ONE MKT                           
         B     MKTXX                                                            
                                                                                
MKT05    DS    0H                                                               
         CLC   RAGY,=C'ON'         FOR AGENCY INNOCEAN CANADA                   
         BNE   MKT10                                                            
         CLC   RNUM,=C'D4'         D4 NO RESTRICTIONS                           
         BE    MKTXX                                                            
         CLC   RNUM,=C'D8'         D8 NO RESTRICTIONS                           
         BE    MKTXX                                                            
                                                                                
* AGENCY=MI                                                                     
MKT10    CLC   RAGY,=C'MI'         FOR AGY MI                                   
         BNE   MKT11                                                            
         CLC   RNUM,=C'M8'         M8                                           
         BE    MKTXX               NO MARKET RESTRICTIONS                       
         CLC   RNUM,=C'D8'         D8                                           
         BE    MKTXX               NO MARKET RESTRICTIONS                       
                                                                                
* AGENCY=OU                                                                     
MKT11    CLC   RAGY,=C'OU'         OMDTO                                        
         BNE   MKT12                                                            
         CLC   RNUM,=C'M8'         M8                                           
         BE    MKTXX               NO MARKET RESTRICTIONS                       
         CLC   RNUM,=C'D6'         D6                                           
         BE    MKTXX               NO MARKET RESTRICTIONS                       
                                                                                
* AGENCY=TY                                                                     
MKT12    CLC   RAGY,=C'TY'         FOR AGY TY                                   
         BNE   MKT14                                                            
         CLC   RNUM,=C'M8'         M8                                           
         BE    MKTXX               NO MARKET RESTRICTIONS                       
         CLC   RNUM,=C'M9'         M9                                           
         BE    MKTXX               NO MARKET RESTRICTIONS                       
         CLC   RNUM,=C'D2'         D2                                           
         BE    MKTXX               NO MARKET RESTRICTIONS                       
         CLC   RNUM,=C'D3'         D3                                           
         BE    MKTXX               NO MARKET RESTRICTIONS                       
         CLC   RNUM,=C'D4'         D4                                           
         BE    MKTXX               NO MARKET RESTRICTIONS                       
         CLC   RNUM,=C'D5'         D5                                           
         BE    MKTXX               NO MARKET RESTRICTIONS                       
         CLC   RNUM,=C'D7'         D7                                           
         BE    MKTXX               NO MARKET RESTRICTIONS                       
         CLC   RNUM,=C'DX'         DX                                           
         BE    MKTXX               NO MARKET RESTRICTIONS                       
         CLC   RNUM,=C'D8'         D8                                           
         BE    MKTXX               NO MARKET RESTRICTIONS                       
         CLC   RNUM,=C'M3'         M3                                           
         BE    MKTXX               NO MARKET RESTRICTIONS                       
         CLC   RNUM,=C'M4'         M4                                           
         BE    MKTXX               NO MARKET RESTRICTIONS                       
         CLC   RNUM,=C'm4'         m4                                           
         BE    MKTXX               NO MARKET RESTRICTIONS                       
                                                                                
* AGENCY = WI                                                                   
MKT14    EQU   *                                                                
         CLC   RAGY,=C'WI'         FOR WI                                       
         BNE   MKT16                                                            
         CLC   RNUM,=C'A2'         A2                                           
         BE    MKTXX                                                            
         CLC   RNUM,=C'D4'         D4                                           
         BE    MKTXX                                                            
         CLC   RNUM,=C'D6'         D6                                           
         BE    MKTXX                                                            
         CLC   RNUM,=C'D7'         D7                                           
         BE    MKTXX                                                            
         CLC   RNUM,=C'D1'         D1                                           
         BE    MKTXX                                                            
         CLC   RNUM,=C'M9'         M9                                           
         BE    MKTXX                                                            
         CLC   RNUM,=C'D3'         D3                                           
         BE    MKTXX                                                            
         CLC   RNUM,=C'M3'         M3                                           
         BE    MKTXX                                                            
*                                                                               
MKT16    EQU   *                                                                
         CLC   RAGY,=C'YR'         FOR YR AGENCY                                
         BNE   MKT20                                                            
         CLC   RNUM,=C'D4'         D4 NO MKT RESTRICTIONS                       
         BE    MKTXX                                                            
                                                                                
MKT20    EQU   *                                                                
                                                                                
*  - REPORTS                                                                    
                                                                                
         CLC   RNUM,=C'D8'         IF D8                                        
         BNE   MKTRP10                                                          
         TM    PROSAVE,X'04'       AND NOT SINGLE PRODUCT                       
         BO    MKTXX                                                            
         B     MKT100              LIMITED TO ONE MARKET                        
                                                                                
MKTRP10  CLC   RNUM,=C'D4'         IF D4                                        
         BNE   MKTRP15                                                          
         TM    PROSAVE,X'04'       AND SINGLE PRODUCT                           
         BO    MKTXX               OK                                           
                                                                                
MKTRP15  DS    0H                  SPARE                                        
                                                                                
                                                                                
                                                                                
MKTRP20  CLC   RNUM,=C'M9'         IF M9                                        
         BNE   MKTRP25                                                          
         TM    PROSAVE,X'04'       AND SINGLE PRODUCT                           
         BO    MKTXX               OK                                           
                                                                                
MKTRP25  CLC   RNUM,=C'M3'         IF M3                                        
         BNE   MKTRP30                                                          
         TM    PROSAVE,X'04'       AND SINGLE PRODUCT                           
         BO    MKTXX               OK                                           
                                                                                
MKTRP30  CLC   RNUM,=C'D3'         IF D3                                        
         BNE   MKTRP35                                                          
         TM    PROSAVE,X'04'       AND SINGLE PRODUCT                           
         BO    MKTXX               OK                                           
                                                                                
MKTRP35  CLC   RNUM,=C'DC'         IF DC                                        
         BNE   MKT100                                                           
         TM    PROSAVE,X'04'       AND SINGLE PRODUCT                           
         BO    MKTXX               OK                                           
                                                                                
                                                                                
MKT100   DS    0H                  MARKET RESTRICTIONS                          
         TM    MKTSAVE,X'08'          .SOON ONLY ALLOWS 1 MARKET                
         BO    MKTXX                                                            
         TM    STASAVE,X'04'          .OR ONE STATION                           
         BO    MKTXX                                                            
                                                                                
         MVI   ROUTNUM,MKTNUM      SET CURSOR                                   
         B     SOONERR                                                          
                                                                                
MKTXX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
* REQUEST DATE SOON RESTRICTIONS                                                
                                                                                
         CLC   RSTRD,=6X'40'                                                    
         BE    STENDXX                                                          
         CLC   RENDD,=6X'40'                                                    
         BE    STENDXX                                                          
*                                                                               
         GOTO1 PERVERT,DMCB,RSTRD,RENDD                                         
                                                                                
*                                                                               
* REPORT RESTRICTIONS                                                           
         CLC   RNUM,=C'Z5'          Z5                                          
         BE    STENDXX              HAS NO RESTRICTIONS                         
         CLC   RNUM,=C'Z7'          Z7                                          
         BE    STENDXX              HAS NO RESTRICTIONS                         
*                                                                               
STEND10  CLC   8(2,R1),=H'371'      EVERYBODY ELSE GETS A YEAR                  
         BNH   STENDXX                                                          
*                                                                               
* AGY=YR                                                                        
         CLC   RAGY,=C'YR'         YR AGENCY                                    
         BNE   SOONERR                                                          
         CLC   RNUM,=C'M9'         M9 REPORT                                    
         BNE   SOONERR                                                          
         CLC   8(2,R1),=H'403'     13 MONTHS                                    
         BH    SOONERR                                                          
         B     STENDXX                                                          
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
*&&DO                                                                           
STEND20  EQU   *                                                                
                                                                                
* DDS WILL HAVE 1 YEAR RESTRICTION                                              
         CLI   DDS,1                                                            
         BE    STEND10                                                          
                                                                                
         CLC   RAGY,=C'BN'          AGENCY BN (PHD) GETS 1 YEAR                 
         BE    STEND10                                                          
*                                                                               
         CLC   RAGY,=C'U#'          AGENCY M2TO GETS 1 YEAR                     
         BE    STEND10                                                          
*                                                                               
         CLC   RNUM,=C'A2'         ..A2                                         
         BE    STEND10                                                          
         CLC   RNUM,=C'AX'         ..AX                                         
         BE    STEND10                                                          
         CLC   RNUM,=C'RE'         ..RE                                         
         BE    STEND10                                                          
         CLC   RNUM,=C'K5'         ..K5                                         
         BE    STEND10                                                          
         CLC   RNUM,=C'KL'         ..KL                                         
         BE    STEND10                                                          
         CLC   RNUM,=C'DX'         ..DX                                         
         BE    STEND10                                                          
         CLC   RNUM,=C'SS'         ..SS                                         
         BE    STEND10                                                          
         CLC   RNUM,=C'AB'         ..AB                                         
         BNE   STEND20                                                          
* - AGENCIES                                                                    
                                                                                
* AGENCY=JT                                                                     
         CLC   RAGY,=C'JT'        JT                                            
         BE    STEND20A                                                         
         CLC   RAGY,=C'HY'        HY                                            
         BE    STEND20A                                                         
         CLC   RAGY,=C'H0'        H0                                            
         BNE   STEND20B                                                         
STEND20A CLC   8(2,R1),=H'186'     6 MONTHS                                     
         BNH   STENDXX                                                          
                                                                                
* AGENCY=MC                                                                     
STEND20B DS    0H                                                               
         CLC   RAGY,=C'MC'         MC                                           
         BNE   STEND20F                                                         
         CLC   RNUM,=C'D2'         D2                                           
         BNE   STEND20F                                                         
         CLC   8(2,R1),=H'371'     ..GET 1 YEAR                                 
         BNH   STENDXX                                                          
         B     STEND100                                                         
*                                                                               
* AGENCY=DSMO                                                                   
                                                                                
STEND20F CLC   RAGY,=C'DA'         DSMO AGENCY                                  
         BNE   STEND22                                                          
         CLC   RNUM,=C'D4'         FOR D4                                       
         BE    STEND21                                                          
         CLC   RNUM,=C'D2'         FOR D2                                       
         BNE   *+14                                                             
STEND21  CLC   8(2,R1),=H'186'     6 MONTHS                                     
         BNH   STENDXX                                                          
                                                                                
         CLC   RNUM,=C'DN'         FOR DN                                       
         BNE   STEND22                                                          
         CLC   8(2,R1),=H'128'    18 WEEKS                                      
         BNH   STENDXX                                                          
                                                                                
* AGENCY=BATO                                                                   
STEND22  CLC   RAGY,=C'BA'         BATO AGENCY                                  
         BNE   STEND29                                                          
         CLC   RNUM,=C'D2'         D2                                           
         BE    STEND28                                                          
         CLC   RNUM,=C'D4'         D4                                           
         BE    STEND28                                                          
         CLC   RNUM,=C'D5'         D5                                           
         BE    STEND28                                                          
         CLC   RNUM,=C'D8'         D8                                           
         BE    STEND28                                                          
         CLC   RNUM,=C'DN'         DN                                           
         BE    STEND28                                                          
         CLC   RNUM,=C'M2'         M2                                           
         BE    STEND28                                                          
         CLC   RNUM,=C'M8'         M8                                           
         BE    STEND28                                                          
         CLC   RNUM,=C'M9'         M9                                           
         BE    STEND28                                                          
         CLC   RNUM,=C'N5'         N5                                           
         BE    STEND28                                                          
         CLC   RNUM,=C'RS'         RS                                           
         BE    STEND28                                                          
         CLC   RNUM,=C'RN'         RN                                           
         BNE   STEND30                                                          
STEND28  CLC   8(2,R1),=H'91'     IF 13 WEEKS                                   
         BNH   STENDXX            OK                                            
                                                                                
         CLC   8(2,R1),=H'128'    ..IF 18 WEEKS                                 
         BNH   STEND28D                                                         
         CLC   RNUM,=C'M2'        .. OK FOR M2                                  
         BE    STENDXX                                                          
         CLC   RNUM,=C'M8'        .. OK FOR M8                                  
         BE    STENDXX                                                          
                                                                                
STEND28D CLC   8(2,R1),=H'185'    UP TO 26 WEEKS                                
         BH    STEND100                                                         
         TM    MKTSAVE,X'08'      ONLY ONE MARKET                               
         BO    STENDXX                                                          
         B     STEND100                                                         
                                                                                
* AGY=M1                                                                        
STEND29  CLC   RAGY,=C'M1'         M1                                           
         BNE   STEND30                                                          
         CLC   RNUM,=C'D2'         D2                                           
         BE    STEND29D                                                         
         CLC   RNUM,=C'M2'         M2                                           
         BE    STEND29D                                                         
         CLC   RNUM,=C'M3'         M3                                           
         BNE   STEND30                                                          
STEND29D CLC   8(2,R1),=H'371'     ..GET 1 YEAR                                 
         BNH   STENDXX                                                          
         B     STEND100                                                         
*                                                                               
* AGY=OMDTO                                                                     
STEND30  CLC   RAGY,=C'OU'         OMDTO                                        
         BNE   STEND34                                                          
         CLC   RNUM,=C'D4'         D4                                           
         BE    WEEKS34                                                          
         CLC   RNUM,=C'RN'         RN                                           
         BE    STEND32                                                          
         CLC   RNUM,=C'M9'         M9                                           
         BE    STEND32                                                          
         CLC   RNUM,=C'RX'         RX                                           
         BE    STEND32                                                          
         CLC   RNUM,=C'M8'         M8                                           
         BE    STEND32                                                          
         CLC   RNUM,=C'N5'         N5                                           
         BE    STEND32                                                          
         CLC   RNUM,=C'D8'         D8                                           
         BE    STEND32                                                          
         CLC   RNUM,=C'DN'         DN                                           
         BE    STEND32                                                          
         CLC   RNUM,=C'D5'         D5                                           
         BE    STEND32                                                          
         CLC   RNUM,=C'D2'         D2                                           
         BNE   STEND34                                                          
STEND32  CLC   8(2,R1),=H'186'     GET 26 WEEKS / 6 MONTHS                      
         BNH   STENDXX                                                          
         B     STEND100                                                         
                                                                                
* AGY=MCKIM                                                                     
STEND34  CLC   RAGY,=C'MI'         MCKIM AGENCY                                 
         BNE   STEND50                                                          
         CLC   RNUM,=C'DN'         DN                                           
         BE    STEND48                                                          
         CLC   RNUM,=C'D8'         D8                                           
         BE    STEND48                                                          
         CLC   RNUM,=C'M8'         M8                                           
         BE    STEND48                                                          
         CLC   RNUM,=C'RN'         RN                                           
         BE    STEND48                                                          
         CLC   RNUM,=C'RX'         RX                                           
         BE    STEND48                                                          
         CLC   RNUM,=C'D2'         D2                                           
         BE    STEND48                                                          
         CLC   RNUM,=C'D4'         D4                                           
         BE    STEND48                                                          
         CLC   RNUM,=C'D5'         D5                                           
         BE    STEND48                                                          
         CLC   RNUM,=C'N5'         N5                                           
         BNE   STEND50                                                          
STEND48  CLC   8(2,R1),=H'186'     GET 26 WEEKS / 6 MONTHS                      
         BNH   STENDXX                                                          
         B     STEND100                                                         
                                                                                
* AGENCY=SS                                                                     
STEND50  CLC   RAGY,=C'SS'         SVVA AGENCY                                  
         BNE   STEND52                                                          
         CLC   RNUM,=C'M2'         M2                                           
         BNE   STEND52                                                          
         CLC   8(2,R1),=H'371'     53 WEEKS                                     
         BNH   STENDXX                                                          
         B     STEND100                                                         
                                                                                
                                                                                
                                                                                
* AGENCY=YR                                                                     
STEND52  EQU   *                                                                
         CLC   RAGY,=C'YR'         YR AGENCY                                    
         BNE   STEND55                                                          
         CLC   RNUM,=C'D4'         D4 REPORT                                    
         BNE   STEND52A                                                         
         CLC   8(2,R1),=H'186'     6 MONTHS                                     
         BNH   STENDXX                                                          
         B     STENDERR                                                         
STEND52A CLC   RNUM,=C'M9'         M9 REPORT                                    
         BNE   STEND53                                                          
         CLC   8(2,R1),=H'403'     13 MONTHS                                    
         BNH   STENDXX                                                          
         B     STENDERR                                                         
STEND53  CLC   RNUM,=C'D5'         D5 REPORT                                    
         BE    STN53A                                                           
         CLC   RNUM,=C'N5'         N5 REPORT                                    
         BE    STN53A                                                           
         CLC   RNUM,=C'M2'         M2 REPORT                                    
         BNE   STEND55                                                          
STN53A   CLC   8(2,R1),=H'120'     17 WEEKS                                     
         BNH   STENDXX                                                          
         B     STENDERR                                                         
                                                                                
STEND55  EQU   *                                                                
         CLC   RAGY,=C'MW'         MW AGENCY                                    
         BNE   STEND56                                                          
         CLC   RNUM,=C'D2'         D2 REPORT                                    
         BNE   STEND56                                                          
         CLC   8(2,R1),=H'134'     19 WEEKS                                     
         BH    STENDERR                                                         
         B     STENDXX                                                          
                                                                                
STEND56  CLC   RAGY,=C'TB'         FOR ZOTO                                     
         BNE   STEND57                                                          
*                                                                               
         CLC   =C'M2',RNUM         FOR M2                                       
         BE    STEND56D                                                         
         CLC   =C'M8',RNUM         FOR M8                                       
         BE    STEND56D                                                         
         CLC   =C'M9',RNUM         FOR M9                                       
         BE    STEND56D                                                         
         CLC   =C'D2',RNUM         FOR D2                                       
         BE    STEND56D                                                         
         CLC   =C'D4',RNUM         FOR D4                                       
         BE    STEND56D                                                         
         CLC   =C'D5',RNUM         FOR D5                                       
         BE    STEND56D                                                         
         CLC   =C'D8',RNUM         FOR D8                                       
         BE    STEND56D                                                         
         CLC   =C'DN',RNUM         FOR DN                                       
         BE    STEND56D                                                         
         CLC   =C'RS',RNUM         FOR RS                                       
         BE    STEND56D                                                         
         CLC   =C'N5',RNUM         FOR N5                                       
         BE    STEND56D                                                         
         CLC   =C'RN',RNUM         FOR RN                                       
         BE    STEND56D                                                         
         CLC   =C'RX',RNUM         FOR RX                                       
         BE    STEND56D                                                         
         B     STEND100                                                         
STEND56D CLC   8(2,R1),=H'163'     23 WEEKS                                     
         BH    STENDERR                                                         
         B     STENDXX             OK                                           
                                                                                
STEND57  CLC   RAGY,=C'DN'         FOR 'DN', (AGY KMVA,PJVA ETC)                
         BE    STEND58                                                          
         CLC   RAGY,=C'WT'         WITO AGENCY                                  
         BNE   STEND59                                                          
         CLC   RNUM,=C'D4'         D4 REPORT                                    
         BE    STEND58                                                          
         CLC   RNUM,=C'D6'         D6 REPORT                                    
         BNE   STEND59                                                          
STEND58  CLC   8(2,R1),=H'183'     26 WEEKS                                     
         BH    STENDERR                                                         
         B     STENDXX             NO PRODUCT RESTRICTIONS                      
                                                                                
STEND59  EQU   *                                                                
         CLC   RAGY,=C'UB'         FOR CARME                                    
         BNE   STEND60                                                          
         CLC   =C'M9',RNUM         FOR M9                                       
         BNE   STEND60                                                          
         CLC   =C'PKO',RCLI        AND CLI=PKO                                  
         BNE   STEND60                                                          
         CLC   8(2,R1),=H'371'     1 YEAR                                       
         BH    STENDERR                                                         
         B     STENDXX             OK                                           
                                                                                
STEND60  EQU   *                                                                
         CLC   RAGY,=C'H9'         FOR H9                                       
         BE    *+14                                                             
         CLC   RAGY,=C'O0'         FOR O0                                       
         BNE   STEND62                                                          
*                                                                               
         CLC   =C'M8',RNUM         FOR M8                                       
         BE    STEND61                                                          
         CLC   =C'M9',RNUM         FOR M9                                       
         BE    STEND61                                                          
         CLC   =C'D2',RNUM         FOR D2                                       
         BE    STEND61                                                          
         CLC   =C'D4',RNUM         FOR D4                                       
         BE    STEND61                                                          
         CLC   =C'D5',RNUM         FOR D5                                       
         BE    STEND61                                                          
         CLC   =C'RS',RNUM         FOR RS                                       
         BE    STEND61                                                          
         CLC   =C'N5',RNUM         FOR N5                                       
         BE    STEND61                                                          
         CLC   =C'RN',RNUM         FOR RN                                       
         BE    STEND61                                                          
         CLC   =C'RX',RNUM         FOR RX                                       
         BE    STEND61                                                          
         CLC   =C'M2',RNUM         FOR M2                                       
         BE    STEND61                                                          
         B     STEND100                                                         
STEND61  CLC   8(2,R1),=H'155'     22 WEEKS                                     
         BH    STENDERR                                                         
         B     STENDXX             OK                                           
                                                                                
STEND62  EQU   *                   AGY=VKTO                                     
         CLC   RAGY,=C'VB'                                                      
         BNE   STEND64                                                          
         CLC   =C'D2',RNUM                                                      
         BE    STEND63                                                          
         CLC   =C'D5',RNUM                                                      
         BE    STEND63                                                          
         CLC   =C'N5',RNUM                                                      
         BNE   STEND100                                                         
STEND63  CLC   8(2,R1),=H'147'     21 WEEKS                                     
         BH    STENDERR                                                         
         B     STENDXX             OK                                           
*                                                                               
STEND64  EQU   *                   MMCTO                                        
         CLC   RAGY,=C'NT'                                                      
         BNE   STEND67                                                          
*                                                                               
         CLC   =C'D2',RNUM         FOR D2                                       
         BE    STEND65                                                          
         CLC   =C'D4',RNUM         FOR D4                                       
         BE    STEND65                                                          
         CLC   =C'D5',RNUM         FOR D5                                       
         BE    STEND65                                                          
         CLC   =C'N5',RNUM         FOR N5                                       
         BE    STEND65                                                          
         CLC   =C'M2',RNUM         FOR M2                                       
         BE    STEND65                                                          
         CLC   =C'M4',RNUM         FOR M4                                       
         BE    STEND65                                                          
         CLC   =C'M8',RNUM         FOR M8                                       
         BE    STEND65                                                          
         CLC   =C'M9',RNUM         FOR M9                                       
         BNE   STEND100                                                         
STEND65  CLC   8(2,R1),=H'371'     1 YEAR                                       
         BH    STENDERR                                                         
         B     STENDXX             OK                                           
*                                                                               
STEND67  EQU   *                   M2TO                                         
                                                                                
STEND100 CLC   =C'K1',RNUM         K1 (INITIALLY) VERY RESTRICTED               
         BNE   STEND110                                                         
         CLC   8(2,R1),=H'28'      4 WEEKS                                      
         BH    STENDERR                                                         
         B     STENDXX                                                          
STEND110 CLC   8(2,R1),=H'104'     OTHERS GET A QUARTER                         
         BNH   STENDXX                                                          
*&&                                                                             
*                                                                               
*                                                                               
*                                                                               
STENDERR MVI   ROUTNUM,STENDNUM                                                 
         B     SOONERR                                                          
*                                                                               
*                                                                               
STENDXX  EQU   *                                                                
                                                                                
* NO MORE CHECKS                                                                
                                                                                
SOONXX   B     XIT                                                              
                                                                                
*                                                                               
SOONERR  MVC   FERN,=AL2(7)                                                     
         LA    R1,CURSTBL         MATCH REQ NUM WITH TABLE                      
CHKREQA  CLC   RNUM,0(R1)                                                       
         BE    CHKREQB                                                          
         LA    R1,10(R1)                                                        
         CLI   0(R1),X'FF'         END OF TABLE                                 
         BE    CHKREQ0             YES                                          
         B     CHKREQA             NO                                           
CHKREQB  ZIC   RE,ROUTNUM          INDEX INTO CURSTBL                           
         LA    R1,2(R1)            BUMP OVER REP ID                             
         AR    R1,RE               POINT TO TABLE REQ ID                        
         MVC   ROUTNUM,0(R1)       SET REQ ID TO ROUTNUM                        
*                                                                               
CHKREQ0  LA    R0,24                         SEARCH REQ MAP TABLE               
         LA    R1,LREQMAP                                                       
CHKREQ1  CLI   0(R1),127                                                        
         BE    CHKREQ2                                                          
         CLC   ROUTNUM,0(R1)                                                    
         BE    CHKREQ3                                                          
         LA    R1,3(R1)                                                         
         BCT   R0,CHKREQ1                                                       
CHKREQ2  LA    R1,LREQMAP                    NOT IN TBL POSN TO 1ST FLD         
CHKREQ3  MVC   HALF,1(R1)                                                       
         LR    R6,R3                                                            
         AH    R6,HALF                                                          
CHKREQ4  ST    R6,FADR                       POSN CURSOR TO ROUTNUM FLD         
         B     SOONXX                                                           
         EJECT                                                                  
*                                                                               
CLINUM   EQU   0                                                                
PRODNUM  EQU   1                                                                
ESTNUM   EQU   2                                                                
MKTNUM   EQU   3                                                                
STENDNUM EQU   4                                                                
         EJECT                                                                  
* LIMITED ACCESS FOR SIGN-ON IDS                                                
* READ CONTROL FILE                                                             
* PASS R4 WHICH POINTS TO CT ELEM BACK TO CALLER                                
*                                                                               
CHKID    NTR1                                                                   
         LA    R4,SPTREC                                                        
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,USRID                                                    
         GOTO1 DATAMGR,DMCB,(0,DMREAD),CTFILE,(R4),(R4)                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,28(R4)                                                        
CHKID05  CLI   0(R4),X'02'                                                      
         BE    CHKID10                                                          
         ZIC   RE,1(R4)                                                         
         CLI   0(R4),2                                                          
         BNH   *+6                                                              
         DC    H'0'                                                             
         AR    R4,RE                                                            
         B     CHKID05                                                          
         DROP  R4                                                               
         USING CTDSCD,R4                                                        
CHKID10  XIT1  REGS=(R4)                                                        
         DROP  R4                                                               
DMREAD   DC    CL8'DMREAD'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
         EJECT                                                                  
* GENERAL ROUTINE                                                               
* REQUEST DATE SOON RESTRICTIONS                                                
                                                                                
DATECHK  NTR1                                                                   
         CLC   RSTRD,=6X'40'                                                    
         BE    STENDXX                                                          
         CLC   RENDD,=6X'40'                                                    
         BE    STENDXX                                                          
                                                                                
         GOTO1 PERVERT,DMCB,RSTRD,RENDD                                         
         CLC   8(2,R1),HALF                                                     
         BH    NOTOK                                                            
         B     OK                                                               
*                                                                               
OK       SR    RE,RE                                                            
*                                                                               
NOTOK    LTR   RE,RE                                                            
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* TABLE OF REPORT IDS AND REQ TABLE IDS FROM SPREQ00                            
* NEEDED TO POSITION CURSOR TO CORRECT SCREEN FIELD                             
*  CL2=REPORT ID, XL5=CLI,PROD,EST,MKT,ST-END, XL3=SPARE                        
CURSTBL  DS    0H                                                               
         DC    C'PF',X'0203050609',X'000000'                                    
         DC    C'N5',X'025D050009',X'000000'                                    
         DC    C'RN',X'0200050009',X'000000'                                    
         DC    C'I3',X'0204055E09',X'000000'                                    
         DC    C'KA',X'0204055E09',X'000000'                                    
         DC    C'DN',X'0200050009',X'000000'                                    
         DC    C'I2',X'0204055E09',X'000000'                                    
         DC    C'RS',X'025D055E09',X'000000'                                    
         DC    C'T9',X'025D055E09',X'000000'                                    
         DC    C'M2',X'025D055E09',X'000000'                                    
         DC    C'M3',X'025D055E09',X'000000'                                    
         DC    C'M4',X'025D055E09',X'000000'                                    
         DC    C'm4',X'025D055E09',X'000000'      lower case m4                 
         DC    C'M8',X'0200055E09',X'000000'                                    
         DC    C'M9',X'025D055E09',X'000000'                                    
         DC    C'K4',X'02E4055E09',X'000000'                                    
         DC    C'DC',X'025D055E09',X'000000'                                    
         DC    C'D4',X'025D055E09',X'000000'                                    
         DC    C'D2',X'025D055E09',X'000000'                                    
         DC    C'D3',X'025D055E09',X'000000'                                    
         DC    C'D5',X'025D055E09',X'000000'                                    
         DC    C'D6',X'025D055E09',X'000000'                                    
         DC    C'J6',X'025D055E09',X'000000'                                    
         DC    C'D8',X'025D055E09',X'000000'                                    
         DC    C'DR',X'025D055E09',X'000000'                                    
         DC    C'A2',X'025D055E09',X'000000'                                    
         DC    C'AB',X'025D055E09',X'000000'                                    
         DC    C'BA',X'0203000009',X'000000'                                    
         DC    C'K5',X'02E4055E09',X'000000'                                    
         DC    C'KL',X'02E4055E09',X'000000'                                    
         DC    C'DD',X'025D055E09',X'000000'                                    
         DC    C'D7',X'025D055E09',X'000000'                                    
         DC    C'D1',X'025D055E09',X'000000'                                    
         DC    C'DU',X'025D050009',X'000000'                                    
         DC    C'PX',X'025D050009',X'000000'                                    
         DC    C'AX',X'025D055E09',X'000000'                                    
         DC    C'DL',X'0200050009',X'000000'                                    
         DC    C'RZ',X'025D055E09',X'000000'                                    
         DC    C'N2',X'0204050009',X'000000'                                    
         DC    C'MY',X'0203000000',X'000000'                                    
         DC    C'J1',X'0000000009',X'000000'                                    
         DC    C'DX',X'025D055E09',X'000000'                                    
         DC    C'C1',X'0200005E00',X'000000'                                    
         DC    C'MG',X'025D055E09',X'000000'                                    
         DC    C'RY',X'025D055E09',X'000000'                                    
         DC    C'RX',X'0200050009',X'000000'                                    
         DC    C'SY',X'0200050000',X'000000'                                    
         DC    C'I6',X'0200000009',X'000000'                                    
***      DC    C'IL',X'0204055E09',X'000000'                                    
         DC    C'MN',X'0204050609',X'000000'                                    
         DC    C'YT',X'0204050009',X'000000'                                    
         DC    C'C2',X'0200050000',X'000000'                                    
         DC    C'SS',X'0203050009',X'000000'                                    
         DC    C'K1',X'02DB055E09',X'000000'                                    
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE SPREQSAVE                                                      
       ++INCLUDE SPREQTEMP                                                      
       ++INCLUDE FLDIND                                                         
         SPACE 2                                                                
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
       ++INCLUDE SPREQFFBD                                                      
       ++INCLUDE DDOFFICED                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
       ++INCLUDE NEDDEQUS                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'189SPREQ05   10/14/16'                                      
         END                                                                    
