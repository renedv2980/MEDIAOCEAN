*          DATA SET NEMED04T   AT LEVEL 001 AS OF 03/06/00                      
*          DATA SET NEMED04    AT LEVEL 044 AS OF 07/09/99                      
*PHASE T31E04A,+0                                                               
*                                                                               
         TITLE 'NEMED04 - EDIT FOR THE SCHEDULE EFFICIENCY ANALYSIS'            
***********************************************************************         
* NEMED04(T31E04)  -EDITS NETWORK SCHEDULE EFFICIENCY ANALYSIS REPORT *         
*                   SCREEN                                            *         
*    INPUT    -LOCATION OF THE GEND SPOOL DSECT                       *         
*    OUTPUT   -NETBLOCK -THE BLOCK USED TO READ THE NETWORK FILE      *         
*                        (FIELDS FILLED BY NETIO)                     *         
*    GLOBALS  -R2-POINTS TO CURRENT FIELD ON THE SCREEN               *         
*    CALLS TO: NVVALID(VALIDATION ROUTINES)                           *         
***********************************************************************         
         PRINT NOGEN                                                            
NEMED04  CSECT                                                                  
         NMOD1 0,**SLED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1                                                       
         ST    R7,NBADEM                                                        
         USING NDDEMBLK,R7                                                      
         EJECT                                                                  
         SPACE 2                                                                
***********************INITIALIZE NETBLOCK*****************************         
*              ASSUMES NETBLOCK IS ALREADY INITIALIZED                          
*              DONE BY CALL TO NVAGY OR NVAGYOUT                                
         SPACE 1                                                                
         MVI   NBQINIT,0           DO ALL THE VALIDATIONS EVERY TIME            
*                                  NEEDED IF ALREADY VALIDATED BITS NOT         
*                                  USED                                         
         LA    R2,SPLCLIH          CLIENT                                       
         NETGO NVCLI,DMCB,SPLCLIN  EXPAND                                       
         OI    SPLCLINH+6,X'80'    TRANSMIT IT                                  
         L     RE,NBAIO                                                         
         USING CLTHDR,RE                                                        
         LA    RE,CLIST                                                         
         L     RF,ANETWS4                                                       
         LA    RF,2000(RF)         BUMP                                         
         LA    R1,880                                                           
         MOVE  ((RF),(R1)),(RE)    SAVE CLIST                                   
*                                                                               
         LA    R2,SPLPROH          PRODUCT                                      
         NETGO NVPRD,DMCB,SPLPRON  EXPAND                                       
         OI    SPLPRONH+6,X'80'    TRANSMIT IT                                  
*                                                                               
         LA    R2,SPLESTH          ESTIMATE                                     
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK                                   
         NETGO NVDELHOM,DMCB,NDDEMOS                                            
         OI    SPLESTNH+6,X'80'    TRANSMIT IT                                  
         L     R3,NBAIO                                                         
         USING ESTHDR,R3                                                        
         TRT   SPLEST(8),TAB                                                    
         BZ    NODEMS                                                           
         MVC   NDDEMOS(60),EDEMLST                                              
         MVC   NDDEMOS+60(3),EDEM21                                             
NODEMS   EQU   *                                                                
         DROP  R3                                                               
*                                                                               
         MVI   FTERMFLG,1                                                       
*                                                                               
         LA    R2,SPLNETH          NETWORK                                      
         NETGO NVNETALL,DMCB         EXPAND                                     
*                                                                               
         LA    R2,SPLPAKH          PACKAGE                                      
         NETGO NVPAKLOK,DMCB,SPLPAKN  EXPAND                                    
         OI    SPLPAKNH+6,X'80'    TRANSMIT IT                                  
         USING NPRECD,R3                                                        
         MVC   NBUNCODE,NPAKUNCD                                                
         DROP  R3                                                               
*                                                                               
         LA    R2,SPLFVH                                                        
         CLI   SPLFV,C'V'                                                       
         BE    CT                                                               
         CLI   SPLFV,C'P'                                                       
         BE    CT                                                               
ERR      MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
CT       LA    R2,SPLCTH                                                        
         CLI   SPLCT,C'P'                                                       
         BE    GD                                                               
         CLI   SPLCT,C'U'                                                       
         BE    GD                                                               
         B     ERR                                                              
*                                                                               
GD       LA    R2,SPLGDH                                                        
         CLI   SPLGD,C'Y'                                                       
         BE    DP                                                               
         CLI   SPLGD,C'N'                                                       
         BE    DP                                                               
         CLI   5(R2),0                                                          
         BNE   ERR                                                              
         MVI   SPLGD,C'N'                                                       
         OI    SPLGDH+6,X'80'                                                   
*                                                                               
DP       LA    R2,SPLDPTH          DAYPART                                      
         NETGO NVDPTALL,DMCB,SPLDPTN EXPAND                                     
         OI    SPLDPTNH+6,X'80'    TRANSMIT IT                                  
*                                                                               
         LA    R2,SPLSDTH                                                       
         NETGO NVSTRDAT,DMCB       START DATE(OPTIONAL)                         
*                                                                               
         LA    R2,SPLEDTH          END DATE(OPTIONAL).ALSO INSURES THAT         
         NETGO NVENDDAT,DMCB       THE END DATE IS NOT BEFORE THE START         
*                                  ONE.                                         
         LA    R2,SPLDEMOH         DEMOS                                        
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
*                                                                               
         OI    NBINDS,X'80'        DEFAULT IS TO EQUIVALENCE OVERRIDES          
         LA    R2,SPLOPTH                    OPTIONS                            
         CLI   5(R2),0                                                          
         BE    ENDED                                                            
         L     R3,ANETWS2                                                       
         GOTO1 SCANNER,DMCB,SPLOPTH,(5,0(R3))                                   
         ZIC   R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BZ    ENDED                                                            
OPT00    CLC   =C'EQU',12(R3)              EQUIVALENCE OVERRIDES                
         BNE   OPT10                                                            
         CLI   22(R3),C'N'                                                      
         BNE   OPT100                                                           
         NI    NBINDS,X'FF'-X'80'          DON'T EQUIV OVERRIDES                
         B     OPT100                                                           
OPT10    CLC   =C'PRIMP',12(R3)            INCREASE PRECISION                   
         BNE   OPT20                                                            
         OI    NBINDS,X'40'                                                     
         B     OPT100                                                           
OPT20    DS    0H                                                               
         CLI   12(R3),C'$'         DOLLAR OPTION                                
         BNE   OPT30                                                            
         OI    NBINDS,X'10'                                                     
         B     OPT100                                                           
OPT30    DS    0H                                                               
         CLC   =C'CAB',12(R3)                                                   
         BNE   OPT40                                                            
         MVI   NBPREOPT,C'Y'                                                    
         MVI   NBHUNOPT,C'Y'                                                    
         B     OPT100                                                           
OPT40    DS    0H                                                               
         B     EDERR                                                            
*                                                                               
OPT100   LA    R3,32(R3)                                                        
         BCT   R1,OPT00                                                         
*                                                                               
ENDED    LA    R2,SPLCLIH          NORMAL END OF EDIT                           
         XIT1  REGS=(R2)                                                        
EDERR    MVI   ERROR,INVALID                                                    
         GOTO1 ERREX,DMCB                                                       
TAB      DC    107X'00'                                                         
         DC    X'01'                                                            
         DC    85X'00'                                                          
         DC    9X'01'                                                           
         DC    7X'00'                                                           
         DC    9X'01'                                                           
         DC    8X'00'                                                           
         DC    8X'01'                                                           
         DC    22X'00'                                                          
         EJECT                                                                  
DEMOS    DSECT                                                                  
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDF4D                                                       
       ++INCLUDE NEGENPACK                                                      
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001NEMED04T  03/06/00'                                      
         END                                                                    
