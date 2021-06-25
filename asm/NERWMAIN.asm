*          DATA SET NERWMAIN   AT LEVEL 010 AS OF 08/10/00                      
* MAIN NETPAK OVERNIGHT REPORTING MODULE                                        
*************************************                                           
*  INPUTS FROM EDIT : ADRIVE - A(DRIVE TABLE TO USE)                            
*                     ANETWS1 - CLIENT HEADER                                   
*PHASE T31E2BA                                                                  
*INCLUDE NEWRW                                                                  
*INCLUDE NERWIN                                                                 
*INCLUDE NERWOUT                                                                
*INCLUDE UNTIME                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE CENTER                                                                 
         TITLE 'T31E2B - NEW NETWORK OVERNIGHT REPORT MAIN MODULE'              
         PRINT NOGEN                                                            
NERWMAIN CSECT                                                                  
         NMOD1 0,**OVRW**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING NERWMAIN+4096,RA                                                 
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R8,ANETWS2          USE W/S AREA 2 FOR W/S                       
         USING OVERD,R8                                                         
         ST    R2,RELO                                                          
*                                                                               
         L     R7,=A(LOCALWS)                                                   
         A     R7,RELO                                                          
         USING WSDS,R7                                                          
*                                                                               
*                                                                               
****** INITIALIZE BLOCK                                                         
*                                                                               
         L     R2,=V(NEWRW)                                                     
         A     R2,RELO                                                          
         ST    R2,VNEWRW                                                        
         L     R1,=A(XTRASTOR)                                                  
         ST    R1,ABEGSTR          EXTRA STORAGE                                
         LA    R1,2048(R1)         MARK END FOR NOW                             
         LA    R1,2048(R1)                                                      
         ST    R1,AENDSTR                                                       
         ST    R1,AWS              PUT W/S FOR MODULES HERE                     
         LA    R1,MONLIST                                                       
         ST    R1,APERLST1                                                      
         LA    R1,INHOOK                                                        
         ST    R1,AINHOOK                                                       
*                                                                               
         EJECT                                                                  
         MVI   NREPTYP,C'A'        SET UP AS ACCTG REPORT                       
         MVI   PERTYPE,C'W'        SET UP FOR WEEKS                             
         MVI   PERTYPE+1,1         USE MONTHS IF TOO MANY WEEKS                 
         MVI   PERTYPE+2,0         NEVER USE QUARTERS                           
*                                                                               
         SPACE 2                                                                
*                                                                               
***********************                                                         
PROCDAT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBVALDAT     PROCESS DATES                                
         BE    IN6                                                              
         B     PROCDAT             OTHER MODES ARE IGNORED                      
*                                                                               
IN6      LA    R1,MAXMONTS         SET UP MAX SIZE OF MONTH (WEEK) LIST         
         ST    R1,NUMMONS          GET LIST INTO MONLIST. NUMMONS IS            
*                                    NEW SIZE OF LIST                           
         MVI   PERTYPE,C'M'                                                     
         NETGO NVWKLST,DMCB,NUMMONS,MONLIST,PERTYPE                             
*                                                                               
         GOTO1 =V(NERWIN),DMCB,(RC),(R8)                                        
*                                                                               
*                                                                               
TXMOD    XMOD1                                                                  
*                                                                               
PROCERR  DC    F'0'                                                             
*                                                                               
         EJECT                                                                  
INHOOK   NTR1                                                                   
         GOTO1 =V(NERWOUT),DMCB,(RC),(R8)                                       
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR W/S                                                    
         SPACE 3                                                                
OVERD    DSECT                                                                  
       ++INCLUDE NERWBLOCK                                                      
       ++INCLUDE NETDEMOD                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE NEACCTBLK                                                      
*                                                                               
*                                                                               
*                                                                               
RELO     DS    A                                                                
*                                                                               
*                                                                               
RESTWS   EQU   *                   REST OF WORKING STORAGE                      
*                                                                               
*********                                                                       
* RESTWS CONFIGURED:                                                            
*    DRIVE TABLE  (CHAIN OF DTABS, 0 TERMINATED)                                
*                                                                               
         EJECT                                                                  
WSDS     DSECT                                                                  
PERTYPE  DS    CL4                 1ST BYTE IS PERIOD TYPE                      
MAXMONTS EQU   16                  MAX MONS (WKS) IN LIST                       
MONLIST  DS    CL(4*MAXMONTS)                                                   
NUMMONS  DS    F                                                                
*                                                                               
*                                                                               
*** LOCAL  W/S                                                                  
LOCALWS  CSECT                                                                  
         DS    4000CL1                                                          
*                                                                               
XTRASTOR CSECT                                                                  
         DS   10000CL1                                                          
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE NERWINCLS                                                      
       ++INCLUDE NERWEQUS                                                       
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010NERWMAIN  08/10/00'                                      
         END                                                                    
