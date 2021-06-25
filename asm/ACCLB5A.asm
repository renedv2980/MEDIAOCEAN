*          DATA SET ACCLB5A    AT LEVEL 245 AS OF 08/16/00                      
*PHASE T6215AA                                                                  
CLB5A    TITLE '- PC COMMS - DATA SOURCE LIST'                                  
CLB5A    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB5A**,R8,RR=RE                                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         ST    RE,BORELO                                                        
         L     RC,ALINK                                                         
         USING LINKD,RC                                                         
*                                                                               
         CLI   LINKMODE,ROUTSN                                                  
         BNL   EXITY                                                            
         XR    RF,RF                                                            
         IC    RF,LINKMODE                                                      
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     SETMAP              SET A(MAP TABLE)                             
         B     RCVFST              FIRST FOR RECEIVE                            
         B     RCVHDRF             FIRST FOR MAP HEADER RECEIVE                 
         B     RCVDATA             DATA RECEIVE                                 
         B     RCVHDRL             LAST FOR MAP HEADER RECEIVE                  
         B     RCVLST              LAST FOR RECEIVE                             
         B     SND                 SEND                                         
         B     SNDEL               SEND ELEMENT                                 
         B     SNDFLD              SEND ELEMENT FIELD                           
ROUTSN   EQU   (*-ROUTS)/L'ROUTS                                                
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITY    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* SET A(MAP TABLE)                                                    *         
***********************************************************************         
         SPACE 1                                                                
SETMAP   DS    0H                                                               
         LA    RF,MAPTAB           SET A(MAP TABLE)                             
         ST    RF,AMAPTAB                                                       
         GOTO1 AVALTAB             ?? FOR NOW                                   
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR RECEIVE                                                   *         
***********************************************************************         
         SPACE 1                                                                
RCVFST   DS    0H                  SWITCH INTO CONTROL SYSTEM                   
*                                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR MAP HEADER RECEIVE                                        *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRF  DS    0H                                                               
         ICM   RF,15,ORCVHDRF                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* ELEMENT DATA RECEIVE                                                *         
***********************************************************************         
         SPACE 1                                                                
RCVDATA  DS    0H                                                               
         ICM   RF,15,ORCVDATA                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
         SPACE 1                                                                
                                                                                
RCVMODE  DS    0H                  IF WRITING RECORD - TEST CAN DO              
         B     EXITY                                                            
                                                                                
         SPACE 1                                                                
***********************************************************************         
* LAST FOR MAP HEADER RECEIVE                                         *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRL  DS    0H                                                               
*                                                                               
RHDRL02  DS    0H                                                               
         ICM   RF,15,ORCVHDRL                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR RECEIVE                                                    *         
***********************************************************************         
         SPACE 1                                                                
RCVLST   DS    0H                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SEND                                                                *         
***********************************************************************         
         SPACE 1                                                                
SND      DS    0H                                                               
*                                                                               
         LA    R2,IOKEY                                                         
         USING SCMRECD,R2                                                       
         XC    SCMKEY,SCMKEY                                                    
         MVI   SCMKTYP,SCMKTYPQ                                                 
         MVC   SCMKCPY,CUABIN                                                   
         LA    R1,IOHIGH+IOACCDIR+IO1                                           
         B     *+8                                                              
SNDD2    LA    R1,IOSEQ+IOACCDIR+IO1                                            
         GOTO1 AIO                                                              
         CLC   SCMKEY(SCMKCODE-SCMKEY),IOKEYSAV                                 
         BNE   SNDDSX                                                           
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASNDHDR,BOPARM,MH#NARR                                           
         GOTO1 ASNDDATA,BOPARM,01,SCMKCODE                                      
         GOTO1 ASNDREC,BOPARM,AIO1                                              
         B     SNDD2                                                            
                                                                                
SNDDSX   DS    0H                                                               
         B     EXITY                                                            
         DROP  R2                                                               
         SPACE 1                                                                
*                                                                               
***********************************************************************         
* SEND ELEMENT                                                        *         
***********************************************************************         
         SPACE 1                                                                
SNDEL    DS    0H                                                               
         ICM   RF,15,OSNDEL                                                     
         BNZR  RF                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
***********************************************************************         
* SEND ELEMENT FIELD                                                  *         
***********************************************************************         
         SPACE 1                                                                
SNDFLD   DS    0H                                                               
         ICM   RF,15,OSNDFLD                                                    
         BNZR  RF                                                               
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
CON      DC    C'CON'                                                           
DMKEY    DC    C'DMKEY  '                                                       
GENFIL   DC    C'GENFIL '                                                       
ADDEND   DC    C'ADD=END'                                                       
ADDCODE  DC    C'ADD=CODE'                                                      
EFFS     DC    X'FFFFFFFF'                                                      
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
***********************************************************************         
* MAP TABLE                                                           *         
***********************************************************************         
         SPACE 1                                                                
MAPTAB   DS    0X                                                               
*                                                                               
M#NARRQ  DS    0X                  ** DATA SOURCE REQUEST **                    
         DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(MH#NARR)        ELEMENT CODE                                 
         DC    AL2(M#NARRQX+1-M#NARRQ) DISP TO NEXT ELEMENT                     
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'CODE'           TEXT INDENTIFIER                             
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'SCMKCODE)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
M#NARRQX DC    AL1(0)              END-OF-ELEMENT FIELDS                        
*                                                                               
M#SCM    DS    0X                                                               
         DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(SCMELQ)         ELEMENT CODE                                 
         DC    AL2(M#SCMX+1-M#SCM) DISP TO NEXT ELEMENT                         
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
                                                                                
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'NARR '          TEXT INDENTIFIER                             
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(SCMNARR-SCMELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
                                                                                
M#SCMX   DC    AL1(0)                                                           
                                                                                
M#COIEL  DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(COIELQ)         ELEMENT CODE                                 
         DC    AL2(M#COIELX+1-M#COIEL) DISP TO NEXT ELEMENT                     
         DC    AL1(MHIRECEL)       INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
                                                                                
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'DESCR'          TEXT INDENTIFIER                             
         DC    AL1(MDTCHQ)         DATA TYPE                                    
         DC    AL1(L'COIDESC)      DATA LENGTH                                  
         DC    AL1(COIDESC-COIELD) DATA DISPLACEMENT                            
         DC    AL1(MDIELFLD)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
                                                                                
M#COIELX DC    AL1(0)                                                           
                                                                                
*                                                                               
*                                                                               
MAPTABX  DC    X'00'               E-O-T                                        
         EJECT                                                                  
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
***********************************************************************         
* SAVED WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
* TWAD     DSECT                                                                
*        ORG   OSVALS                                                           
*        DS    (L'OSVALS-(*-OSVALS))X                                           
         EJECT                                                                  
* ACCLBLINK                                                                     
*      ++INCLUDE ACCLBLINK                                                      
         SPACE 1                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
LINKD    DSECT                                                                  
         ORG   OVRWS                                                            
REQINDS  DS    XL1                 REQUEST INDICATORS                           
REQIAGY  EQU   X'80'               AGENCY LEVEL RECORDS                         
REQIRDS  EQU   X'40'               RECEIVED DATA SOURCE KEY#                    
REQIRKW  EQU   X'20'               RECEIVED KEYWORD LIST NUMBER                 
LANG     DS    XL1                 LANGUAGE CODE                                
CTRYMASK DS    XL4                 COUNTRY CODE MASK                            
         DS    (L'OVRWS-(*-OVRWS))X                                             
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'245ACCLB5A   08/16/00'                                      
         END                                                                    
