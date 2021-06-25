*          DATA SET NEMED64    AT LEVEL 054 AS OF 11/03/10                      
*PHASE T31E64A,+0                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE CLUNPK                                                                 
         TITLE 'T31E64 - VENDOR LOCK'                                           
         PRINT NOGEN                                                            
************************************************************                    
*                                                                               
*   THIS PROGRAM READS STATION RECORDS AND REPORTS THOSE THAT                   
*   HAVE NO UNITS AGAINST THEM- ABILITY TO UPDATE THOSE STATIONS                
*   WITH LOCKED INFORMATION                                                     
*                                                                               
**************************************************************                  
T31E64   CSECT                                                                  
         NMOD1 0,**VNLK**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R7,ANETWS1                                                       
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         XC    COUNTER,COUNTER                                                  
         XC    COUNTER2,COUNTER2                                                
                                                                                
*                                                                               
FX0      BRAS  RE,LOADSTAT         LOAD STATION TABLE                           
         BAS   RE,STALOCK                                                       
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
KEYFILT  NTR1                                                                   
         LA    R2,KEY                                                           
         USING NUKPKEY,R2                                                       
KYF5     OC    BINSTR,BINSTR                                                    
         BNZ   KYF6                                                             
         GOTO1 DATCON,DMCB,NBSELSTR,(2,BINSTR)                                  
KYF6     CLC   NUKPDATE,BINSTR                                                  
         BL    KYNOTOK                                                          
*                                                                               
KYOK     SR    R2,R2                                                            
KYNOTOK  LTR   R2,R2         CC NOT = REJECTS UNIT                              
*                                                                               
KYFX     XIT1                                                                   
         DROP R2                                                                
         EJECT                                                                  
                                                                                
*                                                                               
STALOCK  NTR1                                                                   
* SET UP AND READ X'84' KEY                                                     
         XC    KEY,KEY           CLEAR KEY                                      
         LA    R2,KEY                                                           
         USING NUKPKEY,R2                                                       
         MVI   0(R2),X'84'                                                      
         MVC   NUKPAM,NBACTAM                                                   
STAL10   MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'UNTDIR  ',KEY,KEY,0                   
         B     STAL20                                                           
                                                                                
STALSEQ  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'UNTDIR  ',KEY,KEY,0                   
*                                                                               
STAL20   CLC   KEY(2),KEYSAVE                                                   
         BNE   STAL50                                                           
         BAS   RE,KEYFILT        DO WE WANT THIS UNIT?                          
         BNE   STALSEQ             NO                                           
         B     STAL30              YES                                          
*                                                                               
STAL30   DS    0H                                                               
         LA    R2,1000                                                          
         L     R1,=A(STATBL)                                                    
STAL32   CLC   KEY+4(4),0(R1)      STATION MATCH ?                              
         BNE   STAL35                                                           
         CLC   20(3,R1),=3C'0'    CLIENT EXCEPTION ?                            
         BNE   STAL35                                                           
STAL34   MVI   5(R1),X'FF'         MARK STATION AS USED                         
*                                                                               
STAL34B  ZIC   R1,KEY+7            SET TO SKIP READ                             
         LA    R1,1(R1)                                                         
         STC   R1,KEY+7                                                         
         B     STAL10              READ HI                                      
*                                                                               
STAL35   LA    R1,26(R1)                                                        
         BCT   R2,STAL32                                                        
         B     STAL34B                                                          
***      CLI   SPLMED,X'40'        MEDIA FILTER?                                
***      BH    STAL34B             YES-SKIP UNIT                                
***      DC    H'0'                WHY NO FIND ? - COULD BE LOCKED !            
*                                                                               
STAL50   DS    0H                                                               
         MVC   P(14),=C'TOTAL NETWORKS'                                         
         EDIT  (B4,COUNTER),(8,P+23)                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R3,=A(STATBL)                                                    
         LA    R2,1000                                                          
         CLI   0(R3),0             NO STATIONS FOUND ?                          
         BE    STALX                                                            
*                                                                               
STAL52   CLI   5(R3),X'FF'         STATIONS HAS UNITS                           
         BE    STAL53              YES                                          
         L     R1,COUNTER2         NO                                           
         LA    R1,1(R1)                                                         
         ST    R1,COUNTER2                                                      
         CLI   SPLTEST,C'N'        TEST RUN ?                                   
         BNE   STAL53              YES                                          
         CLI   TWAWRITE,C'N'                                                    
         BE    STAL53                                                           
         BAS   RE,LOCKIT           NO, LOCK STATION                             
STAL53   LA    R3,26(R3)                                                        
         CLI   0(R3),X'40'                                                      
         BNH   STAL54                                                           
         BCT   R2,STAL52                                                        
STAL54   MVC   P(21),=C'TOTAL NETWORKS LOCKED'                                  
         EDIT  (B4,COUNTER2),(8,P+23)                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         B     STAL55                        PATCH THIS TO GET LIST             
*                                            OF ALL NETWORKS ON FILE            
*                                                                               
         MVC   P(16),=C'NETWORKS ON FILE'                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         L     R3,=A(STATBL)                                                    
         LA    R2,1000                                                          
STAL54B  MVC   P(26),0(R3)                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,26(R3)                                                        
         CLI   0(R3),X'40'                                                      
         BNH   STAL55                                                           
         BCT   R2,STAL54B                                                       
*                                                                               
STAL55   DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(15),=C'LOCKED NETWORKS'                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         L     R3,=A(STATBL)                                                    
         LA    R2,1000                                                          
         CLI   0(R3),0             NO STATIONS FOUND                            
         BE    STALX                                                            
STAL55B  CLI   5(R3),X'FF'                                                      
         BE    STAL60                                                           
*                                                                               
         L     R4,AIO                                                           
         USING MKTREC,R4                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVI   KEY+1,C'N'       MEDIA                                           
         MVC   KEY+2(4),6(R3)      STATION-MARKET                               
         MVC   KEY+6(2),NBSELAGY   AGENCY                                       
         MVC   KEY+8(7),=7C'0'     FILL ZEROS                                   
         GOTO1 HIGH                                                             
         CLC   KEY(2),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(4),0(R3)          STATION CALL LETTERS                         
         MVC   P+5(24),MKTNAME     STATION NAME                                 
         MVC   P+31(1),4(R3)       STATION TYPE                                 
         MVC   P+34(15),11(R3)     STATION KEY                                  
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
STAL60   LA    R3,26(R3)                                                        
         CLI   0(R3),X'40'                                                      
         BNH   STALX                                                            
         BCT   R2,STAL55B                                                       
         B     STALX                                                            
         DROP  R4                                                               
*                                                                               
STALX    DS     0H                                                              
         XIT1                                                                   
*  R3 POINTS TO STATION TABLE                                                   
LOCKIT   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(15),11(R3)   STATION KEY IN STATION TABLE                    
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMREAD'),=C'STATION',KEY,AIO              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AIO                                                           
         USING STAREC,R1                                                        
         OI    SFLAG1,X'04'      LOCKED                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'STATION',KEY,AIO                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         DROP  R1                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
LOADSTAT NTR1  BASE=*,LABEL=*                                                   
* -> LOAD STATION TABLE                                                         
         XC    COUNTER,COUNTER                                                  
         L     R2,AIO                                                           
         USING STAREC,R2                                                        
         L     R3,=A(STATBL)           CL4 STA / CL1 STATYPE                    
         LA    R4,1000                                                          
         NETGO NVSETSTA,DMCB                                                    
         MVC   FILENAME,=C'STATION '                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,C'S'                                                         
         MVI   KEY+1,C'N'                                                       
         GOTO1 HIGH                                                             
         B     FX2                                                              
FX3      GOTO1 SEQ                                                              
*                                                                               
FX2      CLC   KEY(2),KEYSAVE          TYPE/MEDIA                               
         BNE   STATX                                                            
         CLC   STAKAGY,NBSELAGY       AGY                                       
         BNE   FX3                                                              
*                                                                               
         TM    SFLAG1,X'04'        STATION LOCKED ?                             
         BO    FX3                                                              
*                                                                               
         CLI   SPLMED,X'40'        MEDIA FILTER ?                               
         BNH   *+14                                                             
         CLC   SPLMED,STYPE                                                     
         BNE   FX3                                                              
*                                                                               
         MVC   0(4,R3),STAKCALL                                                 
         MVC   4(1,R3),STYPE       STATION TYPE                                 
         MVC   6(4,R3),SMKT                                                     
         MVC   11(15,R3),KEY                                                    
         B     FX2B                                                             
*                                                                               
**       MVC   5(1,R3),SPTYPE                                                   
**       CLI   5(R3),X'40'         IF POSTING TYPE BLANK                        
**       BH    *+10                                                             
**       MVC   5(1,R3),4(R3)       USE STATION TYPE                             
**       MVC   6(1,R3),SOVBKTYP    OVERRIDE BOOK TYPE                           
**       CLI   6(R3),X'40'         IF NO BKTYP                                  
**       BH    *+10                                                             
**       MVC   6(1,R3),5(R3)       USE POST TYPE                                
**       MVC   7(1,R3),STRTYPE     OVERRIDE TRAFFIC TYPE                        
**       CLI   7(R3),X'40'         IF NO TRAFFIC TYPE                           
**       BH    *+10                                                             
**       MVC   7(1,R3),5(R3)       USE POST TYPE                                
**       MVC   8(1,R3),SUBMEDIA    SUB MEDIA                                    
FX2B     L     R1,AIO                                                           
**       MVC   P(100),0(R1)                                                     
**       GOTO1 SPOOL,DMCB,(R8)                                                  
         L     RE,COUNTER                                                       
         LA    RE,1(RE)                                                         
         ST    RE,COUNTER                                                       
         LA    R3,26(R3)                                                        
         BCT   R4,FX3                                                           
         MVC   P+1(32),=C'STATION TABLE FULL *************'                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     STATX                                                            
STATX    DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
*                                                                               
COUNTER  DS    F                                                                
COUNTER2 DS    F                                                                
STATBL   DS    CL10000                                                          
         DS    CL10000                                                          
         DS    CL6000                                                           
                                                                                
         EJECT                                                                  
         LTORG                                                                  
                                                                                
*** PRINT LINE DESECT ***                                                       
         SPACE                                                                  
PLINED   DSECT                                                                  
PLNET    DS    CL4                 NETWORK                                      
         DS    CL2                                                              
PLPKCDE  DS    CL3                 PACKAGE CODE                                 
         DS    CL2                                                              
PLPKNM   DS    CL16                PACKAGE NAME                                 
         DS    CL2                                                              
PLPRGCDE DS    CL6                 PROGRAM CODE                                 
         DS    CL2                                                              
PLPRGNM  DS    CL16                PROGRAM NAME                                 
         DS    CL2                                                              
PLUNTOT  DS    CL5                 TOTAL UNITS                                  
         DS    CL2                                                              
ENDP     EQU   *-PLNET                                                          
*                                                                               
**** PASDATA STORAGE (IN W/S AREA1 FROM EDIT OVERLAY) ***                       
WORKD    DSECT                                                                  
         DS    0F                                                               
RELO     DS    F                                                                
NOSTAT   DS    F                                                                
NO02     DS    F                                                                
MYELEM   DS    CL20                                                             
*                                                                               
RSTAT    DS    CL1                 FLAG IF NURSTAT UPDATED                      
BINSTR   DS    CL2                                                              
BINEND   DS    CL2                                                              
KEYSV    DS    CL30                                                             
KEY2     DS    CL40                                                             
KEY2SV   DS    CL40                                                             
*                                                                               
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE4D                                                       
       ++INCLUDE DDGENTWA                                                       
*                                                                               
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENUBILL                                                     
       ++INCLUDE DEDEMEQUS2                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054NEMED64   11/03/10'                                      
         END                                                                    
