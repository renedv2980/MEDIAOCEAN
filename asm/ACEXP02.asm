*          DATA SET ACEXP02    AT LEVEL 004 AS OF 02/10/98                      
*PHASE T61502A,+0                                                               
         TITLE 'T61502 - LIST OF ACCOUNTS'                                      
T61502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T61502,R7,RR=R2                                                
         L     RC,0(,R1)           RC=GENCON STORAGE AREA                       
*                                                                               
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA             RA=A(TWA)                                    
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
*                                                                               
         USING T615FFD,RA                                                       
         USING SYSD,R9                                                          
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R6,SAVXTWA          R6=LOCAL SAVED STORAGE                       
         A     R6,=AL4(SAVXDSP)    DISPLACEMENT TO 'LIST' SAVE AREA             
*                                                                               
         USING LWSD,R6                                                          
*                                                                               
         ST    R2,RELO                                                          
         GOTO1 AUTH                                                             
         EJECT ,                                                                
*              INITIAL ROUTINES                                                 
         CLI   MODE,VALKEY         VALIDATE                                     
         BNE   LST10                                                            
         XC    MYKEY(LWSLEN1),MYKEY START AGAIN - CLEAR SAVED STORAGE           
         LA    R4,LASTKEY              BUILD THE RECORD KEY                     
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),COMPANY                                              
         LA    R2,LOGUNTH          UNIT                                         
         GOTO1 ANY                                                              
         MVC   ACKEYACC+1(1),WORK                                               
         MVC   KEY,LASTKEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         LA    R2,LOGLEDH          AND LEDGER ARE REQUIRED                      
         GOTO1 ANY                                                              
         MVC   ACKEYACC+2(1),WORK                                               
         MVC   KEY,LASTKEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         LA    R2,LOGACCH                                                       
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 ANY                                                              
         MVC   ACKEYACC+3(12),WORK                                              
         B     XIT                                                              
         SPACE 1                                                                
LST10    CLI   MODE,LISTRECS                                                    
         BNE   XIT                                                              
         MVC   LOGHED1,ACCTH1      HEADLINES TO SCREEN                          
         MVC   LOGHED2,ACCTH2                                                   
         CLC   LASTKEY+1(2),=C'3M'                                              
         BNE   *+16                                                             
         MVC   LOGHED1,ACCTHM1     SPECIAL FOR ACN AND VEHICLE                  
         MVC   LOGHED2,ACCTHM2                                                  
         OI    LOGHED1H+6,X'80'    AND TRANSMIT                                 
         OI    LOGHED2H+6,X'80'                                                 
         LA    R2,LISTAR           R2=SCREEN LINE (MUST BE LIST)                
         OC    KEY,KEY                                                          
         BNZ   LST20                                                            
         MVC   KEY,LASTKEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                READ FIRAT RECORD  TO START                  
         B     LST25                                                            
         EJECT                                                                  
*              GENERATE LISTING                                                 
         SPACE 1                                                                
         USING LINED,R2                                                         
         USING ACKEYD,R4                                                        
LST20    MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
LST25    L     R4,AIO                                                           
         CLC   LASTKEY(3),0(R4)    IF COMP/U/L CHANGE                           
         BNE   ACCX                GET OUT                                      
         CLI   ACKEYACC+3,C' '                                                  
         BE    LST20               SKIP LEDGER RECORD                           
         CLC   ACKEYWRK(ACLENGTH-ACKEYWRK),SPACES                               
         BE    LST27                                                            
         LA    R4,KEY              ACKEYWRK NOT SPACES                          
         MVI   ACKEYWRK,X'FF'                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     LST25                                                            
         SPACE 1                                                                
LST27    MVC   LASTKEY,0(R4)       SAVE KEY AND DISPLAY                         
         MVC   LINED(LINLEN),SPACES                                             
         MVC   LINACCT,ACKEYACC+3                                               
         GOTO1 NAMOUT,DMCB,AIO,LINACNME                                         
         GOTO1 GETL,DMCB,(X'30',AIO),0                                          
         CLI   ELERR,0                                                          
         BNE   LST40                                                            
         L     R5,ELADDR                                                        
         USING ACSTATD,R5                                                       
         MVC   LINACFLT,=C'.........'                                           
         CLI   ACSTFILT,X'41'                                                   
         BL    *+10                                                             
         MVC   LINF1,ACSTFILT                                                   
         CLI   ACSTFILT+1,X'41'                                                 
         BL    *+10                                                             
         MVC   LINF2,ACSTFILT+1                                                 
         CLI   ACKEYACC+1,C'S'     IN US UNIT S,LEDGER P,Q,S &T                 
         BNE   LST29               ACSTANAL NOT DISPLAYED                       
         CLI   ACKEYACC+2,C'P'                                                  
         BE    LST31                                                            
         CLI   ACKEYACC+2,C'Q'                                                  
         BE    LST31                                                            
         CLI   ACKEYACC+2,C'S'                                                  
         BE    LST31                                                            
         CLI   ACKEYACC+2,C'T'                                                  
         BE    LST31                                                            
LST29    CLI   ACSTANAL,X'41'                                                   
         BL    *+10                                                             
         MVC   LINF3,ACSTANAL                                                   
LST31    CLI   ACSTSUB,X'41'                                                    
         BL    *+10                                                             
         MVC   LINFS,ACSTSUB                                                    
         CLI   ACSTCOST,X'41'                                                   
         BL    *+10                                                             
         MVC   LINFA,ACSTCOST                                                   
         TM    ACSTSTAT,X'80'                                                   
         BNO   *+8                                                              
         MVI   LINFP,C'P'                                                       
         TM    ACSTSTAT,X'40'                                                   
         BNO   *+8                                                              
         MVI   LINFC,C'C'                                                       
         TM    ACSTSTAT,X'20'                                                   
         BNO   *+8                                                              
         MVI   LINFL,C'L'                                                       
         TM    ACSTSTAT,X'01'                                                   
         BNO   *+8                                                              
         MVI   LINFD,C'D'                                                       
         SPACE 1                                                                
LST40    CLC   ACKEYACC+1(2),=C'3M'                                             
         BNE   LST50                                                            
         GOTO1 GETL,DMCB,(X'23',AIO),0                                          
         CLI   ELADDR,0                                                         
         BNE   LST42               NO ACN NUMBER                                
         L     R5,ELADDR                                                        
         USING ACOTHERD,R5                                                      
         MVC   LINACN,ACOTNUM                                                   
LST42    GOTO1 GETL,DMCB,(X'25',AIO),0                                          
         CLI   ELADDR,0                                                         
         BNE   LST50               NO VEHICLE NUMBER                            
         L     R5,ELADDR                                                        
         USING ACNOD,R5                                                         
         MVC   LINVEH,ACNOLEN+3                                                 
LST50    GOTO1 LISTMON             LET CONTROLLER KEEP TRACK                    
         B     LST20               AND TRY NEXT                                 
         SPACE 1                                                                
ACCX     B     XIT                 RETURN TO CONTROLLER                         
         EJECT                                                                  
*              EXIT ROUTINES                                                    
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 EXIT                                                             
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT ,                                                                
*              CONSTANTS, LITERAL POOL, ETC.                                    
         SPACE 1                                                                
*        COMPLETION MESSAGES                                                    
MSG1     DC    C'** ERROR ** INPUT UNIT AND LEDGER'                             
         SPACE 1                                                                
*        HEADLINES                                                              
ACCTH1   DC    CL36'SEL ACCOUNT CODE ------------ACCOUNT'                       
         DC    CL41' NAME------------ FILT/STAT'                                
ACCTH2   DC    CL36'---'                                                        
         DC    CL41'                  123SAPCLD'                                
ACCTHM1  DC    CL36'SEL ACCOUNT CODE ------------ACCOUNT'                       
         DC    CL41' NAME------------ FILT/STAT ACN#  VEHICLE'                  
ACCTHM2  DC    CL36'---'                                                        
         DC    CL41'                  123SAPCLD ----- --------'                 
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
*              DSECT TO COVER SCREEN LINE                                       
         SPACE 1                                                                
*                                  ACTION=LIST USE A(SCREEN LINE)               
*                                  ACTION=PRINT USE A(P)                        
         SPACE 1                                                                
LINED    DSECT                                                                  
LINACCT  DS    CL12                ACCOUNT CODE                                 
         DS    CL1                                                              
LINACNME DS    CL36                ACCOUNT NAME                                 
         DS    CL1                                                              
LINACFLT DS    CL9                 ACCOUNT FILTERS                              
         ORG   LINACFLT                                                         
LINF1    DS    CL1                                                              
LINF2    DS    CL1                                                              
LINF3    DS    CL1                                                              
LINFS    DS    CL1                                                              
LINFA    DS    CL1                                                              
LINFP    DS    CL1                                                              
LINFC    DS    CL1                                                              
LINFL    DS    CL1                                                              
LINFD    DS    CL1                                                              
         DS    CL1                                                              
LINACN   DS    CL5                 ACN NUMBER                                   
         DS    CL1                                                              
LINVEH   DS    CL8                 VEHICLE                                      
LINLEN   EQU   *-LINED                                                          
         EJECT ,                                                                
*              LOCAL SAVED STORAGE                                              
         SPACE 1                                                                
LWSD     DSECT                                                                  
RELO     DS    F                                                                
         SPACE 1                                                                
MYKEY    DS    CL(L'KEY)           START  OF STORAGE TO BE CLEARED              
LASTKEY  DS    CL(L'KEY)           LAST KEY FOR PAGING                          
LWSLEN1  EQU   *-MYKEY             LENGTH OF STORAGE TO BE CLEARED              
LWSLEN   EQU   *-LWSD              LENGTH OF SAVED STORAGE                      
         EJECT ,                                                                
       ++INCLUDE ACEXPFFD                                                       
         EJECT ,                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE ACEXPFDD                                                       
         EJECT ,                                                                
       ++INCLUDE DDGENTWA                                                       
         EJECT ,                                                                
* ACEXPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACEXPWORKD                                                     
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACEXP02   02/10/98'                                      
         END                                                                    
