*          DATA SET SRTTO00    AT LEVEL 005 AS OF 12/07/09                      
*PHASE T12D00A                                                                  
*INCLUDE SQUASHER                                                               
         TITLE '$TTO - TERMINAL TIMEOUT - FREE ALL SESSIONS'                    
         PRINT NOGEN                                                            
TIMEOUT  CSECT                                                                  
         NMOD1 SRWORKX-SRWORKD,**$TTO**,RR=R6,CLEAR=YES                         
         USING SRWORKD,RC                                                       
         ST    R1,SAVER1                                                        
         LR    R7,R1               R7=A(SR PARAM LIST)                          
         USING SRPARMD,R7                                                       
         L     RA,SRQATWA          RA=A(TWA)                                    
         USING SRTTOFFD,RA                                                      
         L     R8,SRQATIA          R8=A(TIA)                                    
         L     R9,SRQASYSF         R9=A(SYSFACS)                                
         USING SYSFACD,R9                                                       
         L     R4,SRQAUTL          R4=A(UTL ENTRY)                              
         USING UTLD,R4                                                          
         LA    R4,0(R4)                                                         
         ST    R4,MYUTL            SAVE MY UTL ENTRY ADDRESS                    
         SPACE 1                                                                
TTO1     L     RE,VSSB             EXTRACT SSB DATA                             
         MVC   MAXSES,SSBSSMAX-SSBD(RE)                                         
         MVC   RECLEN,SSBTWAL-SSBD(RE)                                          
         MVC   CHKDSP,=Y(CHKPTDSP)                                              
         CLC   RECLEN,=H'14336'    FIX FOR OLD TEMPSTR                          
         BNE   *+10                                                             
         MVC   CHKDSP,=H'12800'    CHECKPOINT STARTS AT 12.5K FOR 1.5K          
         MVC   SYSIDTY,SSBSYSID-SSBD(RE)                                        
         SPACE 1                                                                
TTO2     L     RE,=V(SQUASHER)     RELOCATE V(SQUASHER)                         
         AR    RE,R6                                                            
         ST    RE,ASQUASH                                                       
         MVC   ATWASVR,VTWASVR                                                  
         ICM   RE,15,=V(TWASVR)    RELOCATE V(TWASVR) IF INCLUDED               
         BZ    *+10                                                             
         AR    RE,R6                                                            
         ST    RE,ATWASVR                                                       
         EJECT                                                                  
***********************************************************************         
* BUILD MESSAGE THAT TERMINAL HAS BEEN TIMED OUT                      *         
***********************************************************************         
         SPACE 1                                                                
TTODISC  MVC   CURSES,TSESSION     SAVE SESSION                                 
         MVC   NEWSES,TSESSION                                                  
*                                                                               
TTODISC3 MVI   MSG,C' '            INITIALISE OUTPUT MESSAGE AREA               
         MVC   MSG+1(L'MSG-1),MSG                                               
         SR    R1,R1                                                            
         IC    R1,TLANG            GET LANGUAGE CODE                            
         CHI   R1,7                                                             
         BNH   *+6                                                              
         SR    R1,R1                                                            
         SLL   R1,2                                                             
         EX    0,TTOMSG(R1)        GET CORRECT MESSAGE FOR LANGUAGE             
         B     DONED0                                                           
*                                                                               
TTOMSG   LA    RE,ENGMSG           ENGLISH DEFAULT                              
         LA    RE,ENGMSG           ENGLISH UK                                   
         LA    RE,ENGMSG           ENGLISH US                                   
         LA    RE,GERMSG           GERMAN                                       
         LA    RE,FREMSG           FRENCH                                       
         LA    RE,SPAMSG           SPANISH                                      
         LA    RE,DUTMSG           DUTCH                                        
         LA    RE,ENGMSG           N/D                                          
*                                                                               
DONED0   MVC   MSG+00(07),=C'ED>2004' FOR CMV                                   
         MVC   MSG+08(40),1(RE)    OUTPUT DISCONNECTED MESSAGE                  
         L     R1,VSSB                                                          
         LA    R1,SSBVTID-SSBD(R1)                                              
         SR    R0,R0                                                            
         IC    R0,0(RE)                                                         
         LA    RF,MSG                                                           
         AR    RF,R0                                                            
         MVC   8(8,RF),0(R1)       MOVE IN FACPAK NAME                          
         EJECT                                                                  
***********************************************************************         
* DISCONNECT ALL ACTIVE SESSIONS                                      *         
***********************************************************************         
         SPACE 1                                                                
DISC     XR    R5,R5               TEST IF CURRENT SESSION ACTIVE               
         IC    R5,TSESSION                                                      
         IC    RF,SSACBITS(R5)                                                  
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    TSSBITS,0                                                        
         BZ    DISC2                                                            
         XC    DMCB(12),DMCB       DISCONNECT CURRENT ACTIVE SESSION            
         ST    R9,DMCB                                                          
         STC   R5,DMCB                                                          
         MVI   DMCB+4,X'80'        SET DISCONNECT                               
         GOTO1 ATWASVR,DMCB                                                     
         BAS   RE,CHKMVE           MOVE NEW DISCONNECTED TWA TO MYTWA           
*                                                                               
DISC2    LH    R5,MAXSES           LOOP THROUGH ALL SESSIONS                    
         SH    R5,=H'1'            R5=HIGHEST SESSION NUMBER                    
         BNM   *+6                                                              
         DC    H'0'                                                             
         MVC   SSESSION,TSESSION   SAVE CURRENT SESSION ID                      
         MVC   SSSBITS,TSSBITS     SAVE SESSION ACTIVE BITS                     
*                                                                               
DISC3    IC    RF,SSACBITS(R5)     GET SESSION ACTIVE BIT MASK                  
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    TSSBITS,0           TEST IF THIS SESSION IS ACTIVE               
         BO    DISC4               YES DISCONNECT IT                            
         LTR   R5,R5               TEST IF FIRST SESSION                        
         BZ    DISC5               YES ALWAYS LEAVE IN SESSION A                
         SH    R5,=H'1'                                                         
         B     DISC3               BACK TO NEXT SESSION                         
*                                                                               
DISC4    XC    DMCB(12),DMCB       CONNECT TO ACTIVE SESSION                    
         ST    R9,DMCB                                                          
         STC   R5,DMCB                                                          
         MVI   DMCB+4,X'20'        SET COMPLETEUTL                              
         GOTO1 ATWASVR,DMCB                                                     
*                                                                               
         XC    DMCB(12),DMCB       DISCONNECT FROM ACTIVE SESSION               
         ST    R9,DMCB                                                          
         STC   R5,DMCB                                                          
         MVI   DMCB+4,X'80'+X'40'  SET DISCONNECT/TWA0INTWA                     
         GOTO1 ATWASVR,DMCB                                                     
         BAS   RE,CHKMVE           MOVE NEW DISCONNECTED TWA TO MYTWA           
         LTR   R5,R5                                                            
*****    BZ    DISCX               FINISHED IF FIRST SESSION                    
         BZ    DISC5         ????  FINISHED IF FIRST SESSION                    
         SH    R5,=H'1'                                                         
         B     DISC3               BACK TO NEXT SESSION                         
*                                                                               
DISC5    XC    DMCB(12),DMCB       LEAVE TERMINAL IN CURRENT SESSION            
         ST    R9,DMCB                                                          
         XR    R5,R5                                                            
         IC    R5,CURSES                                                        
         STC   R5,DMCB                                                          
         MVI   DMCB+4,X'20'        SET COMPLETE TWA/UTL/TCB                     
         GOTO1 ATWASVR,DMCB                                                     
*                                                                               
DISCX    MVC   TSVCREQ,$XMTALL     SET TO TRANSMIT SCREEN                       
         MVI   TNAHNAH,0           INITIALISE SESSION INFO                      
         MVI   TSSXBITS,0                                                       
         MVI   TSSBITS,0                                                        
         MVI   TSSSWAP,X'FF'                                                    
         MVI   TSSRSRV,X'FF'                                                    
*        NI    TSTAT6,255-TST6STRO-TST6STFU-TST6STSS                            
*        NI    TSTAT8,255-TST8STSS-TST8DRTY-TST8BINT                            
         MVI   SRPARM1,X'FF'       SET VALUE TO FORCE CHECKPOINT/WRITE          
*                                                                               
         LA    RF,100              OUTPUT SQUASHED MESSAGE                      
         GOTO1 ASQUASH,DMCB,MSG,(RF)                                            
         MVC   SRVMSG(60),MSG                                                   
         LA    R2,SRVSREQH                                                      
*                                                                               
EXIT     NI    SRVSREQH+6,X'BF'                                                 
         OI    6(R2),X'40'                                                      
         XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* COPY/WRITE CONNECTED TWA BACK TO TEMPSTR TWA#0                      *         
***********************************************************************         
         SPACE 1                                                                
CHKMVE   ST    RE,SAVERE                                                        
         LR    R0,RA               GET A(TWA)                                   
         LH    R1,RECLEN                                                        
         SR    RE,RE                                                            
         ICM   RE,7,DMCB+5         GET A(NEW TWA FROM TWASVR)                   
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE TWASVR'S NEW TWA TO MY TWA              
CHKMVX   L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
CHKWRT   NTR1                      WRITE TWA0 WITH CHECKPOINT DATA              
         SR    R0,R0                                                            
         ICM   R0,3,TNUM           SET TERMINAL NUMBER/PAGE ZERO                
         MVC   DMCB1+20(2),=C'L='  SPECIAL PARM FOR TWA0 READ/WRITES            
         MVC   DMCB1+22(2),RECLEN                                               
         NI    TSTAT5,255-TST5TCP  TURN OFF VTAM TRM CHKPNT PENDING             
         GOTO1 VDATAMGR,DMCB1,=C'DMWRT',=C'TEMPSTR',(R0),(RA)                   
         CLI   DMCB1+8,0                                                        
CHKWRTX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERALS                                              *         
***********************************************************************         
         SPACE 1                                                                
ENGMSG   DC    X'16',CL40'Terminal timed out on .......      '                  
         DC    X'00',CL40'Hit enter to reconnect'                               
GERMSG   DC    X'0E',CL40'Verbindung Zu .......  unterbrochen'                  
         DC    X'00',CL40'Enter zum Neuverbinden'                               
FREMSG   DC    X'17',CL40'Terminal d{connect{ de .......  '                     
         DC    X'00',CL40'Frappez Entr{e pour reconnecter'                      
SPAMSG   DC    X'19',CL40'Terminal desconectado de .......  '                   
         DC    X'00',CL40'Apretar la tecla de entrada para reconectar'          
ITAMSG   DC    X'1D',CL40'Terminale sconnessionato dal .......  '               
         DC    X'00',CL40'Abassi il tasto ENTRARE per il riconnessione'         
DUTMSG   DC    X'0F',CL40'Verbinding met .......  verbroken'                    
         DC    X'00',CL40'Druk op ENTER om de verbinding te herstellen'         
         SPACE 1                                                                
$XMTALL  DC    X'0126'                                                          
SSACBITS DC    X'0102040810204080'                                              
SSNABITS DC    X'FEFDFBF7EFDFBF7F'                                              
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
         SPACE 1                                                                
SRWORKD  DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
SAVER1   DS    A                                                                
SAVERE   DS    A                                                                
MYUTL    DS    A                                                                
ASQUASH  DS    A                                                                
ATWASVR  DS    A                                                                
TIME     DS    PL4                                                              
DMCB     DS    6F                                                               
DMCB1    DS    6F                                                               
RECLEN   DS    H                                                                
CHKDSP   DS    H                                                                
MSG      DS    CL100                                                            
SYSIDTY  DS    X                                                                
XTL      DS    X                                                                
XTT      DS    CL10                                                             
MAXSES   DS    H                                                                
SSESSION DS    X                                                                
SSSBITS  DS    X                                                                
CURSES   DS    X                                                                
NEWSES   DS    X                                                                
ERRNUM   DS    X                                                                
SESS     DS    X                                                                
SRWORKX  DS    0C                                                               
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS                                                        *         
***********************************************************************         
         SPACE 1                                                                
SRTTOFFD DSECT                                                                  
         DS    CL64                                                             
* SRTTOFFD                                                                      
       ++INCLUDE SRTTOFFD                                                       
         SPACE 1                                                                
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         SPACE 1                                                                
* FACHKPT                                                                       
         PRINT OFF                                                              
       ++INCLUDE FACHKPT                                                        
         SPACE 1                                                                
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SRTTO00   12/07/09'                                      
         END                                                                    
