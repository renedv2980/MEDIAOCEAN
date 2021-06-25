*          DATA SET SRSHC00    AT LEVEL 005 AS OF 07/30/03                      
*PHASE T10600A                                                                  
         TITLE '$SHCCTL - SHUTTLE CONTROL'                                      
         PRINT NOGEN                                                            
SHC      CSECT                                                                  
         NMOD1 WRKX-WRKD,*$SHC**,CLEAR=YES,RR=R4                                
         USING WRKD,RC                                                          
         ST    R4,RELO                                                          
         LA    RE,*+10                                                          
         O     RE,=XL4'80000000'                                                
         BSM   0,RE                THIS IS NOW IN XA MODE                       
         LR    R2,R1                                                            
         USING SRPARMD,R2          R2=A(S/R PARAM LIST)                         
         L     R3,SRPARM6                                                       
         USING SRSHCFFD,R3         R3=A(TWA)                                    
         L     R5,SRPARM3                                                       
         USING UTLD,R5             R5=A(UTL ENTRY)                              
         L     RA,SRPARM1                                                       
         USING SYSFACD,RA          RA=A(SYS FAC LIST)                           
*                                                                               
         ST    R5,TRMAUTL          SAVE TERMINAL DATA                           
         MVC   TRMNUM,TNUM                                                      
         MVC   TRMLUID,TLUID                                                    
*                                                                               
         L     R4,SRPARM4          EXTRACT ROUTINES FROM COMFACS                
         USING COMFACSD,R4                                                      
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VTERMVAL,CTERMVAL                                                
*                                                                               
         GOTO1 VTICTOC,DMCB,C'SGET'                                             
         MVC   TIMEHMS,DMCB        SAVE THE TIME P'0HHMMSS+'                    
*                                                                               
         XC    MSG,MSG                                                          
         EJECT                                                                  
VALP1    LA    R4,SRVP1H           P1=ACTION                                    
         USING FLDHDRD,R4                                                       
         SR    R1,R1                                                            
         IC    R1,FLDILEN                                                       
         LTR   R1,R1               ACTION IS REQUIRED 1 THRU 8 CHRS             
         BZ    ERR1                                                             
         CH    R1,=H'8'                                                         
         BH    ERR2                                                             
         STC   R1,ACTNLEN          SET ACTION INPUT LENGTH                      
         MVC   ACTNNAME,SPACES                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     VALP1A                                                           
         MVC   ACTNNAME(0),FLDDATA SET ACTION INPUT NAME                        
*                                                                               
VALP1A   LA    RE,ACTNTBL          SEARCH ACTION TABLE                          
         SR    RF,RF                                                            
VALP1B   CLI   0(RE),X'FF'         TEST END OF TABLE                            
         BE    ERR2                                                             
         CLC   ACTNLEN,ACTNLEN-ACTND(RE)                                        
         BNE   VALP1C                                                           
         CLC   ACTNNAME,ACTNNAME-ACTND(RE)                                      
         BE    VALP1D                                                           
VALP1C   LA    RE,L'ACTND(RE)                                                   
         B     VALP1B                                                           
VALP1D   MVC   ACTND,0(RE)         EXTRACT ACTION INFO FROM TABLE               
         EJECT                                                                  
VALP2    LA    R4,SRVP2H           P2=SHUTTLE ID (TRMNUM OR LUID)               
         USING FLDHDRD,R4                                                       
         XC    RMCAUTL,RMCAUTL     INITIALISE LINE/CUDV DATA                    
         XC    RMCNUM,RMCNUM                                                    
         MVC   RMCLUID,SPACES                                                   
         CLI   FLDILEN,0           NO SHUTTLE ID INPUT                          
         BE    VP2X                                                             
VP2B     XC    DMCB(20),DMCB                                                    
         GOTO1 VTERMVAL,DMCB,(R4)  VALIDATE TERM NUM OR LUID                    
         ICM   R5,15,DMCB+4        GET A(UTL) ENTRY                             
         BZ    ERR2                                                             
         MVC   RMCNUM,TNUM                                                      
         ICM   R6,15,TXPRNT        TERMINAL IS NOT A PRINTER                    
         BZ    ERR2                                                             
         ST    R6,RMCAPRQ                                                       
         TM    TTYPE,TTYPERMC                                                   
         BZ    ERR2                TERMINAL IS NOT A SHUTTLE                    
VP2B1    ST    R5,RMCAUTL                                                       
         MVC   RMCLUID,TLUID                                                    
VP2X     EQU   *                                                                
         EJECT                                                                  
VAL      TM    ACTNFLG1,X'80'      TEST IF ACTION NEEDS LINECUDV                
         BZ    VAL1                NO                                           
         OC    RMCAUTL,RMCAUTL     YES TEST IF KNOWN                            
         BNZ   VAL1                                                             
         LA    R4,SRVP2H           ERROR MISSING P2 VALUE                       
         B     ERR1                                                             
VAL1     L     RF,ACTNROUT         GET AND GOTO ROUTINE FOR ACTION              
         A     RF,RELO                                                          
         BR    RF                                                               
         SPACE 2                                                                
WHO      EQU   *                   WHOAMI                                       
         LA    R4,SRVP2H                                                        
         XC    SRVP2,SRVP2                                                      
         MVC   FLDDATA(8),TRMLUID                                               
         OI    6(R4),X'80'                                                      
         B     MSGOUT                                                           
         SPACE 2                                                                
RES      EQU   *                   RESEND BUFFER                                
         LA    R4,SRVP3H                                                        
         CLI   FLDILEN,1           P3= ONE DIGIT BUFFER NUMBER                  
         BL    ERR1                                                             
         BH    ERR2                                                             
         CLI   FLDDATA,C'0'                                                     
         BL    ERR2                                                             
         CLI   FLDDATA,C'9'                                                     
         BH    ERR2                                                             
         MVC   FULL(1),FLDDATA    FULL(1) CONTAINS REQUIRED RBN                 
         NI    FULL,X'0F'                                                       
*                                                                               
RES1     L     R6,RMCAPRQ         POINT TO PRINTER QUEUE                        
         USING PRQD,R6                                                          
         SR    R1,R1                                                            
         IC    R1,FULL                                                          
         SLL   R1,1                                                             
         LA    R1,RBNBITS(R1)     POINT TO BIT MASK FOR RBN                     
         MVC   HALF,PRSVRBB                                                     
         NC    HALF,0(R1)         TEST IF BIT ON IN PRQ                         
         BZ    ERR2               NO ERROR CANT RESEND BUFFER                   
*                                                                               
RES2     XC    PRSVXAC(4),PRSVXAC CLEAR EXTERNAL ACTION DATA                    
         MVI   PRSVXAC,1          SET ACTION TO RESEND                          
         MVC   PRSVXAC1,FULL      SET BUFFER NUMBER REQUIRED                    
         B     MSGOUT                                                           
         SPACE 2                                                                
LOG      EQU   *                   LOG MESSAGE TO ADRFILE                       
         LA    R4,SRVP3H           P3= MESSAGE TEXT                             
         SR    R1,R1                                                            
         IC    R1,FLDILEN                                                       
         LTR   R1,R1                                                            
         BZ    ERR1                ERROR NO MESSAGE TEXT                        
         BCTR  R1,0                                                             
         MVC   WRK(16),SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WRK(0),FLDDATA      EXTRACT MESSAGE TEXT INTO WRK                
*                                                                               
LOG1     XC    LOGREC,LOGREC       BUILD STANDARD PQ LOG REC                    
         MVC   LOGID(4),=C'$PQM'                                                
         MVC   LOGLUID,RMCLUID                                                  
         MVC   LOGTIME,TIMEHMS                                                  
         MVC   LOGTEXT,WRK                                                      
         GOTO1 VLOGGER,LOGREC      OUTPUT LOGREC TO ADRFILE                     
         B     MSGOUT                                                           
         EJECT                                                                  
MSGOUT   MVC   SRVMSG(2),=C'OK'    MOVE OUTPUT MESSAGE TO SCREEN                
         OI    SRVMSGH+6,X'80'                                                  
         LA    R4,SRVP1H                                                        
         OI    6(R4),X'40'         POSN CURSOR ON REQUIRED FIELD                
         B     EXIT                                                             
         SPACE 2                                                                
ERR1     MVC   MSG(17),=C'MISSING PARAMETER'                                    
         B     ERRX                                                             
ERR2     MVC   MSG(17),=C'INVALID PARAMETER'                                    
         B     ERRX                                                             
         SPACE 2                                                                
ERRX     MVC   SRVMSG(2),=C'NO'                                                 
         OI    SRVMSGH+6,X'80'                                                  
         OI    6(R4),X'40'         POSN CURSOR ON INVALID FIELD                 
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
* PHYSICALLY START PRINTER DEVICE                                               
*                                                                               
STRP     NTR1                                                                   
         L     R5,RMCAUTL                                                       
         GOTO1 VLCM,PLIST,VTPRSTRT,(R5)                                         
STRPX    XIT1                                                                   
         EJECT                                                                  
         DS    0F                                                               
ACTNTBL  DS    0CL16               TABLE OF VALID ACTIONS                       
*                                                                               
         DC    CL8'WHOAMI  ',AL1(6),AL1(1),X'0000',AL4(WHO)                     
         DC    CL8'RESEND  ',AL1(6),AL1(2),X'8000',AL4(RES)                     
         DC    CL8'LOG     ',AL1(3),AL1(3),X'8000',AL4(LOG)                     
*                                                                               
ACTNTBLX DC    X'FF',15X'00'                                                    
         SPACE 1                                                                
RBNBITS  DC    X'80004000200010000800040002000100'                              
         DC    X'00800040000000000000000000000000'                              
         SPACE 1                                                                
SPACES   DC    CL16' '                                                          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
WRKD     DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
DMCB     DS    6F                                                               
PLIST    DS    6F                                                               
TIMEHMS  DS    PL4                                                              
WRK      DS    CL32                                                             
MSG      DS    CL60                                                             
*                                                                               
RELO     DS    A                                                                
VHEXOUT  DS    A                                                                
VHEXIN   DS    A                                                                
VTERMVAL DS    A                                                                
*                                                                               
TRMAUTL  DS    A                   TERMINAL A(UTL ENTRY)                        
TRMNUM   DS    H                   TERMINAL NUMBER                              
         DS    H                                                                
TRMLUID  DS    CL8                 TERMINAL LUID                                
*                                                                               
RMCAUTL  DS    A                   SHUTTLE A(UTL ENTRY)                         
RMCAPRQ  DS    A                   SHUTTLE A(PRQ ENTRY)                         
RMCNUM   DS    H                   SHUTTLE TERM NUM                             
         DS    H                                                                
RMCLUID  DS    CL8                 SHUTTLE LUID                                 
*                                                                               
ACTND    DS    0CL16               ACTION DATA                                  
ACTNNAME DS    CL8                 ACTION NAME                                  
ACTNLEN  DS    AL1                 ACTION LENGTH                                
ACTNNUM  DS    AL1                 ACTION NUMBER                                
ACTNFLG1 DS    XL1                 ACTION FLAG1                                 
ACTNFLG2 DS    XL1                 ACTION FLAG2                                 
ACTNROUT DS    AL4                 ACTION A(PROOCESSING ROUTINE)                
*                                                                               
LOGREC   DS    0CL64                                                            
LOGID    DS    CL4                 $PQM IDENTIFIES A PQ MESSAGE                 
LOGLUID  DS    CL8                 LUID OF RMC                                  
LOGTIME  DS    PL4                 TIME OF MESSAGE P'0HHMMSS+'                  
LOGTEXT  DS    CL16                MESSAGE TEXT                                 
LOGSRC   DS    CL6                 SOURCE OF MESSAGE                            
         DS    CL26                N/D                                          
*                                                                               
WRKX     DS    0C                                                               
         EJECT                                                                  
*FAPRQ                                                                          
       ++INCLUDE FAPRQ                                                          
         EJECT                                                                  
*DDFLDHDR                                                                       
       ++INCLUDE DDFLDHDR                                                       
         EJECT                                                                  
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
         SPACE 2                                                                
*FADSECTS                                                                       
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
SRSHCFFD DSECT                                                                  
         DS    CL64                                                             
*SRSHCFFD                                                                       
       ++INCLUDE SRSHCFFD                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SRSHC00   07/30/03'                                      
         END                                                                    
