*          DATA SET DMVSAM1    AT LEVEL 033 AS OF 11/03/99                      
*PHASE VSAMTEST                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         PRINT NOGEN                                                            
         TITLE 'VSAM TEST FOR SPOTDIR1'                                         
DMVSAM   CSECT                                                                  
         NBASE WORKL,**VSAM**,=V(REGSAVE)                                       
         USING WORKD,RC                                                         
         LA    R1,SPTDIR1                                                       
         BRAS  RE,VSOPEN           OPEN VSAM FILE                               
*                                                                               
         MVC   RPL,RPLSPT1         COPY IN RPL TO W/S                           
         LA    R3,RPL                                                           
         USING IFGRPL,R3                                                        
         LA    RF,KEY              SET A(KEY)                                   
         ST    RF,RPLARG                                                        
         LA    RF,ECB              SET A(ECB)                                   
         ST    RF,RPLECB                                                        
         LA    RF,IO               SET A(IO)                                    
         ST    RF,RPLAREA                                                       
*                                                                               
         BRAS  RE,READALL                                                       
         B     DMVSAMX                                                          
*                                                                               
         XC    KEY,KEY                                                          
         XC    IO,IO                                                            
         BRAS  RE,HIGH                                                          
         LA    RF,IO                                                            
         GOTO1 VPRNTBL,DMCB,=C'RECORD HIGH',(RF),C'DUMP',18,=C'1D'              
         GOTO1 (RF),(R1),=C'FEEDBACK',RTNFDBK,C'DUMP',3,=C'1D'                  
*                                                                               
         MVC   KEY,IO                                                           
         XC    IO,IO                                                            
         BRAS  RE,SEQ                                                           
         GOTO1 VPRNTBL,DMCB,=C'RECORD SEQ',IO,C'DUMP',18,=C'1D'                 
         GOTO1 (RF),(R1),=C'FEEDBACK',RTNFDBK,C'DUMP',3,=C'1D'                  
*                                                                               
         MVC   KEY,IO                                                           
         XC    IO,IO                                                            
         BRAS  RE,SEQ                                                           
         GOTO1 VPRNTBL,DMCB,=C'RECORD SEQ',IO,C'DUMP',18,=C'1D'                 
         GOTO1 (RF),(R1),=C'FEEDBACK',RTNFDBK,C'DUMP',3,=C'1D'                  
*                                                                               
         MVC   KEY,IO                                                           
         XC    IO,IO                                                            
         BRAS  RE,GETUP                                                         
         GOTO1 VPRNTBL,DMCB,=C'RECORD GETUP',IO,C'DUMP',18,=C'1D'               
         GOTO1 (RF),(R1),=C'FEEDBACK',RTNFDBK,C'DUMP',3,=C'1D'                  
*                                                                               
         XR    RF,RF                                                            
         IC    RF,IO+13                                                         
         X     RF,=XL4'FFFFFFFF'                                                
         STC   RF,IO+13                                                         
         BRAS  RE,PUTKEY                                                        
         GOTO1 VPRNTBL,DMCB,=C'RECORD PUTKEY',IO,C'DUMP',18,=C'1D'              
         GOTO1 (RF),(R1),=C'FEEDBACK',RTNFDBK,C'DUMP',3,=C'1D'                  
*                                                                               
DMVSAMX  LA    R1,SPTDIR1                                                       
         BRAS  RE,VSCLSE           CLOSE VSAM FILE                              
         XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ ENTIRE FILE SEQUENTIALLY                            *         
***********************************************************************         
         SPACE 1                                                                
READALL  NTR1  ,                                                                
         XC    KEY,KEY                                                          
         XC    IO,IO                                                            
         BRAS  RE,HIGH                                                          
         LA    RF,IO                                                            
         GOTO1 VPRNTBL,DMCB,=C'FIRST KEY',(RF),C'DUMP',18,=C'1D'                
*                                                                               
         LHI   R0,500                                                           
RDALL02  MVC   KEY,IO                                                           
         XC    IO,IO                                                            
         BRAS  RE,SEQ                                                           
         BNZ   READALLX                                                         
         BCT   R0,RDALL02                                                       
*                                                                               
         GOTO1 VPRNTBL,DMCB,=C'500 KEYS LATER',IO,C'DUMP',18,=C'1D'             
         LHI   R0,500                                                           
         B     RDALL02                                                          
*                                                                               
READALLX J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET RECORD                                                          *         
***********************************************************************         
         SPACE 1                                                                
GET      NTR1  ,                                                                
         OI    RPLOPT1,RPLDIR                                                   
         NI    RPLOPT1,255-(RPLSEQ+RPLKGE)                                      
         OI    RPLOPT2,RPLNSP      SET OPTCD=NSP (NOTE STRING POSN)             
         NI    RPLOPT2,255-RPLUPD                                               
         XC    ECB,ECB                                                          
*                                                                               
         GET   RPL=(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         BRAS  RE,CHECK            WAIT FOR I/O COMPLETION                      
*                                                                               
         BRAS  RE,SETRTN                                                        
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET RECORD FOR UPDATE                                               *         
***********************************************************************         
         SPACE 1                                                                
GETUP    NTR1  ,                                                                
         OI    RPLOPT1,RPLDIR                                                   
         NI    RPLOPT1,255-(RPLSEQ+RPLKGE)                                      
         NI    RPLOPT2,255-RPLNSP                                               
         OI    RPLOPT2,RPLUPD                                                   
         XC    ECB,ECB                                                          
         GET   RPL=(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         BRAS  RE,CHECK            WAIT FOR I/O COMPLETION                      
         BRAS  RE,SETRTN                                                        
         BZ    *+6                                                              
         DC    H'0'                NON-ZERO RETURN                              
*                                                                               
         LA    R2,SPTDIR1                                                       
         LA    R4,WORK                                                          
         SHOWCB ACB=(R2),AREA=(R4),LENGTH=80,FIELDS=(CINV)                      
*                                                                               
*                                                                               
* RELCI = CISIZE * MOD( RPLDDDD / CISIZE )                                      
* CISIZE IS IN FIRST 4 BYTES OF WORK                                            
*                                                                               
         ICM   RE,15,RPLDDDD                                                    
         SRDL  RE,32                                                            
         D     RE,WORK             RF = MOD( RPLDDDD / CISIZE )                 
         XR    RE,RE                                                            
         M     RE,WORK             RF = RELCI                                   
*                                                                               
         ST    RF,WORK+8                                                        
         MVC   WORK+4(4),RPLDDDD                                                
         L     RF,VPRNTBL                                                       
         GOTO1 (RF),DMCB,=C'CISIZE/RPLDDD/RELCI',WORK,C'DUMP',12,=C'1D'         
*                                                                               
* NOW LOCK RELCI WITH CALL TO (?) LOCKER                                        
*                                                                               
         OI    RPLOPT1,RPLDIR                                                   
         NI    RPLOPT1,255-(RPLSEQ+RPLKGE)                                      
         NI    RPLOPT2,255-RPLNSP                                               
         OI    RPLOPT2,RPLUPD                                                   
         XC    ECB,ECB                                                          
         GET   RPL=(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         BRAS  RE,CHECK            WAIT FOR I/O COMPLETION                      
         BRAS  RE,SETRTN                                                        
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT HIGH RECORD                                                *         
***********************************************************************         
         SPACE 1                                                                
HIGH     NTR1  ,                                                                
         NI    RPLOPT1,255-(RPLSEQ+RPLSKP)                                      
         OI    RPLOPT1,RPLDIR+RPLKGE                                            
*                                                                               
         NI    RPLOPT2,255-RPLUPD                                               
         OI    RPLOPT2,RPLNSP                                                   
*                                                                               
         XC    ECB,ECB                                                          
         GET   RPL=(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         BRAS  RE,CHECK            WAIT FOR I/O COMPLETION                      
         BRAS  RE,SETRTN                                                        
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT SEQUENTIAL RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
SEQ      NTR1  ,                                                                
         OI    RPLOPT1,RPLSEQ      SET OPTCD=SEQ                                
         NI    RPLOPT1,255-RPLDIR                                               
         OI    RPLOPT2,RPLNSP      SET OPTCD=NSP (NOTE STRING POSN)             
         NI    RPLOPT2,255-RPLUPD                                               
         XC    ECB,ECB                                                          
         GET   RPL=(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         BRAS  RE,CHECK            WAIT FOR I/O COMPLETION                      
         BRAS  RE,SETRTN                                                        
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD RECORD BY KEY                                                   *         
***********************************************************************         
         SPACE 1                                                                
ADDKEY   NTR1  ,                                                                
         OI    RPLOPT1,RPLDIR      SET OPTCD=DIR                                
         NI    RPLOPT1,255-RPLSEQ                                               
         NI    RPLOPT2,255-RPLUPD  SET OPTCD=NUP                                
         XC    ECB,ECB                                                          
         PUT   RPL=(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         BRAS  RE,CHECK            WAIT FOR I/O COMPLETION                      
         BRAS  RE,SETRTN                                                        
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD RECORD FOR LOAD                                                 *         
***********************************************************************         
         SPACE 1                                                                
ADDLOAD  NTR1  ,                                                                
         OI    RPLOPT1,RPLSEQ      SET OPTCD=SEQ FOR LOAD MODE                  
         NI    RPLOPT1,255-RPLDIR                                               
         NI    RPLOPT2,255-RPLUPD  SET OPTCD=NUP                                
         XC    ECB,ECB                                                          
         PUT   RPL=(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         BRAS  RE,CHECK            WAIT FOR I/O COMPLETION                      
         BRAS  RE,SETRTN                                                        
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PUT RECORD BY KEY                                                   *         
***********************************************************************         
         SPACE 1                                                                
PUTKEY   NTR1  ,                                                                
*                                                                               
*              FIRST NEED TO READ RECORD FOR UPDATE                             
*                                                                               
         OI    RPLOPT1,RPLDIR                                                   
         NI    RPLOPT1,255-(RPLSEQ+RPLKGE)                                      
         NI    RPLOPT2,255-RPLNSP                                               
         OI    RPLOPT2,RPLUPD                                                   
         XC    ECB,ECB                                                          
         GET   RPL=(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         BRAS  RE,CHECK            WAIT FOR I/O COMPLETION                      
         BRAS  RE,SETRTN                                                        
         BZ    *+6                                                              
         DC    H'0'                NON-ZERO RETURN                              
*                                                                               
         LA    R2,SPTDIR1                                                       
         LA    R4,WORK                                                          
         SHOWCB ACB=(R2),AREA=(R4),LENGTH=80,FIELDS=(CINV)                      
*                                                                               
*                                                                               
* RELCI = CISIZE * MOD( RPLDDDD / CISIZE )                                      
* CISIZE IS IN FIRST 4 BYTES OF WORK                                            
*                                                                               
         ICM   RE,15,RPLDDDD                                                    
         SRDL  RE,32                                                            
         D     RE,WORK             RF = MOD( RPLDDDD / CISIZE )                 
         XR    RE,RE                                                            
         M     RE,WORK             RF = RELCI                                   
*                                                                               
         ST    RF,WORK+8                                                        
         MVC   WORK+4(4),RPLDDDD                                                
         L     RF,VPRNTBL                                                       
         GOTO1 (RF),DMCB,=C'CISIZE/RPLDDD/RELCI',WORK,C'DUMP',12,=C'1D'         
*                                                                               
* NOW LOCK RELCI WITH CALL TO (?) LOCKER                                        
*                                                                               
         OI    RPLOPT1,RPLDIR                                                   
         NI    RPLOPT1,255-(RPLSEQ+RPLKGE)                                      
         NI    RPLOPT2,255-RPLNSP                                               
         OI    RPLOPT2,RPLUPD                                                   
         XC    ECB,ECB                                                          
         GET   RPL=(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         BRAS  RE,CHECK            WAIT FOR I/O COMPLETION                      
         BRAS  RE,SETRTN                                                        
         J     EXIT                                                             
????                                                                            
         OI    RPLOPT1,RPLDIR      SET OPTCD=DIR                                
         NI    RPLOPT1,255-RPLSEQ                                               
         OI    RPLOPT2,RPLUPD      SET OPTCD=UPD                                
         XC    ECB,ECB                                                          
         PUT   RPL=(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         BRAS  RE,CHECK            WAIT FOR I/O COMPLETION                      
         BRAS  RE,SETRTN                                                        
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                                                              
         J     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         J     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* CHECK COMPLETION (WAIT)                                             *         
***********************************************************************         
         SPACE 1                                                                
CHECK    NTR1  ,                   WAIT FOR AND CHECK I/O COMPLETION            
         LA    R1,ECB                                                           
         CHECK RPL=(R3)            LET VTAM DO WAIT HERE                        
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* SET RETURN CODE IN RTNFDBK IN W/S                                   *         
***********************************************************************         
         SPACE 1                                                                
SETRTN   MVC   RTNFDBK,RPLFDBK                                                  
         OC    RTNFDBK,RTNFDBK     XIT WITH CC EQL IF ALL OK                    
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OPEN FILE                                                *         
* NTRY: R1     = ACB OF FILE                                          *         
* EXIT: CC EQ  = FILE OPENED OK                                       *         
***********************************************************************         
         SPACE 1                                                                
VSOPEN   NTR1  ,                                                                
         LR    R3,R1                                                            
         USING IFGACB,R3                                                        
         TM    ACBOFLGS,ACBOPEN    ACB OPEN ALREADY?                            
         JO    EXITOK              YES                                          
*                                                                               
         OPEN  ((R3))                                                           
         LTR   RF,RF               RF HOLDS ERROR RETURN CODE                   
         JZ    EXITOK              ALLES GUT                                    
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,ACBERFLG       R0 HOLDS ERROR FLAG                          
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CLOSE FILE                                               *         
* NTRY: R1     = ACB OF FILE                                          *         
* XIT: CC EQ   = FILE CLOSED OK                                       *         
***********************************************************************         
         SPACE 1                                                                
VSCLSE   NTR1  ,                                                                
         LR    R3,R1                                                            
         USING IFGACB,R3                                                        
         TM    ACBOFLGS,ACBOPEN    ACB OPEN?                                    
         JZ    EXITOK              NO                                           
*                                                                               
         CLOSE ((R3))                                                           
         LTR   RF,RF               RF HOLDS ERROR RETURN CODE                   
         BZ    EXITOK              ALLES GUT                                    
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,ACBERFLG       R0 HOLDS ERROR FLAG                          
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* EQUATED VALUES                                                      *         
***********************************************************************         
         SPACE 1                                                                
EOT      EQU   X'FF'                                                            
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ACB DEFINITIONS                                                     *         
***********************************************************************         
         PRINT GEN                                                              
SPTDIR1  ACB   AM=VSAM,                                                X        
               DDNAME=SPTDIR1,                                         X        
               EXLST=S1EXLST,                                          X        
               MACRF=(KEY,CNV,DIR,SEQ,SKP,OUT),                        X        
               RMODE31=ALL,                                            X        
               STRNO=32                                                         
         PRINT NOGEN                                                            
*                                                                               
S1EXLST  EXLST AM=VSAM,                                                X        
               JRNAD=S1JRNAD                                                    
*                                                                               
*                                                                               
RPLSPT1 RPL    ACB=SPTDIR1,                                            X        
               AM=VSAM,KEYLEN=13,RECLEN=18,AREALEN=20,                 X        
               ECB=DECB,                                               X        
               ARG=DKEY,                                               X        
               AREA=DIO,                                               X        
               OPTCD=(KEY,DIR,NUP,MVE,ASY)                                      
         PRINT NOGEN                                                            
*                                                                               
DECB     DC    F'0'                                                             
DKEY     DC    F'0'                                                             
DIO      DC    F'0'                                                             
*                                                                               
VPRNTBL  DC    V(PRNTBL)                                                        
         EJECT                                                                  
***********************************************************************         
* JRNAD EXIT ROUTINE CODE                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING S1JRNAD,RF                                                       
S1JRNAD  L     RD,=A(S1JWORK)                                                   
         DROP  RF                                                               
*                                                                               
         NMOD1 S1JWORKL,S1JRNAD,CLEAR=YES                                       
         USING S1JWORKD,RC                                                      
*                                                                               
         CLI   20(R1),X'50'        CALLED BECAUSE OF CI/CA SPLIT                
         JNE   S1JRNADX                                                         
*                                                                               
* NEED TO ADD CODE HERE TO ADD EXTRA LOCK FOR PARTICULAR CI?                    
*                                                                               
*                                                                               
         MVI   21(R1),X'8C'        CANCEL SPLIT                                 
S1JRNADX XMOD1 ,                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
S1JWORK  DS    1000D                                                            
*                                                                               
S1JWORKD DSECT                                                                  
S1DUB    DS    D                                                                
S1JWORKL EQU   *-S1JWORKD                                                       
*                                                                               
DMVSAM   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
RPL      DS    XL128                                                            
ECB      DS    A                                                                
KEY      DS    XL13                                                             
IO       DS    XL20                                                             
DMCB     DS    6F                                                               
WORK     DS    XL80                                                             
RTNFDBK  DS    X                                                                
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
***********************************************************************         
         SPACE 1                                                                
         EJECT                                                                  
         IFGACB AM=VSAM                                                         
         EJECT                                                                  
         IFGRPL AM=VSAM                                                         
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033DMVSAM1   11/03/99'                                      
         END                                                                    
***********************************************************************         
         SPACE 2                                                                
ADD      ST    RE,SAVERE           ADD RECORD WITH KEY IN GFKEY                 
         XC    FDBK,FDBK                                                        
         OI    RPLOPT1,RPLDIR      SET OPTCD=DIR                                
         NI    RPLOPT1,255-RPLSEQ                                               
         NI    RPLOPT2,255-RPLUPD  SET OPTCD=NUP                                
         TM    MODE,X'02'                                                       
         BZ    ADD1                                                             
         OI    RPLOPT1,RPLSEQ      SET OPTCD=SEQ FOR LOAD MODE                  
         NI    RPLOPT1,255-RPLDIR                                               
ADD1     XC    GFECB,GFECB                                                      
         PUT   RPL=(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   ADD2                                                             
         BAS   RE,CHECK            WAIT FOR I/O COMPLETION                      
ADD2     MVC   FDBK+1(3),RPLFDBK                                                
         OC    FDBK,FDBK           XIT WITH CC EQL IF ALL OK                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
DEL      ST    RE,SAVERE           DEL RECORD WITH KEY IN GFKEY                 
         XC    FDBK,FDBK                                                        
         OI    RPLOPT1,RPLDIR      SET OPTCD=DIR                                
         NI    RPLOPT1,255-RPLSEQ                                               
         OI    RPLOPT2,RPLUPD      SET OPTCD=UPD                                
DEL1     XC    GFECB,GFECB                                                      
         ERASE RPL=(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   DEL2                                                             
         BAS   RE,CHECK            WAIT FOR I/O COMPLETION                      
DEL2     MVC   FDBK+1(3),RPLFDBK                                                
         OC    FDBK,FDBK           XIT WITH CC EQL IF ALL OK                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
PUT      ST    RE,SAVERE           PUT RECORD WITH KEY IN GFKEY                 
         XC    FDBK,FDBK                                                        
         OI    RPLOPT1,RPLDIR      SET OPTCD=DIR                                
         NI    RPLOPT1,255-RPLSEQ                                               
         OI    RPLOPT2,RPLUPD      SET OPTCD=UPD                                
PUT1     XC    GFECB,GFECB                                                      
         PUT   RPL=(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   PUT2                                                             
         BAS   RE,CHECK            WAIT FOR I/O COMPLETION                      
PUT2     MVC   FDBK+1(3),RPLFDBK                                                
         OC    FDBK,FDBK           XIT WITH CC EQL IF ALL OK                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
SEQ      ST    RE,SAVERE           GET NEXT SEQUENTIAL RECORD                   
         XC    FDBK,FDBK                                                        
         OI    RPLOPT1,RPLSEQ      SET OPTCD=SEQ                                
         NI    RPLOPT1,255-RPLDIR                                               
         OI    RPLOPT2,RPLNSP      SET OPTCD=NSP (NOTE STRING POSN)             
         NI    RPLOPT2,255-RPLUPD                                               
         CLI   UPDT,C'Y'                                                        
         BNE   *+12                                                             
         OI    RPLOPT2,RPLUPD      SET OPTCD=UPD                                
         NI    RPLOPT2,255-RPLNSP                                               
SEQ1     XC    GFECB,GFECB                                                      
         GET   RPL=(R3)                                                         
         LTR   RF,RF                                                            
         BNZ   SEQ2                                                             
         BAS   RE,CHECK            WAIT FOR I/O COMPLETION                      
SEQ2     MVC   FDBK+1(3),RPLFDBK                                                
         OC    FDBK,FDBK           XIT WITH CC EQL IF ALL OK                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
*VALIDATE ACTION IN PARAM1 AND OPEN FILE IF NOT ALREADY OPEN          *         
***********************************************************************         
         SPACE 1                                                                
ACTNVAL  L     RF,=A(ACTNTAB)      VALIDATE ACTION IN FIRST PARAMETER           
         SR    R1,R1                                                            
         SR    RE,RE                                                            
         ICM   RE,7,1(R2)          RE=A(ACTION NAME) OR ACTION NUMBER           
         CH    RE,=Y(ACTNMAX)                                                   
         BH    ACTNV1              ACTION IS A NAME                             
         SH    RE,=H'1'                                                         
         BM    ACTNINV             ACTION IS A NUMBER IN LAST BYTE              
         MH    RE,=Y(L'ACTNTAB)                                                 
         AR    RF,RE               INDEX INTO ACTION TABLE                      
         B     ACTNV2                                                           
*                                                                               
ACTNV1   ICM   R1,1,1(RF)          SEARCH TABLE OF VALID ACTION NAMES           
         BZ    ACTNINV                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   2(0,RF),0(RE)                                                    
         BE    ACTNV2                                                           
         LA    RF,L'ACTNTAB(RF)                                                 
         B     ACTNV1                                                           
*                                                                               
ACTNV2   IC    R1,0(RF)            EXTRACT ACTION NUMBER AND FLAGS              
         STC   R1,ACTN                                                          
         STC   R1,SACTN            SAVE THIS ACTION                             
         MVC   ACTNFLGS,8(RF)                                                   
         SLL   R1,2                                                             
         L     R5,=A(ACTNRTN)      R5=A(ROUTINE) FOR ACTION                     
         L     R5,0(R1,R5)                                                      
         TM    ACTNFLGS+1,X'01'                                                 
         BOR   R5                  GO DIRECT TO OPEN/CLOSE ACTIONS              
         TM    OPNFLG,X'01'                                                     
         BOR   R5                                                               
         BAS   RE,OPN              DO A NORMAL OPEN ON FIRST CALL               
         BER   R5                                                               
         L     R0,FDBK                                                          
         STCM  R0,15,*+6                                                        
         DC    H'0',XL4'00',CL12' OPN ERR '                                     
         EJECT                                                                  
***********************************************************************         
*PROCESS EACH INDIVIDUAL ACTION AND SET CORRECT RETURN IN DMCB+8      *         
***********************************************************************         
         SPACE 1                                                                
ACTNGET  MVC   GFKEY(9),0(R6)      SET KEY FROM KEY                             
         TM    0(R2),X'80'                                                      
         BZ    *+8                                                              
         MVI   UPDT,C'Y'                                                        
         MVI   HIGH,C'Y'           SET TO DO KGE                                
         BAS   RE,GET                                                           
         BNE   ACTNGET1                                                         
         CLC   GFKEY(9),0(R7)      TEST IF KEY MATCHES                          
         BE    XIT                 YES                                          
         OI    8(R2),X'10'         NO SET NOT FOUND                             
         B     XIT                                                              
ACTNGET1 CLI   FDBK+3,X'10'        TEST RECORD NOT FOUND                        
         BNE   *+12                                                             
         OI    8(R2),X'10'                                                      
         B     XIT                                                              
         L     R0,FDBK                                                          
         STCM  R0,15,*+6                                                        
         DC    H'0',XL4'00',CL12' GET FDBK'                                     
         SPACE 2                                                                
ACTNGHI  MVC   GFKEY(9),0(R6)      SET KEY FROM KEY                             
         TM    0(R2),X'80'                                                      
         BZ    *+8                                                              
         MVI   UPDT,C'Y'                                                        
         MVI   HIGH,C'Y'                                                        
         BAS   RE,GET                                                           
         BE    XIT                                                              
         CLI   FDBK+3,X'10'        TEST KEY HIGHER THAN THE MAXIMUM             
         BNE   *+12                                                             
         OI    8(R2),X'80'+X'10'                                                
         B     XIT                                                              
         L     R0,FDBK                                                          
         STCM  R0,15,*+6                                                        
         DC    H'0',XL4'00',CL12' GHI FDBK'                                     
         SPACE 2                                                                
ACTNSEQ  BAS   RE,SEQ                                                           
         BE    XIT                                                              
         CLI   FDBK+3,X'04'        TEST EOF                                     
         BNE   *+12                                                             
         OI    8(R2),X'80'+X'10'                                                
         B     XIT                                                              
         L     R0,FDBK                                                          
         STCM  R0,15,*+6                                                        
         DC    H'0',XL4'00',CL12' SEQ FDBK'                                     
         SPACE 2                                                                
ACTNADD  MVC   GFKEY(9),0(R7)      SET KEY FROM RECORD                          
         SR    RF,RF                                                            
         ICM   RF,3,14(R7)         SET LEN FROM RECORD                          
         ST    RF,RPLRLEN                                                       
         BAS   RE,ADD                                                           
         BE    XIT                                                              
*                                  TEST VSAM SPACE ALLOCATION REPORTED          
         CLC   FDBK+1(3),=XL3'000004'   IN REASON CODE BYTE (RETURN=0)          
         BE    XIT                                                              
*                                                                               
         CLI   FDBK+3,X'08'        TEST DUPLICATE KEY                           
         BNE   *+12                                                             
         OI    8(R2),X'20'                                                      
         B     XIT                                                              
         L     R0,FDBK                                                          
         STCM  R0,15,*+6                                                        
         DC    H'0',XL4'00',CL12' ADD FDBK'                                     
         SPACE 2                                                                
ACTNPUT  MVC   GFKEY(9),0(R7)      SET KEY FROM RECORD                          
         SR    RF,RF                                                            
         ICM   RF,3,14(R7)         SET LEN FROM RECORD                          
         ST    RF,RPLRLEN                                                       
         BAS   RE,PUT                                                           
         BE    XIT                                                              
         L     R0,FDBK                                                          
         STCM  R0,15,*+6                                                        
         DC    H'0',XL4'00',CL12' PUT FDBK'                                     
         SPACE 2                                                                
ACTNDEL  MVC   GFKEY(9),0(R6)      SET KEY FROM KEY                             
         BAS   RE,DEL                                                           
         BE    XIT                                                              
         L     R0,FDBK                                                          
         STCM  R0,15,*+6                                                        
         DC    H'0',XL4'00',CL12' DEL FDBK'                                     
         SPACE 2                                                                
ACTNOPN  TM    OPNFLG,X'01'        FILE MUST NOT ALREADY BE OPEN                
         BO    ACTNOPNE                                                         
         L     R4,AGFKACB          SET ACB FOR NORMAL MODE                      
         USING IFGACB,R4                                                        
         OI    ACBMACR1,ACBDIR     TURN ON MACRF=DIR                            
         OI    ACBMACR1,ACBIN      TURN ON MACRF=IN                             
         NI    ACBMACR1,255-ACBOUT TURN OFF MACRF=OUT                           
         NI    ACBMACR3,255-ACBSIS TURN OFF MACRF=SIS                           
         MVI   MODE,X'00'                                                       
         BAS   RE,OPN                                                           
         BE    XIT                                                              
ACTNOPNE L     R0,FDBK                                                          
         STCM  R0,15,*+6                                                        
         DC    H'0',XL4'00',CL12' OPN ERR '                                     
         SPACE 2                                                                
ACTNOPU  TM    OPNFLG,X'01'        FILE MUST NOT ALREADY BE OPEN                
         BO    ACTNOPUE                                                         
         L     R4,AGFKACB          SET ACB FOR NORMAL MODE                      
         USING IFGACB,R4                                                        
         OI    ACBMACR1,ACBDIR     TURN ON MACRF=DIR                            
         NI    ACBMACR1,255-ACBIN  TURN OFF MACRF=IN                            
         OI    ACBMACR1,ACBOUT     TURN ON MACRF=OUT                            
         NI    ACBMACR3,255-ACBSIS TURN OFF MACRF=SIS                           
         MVI   MODE,X'01'          SET MODE IS UPDATIVE                         
         BAS   RE,OPN                                                           
         BE    XIT                                                              
ACTNOPUE L     R0,FDBK                                                          
         STCM  R0,15,*+6                                                        
         DC    H'0',XL4'00',CL12' OPU ERR '                                     
         SPACE 2                                                                
ACTNOPL  TM    OPNFLG,X'01'        FILE MUST NOT ALREADY BE OPEN                
         BO    ACTNOPLE                                                         
         L     R4,AGFKACB          SET ACB FOR LOAD MODE                        
         USING IFGACB,R4                                                        
         NI    ACBMACR1,255-ACBDIR TURN OFF MACRF=DIR                           
         NI    ACBMACR1,255-ACBIN  TURN OFF MACRF=IN                            
         OI    ACBMACR1,ACBOUT     TURN ON MACRF=OUT                            
         OI    ACBMACR3,ACBSIS     TURN ON MACRF=SIS                            
         MVI   MODE,X'02'          SET MODE IS LOAD                             
         BAS   RE,OPN                                                           
         BE    XIT                                                              
ACTNOPLE L     R0,FDBK                                                          
         STCM  R0,15,*+6                                                        
         DC    H'0',XL4'00',CL12' OPL ERR '                                     
         SPACE 2                                                                
ACTNCLS  BAS   RE,CLS                                                           
         BE    XIT                                                              
         L     R0,FDBK                                                          
         STCM  R0,15,*+6                                                        
         DC    H'0',XL4'00',CL12' CLS ERR '                                     
         SPACE 2                                                                
ACTNINV  DC    H'0',CL16'INVALID ACTION'                                        
         EJECT                                                                  
