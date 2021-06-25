*          DATA SET SRIND00A   AT LEVEL 051 AS OF 05/01/02                      
*PHASE T16500A                                                                  
*&&      SET   SF=Y,TT=N,NOP=N                                                  
         TITLE '$INDFILE - 3270 STRUCTURED FIELD TRANSMISSIONS'                 
         PRINT NOGEN                                                            
INDFILE  CSECT                                                                  
*&&NOP                                                                          
R0       EQU   0                                                                
R1       EQU   1                                                                
R2       EQU   2                                                                
R3       EQU   3                                                                
R4       EQU   4                                                                
R5       EQU   5                                                                
R6       EQU   6                                                                
R7       EQU   7                                                                
R8       EQU   8                                                                
R9       EQU   9                                                                
RA       EQU   10                                                               
RB       EQU   11                                                               
RC       EQU   12                                                               
RD       EQU   13                                                               
RE       EQU   14                                                               
RF       EQU   15                                                               
         STM   RE,RC,12(RD)                                                     
         BASR  RB,0                                                             
         LA    RB,0(RB)                                                         
         SH    RB,12(RB)                                                        
         B     32(RB)                                                           
         DC    AL2(6)                                                           
         DC    AL2((WORKL+7)/8)                                                 
         DC    CL8'**INDF**'                                                    
         DC    AL2(4096)                                                        
         USING *-32,RB                                                          
         XR    RC,RC                                                            
         ICM   RC,3,20(RB)                                                      
         SLL   RC,3                                                             
         LA    RC,72(RC,RD)                                                     
         ST    RD,4(RC)                                                         
         ST    RC,8(RD)                                                         
         MVC   0(4,RD),24(RB)                                                   
         LR    RD,RC                                                            
         L     RC,4(RD)                                                         
         LA    RC,72(RC)                                                        
         CNOP  0,4                                                              
         B     *+8                                                              
         DC    A(*)                                                             
         LA    R2,*-4                                                           
         S     R2,*-8                                                           
         USING WORKD,RC                                                         
*&&                                                                             
         NMODL WORKL,**INDF**,CLEAR=YES,RR=R2                                   
         USING WORKD,RC                                                         
         ST    R2,RELO                                                          
         USING SRPARMD,R1                                                       
         MVC   SRPARS,SRPARM1      SAVE CALLING PARAMS                          
         L     RA,SRQATWA                                                       
         USING T165FFD,RA                                                       
         L     R9,SRQATIA          USED TO READ TWA SAVE STORAGE                
         USING SRSD,R9                                                          
         L     R3,SRQAUTL                                                       
         USING UTLD,R3                                                          
         ST    R3,AUTL                                                          
*                                                                               
         MVC   ASYSFAC,SRQASYSF                                                 
         MVC   ACOMFACS,SRQACOMF                                                
         DROP  R1                                                               
*                                                                               
         NI    TTYPE2,X'FF'-TTYPE2CH     RESET CHAINED WRITE FLAG               
         L     R1,=A(BUFFER)                                                    
         A     R1,RELO                                                          
         ST    R1,ABUFFER                                                       
         L     R1,=A(PKZIP)                                                     
         A     R1,RELO                                                          
         ST    R1,APKZIP                                                        
*                                                                               
         L     RE,ASYSFAC                                                       
         USING SYSFACD,RE                                                       
         MVC   AIMPLODE,VIMPLODE                                                
         L     RE,VSSB             GET SSB ADDRESS                              
         MVC   RECLEN,SSBTWAL-SSBD(RE)                                          
         DROP  RE                                                               
*                                                                               
         BAS   RE,RDTWA            READ TWA11                                   
*                                                                               
         CLC   SV$INDF,=C'$INDF'   TEST FIRST TIME IN                           
         BE    MAIN02                                                           
         MVC   SV$INDF,=C'$INDF'   SET $INDF ID                                 
         BAS   RE,FLDWRT                                                        
         B     MAINX                                                            
*                                                                               
MAIN02   NI    TTYPE2,255-TTYPE2SF                                              
         MVC   INDOUT(30),=CL30'Who''s your daddy!!'                            
         OI    INDOUTH+(FHOI-FHD),FHOITR                                        
*                                                                               
MAINX    BAS   RE,WRTWA                                                         
         XMOD1 ,                                                                
         EJECT                                                                  
***********************************************************************         
* WRITE STRUCTURED FIELD TO TBUFF                                     *         
***********************************************************************         
         SPACE 1                                                                
FLDWRT   NTR1  ,                                                                
         GOTO1 APKZIP              ZIP UP BUFFER                                
*                                                                               
         L     R8,TBUFF                                                         
         MVC   0(WSFLN,R8),WSF     SET WRITE STRUCTURED FIELD                   
         LA    R8,WSFLN(R8)                                                     
         MVC   0(INSERTLN,R8),INSERT                                            
         LA    R2,10(R8)           LENGTH BYTES                                 
*                                                                               
         LA    R7,INSERTLN+2(R8)                                                
         LA    R8,INSERTLN(R8)     R8 IS LENGTH AREA                            
*                                                                               
         LH    R1,OUTBUFFL         FIRST HALFWORD IS LENGTH                     
         LA    R1,8(R1)                                                         
         STCM  R1,3,0(R7)                                                       
         LA    R7,2(R7)                                                         
         LA    R0,1                FIRST FULLWORD IS FRAME                      
         STCM  R0,15,0(R7)                                                      
         LA    R7,4(R7)                                                         
         XC    0(2,R7),0(R7)                                                    
         OI    0(R7),X'C0'                                                      
         LA    R7,2(R7)                                                         
         LR    R0,R7                                                            
         LH    R1,OUTBUFFL                                                      
         LA    RE,OUTBUFF                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY IN OUTPUT BUFFER                        
*                                                                               
         AH    R7,OUTBUFFL                                                      
         LR    R1,R7               R1=LENGTH OF DATA+COMMAND                    
         SR    R1,R2                                                            
         STCM  R1,3,0(R2)                                                       
         LA    R1,3(R7)                                                         
         SR    R1,R8                                                            
         STCM  R1,3,0(R8)                                                       
*                                                                               
         L     RE,TBUFF                                                         
         SR    R7,RE                 R8 HAS DATA LEN                            
         SH    RE,=H'8'              BACK UP TO HEADER                          
         STH   R7,6(RE)              SET MESSAGE LENGTH                         
*                                                                               
         OI    TTYPE2,TTYPE2SF+TTYPE2CH                                         
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* READ TWAB                                                           *         
***********************************************************************         
         SPACE 1                                                                
RDTWA    NTR1  ,                                                                
         LA    R2,SRPAGENO         READ IN S/R SAVE TWA                         
         SLL   R2,32-8                                                          
         ICM   R2,3,TNUM                                                        
         L     RF,ACOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 (RF),DMCB,(X'80',=C'DMREAD'),=C'TEMPSTR',(R2),(R9)               
         CLI   8(R1),0                                                          
         BE    RDTWAX                                                           
         DC    H'0'                                                             
*                                                                               
RDTWAX   XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* WRITE TWAB                                                          *         
***********************************************************************         
         SPACE 1                                                                
WRTWA    NTR1                                                                   
         LA    R2,SRPAGENO         WRITE BACK S/R SAVE DATA                     
         SLL   R2,32-8                                                          
         ICM   R2,3,TNUM                                                        
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         L     RF,ACOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'00',=C'DMWRT'),=C'TEMPSTR',(R2),(R9)                
         CLI   8(R1),0                                                          
         BE    WRTWAX                                                           
         DC   H'0'                                                              
*                                                                               
WRTWAX   XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* IND$FILE MESSAGES                                                   *         
***********************************************************************         
         SPACE 1                                                                
WSF      DC    X'F3'               WSF                                          
         DC    X'0006'             SF LEN                                       
         DC    X'40'               OUTBOUND 3270 DS ID CODE                     
         DC    X'00'               PID                                          
         DC    X'F1C2'             WRITE                                        
WSFLN    EQU   *-WSF                                                            
*                                                                               
INSERT   DC    AL2(10)                                                          
         DC    X'D047110105008000'   BYTES 2-9                                  
*                                                                               
         DC    AL2(0)                BYTES 10-11                                
         DC    X'D04704C08061'       BYTES 12-17                                
INSERTLN EQU   *-INSERT                                                         
*                                                                               
RDPART   DC    X'0005'                                                          
         DC    X'01FF02'           READ PARTITION 1 QUERY                       
RDPARTLN EQU   *-RDPART                                                         
*                                                                               
HOSTOP   DC    AL2(HOSTOPLN)                                                    
         DC    X'D00012010601010403'   BYTES 2-10                               
         DC    X'0A0A00000000110101'   BYTES 11-19                              
         DC    X'0050055203F003'       BYTES 20-26                              
         DC    X'0946543A44415441'                                              
HOSTOPLN EQU   *-HOSTOP                                                         
*                                                                               
HOSTOP2  DC    AL2(HOSTOP2L)                                                    
         DC    X'D00012010601010403'   BYTES 2-10                               
         DC    X'0A0A00000000110101'   BYTES 11-19                              
         DC    X'0050055203F003'       BYTES 20-26                              
         DC    X'0946543A4D534720'                                              
HOSTOP2L EQU   *-HOSTOP2                                                        
*                                                                               
XFRCOMP  DS    0H                                                               
         DC    X'000AD047110105008000005FD04704C080'                            
         DC    X'61005A5452414E53303320202046696C65207472'                      
         DC    X'616E7366657220636F6D706C65746524'                              
         DC    52X'20'                                                          
XFRCOMPL EQU   *-XFRCOMP                                                        
*                                                                               
CLOSE    DC    AL2(CLOSELN)                                                     
         DC    X'D04112'                                                        
CLOSELN  EQU   *-CLOSE                                                          
         SPACE 1                                                                
TRANERR  DS    0C                                                               
         DC    X'00',X'0AD04711',X'01050080',X'00005FD0',X'4704C080'            
         DC    X'61005A54'                                                      
         DC    X'52414E53',X'31372020',X'204D6973',X'73696E67'                  
         DC    X'206F7220',X'696E636F',X'72726563'                              
         DC    X'74205453',X'4F206461',X'74612073',X'6574206E'                  
         DC    X'616D653A',X'2066696C',X'65207472'                              
         DC    X'616E7366',X'65722063',X'616E6365',X'6C656424'                  
         DC    X'20202020',X'20202020',X'20202020'                              
TRANELN  EQU   *-TRANERR                                                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PKZIP PRINT BUFFER AT PBUFF+2 FOR LENGTH AT PBUFF                   *         
***********************************************************************         
         SPACE 1                                                                
PKZIP    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    PKREAD,PKREAD       PARAMETERS FOR READ                          
         OI    PKREAD,X'80'        DIRECT ADDRESS MODE                          
         LA    RF,READ                                                          
         ST    RF,PKREAD+4                                                      
         XC    PKWRITE,PKWRITE     PARAMETERS FOR WRITE                         
         OI    PKWRITE,X'80'       DIRECT ADDRESS MODE                          
         LA    RF,WRITE                                                         
         ST    RF,PKWRITE+4                                                     
         LH    RF,=Y(2048)         DICTIONARY SIZE (1024,2048 OR 4096)          
         ST    RF,DICTPARM                                                      
         LA    RF,IMPLBNRY         BINARY COMPRESSION                           
*        LA    RF,IMPLASCI         ASCII  COMPRESSION                           
         ST    RF,COMPPARM                                                      
*                                                                               
         LA    R3,PZPARMS                                                       
         USING IMPLODDS,R3                                                      
         LA    RF,PKREAD                                                        
         ST    RF,IMPLRDFH       P1  SET A(PARAMETERS FOR READ)                 
         ST    RC,IMPLRDDT       P2  SET A(WORK AREA FOR READ)                  
         LA    RF,PKWRITE                                                       
         ST    RF,IMPLWRFH       P3  SET A(PARAMETERS FOR WRITE)                
         ST    RC,IMPLWRDT       P4  SET A(WORK AREA FOR WRITE)                 
*                                                                               
         LA    RF,ZIPWORK                                                       
         ST    RF,IMPLWKBF       P5  SET A(64K BUFFER FOR ZIP W/S)              
*                                                                               
         LA    RF,DICTPARM                                                      
         ST    RF,IMPLDCSZ       P6  SET A(DICTIONARY SIZE)                     
         LA    RF,COMPPARM                                                      
         ST    RF,IMPLMODE       P7  SET A(COMPRESSION MODE)                    
         XC    IMPLFHPL,IMPLFHPL P8                                             
         XC    IMPLCRC,IMPLCRC   P9                                             
*                                                                               
         WTO   'ENTERING ZIP'                                                   
*                                                                               
         ST    RD,PZSAVERD         SAVE MY RD CHAIN                             
         LA    RD,PZREGS           GIVE ZIP A 72 BYTE SAVEAREA                  
         GOTO1 AIMPLODE,PZPARMS                                                 
*                                                                               
         L     RD,PZSAVERD         RESTORE MY RD                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                NZ RETURN IS AN ERROR                        
*                                                                               
         BAS   RE,ZIPMSG                                                        
*                                                                               
PKZIPX   XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* WRITE OUT SUCCESS MESSAGE                                           *         
***********************************************************************         
         SPACE 1                                                                
ZIPMSG   NTR1  ,                                                                
         L     R0,PZINONE                                                       
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  INPNUM,DUB                                                       
*                                                                               
         L     R0,PZOUTLEN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  OUTNUM,DUB                                                       
*                                                                               
         XR    R0,R0                HAVE TO CLEAR FOR MULTILINE WTO             
         WTO   TEXT=((MSGHL,C),(MSG1L,D),(MSG2L,D),(0,E))                       
ZIPMSGX  XIT1  ,                                                                
*                                                                               
MSGHL    DC    AL2(27)                                                          
MSGH     DC    CL27'*PKZIP STREAM DATA RESULTS*'                                
*                                                                               
MSG1L    DC    AL2(L'MSG1+L'INPNUM)                                             
MSG1     DC    C'Input  byte count='                                            
INPNUM   DC    CL6'       '                                                     
*                                                                               
MSG2L    DC    AL2(L'MSG2+L'INPNUM)                                             
MSG2     DC    C'Output byte count='                                            
OUTNUM   DC    CL6'       '                                                     
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ ROUTINE CALLED BY PKZIP AS REQUIRED                            *         
***********************************************************************         
         SPACE 1                                                                
READ     STM   RE,RC,12(RD)                                                     
         BASR  RB,R0                                                            
         LA    RB,0(RB)                                                         
         SH    RB,12(RB)                                                        
         B     32(RB)                                                           
         DC    AL2(6)                                                           
         DC    AL2(0)                                                           
         DC    CL8'**RZIP**'                                                    
         DC    AL2(4096)                                                        
         USING *-32,RB                                                          
*                                                                               
         LR    R3,R1               GET INCOMING PARAMS                          
         USING FILEHDS,R3                                                       
*                                                                               
         L     RC,FHCLDATA         RESTORE OUR W/S                              
         USING WORKD,RC                                                         
         ST    RD,CALLRD                                                        
         L     RD,SAVERD           RESTORE OUR RD CHAIN                         
*                                                                               
         L     RF,FHOPTYPE         A(OPERATION TYPE)                            
         CLI   3(RF),FHOPREAD      READ NEXT BUFFER OF DATA                     
         BE    READBUFF                                                         
         CLI   3(RF),FHOPOPRD      OPEN FILE FOR READ                           
         BE    READOPEN                                                         
         CLI   3(RF),FHOPCLRD      CLOSE FILE FOR READ                          
         BE    READCLSE                                                         
         DC    H'0'                                                             
*                                                                               
READOPEN WTO   'READ OPEN CALL'                                                 
*                                                                               
         L     R1,ABUFFER                                                       
         ST    R1,PZINADR                                                       
         LH    R1,=Y(BUFFERL)                                                   
         ST    R1,PZINLEN                                                       
         ST    R1,PZINONE                                                       
         B     READX                                                            
*                                                                               
READCLSE WTO   'READ CLOSE CALL'                                                
         B     READX                                                            
*                                                                               
READBUFF WTO   'READ BUFFER CALL'                                               
*                                                                               
         L     R4,FHBUFFER         R4=A(BUFFER TO FILL)                         
         L     R5,FHBFFRSZ         R5=A(BUFFER LENGTH)                          
         ICM   R5,15,0(R5)         R5=BUFFER LENGTH                             
*                                                                               
         L     RE,PZINADR          RE=COPY FROM POINT                           
         ICM   RF,15,PZINLEN       LENGTH LEFT TO COPY                          
         BZ    READ02                                                           
*                                                                               
         CR    RF,R5               CHECK LENGTH REMAINING                       
         BH    *+6                                                              
         LR    R5,RF               SET LENGTH TO BE AMOUNT REMAINING            
*                                                                               
         SR    RF,R5                                                            
         ST    RF,PZINLEN          REDUCE AMOUNT IN INPUT BUFFER                
         LR    RF,R5                                                            
*                                                                               
         L     R6,FHLENGTH         SET LENGTH MOVED INTO BUFFER                 
         ST    R5,0(R6)                                                         
         MVCL  R4,RE                                                            
         B     READX                                                            
*                                                                               
READ02   L     RE,FHLENGTH         SET NO MORE DATA                             
         ST    RF,0(RE)                                                         
         B     READX                                                            
*                                                                               
READX    XR    RF,RF                                                            
         L     RE,FHRESULT         SET RESULT OK IN ALL CASES                   
         ST    RF,0(RE)                                                         
*                                                                               
         L     RD,CALLRD                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WRITE TO FILE                                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
WRITE    STM   RE,RC,12(RD)                                                     
         BASR  RB,R0                                                            
         LA    RB,0(RB)                                                         
         SH    RB,12(RB)                                                        
         B     32(RB)                                                           
         DC    AL2(6)                                                           
         DC    AL2(0)                                                           
         DC    CL8'**WZIP**'                                                    
         DC    AL2(4096)                                                        
         USING *-32,RB                                                          
*                                                                               
         LR    R3,R1               RESTORE OUR W/S                              
         USING FILEHDS,R3                                                       
*                                                                               
         ICM   RC,15,FHCLDATA                                                   
         USING WORKD,RC                                                         
         ST    RD,CALLRD                                                        
         L     RD,SAVERD                                                        
*                                                                               
         L     RF,FHOPTYPE                                                      
         CLI   3(RF),FHOPWRIT      PUT NEXT BUFFER OF DATA                      
         BE    WRITBUFF                                                         
         CLI   3(RF),FHOPOPWR      OPEN FILE FOR WRITE                          
         BE    WRITOPEN                                                         
         CLI   3(RF),FHOPCLWR      CLOSE FILE FOR WRITE                         
         BE    WRITCLSE                                                         
         DC    H'0'                                                             
*                                                                               
WRITOPEN WTO   'WRITE OPEN CALL'                                                
*                                                                               
         XC    PZOUTLEN,PZOUTLEN                                                
         LA    RF,OUTBUFF                                                       
         ST    RF,PZOUTADR                                                      
         B     WRITEX                                                           
*                                                                               
WRITCLSE WTO   'WRITE CLOSE CALL'                                               
         B     WRITEX                                                           
*                                                                               
WRITBUFF WTO   'WRITE BUFFER CALL'                                              
*                                                                               
         L     R4,FHBUFFER         R4=A(BUFFER TO WRITE)                        
         L     R5,FHBFFRSZ         R5=A(BUFFER LENGTH)                          
         ICM   R5,15,0(R5)         R5=BUFFER LENGTH                             
         A     R5,PZOUTLEN                                                      
         ST    R5,PZOUTLEN                                                      
         STH   R5,OUTBUFFL                                                      
*                                                                               
         LA    RE,OUTBUFF                                                       
         LR    RF,R5                                                            
         MVCL  RE,R4                                                            
*                                                                               
WRITEX   XR    RF,RF                                                            
         L     RE,FHRESULT         SET RESULT OK IN ALL CASES                   
         ST    RF,0(RE)                                                         
         L     RD,CALLRD                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
BUFFER   DC    C'Jacques et Jill went up le hill pour fetch un pail '           
         DC    C'd''eau. Jacques est tombe et casse son tete et Jill '          
         DC    C'came tumbling apres. Bon francais n''est-ce pas? '             
         DC    C'<Page Break> Seriously this is the first real test '           
         DC    C'of PKZIP, the surprisingly good compression ratio '            
         DC    C'we are achieving is due to the use of binary type '            
         DC    C'compression, which is a little surprising since this '         
         DC    C'is text. Perhaps we would do better if we were to tra'         
         DC    C'nslate the text stream to ASCII before compressing it'         
         DC    C'. You never know, this could work............. '               
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Tset 1 Tset 1 Tset 1 Tset 1 Tset 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Tset 1 Tset 1 Tset 1 Tset 1 Tset 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Tset 1 Tset 1 Tset 1 Tset 1 Tset 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' Test 1 Test 1 Test 1 Test 1 Test 1'                           
         DC    C' Test 2 Test 2 Test 2 Test 2 Test 2'                           
         DC    C' ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxy'          
         DC    C' z. Where oh where has my little dog (petit chien) '           
         DC    C' gone, where oh where can it be?? '                            
         DC    C' End of Test Stream 5''x'' to follow xxxxx'                    
BUFFERL  EQU   *-BUFFER                                                         
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
RELO     DS    F                                                                
SRPARS   DS    XL32                                                             
FULL     DS    F                                                                
*                                                                               
ASYSFAC  DS    A                                                                
ACOMFACS DS    A                                                                
AUTL     DS    A                                                                
ABUFFER  DS    A                                                                
APKZIP   DS    A                                                                
AIMPLODE DS    A                                                                
*                                                                               
DMCB     DS    6F                                                               
RECLEN   DS    H                                                                
DATALEN  DS    H                   LENGTH OF DATA LINE                          
BYTE     DS    X                                                                
*                                                                               
PZPARMS  DS    9F                  PARAMETER LIST FOR PKZIP                     
PZWRKA   DS    A                   A(64K W/S FOR PKZIP)                         
PZCALLRD DS    A                   PKZIP CALL RD                                
PZSAVERD DS    A                   PKZIP SAVE RD                                
PZREGS   DS    XL72                72 BYTE REGISTER AREA FOR PKZIP              
PZINLEN  DS    F                                                                
PZINONE  DS    F                                                                
PZINADR  DS    F                                                                
PZOUTLEN DS    F                                                                
PZOUTADR DS    F                                                                
PKREAD   DS    2F                  PARAMETERS FOR READ FILE HANDLER             
PKWRITE  DS    2F                  PARAMETERS FOR WRITE FILE HANDLER            
COMPPARM DS    F                   COMPRESSION PARAMETER                        
DICTPARM DS    F                   DICTIONARY SIZE PARAMETER                    
SAVERD   DS    F                                                                
CALLRD   DS    F                                                                
*                                                                               
OUTBUFFL DS    H                                                                
OUTBUFF  DS    1000X                                                            
*                                                                               
ZIPWORK  DS    64000X                                                           
WORKL    EQU   *-WORKD                                                          
*                                                                               
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                               
**********************************************************************          
         SPACE 1                                                                
* DDFLDIND                                                                      
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
T165FFD  DSECT                                                                  
         DS    CL64                                                             
* SRINDFFD                                                                      
       ++INCLUDE SRINDFFD                                                       
       EJECT                                                                    
         EJECT                                                                  
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
*FADSECTS                                                                       
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
*DDFH                                                                           
       ++INCLUDE DDFH                                                           
*IMPLODDDS                                                                      
         IMPLODDS                                                               
         EJECT                                                                  
*FILEHDS                                                                        
         FILEHDS                                                                
         EJECT                                                                  
***********************************************************************         
* SAVED STORAGE                                                                 
***********************************************************************         
         SPACE 1                                                                
SRSD     DSECT                                                                  
         ORG   SR$INDF             DEFINE SAVE AREA FOR IND$FILE                
SV$INDF  DS    CL5                                                              
SVACTN   DS    X                                                                
SVMSG    DS    X                   SAVE READY MESSAGE #                         
SVERRCD  DS    X                   SAVE ERROR MESSAGE NUMBER                    
SVRECNUM DS    F                   NEXT RECORD NUMBER TO BE PROCESSED           
*                                                                               
SVUKEY   DS    0CL10               WRKF FILE KEY                                
SVUSER   DS    XL2                 USER ID                                      
SVSUBID  DS    CL3                                                              
SVTYPE   DS    CL1                                                              
SVDAY    DS    XL1                                                              
SVCLASS  DS    CL1                                                              
SVFILNUM DS    XL2                 SEQUENCE #                                   
*                                                                               
SVFLAGS  DS    X                                                                
SVFNWKOK EQU   X'80'                - NWK SECURITY CLEARED                      
SVFDONE  EQU   X'40'                - TRANSFER COMLPLETE                        
SVFOPEN  EQU   X'20'                - FILE OPEN                                 
*                                                                               
SVDIE    DS    F                                                                
SVARGS   DS    CL80                SAVE CARD FOR FILENAME                       
SAVEBDSP DS    F                   DISPLACEMENT TO BUFFER                       
SAVEWKID DS    CL8                 WORKER FILE NAME                             
TWASAVA  DS    XL30                                                             
TWASAVB  DS    XL100                                                            
SVINDLQ  EQU   *-SV$INDF                                                        
******************************************************************              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051SRIND00A  05/01/02'                                      
         END                                                                    
