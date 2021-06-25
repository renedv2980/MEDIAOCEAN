*          DATA SET CTGEN02    AT LEVEL 007 AS OF 03/11/13                      
*PHASE TA0B02A                                                                  
         TITLE 'CTGEN02 - FILE MAINTENANCE - OUTPUT TYPE RECORDS'               
GEN02    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GEN2**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING CTOREC,R2           R2=A(RECORD KEY)                             
         LA    R3,APELEM                                                        
         USING CTOUTD,R3           R3=A(RECORD DATA)                            
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     DELREC                                                           
         B     RESREC                                                           
         B     VALSEL                                                           
         B     GETSEL                                                           
         B     DISSEL                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     LSTSCR                                                           
         B     VALREQ                                                           
         B     PRTREP                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF OUTPUT TYPE RECORD                       *         
***********************************************************************         
VALKEY   LA    R2,IOKEY                                                         
         USING CTOKEY,R2           R2=A(OUTPUT TYPE RECORD KEY)                 
         XC    CTOKEY,CTOKEY                                                    
         MVI   CTOKTYP,C'O'                                                     
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,OUTTYPEH      VALIDATE OUTPUT TYPE FIELD                   
         BNE   VALKEYX                                                          
         CLI   FVIFLD,C'0'         MUST START WITH A-I,J-R,S-Z,0-9              
         BL    *+12                                                             
         CLI   FVIFLD,C'9'                                                      
         BNH   VALKEY2                                                          
         CLI   FVIFLD,C'S'                                                      
         BL    *+12                                                             
         CLI   FVIFLD,C'Z'                                                      
         BNH   VALKEY2                                                          
         CLI   FVIFLD,C'J'                                                      
         BL    *+12                                                             
         CLI   FVIFLD,C'R'                                                      
         BNH   VALKEY2                                                          
         CLI   FVIFLD,C'A'                                                      
         BL    *+12                                                             
         CLI   FVIFLD,C'I'                                                      
         BNH   VALKEY2                                                          
         CLI   FVIFLD,C'+'         UNLESS SPECIALS +,&,#                        
         BE    VALKEY2                                                          
         CLI   FVIFLD,C'&&'                                                     
         BE    VALKEY2                                                          
         CLI   FVIFLD,C'#'                                                      
         BE    VALKEY2                                                          
VALKEY1  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALKEYX                                                          
VALKEY2  MVC   CTOKID,FVIFLD                                                    
         MVC   APRECKEY(L'CTOKEY),CTOKEY                                        
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VALKEYY                                                          
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         B     VALKEYY                                                          
         MVI   APINDS,APIOKADD                                                  
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE AN OUTPUT TYPE RECORD                      *         
***********************************************************************         
VALREC   L     R2,AIOAREA1                                                      
         XC    CTOREC(256),CTOREC  CLEAR KEY ETC.                               
         MVC   CTOKEY,APRECKEY                                                  
         LA    R0,CTODATA+1-CTOREC                                              
         STCM  R0,3,CTOLEN                                                      
         XC    APELEM,APELEM                                                    
         MVI   CTOUTEL,CTOUTELQ    DETAIL ELEMENT CODE X'38'                    
         MVI   CTOUTLEN,CTOUTLLQ   DETAIL ELEMENT LENGTH 64                     
         MVI   FVMINL,1            SET FLAG FOR REQUIRED FIELDS                 
         GOTO1 AFVAL,OUTDESTH                                                   
         BNE   VALRECX                                                          
*                                                                               
         LA    R1,TYPETAB          VALIDATE OUTPUT DESTINATION TYPE             
         ZIC   RE,FVXLEN                                                        
VALREC2  CLI   0(R1),0                                                          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   1(0,R1),FVIFLD                                                   
         BE    *+12                                                             
         LA    R1,L'TYPETAB(R1)                                                 
         B     VALREC2                                                          
         MVC   CTOUTTYP,0(R1)                                                   
*                                                                               
         MVI   FVMINL,1            OUTPUT CLASS                                 
         GOTO1 AFVAL,OUTCLASH                                                   
         BNE   VALRECX                                                          
         GOTO1 VALCHAR,CLASS                                                    
         BNE   VALRECX                                                          
         MVC   CTOUTCLS,FVIFLD                                                  
*                                                                               
         MVI   FVMINL,1            PRIORITY                                     
         GOTO1 AFVAL,OUTPRTYH                                                   
         BNE   VALRECX                                                          
         MVC   CTOUTPRI,FVIFLD                                                  
*                                                                               
         MVI   FVMINL,1            FORMS NUMBER                                 
         GOTO1 AFVAL,OUTPWRCH                                                   
         BNE   VALRECX                                                          
         MVC   CTOUTPOW,FVIFLD                                                  
*                                                                               
         MVI   FVMINL,1            CARRIAGE CONTROL                             
         GOTO1 AFVAL,OUTTAPEH                                                   
         BNE   VALRECX                                                          
         MVC   CTOUTCC,FVIFLD                                                   
*                                                                               
         MVI   FVMINL,1            # COPIES                                     
         GOTO1 AFVAL,OUTCPYSH                                                   
         BNE   VALRECX                                                          
         TM    FVIIND,FVINUM                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALRECX                                                          
*        OC    SCFULL,SCFULL       NOP'D FOR PHIL (JOMUDDLO)                    
*        BNZ   *+14                                                             
*        MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*        B     VALRECX                                                          
         MVC   CTOUTCPY,FVIFLD                                                  
*                                                                               
         MVI   FVMINL,1            OUTPUT STATUS                                
         GOTO1 AFVAL,OUTSTATH                                                   
         BNE   VALRECX                                                          
         GOTO1 VALCHAR,DISPS                                                    
         BNE   VALRECX                                                          
         MVC   CTOUTDIS,FVIFLD                                                  
*                                                                               
         MVI   FVMINL,0            SET OPTIONAL FIELD FLAG                      
         MVI   CTOUTSEP,C'Y'       JOB SEPARATOR                                
         GOTO1 AFVAL,OUTPWRDH                                                   
         BNE   *+10                                                             
         MVC   CTOUTSEP,FVIFLD                                                  
         CLI   CTOUTSEP,C'Y'                                                    
         BE    VALREC4                                                          
         CLI   CTOUTSEP,C'N'                                                    
         BE    VALREC4                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
*                                                                               
VALREC4  GOTO1 AFVAL,OUTTDETH      TAPE DETAILS                                 
         MVC   CTOUTTAP,FVIFLD                                                  
*                                                                               
         GOTO1 AFVAL,OUTUREQH      REQUESTABLE STATUS                           
         BNE   VALREC5                                                          
         CLI   OUTUREQ,C'N'                                                     
         BE    VALREC5                                                          
         CLI   OUTUREQ,C'Y'                                                     
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         OI    CTOUTSTA,X'80'                                                   
*                                                                               
VALREC5  GOTO1 AFVAL,OUTFCHH       FICHE CLASS                                  
         BNE   VALREC6                                                          
         CLI   FVILEN,1                                                         
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     VALRECX                                                          
         MVC   CTOUTFCH,FVIFLD                                                  
*                                                                               
VALREC6  GOTO1 AFVAL,OUTCHRSH      3800 CHARS                                   
         BNE   VALREC7                                                          
         CLI   FVILEN,4                                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     VALRECX                                                          
         MVC   CTOUTCHR,FVIFLD                                                  
*                                                                               
VALREC7  GOTO1 AFVAL,OUTCHR2H      CHARS#2                                      
         BNE   VALREC8                                                          
         CLI   FVILEN,4                                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     VALRECX                                                          
         MVC   CTOUTCH2,FVIFLD                                                  
*                                                                               
VALREC8  GOTO1 AFVAL,OUTCHR3H      CHARS#3                                      
         BNE   VALREC9                                                          
         CLI   FVILEN,4                                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     VALRECX                                                          
         MVC   CTOUTCH3,FVIFLD                                                  
*                                                                               
VALREC9  GOTO1 AFVAL,OUTCHR4H      CHARS#4                                      
         BNE   VALRECA                                                          
         CLI   FVILEN,4                                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     VALRECX                                                          
         MVC   CTOUTCH4,FVIFLD                                                  
*                                                                               
VALRECA  GOTO1 AFVAL,OUTFMNH       NUMBER OF FORM DEFINITIONS                   
         BNE   VALRECB                                                          
         TM    FVIIND,FVINUM       MUST BE NUMERIC                              
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALRECX                                                          
         ICM   R0,15,SCFULL        VALUE 1-35                                   
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         CH    R0,=H'35'                                                        
         BH    *-14                                                             
         STC   R0,CTOUTFMN                                                      
*                                                                               
VALRECB  GOTO1 AFVAL,OUTFMDH       FORM DEFINITION                              
         BE    VALRECB1                                                         
         CLI   CTOUTFMN,0          REQUIRED IF NUMBER INPUT                     
         BE    VALRECC                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
VALRECB1 CLI   CTOUTFMN,0          INVALID IF NUMBER NOT INPUT                  
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         CLI   FVILEN,5            MUST BE 5 BYTES LONG                         
         BNE   *+14                                                             
         MVC   CTOUTFMD,FVIFLD     SAVE VALUE                                   
         B     VALRECC                                                          
         BL    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     VALRECX                                                          
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     VALRECX                                                          
*                                                                               
VALRECC  GOTO1 AFVAL,OUTPGNH       NUMBER OF PAGE DEFINITIONS                   
         BE    VALRECC1                                                         
         CLI   CTOUTFMN,0          REQUIRED IF FORMS IS                         
         BE    VALRECD                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
VALRECC1 TM    FVIIND,FVINUM       MUST BE NUMERIC                              
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALRECX                                                          
         ICM   R0,15,SCFULL        VALUE 1-35                                   
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         CH    R0,=H'35'                                                        
         BH    *-14                                                             
         STC   R0,CTOUTPGN                                                      
VALRECC2 CLC   CTOUTPGN,CTOUTFMN   TEST NUM OF PAGEDEFS TO FORMDEFS             
         BE    VALRECD                                                          
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         CLI   CTOUTPGN,1                                                       
         BNE   *-14                                                             
*                                                                               
VALRECD  GOTO1 AFVAL,OUTPGDH       PAGE DEFINITION                              
         BE    VALRECD1                                                         
         CLI   CTOUTPGN,0          REQUIRED IF NUMBER INPUT                     
         BE    VALRECE                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
VALRECD1 CLI   CTOUTPGN,0          INVALID IF NUMBER NOT INPUT                  
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         CLI   FVILEN,5            MUST BE 5 BYTES LONG                         
         BNE   *+14                                                             
         MVC   CTOUTPGD,FVIFLD     SAVE VALUE                                   
         B     VALRECE                                                          
         BL    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     VALRECX                                                          
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     VALRECX                                                          
*                                                                               
VALRECE  GOTO1 AFVAL,OUTPQCH       PQ CLASS (OPTIONAL)                          
         BNE   VALRECF                                                          
         GOTO1 VALCHAR,CLASS                                                    
         BNE   VALRECX                                                          
         MVC   CTOUTPQC,FVIFLD                                                  
*                                                                               
VALRECF  EQU   *                                                                
*                                                                               
VALRECZ  GOTO1 AADDELS,CTOREC      UPDATE RECORD                                
         GOTO1 ASETACT,CTOREC                                                   
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         CLI   APACTN,ACTADD                                                    
         BNE   *+8                                                              
         LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
VALRECY  MVC   FVMSGNO,=AL2(FVFOK)                                              
VALRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A CHARACTER AGAINST A TABLE                     *         
***********************************************************************         
VALCHAR  CLI   0(R1),EOT                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         LTR   RE,RE                                                            
         BR    RE                                                               
         CLC   0(1,R1),FVIFLD                                                   
         BER   RE                                                               
         LA    R1,1(R1)                                                         
         B     VALCHAR                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF OUTPUT TYPE RECORD                        *         
***********************************************************************         
DISKEY   LA    R2,APRECKEY                                                      
         MVC   OUTTYPE,CTOKID                                                   
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY OUTPUT TYPE RECORD                               *         
***********************************************************************         
DISREC   L     R2,AIOAREA1                                                      
         TWAXC OUTDESTH                                                         
*                                                                               
         BAS   RE,GETDET           GET OUTPUT DETAIL ELEMENT                    
         BE    DISRECX             UNLESS VOID                                  
*                                                                               
DISREC4  MVC   OUTCLAS,CTOUTCLS    JES OUTPUT CLASS                             
         MVC   OUTPRTY,CTOUTPRI    PRIORITY                                     
         MVC   OUTPWRC,CTOUTPOW    FORM NO                                      
         MVC   OUTTAPE,CTOUTCC     CC TAPE                                      
         MVC   OUTCPYS,CTOUTCPY    COPIES                                       
         MVC   OUTSTAT,CTOUTDIS    DISPOSITION                                  
         MVC   OUTPWRD,CTOUTSEP    POWER DIVIDERS                               
         MVC   OUTTDET,CTOUTTAP    TAPE DETAILS                                 
         MVI   OUTUREQ,C'N'                                                     
         TM    CTOUTSTA,X'80'                                                   
         BZ    *+8                                                              
         MVI   OUTUREQ,C'Y'                                                     
         MVC   OUTCHRS,CTOUTCHR                                                 
*                                                                               
         LA    R1,TYPETAB                                                       
DISREC6  CLI   0(R1),EOT           LOCATE OUTPUT TYPE                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R1),CTOUTTYP                                                 
         BE    *+12                                                             
         LA    R1,L'TYPETAB(R1)                                                 
         B     DISREC6                                                          
         MVC   OUTDEST,1(R1)                                                    
*                                                                               
         MVC   OUTFCH,CTOUTFCH     FICHE CLASS                                  
         MVC   OUTCHR2,CTOUTCH2    CHARS#2                                      
         MVC   OUTCHR3,CTOUTCH3    CHARS#3                                      
         MVC   OUTCHR4,CTOUTCH4    CHARS#4                                      
         SR    R0,R0                                                            
         ICM   R0,1,CTOUTFMN       NUMBER OF FORM DEFINITIONS                   
         BZ    DISREC8                                                          
         EDIT  (R0),(3,OUTFMN),ALIGN=LEFT,WRK=APWORK,DUB=APDUB                  
         MVC   OUTFMD,CTOUTFMD     FORM DEFINITION                              
         SR    R0,R0                                                            
         ICM   R0,1,CTOUTPGN       NUMBER OF PAGE DEFINITIONS                   
         BZ    DISREC8                                                          
         EDIT  (R0),(3,OUTPGN),ALIGN=LEFT,WRK=APWORK,DUB=APDUB                  
         MVC   OUTPGD,CTOUTPGD     PAGE DEFINITION                              
*                                                                               
DISREC8  MVC   OUTPQC,CTOUTPQC     PQ CLASS                                     
*                                                                               
DISRECX  GOTO1 ADISACT,CTOREC      DISPLAY ACTIVITY DATE                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE AN OUTPUT TYPE RECORD                             *         
***********************************************************************         
DELREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,CTOREC                                                   
         OI    CTOSTAT,X'80'                                                    
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED OUTPUT TYPE RECORD                     *         
***********************************************************************         
RESREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,CTOREC                                                   
         NI    CTOSTAT,X'FF'-X'80'                                              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RESRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
VALSEL   LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
*                                                                               
         LA    R2,APRECKEY         SET UP FIRST LIST RECORD KEY                 
         XC    CTOKEY,CTOKEY                                                    
         MVI   CTOKTYP,C'O'                                                     
         GOTO1 AFVAL,LSTTYPEH                                                   
         BNE   *+10                                                             
         MVC   CTOKID,FVIFLD                                                    
         XC    SELKEY,SELKEY                                                    
*                                                                               
         GOTO1 AFVAL,LSTDESTH                                                   
         BNE   VALSEL2                                                          
         LA    R1,TYPETAB                                                       
         ZIC   RE,FVXLEN                                                        
VALSEL1  CLI   0(R1),0                                                          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALSELX                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   1(0,R1),FVIFLD                                                   
         BE    *+12                                                             
         LA    R1,L'TYPETAB(R1)                                                 
         B     VALSEL1                                                          
         MVC   SELTYP,0(R1)                                                     
*                                                                               
VALSEL2  GOTO1 AFVAL,LSTCLASH                                                   
         BNE   VALSEL3                                                          
         GOTO1 VALCHAR,CLASS                                                    
         BNE   VALSELX                                                          
         MVC   SELCLS,FVIFLD                                                    
*                                                                               
VALSEL3  GOTO1 AFVAL,LSTPRTYH                                                   
         BNE   *+16                                                             
         MVC   SELPRIL,FVXLEN                                                   
         MVC   SELPRI,FVIFLD                                                    
*                                                                               
         GOTO1 AFVAL,LSTPWRCH                                                   
         BNE   *+16                                                             
         MVC   SELPOWL,FVXLEN                                                   
         MVC   SELPOW,FVIFLD                                                    
*                                                                               
         GOTO1 AFVAL,LSTTAPEH                                                   
         BNE   *+16                                                             
         MVC   SELCCL,FVXLEN                                                    
         MVC   SELCC,FVIFLD                                                     
*                                                                               
         GOTO1 AFVAL,LSTCPYSH                                                   
         BNE   VALSEL6                                                          
         TM    FVIIND,FVINUM                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALSELX                                                          
*        OC    SCFULL,SCFULL       NOP'D FOR PHIL (JOMUDDLO)                    
*        BNZ   *+14                                                             
*        MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*        B     VALSELX                                                          
         MVC   SELCPY,FVIFLD                                                    
*                                                                               
VALSEL6  GOTO1 AFVAL,LSTSTATH                                                   
         BNE   VALSEL7                                                          
         GOTO1 VALCHAR,DISPS                                                    
         BNE   VALSELX                                                          
         MVC   SELDIS,FVIFLD                                                    
*                                                                               
VALSEL7  GOTO1 AFVAL,LSTPWRDH                                                   
         BNE   VALSEL8                                                          
         MVC   SELSEP,FVIFLD                                                    
         CLI   SELSEP,C'Y'                                                      
         BE    VALSEL8                                                          
         CLI   SELSEP,C'N'                                                      
         BE    VALSEL8                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALSELX                                                          
*                                                                               
VALSEL8  GOTO1 AFVAL,LSTTDETH                                                   
         BNE   VALSEL9                                                          
         MVC   SELTAPL,FVXLEN                                                   
         MVC   SELTAP,FVIFLD                                                    
*                                                                               
VALSEL9  GOTO1 AFVAL,LSTFCHH                                                    
         BNE   VALSELA                                                          
         MVC   SELFCH,FVIFLD                                                    
*                                                                               
VALSELA  GOTO1 AFVAL,LSTCHRSH                                                   
         BNE   VALSELB                                                          
         MVC   SELCHRL,FVXLEN                                                   
         MVC   SELCHR,FVIFLD                                                    
*                                                                               
VALSELB  GOTO1 AFVAL,LSTAFPH                                                    
         BNE   VALSELC                                                          
         MVC   SELAFP,FVIFLD                                                    
         CLI   SELAFP,C'Y'                                                      
         BE    VALSELC                                                          
         CLI   SELAFP,C'N'                                                      
         BE    VALSELC                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALSELX                                                          
*                                                                               
VALSELC  GOTO1 AFVAL,LSTPQCH                                                    
         BNE   VALSELY                                                          
         GOTO1 VALCHAR,CLASS                                                    
         BNE   VALSELX                                                          
         MVC   SELPQC,FVIFLD                                                    
*                                                                               
VALSELY  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
GETSEL   LA    R2,IOKEY                                                         
         MVC   CTOKEY,APRECKEY                                                  
         CLI   CTOKTYP,C'O'        TEST FIRST FOR LIST                          
         BE    GETSEL2                                                          
         XC    APRECKEY,APRECKEY                                                
         XC    CTOKEY,CTOKEY       YES - SET INITIAL KEY VALUE                  
         MVI   CTOKTYP,C'O'                                                     
         B     GETSEL6             AND DO READ HIGH                             
GETSEL2  TM    APINDS,2            TEST SEQUENCE BROKEN                         
         BZ    GETSEL4                                                          
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GETSEL8                                                          
         B     GETSELN                                                          
GETSEL4  TM    APINDS,1            TEST READ OR READ HIGH                       
         BNZ   GETSEL8                                                          
GETSEL6  LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
GETSEL8  LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GETSELN                                                          
         L     R2,AIOAREA1                                                      
         CLI   CTOKTYP,C'O'                                                     
         BNE   GETSELN                                                          
*                                                                               
         BAS   RE,GETDET           GET OUTPUT DETAILS ELEMENT                   
         BE    GETSEL8             UNLESS VOID                                  
*                                                                               
         SR    RF,RF                                                            
         OC    SELTYP,SELTYP       DESTINATION FILTER                           
         BZ    *+14                                                             
         CLC   SELTYP,CTOUTTYP                                                  
         BNE   GETSEL8                                                          
*                                                                               
         OC    SELCLS,SELCLS       OUTPUT CLASS FILTER                          
         BZ    *+14                                                             
         CLC   SELCLS,CTOUTCLS                                                  
         BNE   GETSEL8                                                          
*                                                                               
         OC    SELPRI,SELPRI       PRIORITY FILTER                              
         BZ    GETSELC                                                          
         IC    RF,SELPRIL                                                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SELPRI(0),CTOUTPRI                                               
         BNE   GETSEL8                                                          
*                                                                               
GETSELC  OC    SELPOW,SELPOW       FORMS FILTER                                 
         BZ    GETSELE                                                          
         IC    RF,SELPOWL                                                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SELPOW(0),CTOUTPOW                                               
         BNE   GETSEL8                                                          
*                                                                               
GETSELE  OC    SELCC,SELCC         CARRIAGE CONTROL FILTER                      
         BZ    GETSELG                                                          
         IC    RF,SELCCL                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SELCC(0),CTOUTCC                                                 
         BNE   GETSEL8                                                          
*                                                                               
GETSELG  OC    SELCPY,SELCPY       COPIES FILTER                                
         BZ    *+14                                                             
         CLC   SELCPY,CTOUTCPY                                                  
         BNE   GETSEL8                                                          
*                                                                               
         OC    SELDIS,SELDIS       OUTPUT STATUS FILTER                         
         BZ    *+14                                                             
         CLC   SELDIS,CTOUTDIS                                                  
         BNE   GETSEL8                                                          
*                                                                               
         OC    SELSEP,SELSEP       JOB SEPARATOR FILTER                         
         BZ    *+14                                                             
         CLC   SELSEP,CTOUTSEP                                                  
         BNE   GETSEL8                                                          
*                                                                               
         OC    SELTAP,SELTAP       TAPE DETAILS FILTER                          
         BZ    GETSELH                                                          
         IC    RF,SELTAPL                                                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SELTAP(0),CTOUTTAP                                               
         BNE   GETSEL8                                                          
*                                                                               
GETSELH  OC    SELFCH,SELFCH       FICHE CLASS FILTER                           
         BZ    GETSELI                                                          
         CLC   SELFCH,CTOUTFCH                                                  
         BNE   GETSEL8                                                          
*                                                                               
GETSELI  OC    SELCHR,SELCHR       3800 CHARCTERS FILTER                        
         BZ    GETSELJ                                                          
         IC    RF,SELCHRL                                                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SELCHR(0),CTOUTCHR                                               
         BNE   GETSEL8                                                          
*                                                                               
GETSELJ  OC    SELAFP,SELAFP       ADVANCED FUNCTIONS FILTER                    
         BZ    GETSELK                                                          
         CLI   SELAFP,C'Y'         AFP SELECT                                   
         BNE   GETSELJ1                                                         
         CLI   CTOUTFMN,0                                                       
         BNE   GETSELK                                                          
         CLI   CTOUTPGN,0                                                       
         BNE   GETSELK                                                          
         B     GETSEL8                                                          
GETSELJ1 CLI   CTOUTFMN,0          NON AFP SELECT                               
         BNE   GETSEL8                                                          
         CLI   CTOUTPGN,0                                                       
         BNE   GETSEL8                                                          
*                                                                               
GETSELK  OC    SELPQC,SELPQC       PQ CLASS FILTER                              
         BZ    *+14                                                             
         CLC   SELPQC,CTOUTPQC                                                  
         BNE   GETSEL8                                                          
*                                                                               
GETSELL  DS    0H                                                               
*                                                                               
GETSELY  MVC   APRECKEY(L'CTOKEY),CTOKEY                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
         MVC   LISTOTYP,CTOKID                                                  
*                                                                               
         BAS   RE,GETDET           GET OUTPUT DETAILS ELEMENT                   
         BE    DISSELX             UNLESS VOID                                  
*                                                                               
DISSEL4  LA    R1,TYPETAB                                                       
DISSEL6  CLI   0(R1),EOT                                                        
         BE    DISSEL8                                                          
         CLC   0(1,R1),CTOUTTYP                                                 
         BE    *+12                                                             
         LA    R1,L'TYPETAB(R1)                                                 
         B     DISSEL6                                                          
         MVC   LISTDEST,1(R1)                                                   
*                                                                               
DISSEL8  MVC   LISTCLAS,CTOUTCLS                                                
         MVC   LISTPRTY,CTOUTPRI                                                
         MVC   LISTFORM,CTOUTPOW                                                
         MVC   LISTCCTP,CTOUTCC                                                 
         MVC   LISTCPYS,CTOUTCPY                                                
         MVC   LISTSTAT,CTOUTDIS                                                
         MVC   LISTSEPS,CTOUTSEP                                                
         MVC   LISTTAPE,CTOUTTAP                                                
         MVC   LISTFCHE,CTOUTFCH                                                
         MVI   LISTCHAR+4,C' '                                                  
         MVC   LISTCHAR(4),CTOUTCHR                                             
         CLI   CTOUTCH2,0          FLAG ADDITIONAL CHARACTER SETS               
         BE    *+8                                                              
         MVI   LISTCHAR+4,C'+'                                                  
         SR    R0,R0                                                            
         ICM   R0,1,CTOUTFMN       NUMBER OF FORM DEFINITIONS                   
         BZ    DISSELA                                                          
         EDIT  (R0),(2,LISTFDNM),ALIGN=LEFT,WRK=APWORK,DUB=APDUB                
*                                                                               
DISSELA  SR    R0,R0                                                            
         ICM   R0,1,CTOUTPGN       NUMBER OF PAGE DEFINITIONS                   
         BZ    DISSELB                                                          
         EDIT  (R0),(2,LISTPDNM),ALIGN=LEFT,WRK=APWORK,DUB=APDUB                
*                                                                               
DISSELB  MVC   LISTPQC,CTOUTPQC    PQ CLASS                                     
*                                                                               
DISSELX  B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
VALREQ   L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPREQH       VALIDATE REQUESTOR                           
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       SET REQUESTOR                                
*                                                                               
         GOTO1 AVALWHEN,REPWHENH   VALIDATE WHEN                                
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALDEST,REPDESTH   VALIDATE DESTINATION ID                      
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALOTYP,REPOTYPH   VALIDATE OUTPUT TYPE                         
         BNE   VALREQX                                                          
*                                                                               
         MVC   REPDESC,REPDESCL    SET REPORT DESCRIPTION                       
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT OUTPUT TYPE REPORT                                 *         
***********************************************************************         
PRTREP   L     R9,AREP                                                          
         L     R2,AIOAREA1                                                      
         XC    CTOKEY,CTOKEY                                                    
         MVI   CTOKTYP,C'O'                                                     
         MVC   IOKEY,CTOKEY        SET INITIAL KEY VALUE                        
         LA    R1,IOHI+IOCONFIL+IO1                                             
         B     *+8                                                              
PRTREP1  LA    R1,IOSQ+IOCONFIL+IO1                                             
         GOTO1 AIO                                                              
         BNE   PRTREPX                                                          
         CLI   CTOKTYP,C'O'        TEST IF AN OUTPUT TYPE RECORD                
         BNE   PRTREPX                                                          
         MVC   LINEOTYP,CTOKID                                                  
*                                                                               
         BAS   RE,GETDET           GET OUTPUT DETAILS ELEMENT                   
         BE    PRTREP9             UNLESS VOID                                  
*                                                                               
PRTREP4  LA    R1,TYPETAB                                                       
PRTREP6  CLI   0(R1),EOT                                                        
         BE    PRTREP8                                                          
         CLC   0(1,R1),CTOUTTYP                                                 
         BE    *+12                                                             
         LA    R1,L'TYPETAB(R1)                                                 
         B     PRTREP6                                                          
         MVC   LINEDEST,1(R1)                                                   
*                                                                               
PRTREP8  MVC   LINECLAS,CTOUTCLS                                                
         MVC   LINEPRTY,CTOUTPRI                                                
         MVC   LINEFORM,CTOUTPOW                                                
         MVC   LINECCTP,CTOUTCC                                                 
         MVC   LINECPYS,CTOUTCPY                                                
         MVC   LINESTAT,CTOUTDIS                                                
         MVC   LINESEPS,=C'NO '                                                 
         CLI   CTOUTSEP,C'N'                                                    
         BE    *+10                                                             
         MVC   LINESEPS,=C'YES'                                                 
         MVC   LINETAPE,CTOUTTAP                                                
         MVC   LINEFCHE,CTOUTFCH                                                
         MVC   LINECHRS,CTOUTCHR                                                
         MVC   LINECHR2,CTOUTCH2                                                
         MVC   LINECHR3,CTOUTCH3                                                
         MVC   LINECHR4,CTOUTCH4                                                
         SR    R0,R0                                                            
         ICM   R0,1,CTOUTFMN       NUMBER OF FORM DEFINITIONS                   
         BZ    PRTREPA                                                          
         EDIT  (R0),(2,LINEFNUM),ALIGN=LEFT,WRK=APWORK,DUB=APDUB                
         MVC   LINEFDEF,CTOUTFMD                                                
PRTREPA  SR    R0,R0                                                            
         ICM   R0,1,CTOUTPGN       NUMBER OF PAGE DEFINITIONS                   
         BZ    PRTREPB                                                          
         EDIT  (R0),(2,LINEPNUM),ALIGN=LEFT,WRK=APWORK,DUB=APDUB                
         MVC   LINEPDEF,CTOUTPGD                                                
PRTREPB  MVC   LINEREQS,=C'NO '                                                 
         TM    CTOUTSTA,X'80'                                                   
         BZ    *+10                                                             
         MVC   LINEREQS,=C'YES'                                                 
*                                                                               
PRTREP9  GOTO1 VREPORT,REPD                                                     
         B     PRTREP1                                                          
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET OUTPUT DETAILS ELEMENT INTO STANDARD BUFFER          *         
***********************************************************************         
GETDET   LA    R1,CTODATA                                                       
GETDET1  CLI   0(R1),0             END OF RECORD                                
         BE    GETDETX                                                          
         CLI   0(R1),CTOUTELQ      OUTPUT DETAIL                                
         BE    GETDET2                                                          
         ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GETDET1                                                          
*                                                                               
GETDET2  XC    APELEM,APELEM       ELEMENT DATA BUFFER USING R3                 
         ZIC   RF,1(R1)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(R1)                                                  
         LTR   R1,R1                                                            
GETDETX  BR    RE                  RETURN .NE. IF FOUND                         
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
REPDESCL DC    C'OUTPUT LIST'                                                   
         SPACE 1                                                                
TYPETAB  DS    0CL6                                                             
         DC    C'PPRINT'                                                        
         DC    C'QQUEUE'                                                        
         DC    C'BBOTH '                                                        
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
CLASS    DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',AL1(EOT)                 
         SPACE 1                                                                
DISPS    DC    C'DHKLINT',AL1(EOT)                                              
         SPACE 1                                                                
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,C'OUTPUT TYPE LIST'                                        
         SPEC  H2,57,C'----------------'                                        
         SPEC  M1,1,C'OUTPUT   DEST.  OUTPUT   OUTPUT   FORM'                   
         SPEC  M2,1,C' TYPE    -----  CLASS   PRIORITY  CODE'                   
         SPEC  M1,41,C' CC   COPIES  OUTPUT  SEPS.   TAPE   FICHE'              
         SPEC  M2,41,C'TAPE  ------  STATUS  -----  DETAIL  CLASS'              
         SPEC  M1,85,C'  3800 CHARACTERS     FORM       PAGE      REQ.'         
         SPEC  M2,85,C'#1   #2   #3   #4    #  DEFN.   #  DEFN.   ----'         
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         EJECT                                                                  
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENFDD                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENDDD                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENBDD                                                       
         ORG                                                                    
         EJECT                                                                  
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL(L'LSTACT1)       ACTION FIELD                                 
LISTLINH DS    XL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTOTYP DS    CL7                 OUTPUT TYPE                                  
         DS    CL1                                                              
LISTDEST DS    CL5                 DESTINATION                                  
         DS    CL2                                                              
LISTCLAS DS    CL1                 OUTPUT CLASS                                 
         DS    CL2                                                              
LISTPRTY DS    CL2                 OUTPUT PRIORITY                              
         DS    CL2                                                              
LISTFORM DS    CL4                 FORMS CODE                                   
         DS    CL2                                                              
LISTCCTP DS    CL4                 CARRIAGE CONTROL TAPE                        
         DS    CL2                                                              
LISTCPYS DS    CL1                 NUMBER OF COPIES                             
         DS    CL2                                                              
LISTSTAT DS    CL1                 OUTPUT STATUS                                
         DS    CL3                                                              
LISTSEPS DS    CL1                 JOB SEPARATORS                               
         DS    CL3                                                              
LISTTAPE DS    CL6                 TAPE DETAILS                                 
         DS    CL2                                                              
LISTFCHE DS    CL1                 FICHE CLASS                                  
         DS    CL2                                                              
LISTCHAR DS    CL5                 3800 CHARACTERS                              
         DS    CL2                                                              
LISTFDNM DS    CL2                 FORM DEFINITION NUMBER                       
         DS    CL2                                                              
LISTPDNM DS    CL2                 PAGE DEFINITION NUMBER                       
         DS    CL2                                                              
LISTPQC  DS    CL1                 PQ CLASS                                     
         ORG   LISTLIN+L'LISTLIN                                                
         SPACE 2                                                                
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
LINEOTYP DS    CL7                 OUTPUT TYPE                                  
         DS    CL2                                                              
LINEDEST DS    CL5                 DESTINATION                                  
         DS    CL4                                                              
LINECLAS DS    CL1                 OUTPUT CLASS                                 
         DS    CL8                                                              
LINEPRTY DS    CL2                 OUTPUT PRIORITY                              
         DS    CL5                                                              
LINEFORM DS    CL4                 FORMS CODE                                   
         DS    CL2                                                              
LINECCTP DS    CL4                 CARRIAGE CONTROL TAPE                        
         DS    CL4                                                              
LINECPYS DS    CL1                 NUMBER OF COPIES                             
         DS    CL7                                                              
LINESTAT DS    CL1                 OUTPUT STATUS                                
         DS    CL6                                                              
LINESEPS DS    CL3                 JOB SEPARATORS                               
         DS    CL3                                                              
LINETAPE DS    CL6                 TAPE DETAILS                                 
         DS    CL4                                                              
LINEFCHE DS    CL1                 FICHE CLASS                                  
         DS    CL4                                                              
LINECHRS DS    CL4                 3800 CHARACTERS                              
         DS    CL1                                                              
LINECHR2 DS    CL4                 3800 CHARACTERS                              
         DS    CL1                                                              
LINECHR3 DS    CL4                 3800 CHARACTERS                              
         DS    CL1                                                              
LINECHR4 DS    CL4                 3800 CHARACTERS                              
         DS    CL2                                                              
LINEFNUM DS    CL2                 FORM DEFINITION NUMBER                       
         DS    CL1                                                              
LINEFDEF DS    CL6                 FORM DEFINITION                              
         DS    CL2                                                              
LINEPNUM DS    CL2                 PAGE DEFINITION NUMBER                       
         DS    CL1                                                              
LINEPDEF DS    CL6                 PAGE DEFINITION                              
         DS    CL2                                                              
LINEREQS DS    CL3                 REQUESTABLE FLAG                             
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
SELKEY   DS    0XL34                                                            
SELOKTYP DS    XL1                                                              
SELTYP   DS    CL1                 DESTINATION TYPE                             
SELCLS   DS    CL1                 OUTPUT CLASS                                 
SELPRIL  DS    XL1                                                              
SELPRI   DS    CL2                 OUTPUT PRIORITY                              
SELPOWL  DS    XL1                                                              
SELPOW   DS    CL4                 FORMS NUMBER                                 
SELCCL   DS    XL1                                                              
SELCC    DS    CL4                 CARRIAGE CONTROL                             
SELCPY   DS    CL1                 COPIES                                       
SELDIS   DS    CL1                 OUTPUT STATUS                                
SELSEP   DS    CL1                 JOB SEPARATORS                               
SELTAPL  DS    XL1                                                              
SELTAP   DS    CL6                 TAPE DETAILS                                 
SELFCH   DS    CL1                 FICHE CLASS                                  
SELCHRL  DS    XL1                                                              
SELCHR   DS    CL4                 3800 CHARACTERS                              
SELAFP   DS    CL1                 ADVANCED FUNCTION FILTER                     
SELPQC   DS    CL1                 PQ CLASS                                     
         ORG   SELKEY+L'SELKEY                                                  
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007CTGEN02   03/11/13'                                      
         END                                                                    
