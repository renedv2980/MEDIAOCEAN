*          DATA SET SRGLO00    AT LEVEL 006 AS OF 05/01/02                      
*PHASE T11500A                                                                  
*INCLUDE TIMEOUT                                                                
*INCLUDE NUMVAL                                                                 
         PRINT NOGEN                                                            
         TITLE '$GLOBAL - GLOBAL DATA AREA MAINTENANCE SERVICE RQ'              
GLOBAL   CSECT                                                                  
         NMOD1 WRKX-WRKD,**$GLO**,R9,CLEAR=YES,RR=RE                            
         USING WRKD,RC             RC=A(W/S)                                    
         ST    RE,RELO                                                          
         ST    R1,APARMS                                                        
         USING SRPARMD,R1                                                       
         L     RA,SRQATWA                                                       
         USING SRGLOFFD,RA         RA=A(TWA)                                    
         L     R7,SRQASYSF                                                      
         USING SYSFACD,R7          R7=A(SYSFACS)                                
         L     R8,VSSB                                                          
         MVC   RECLEN,SSBTWAL-SSBD(R8)                                          
         L     RF,SSBTKADR-SSBD(R8)                                             
         L     RF,TCBWRKA-TCBD(RF)                                              
         L     RF,104(RF)                                                       
         ST    RF,ATSKGLOB         SAVE ADDRESS TASK GLOBALS IN TCB             
         L     R8,SRQAUTL                                                       
         USING UTLD,R8             R8=A(UTL)                                    
         L     RF,SRQATIA                                                       
         ST    RF,ATIA                                                          
         L     RF,SRQATIOB                                                      
         ST    RF,ATIOB                                                         
*                                                                               
         MVC   TRMUSER,TUSER       SAVE UTL INFO                                
         MVC   TRM,TNUM                                                         
         MVC   TRMTYP,TSTAT                                                     
         MVC   TRMTYP1,TTYPE                                                    
*                                                                               
         MVI   DDSFLAG,X'00'       INITIALISE TERMINAL FLAG                     
         TM    TSTAT1,TSTATDDS                                                  
         BZ    *+8                                                              
         OI    DDSFLAG,DDSTRM                                                   
*                                                                               
         MVC   DATAMGR,VDATAMGR                                                 
         L     RE,SRQACOMF         SAVE COMFACS ENTRYS                          
         USING COMFACSD,RE                                                      
         MVC   VGETFACT,CGETFACT                                                
         MVC   VGLOBBER,CGLOBBER                                                
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VDICTATE,CDICTATE                                                
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VSCANNER,CSCANNER                                                
         MVC   VGETHELP,CGETHELP                                                
         MVC   VDATCON,CDATCON                                                  
         DROP  RE                                                               
*                                                                               
         L     RE,=V(TIMEOUT)                                                   
         A     RE,RELO                                                          
         ST    RE,VTIMEOUT                                                      
         L     RE,=V(NUMVAL)                                                    
         A     RE,RELO                                                          
         ST    RE,VNUMVAL                                                       
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9A00A0D'                                       
         L     R1,0(R1)            GET A(SQUASHER)                              
         ST    R1,VSQUASH                                                       
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LU  ',DDDCLST,DDDSLST                            
*                                                                               
         TM    TSVCREQ,X'01'       TEST FIRST TIME                              
         BNO   *+12                                                             
         OI    DDSFLAG,DDSNEW      FORCE RELOAD                                 
         OI    TSVCREQ,X'02'       SET CURSOR FLAG                              
*                                                                               
         MVC   USERID,TUSER        SET USER ID TO LOGON VALUE                   
         MVC   LOGONID,TUSER       AND SAVE LOGON VALUE                         
         TM    DDSFLAG,DDSPUB                                                   
         BZ    *+10                                                             
         MVC   USERID,PUBLICID     SET USER ID TO PUBLIC VALUE                  
         OC    USERID,USERID       NON DDS TERMINALS MUST BE LOGGED ON          
         BNZ   *+12                                                             
         TM    DDSFLAG,DDSTRM                                                   
         BZ    ERR0                                                             
*                                                                               
         SR    R0,R0               GET DATE AND TIME                            
         SR    R1,R1                                                            
         TIME  BIN                 R0=DATE,R1=TIME                              
         STM   R0,R1,MVSTIME                                                    
         OI    MVSDATE+3,X'0F'                                                  
         EJECT                                                                  
*************************************************************                   
*     MAIN CONTROL ROUTINE                                  *                   
*************************************************************                   
         SPACE 1                                                                
GLOBMAIN EQU   *                                                                
         BAS   RE,READSTR          READ SAVED STORAGE IN TWA11                  
         XC    HELP,HELP                                                        
         MVC   HELPKEY,HELPID                                                   
         BAS   RE,HELPSCAN         SACN FOR HELP REQUEST                        
*                                  CALL GLOBBER TO READ TEMPSTORE               
*                                  GLOBAL DATA AREA INTO TCB                    
         XC    DMCB(24),DMCB                                                    
         LA    R6,255                                                           
         GOTO1 VGLOBBER,DMCB,=C'OSHIT',,,(R6)                                   
         CLI   8(R1),GLEBCN        EXPECTING BAD COMMAND NAME ERROR             
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         B     VALACT              GOTO INPUT VALIDATION ROUTINES               
*                                                                               
EXIT     EQU   *                   RETURN HERE IF INPUT OK                      
         BAS   RE,WRITESTR         WRITE SAVED STORAGE TO TWA11                 
         L     RF,ATSKGLOB                                                      
         TM    GLOBSWS-GLOBD(RF),GLOBUPDT                                       
         BZ    EXITERR                                                          
         NI    GLOBSWS-GLOBD(RF),X'FF'-GLOBUPDT                                 
         BAS   RE,SAVEGLOB         WRITE UPDATED GLOBALS TO TEMPSTORE           
*                                                                               
EXITERR  EQU   *                   RETURN HERE IF INPUT ERROR                   
         L     R1,CURSOR                                                        
         OI    6(R1),X'40'         SET CURSOR                                   
         XMOD1 1                                                                
         DROP  R8                                                               
         EJECT                                                                  
*************************************************************                   
*     VALIDATE ACTION FIELD                                 *                   
*************************************************************                   
         SPACE 1                                                                
VALACT   LA    R4,SRVACTH                                                       
         ST    R4,CURSOR           DEFAULT CURSOR POSITION HERE                 
         CLI   HLPFLD,2                                                         
         BE    VALOPT              IGNORE IF HELP REQUEST                       
         SR    R1,R1                                                            
         ICM   R1,1,5(R4)                                                       
         BZ    VALACT3             NO INPUT (DEFAULT ACTION)                    
         BCTR  R1,0                                                             
         LA    RE,ACTTAB           FIND ACTION ROUTINE IN TABLE                 
VALACT1  EX    0,0(RE)             FIND KEYWORD                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SRVACT(0),0(RF)     COMPARE KEYWORD                              
         BE    VALACT2                                                          
         LA    RE,L'ACTTAB(RE)                                                  
         CLI   0(RE),0             TEST END OF TABLE                            
         BNE   VALACT1                                                          
         B     ERR2                INVALID INPUT                                
*                                                                               
VALACT2  MVC   ACTION,4(RE)        GET ACTION INFO                              
         L     R1,5(RE)                                                         
         A     R1,RELO                                                          
         ST    R1,ROUTINE          ADDRESS OF PROCESSING ROUTINE                
         B     VALACTX                                                          
*                                                                               
VALACT3  MVI   ACTION,1            DEFAULT ACT IS DISPLAY                       
         LA    R1,DISPREP                                                       
         ST    R1,ROUTINE                                                       
         LA    RF,SR@DSP                                                        
         B     VALACTX                                                          
*                                                                               
VALACTX  XC    SRVACT,SRVACT       REDISPLAY ACTION FIELD CONTENTS              
         MVC   SRVACT,0(RF)                                                     
         OI    SRVACTH+6,X'80'                                                  
         B     VALOPT              GOTO VALIDATE OPTION FIELD                   
         EJECT                                                                  
*************************************************************                   
*     VALIDATE OPTION FIELD                                 *                   
*************************************************************                   
         SPACE 1                                                                
VALOPT   EQU   *                                                                
         LA    R4,SRVOPTH                                                       
         CLI   HLPFLD,3                                                         
         BE    VALSUB              IGNORE IF HELP REQUESTED                     
         CLI   5(R4),0             TEST NO OPTIONS                              
         BE    VALOPTX                                                          
         GOTO1 VSCANNER,DMCB,(R4),(3,SCANBLK)                                   
         MVC   BYTE,4(R1)          BYTE=NO OF OPTIONS                           
         LA    R4,SCANBLK                                                       
VALOPT1  LA    RE,OPTTAB           SCAN OPTTAB FOR A MATCH                      
         SR    R1,R1                                                            
         ICM   R1,1,0(R4)                                                       
         BZ    VOPTERR                                                          
         BCTR  R1,0                                                             
VALOPT2  EX    0,0(RE)             GET KEYWORD                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),0(RF)                                                   
         BE    VALOPT5             KEYWORD FOUND                                
         LA    RE,12(RE)                                                        
         OC    0(2,RE),0(RE)                                                    
         BNE   VALOPT2                                                          
         B     VOPTERR             INVALID INPUT                                
*                                                                               
VALOPT5  L     RF,4(RE)            RF=OPTION ROUTINE                            
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         LA    R4,32(R4)           NEXT SCANNER ENTRY                           
         SR    R1,R1                                                            
         IC    R1,BYTE             COUNT NUMBER OF ENTRYS                       
         BCTR  R1,0                                                             
         STC   R1,BYTE                                                          
         CLI   BYTE,0                                                           
         BNE   VALOPT1                                                          
*                                                                               
VALOPTX  XC    SRVOPT,SRVOPT       CLEAR OPTION FIELD                           
         OI    SRVOPTH+6,X'80'                                                  
         B     VALSUB              GOTO VALIDATE SUB-ACTION FIELDS              
*                                                                               
VOPTERR  LA    R4,SRVOPTH          OPTION FIELD ERROR                           
         ST    R4,CURSOR                                                        
         B     ERR2                                                             
         EJECT                                                                  
*************************************************************                   
*     VALIDATE SUB-ACTION FIELDS                            *                   
*************************************************************                   
         SPACE 1                                                                
VALSUB   LA    R0,LINENUM          # DISPLAY LINES                              
         LA    R4,SRVSACTH         POINT TO FIRST LINE                          
         CLI   HLPFLD,0                                                         
         BNE   HELPOUT             IGNORE IF HELP REQUESTED                     
         LA    R6,SVTAB            POINT TO LINE SAVE TABLE                     
VALS01   CLI   5(R4),0                                                          
         BNE   VALS02              INPUT FOUND                                  
VALS01A  LA    R6,2(R6)            BUMP TO NEXT LINE                            
         LA    R4,SRVLIN2-SRVLIN1(R4)                                           
         BCT   R0,VALS01             UNTIL END                                  
         TM    DDSFLAG,DDSACT      TEST ANY SUB-ACTIONS                         
         BZ    VALSUBX                                                          
         B     REPACT              PROCESS SUB-ACTIONS                          
*                                                                               
VALS02   SR    R1,R1               VALIDATE SUB-ACTION                          
         IC    R1,5(R4)                                                         
         BCTR  R1,0                                                             
         LA    RE,SUBTAB           SUB-ACTION VALIDATION TABLE                  
VALS03   CLM   R1,1,5(RE)          TEST MIN LENGTH FOR COMPARE                  
         BL    VALS03X                                                          
         EX    0,0(RE)             FIND KEYWORD                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R4),0(RF)       COMPARE KEYWORD                              
         BE    VALS04                                                           
VALS03X  LA    RE,6(RE)                                                         
         CLI   0(RE),0             TEST END OF TABLE                            
         BNE   VALS03                                                           
         ST    R4,CURSOR                                                        
         B     ERR10               INVALID SUB ACTION                           
*                                                                               
VALS04   OI    DDSFLAG,DDSACT      FLAG VALID SUB-ACTION PRESENT                
         MVC   1(1,R6),4(RE)       STORE ACTION IN LINE SAVE TABLE              
         BNZ   VALS01A             NEXT SUB ACTION                              
         ST    R4,CURSOR                                                        
         B     ERR10               INVALID SUB ACTION                           
*                                                                               
VALSUBX  L     RF,ROUTINE          GET ACTION ROUTINE                           
         BR    RF                    AND GOTO IT                                
         EJECT                                                                  
*************************************************************                   
*       PROCESS SUB-ACTION FIELDS                           *                   
*************************************************************                   
         SPACE 1                                                                
REPACT   LA    R3,SRVSACTH         POINT TO LINE                                
         USING REPLINE,R3                                                       
         LA    R4,SVTAB            POINT TO LINE INFO SAVE TABLE                
         SR    R6,R6               ELEMENT CODE                                 
*                                                                               
RACT00   ICM   R6,1,0(R4)          GET SAVED ELEMENT CODE FOR LINE              
         BE    RACT10              NO CODE DISPLAYED                            
         CLI   1(R4),1             UPDATE SUB-ACTION                            
         BE    RACT20                                                           
         CLI   1(R4),2             PURGE SUB-ACTION                             
         BE    RACT30                                                           
*                                                                               
RACT10   MVI   1(R4),0             CLEAR SUB-ACTION SAVE                        
         XC    RLSACT,RLSACT       CLEAR SUB-ACTION FIELD                       
         OI    RLSAHDR+6,X'80'                                                  
         LA    R3,L'RLINE(R3)      NEXT LINE                                    
         LA    R4,2(R4)            NEXT SAVE TABLE ENTRY                        
         LA    R1,SRVLAST                                                       
         CLR   R3,R1               TEST BOTTOM OF SCREEN                        
         BL    RACT00                                                           
         B     RACTX                                                            
*                                                                               
RACT20   TM    RLVAHDR+4,X'80'     UPDATE SUB-ACTION                            
         BZ    RACT10                                                           
         CLI   RLVAHDR+5,0                                                      
         BE    RACT30                                                           
         GOTO1 VGLOBBER,DMCB,=C'PUTF',RLVAHDR,,(R6)                             
         CLI   8(R1),0                                                          
         BE    RACT40                                                           
         DC    H'00'                                                            
*                                  PURGE SUB-ACTION                             
RACT30   GOTO1 VGLOBBER,DMCB,=C'DELE',,,(R6)                                    
         CLI   8(R1),GLEGNF        GLOBAL NOT FOUND                             
         BE    RACT40                                                           
         CLI   8(R1),0             GLOBAL FOUND OK                              
         BE    RACT40                                                           
         DC    H'00'               OTHER ERROR CONDITION                        
*                                                                               
RACT40   L     RF,ATSKGLOB         UPDATE GLOBAL AREA STATUS INFO               
         USING GLOBD,RF                                                         
         MVC   GLOBDATE,MVSDATE                                                 
         MVC   GLOBTIME,MVSTIME                                                 
         MVI   GLOBINFO,C'U'                                                    
         OI    GLOBSWS,GLOBUPDT                                                 
         B     RACT10                                                           
         DROP  RF                                                               
*                                                                               
RACTX    L     RF,ROUTINE          GET ADDRESS ACTION ROUTINE                   
         BR    RF                    AND GOTO IT                                
         DROP  R3                                                               
         EJECT                                                                  
*************************************************************                   
*                 OPTIONS ROUTINES                          *                   
*************************************************************                   
         SPACE 1                                                                
OPSWIT   CLC   22(3,R4),=C'NAS'    SWITCH=NAS ON/OFF SWITCH                     
         BNE   OSWI10                                                           
         XI    SWITCH1,SWINAS      FLAG DISPLAY NOT ASSIGNED ELEMENTS           
         BR    RE                                                               
*                                                                               
OSWI10   CLC   22(3,R4),=C'NUL'    SWITCH=NUL                                   
         BNE   OPERR                                                            
         XI    SWITCH1,SWINUL      FLAG DISPLAY EMPTY ELEMENTS                  
         BR    RE                                                               
*                                                                               
OPCODE   ST    RE,SAVERE           CODE=<START ELEMENT CODE>                    
         TM    3(R4),X'80'         TEST NUMERIC PARAMETER                       
         BNO   OPERR                                                            
         L     R1,8(R4)            EXTRACT BINARY VALUE                         
         C     R1,=F'255'                                                       
         BH    OPERR               TOO BIG                                      
         C     R1,=F'0'                                                         
         BNH   OPERR               TOO SMALL                                    
         STC   R1,SVELCODE         SAVE START ELEMENT CODE                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                  DUMMY ROUTINES                               
OPDDS    ST    RE,SAVERE                                                        
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
OPUSR    ST    RE,SAVERE                                                        
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
OPERR    LA    R1,SRVOPTH          HERE IF OPTION FIELD ERROR                   
         ST    R1,CURSOR                                                        
         B     ERR2                                                             
         EJECT                                                                  
*************************************************************                   
*     DISPLAY GLOBAL REPORT ACTION ROUTINE                  *                   
*************************************************************                   
         SPACE 1                                                                
DISPREP  L     R2,ATSKGLOB         DISPLAY GLOBAL STATUS AREA INFO              
         USING GLOBD,R2                                                         
         SR    R0,R0                                                            
         L     R1,GLOBTIME                                                      
         D     R0,=F'100'                                                       
         ST    R1,DUB                                                           
         GOTO1 VDATCON,DMCB,(X'06',GLOBDATE),(X'0A',SRVDAT)                     
         GOTO1 VTIMEOUT,DMCB,(X'02',DUB),(X'45',SRVTIM)                         
         OI    SRVDATH+6,X'80'                                                  
         OI    SRVTIMH+6,X'80'                                                  
         DROP  R2                                                               
*                                  DISPLAY GLOBAL ELEMENT DATA                  
         LA    R0,LINENUM          # DISPLAY LISTING LINES                      
         LA    R3,SRVSACTH         POINT TO FIRST LINE                          
         USING REPLINE,R3                                                       
         LA    R4,SVTAB            POINT TO LINE INFO SAVE TABLE                
         SR    R6,R6                                                            
         IC    R6,SVELCODE         CURRENT START ELEMENT CODE                   
*                                                                               
DREP10   BAS   RE,LINEREP          GET LINE DATA AND DISPLAY                    
         BNE   DREP30                UNLESS NO DATA                             
*                                                                               
DREP20   STC   R6,0(R4)            SAVE CODE DISPLAYED IN LINE TABLE            
         BCT   R0,DREP40           TEST LAST LINE DONE                          
         B     DREPX                 IF SO END DISPLAY                          
*                                                                               
DREP40   LA    R3,L'RLINE(R3)      NEXT LINE                                    
         LA    R4,2(R4)            NEXT LINE SAVE TABLE ENTRY                   
         LTR   R6,R6               IF END OF GLOBALS                            
         BZ    DREP20                CLEAR REMAINDER OF TABLE                   
*                                                                               
DREP30   LA    R6,1(R6)            NEXT GLOBAL ELEMENT CODE                     
         CL    R6,=F'255'          UPTO MAXIMUM VALUE                           
         BNH   DREP10                                                           
         TWAXC 0(R3),PROT=Y        CLEAR REST OF SCREEN                         
         SR    R6,R6               SET CODE TO 0                                
         B     DREP20              CONTINUE TO CLEAR LINE SAVE TABLE            
*                                                                               
DREPX    B     EXIT                RETURN TO MAIN CONTROL EXIT                  
         DROP  R3                                                               
         EJECT                                                                  
*************************************************************                   
* UPDATE GLOBAL ELEMENT ACTION ROUTINE                      *                   
*************************************************************                   
         SPACE 1                                                                
UPDREP   LA    R3,SRVSACTH         POINT TO FIRST LINE                          
         USING REPLINE,R3                                                       
         LA    R0,LINENUM          # DISPLAY LISTING LINES                      
         LA    R4,SVTAB            POINT TO LINE INFO SAVE TABLE                
*                                                                               
UREP00   CLC   SVELCODE(1),0(R4)   SEE IF CODE CURRENTLY DISPLAYED              
         BE    UREP10                YES                                        
         LA    R3,L'RLINE(R3)      NEXT LINE                                    
         LA    R4,2(R4)            NEXT SAVE TABLE ENTRY                        
         BCT   R0,UREP00           UNTIL END                                    
         B     UREPX               REDISPLAY SCREEN FROM NEW CODE               
*                                                                               
UREP10   SR    R6,R6                                                            
         IC    R6,SVELCODE         CURRENT START CODE                           
         TM    RLVAHDR+4,X'80'     NO DATA INPUT>                               
         BZ    UREPX                                                            
         CLI   RLVAHDR+5,0         FIELD INPUT LENGTH=0                         
         BE    UREP20                                                           
*                                  PUT FIELD INPUT TO ELEMENT DATA              
         GOTO1 VGLOBBER,DMCB,=C'PUTF',RLVAHDR,,(R6)                             
         CLI   8(R1),0                                                          
         BE    UREP30              RETURN GLOBBER OK                            
         DC    H'00'                                                            
*                                  DELETE ELEMENT                               
UREP20   GOTO1 VGLOBBER,DMCB,=C'DELE',,,(R6)                                    
         CLI   8(R1),GLEGNF        GLOBAL NOT FOUND                             
         BE    UREPX                                                            
         CLI   8(R1),0             GLOBAL FOUND OK                              
         BE    UREP30                                                           
         DC    H'00'               OTHER ERROR CONDITION                        
*                                                                               
UREP30   L     RF,ATSKGLOB         UPDATE GLOBAL AREA STATUS INFO               
         USING GLOBD,RF                                                         
         MVC   GLOBDATE,MVSDATE                                                 
         MVC   GLOBTIME,MVSTIME                                                 
         MVI   GLOBINFO,C'U'                                                    
         OI    GLOBSWS,GLOBUPDT                                                 
         DROP  RF                                                               
*                                                                               
UREPX    LA    R3,SRVSACTH         SET CURSOR TO FIRST LINE DATA FIELD          
         LA    RF,RLVAHDR                                                       
         ST    RF,CURSOR                                                        
         B     DISPREP             DO REDISPLAY OF GLOBAL DATA                  
         DROP  R3                                                               
         EJECT                                                                  
*************************************************************                   
* PURGE GLOBAL ELEMENT ACTION ROUTINE                       *                   
*************************************************************                   
         SPACE 1                                                                
PURGACT  LA    R3,SRVSACTH         POINT TO DISPLAY LINE                        
         USING REPLINE,R3                                                       
         LA    R0,LINENUM          # DISPLAY LISTING LINES                      
         LA    R4,SVTAB            POINT TO LINE INFO SAVE TABLE                
*                                                                               
PACT00   CLC   SVELCODE(1),0(R4)   SEE IF CODE CURRENTLY DISPLAYED              
         BE    PACT10                YES                                        
         LA    R3,L'RLINE(R3)      NEXT LINE                                    
         LA    R4,2(R4)            NEXT SAVE TABLE ENTRY                        
         BCT   R0,PACT00           UNTIL END                                    
         B     PACTX               REDISPLAY SCREEN FROM NEW CODE               
*                                                                               
PACT10   SR    R6,R6                                                            
         IC    R6,SVELCODE         GET ELEMENT CODE AND DELETE IT               
         GOTO1 VGLOBBER,DMCB,=C'DELE',,,(R6)                                    
         CLI   8(R1),GLEGNF        GLOBAL NOT FOUND                             
         BE    PACTX                                                            
         CLI   8(R1),0             GLOBAL FOUND OK                              
         BE    PACT20                                                           
         DC    H'00'               OTHER ERROR CONDITION                        
*                                                                               
PACT20   L     RF,ATSKGLOB         UPDATE GLOBAL STATUS DATA                    
         USING GLOBD,RF                                                         
         MVC   GLOBDATE,MVSDATE                                                 
         MVC   GLOBTIME,MVSTIME                                                 
         MVI   GLOBINFO,C'U'                                                    
         OI    GLOBSWS,GLOBUPDT                                                 
         DROP  RF                                                               
*                                                                               
PACTX    B     DISPREP             DO REDISPLAY OF GLOBAL DATA                  
         DROP  R3                                                               
         EJECT                                                                  
*************************************************************                   
*     DISPLAY LAST PAGE ACTION ROUTINE                      *                   
*************************************************************                   
         SPACE 1                                                                
LASTACT  SR    R1,R1                                                            
         IC    R1,SVELCODE         GET CURRENT START ELEMENT CODE               
         LA    RF,LINENUM            AND SUBTRACT # DISPLAY LINES               
         SR    R1,RF                                                            
         BNP   *+12                UPTO MINIMUM OF 1                            
         STC   R1,SVELCODE         SAVE NEW START ELEMENT CODE                  
         B     DISPREP                                                          
         MVI   SVELCODE,1                                                       
         B     DISPREP             DO REDISPLAY OF GLOBAL DATA                  
         SPACE 2                                                                
*************************************************************                   
*     DISPLAY NEXT PAGE ACTION ROUTINE                      *                   
*************************************************************                   
         SPACE 1                                                                
NEXTACT  SR    R1,R1                                                            
         LA    RE,SVTAB            POINT TO LINE INFO SAVE TABLE                
         LA    RF,SVTABL                                                        
         AR    RF,RE               AND END OF IT                                
*                                                                               
NACT10   CLI   0(RE),0             GET LAST ENTRY IN SAVE TABLE                 
         BE    NACT20                                                           
         LA    RE,2(RE)                                                         
         CR    RE,RF                                                            
         BL    *+6                                                              
         DC    H'00'                                                            
         B     NACT10                                                           
*                                                                               
NACT20   LA    RF,SVTAB                                                         
         CR    RE,RF               CHECK NO ENTRIES IN TABLE                    
         BE    NACT30                                                           
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
         IC    R1,0(RE)              ELSE GET ENTRY                             
         B     NACT40                                                           
*                                                                               
NACT30   IC    R1,SVELCODE         IF EMPTY DISPLAY NEXT PAGE                   
         LA    RF,LINENUM                                                       
         AR    R1,RF                                                            
         C     R1,=F'255'                                                       
         BH    NACT50                                                           
*                                                                               
NACT40   STC   R1,SVELCODE         USE LAST CODE AS NEW FIRST CODE              
         B     DISPREP             DO REDISPLAY OF GLOBAL DATA                  
*                                                                               
NACT50   MVI   SVELCODE,255                                                     
         B     DISPREP                                                          
         SPACE 2                                                                
*************************************************************                   
*     CLEAR GLOBAL AREA                                     *                   
*************************************************************                   
         SPACE 1                                                                
CLEARACT L     RE,ATSKGLOB         CLEAR TCB GLOBAL AREA                        
         LA    RF,CHKPTGLL                                                      
         XCEF                                                                   
         L     RE,ATSKGLOB         UPDATE GLOBAL AREA STATUS INFO               
         USING GLOBD,RE                                                         
         MVC   GLOBDATE,MVSDATE                                                 
         MVC   GLOBTIME,MVSTIME                                                 
         MVI   GLOBINFO,C'I'                                                    
         OI    GLOBSWS,GLOBUPDT                                                 
         OI    GLOBSWS,GLOBREAD    FLAG TO STOP GLOBBER READ FROM TWA0          
         B     DISPREP             DO REDISPLAY OF GLOBAL DATA                  
         DROP  RE                                                               
         EJECT                                                                  
*************************************************************                   
*  BUILD REPORT LINE CALLED FROM DISPREP ACTION ROUTINE     *                   
*************************************************************                   
         SPACE 1                                                                
LINEREP  NTR1                                                                   
         USING REPLINE,R3                                                       
         XC    RLCODE,RLCODE       INTIALISE LINE DISPLAY                       
         MVC   RLNAME,=C'............'                                          
         XC    RLVALU,RLVALU                                                    
*                                  DISPLAY ELEMENT CODE                         
         EDIT  (R6),(4,RLCODE),ZERO=NOBLANK,ALIGN=LEFT                          
*                                                                               
         LA    R1,GLOTXT           GET ELEMENT NAME STRING FROM TABLE           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         A     RF,RELO                                                          
         LA    R1,6(R1)                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         CR    R0,R6                                                            
         BE    LREP00              NAME ENTRY FOUND                             
         BXLE  R1,RE,*-10                                                       
         TM    SWITCH1,SWINAS      ENTRY NOT FOUND                              
         BNZ   LREP20              FLAG TO DISPLAY ANYWAY                       
         SR    R1,R1                                                            
         IC    R1,SVELCODE                                                      
         CR    R1,R6               IF NOT CURRENT START CODE                    
         BNE   LREPNO                NO DISPLAY                                 
         B     LREP20                ELSE DISPLAY                               
*                                                                               
LREP00   MVC   RLNAME,1(R1)        EXTRACT SHORT NAME FROM TABLE                
         DROP  R1                                                               
*                                  GET ELEMENT DATA INTO DISPLAY FIELD          
LREP20   GOTO1 VGLOBBER,DMCB,=C'GETF',RLVAHDR,,(R6)                             
         CLI   8(R1),GLEGNF        GLOBAL NOT FOUND                             
         BNE   LREP10                                                           
         TM    SWITCH1,SWINUL      TEST OPTION FLAG                             
         BZ    LREPYES             IF ON DISPLAY ANYWAY                         
         SR    R1,R1                                                            
         IC    R1,SVELCODE                                                      
         CR    R1,R6               IF NO CURRENT START CODE                     
         BNE   LREPNO                NO DISPLAY                                 
         B     LREPYES               ELSE DISPLAY                               
*                                                                               
LREP10   CLI   8(R1),0             GLOBAL FOUND OK                              
         BE    LREPYES                                                          
         DC    H'00'               OTHER ERROR CONDITION                        
*                                                                               
LREPNO   B     NO                  EXIT NOT DISPLAYED                           
*                                                                               
LREPYES  B     YES                 EXIT DISPLAYED OK                            
         DROP  R3                                                               
         EJECT                                                                  
*************************************************************                   
* SAVE GLOBALS IN TEMPSTORE, TWA#0 OR TWA#11                *                   
*************************************************************                   
SAVEGLOB NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         L     R5,ATIA             USE TIA WORK 14K AREA                        
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         CLC   RECLEN,=H'6144'                                                  
         BE    *+12                                                             
         LA    R4,0                SAVED IN TWA#0                               
         B     *+8                                                              
         LA    R4,SRPAGENO         SAVED IN TWA#11                              
         SLL   R4,32-8                                                          
         ICM   R4,3,TRM            READ TEMPSTORE                               
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMREAD'),=C'TEMPSTR',(R4),(R5)            
         CLI   8(R1),0             CHECK DMGR RETURN OK                         
         BE    *+6                                                              
         DC    H'00'                                                            
         XC    DMCB(24),DMCB                                                    
* MOVE GLOBAL AREA FROM TCB INTO TIA TEMPSTORE DATA                             
         SR    R2,R2                                                            
         CLC   RECLEN,=H'6144'                                                  
         BE    *+12                                                             
         LH    R2,=Y(CHKPTGLD)     IF SAVED IN TWA#0                            
         B     *+8                                                              
         LH    R2,=Y(SRGLOBAL-SRSD)  IF SAVED IN TWA#13                         
         AR    R2,R5                                                            
         L     RE,ATSKGLOB                                                      
         LA    RF,CHKPTGLL                                                      
         LR    R3,RF                                                            
         MVCL  R2,RE               MOVE DATA                                    
*                                  WRITE DATA BACK TO TEMPSTORE                 
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',(R4),(R5)                     
         CLI   8(R1),0             CHECK DMGR RETURN OK                         
         BE    *+6                                                              
         DC    H'00'                                                            
         B     YES                 EXIT                                         
         EJECT                                                                  
*************************************************************                   
*                CALL GETHELP AND EXIT                      *                   
*************************************************************                   
HELPOUT  NI    SRVACTH+6,X'FF'-X'40'   UNSET THE CURSOR                         
         TWAXC SRVTABH,PROT=Y                                                   
         LA    R1,HLPIND4                                                       
         CLI   HLPFLD,4            CHECK FOR SEL FIELDS                         
         BNL   HELP005                                                          
         SR    RF,RF                                                            
         IC    RF,HLPFLD           READ HELP FIELD                              
         SLL   RF,2                                                             
         EX    0,HLPIND0(RF)       SELECT WHICH TABLE                           
         B     HELP010                                                          
HLPIND0  DC    XL4'00'             NO FIELD NUMBER                              
         LA    R1,HLPIND1                                                       
         LA    R1,HLPIND2                                                       
         LA    R1,HLPIND3                                                       
*                                                                               
HELP005  L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBSETC   SET CURSOR                                   
         MVI   TIOBCURI,X'01'      ONE PAST                                     
         LA    RE,SRVOPTH          OPTION FIELD                                 
         SR    RE,RA                                                            
         STCM  RE,3,TIOBCURD                                                    
         XC    TIOBCURS,TIOBCURS                                                
         DROP  RF                                                               
*                                                                               
HELP010  CLC   0(2,R1),=H'0'                                                    
         BE    INFO2                                                            
         CLC   0(1,R1),ACTION      MATCH ACTION ???                             
         BE    HELP020                                                          
         CLI   0(R1),X'FF'         FF MATCHES ANY                               
         BE    HELP020                                                          
         LA    R1,2(R1)            NEXT ENTRY                                   
         B     HELP010                                                          
HELP020  MVC   FLAG,1(R1)          FLAG=INDEX                                   
*                                  EXTRA TEXT STUFF                             
HELP025  LA    R1,HELPTAB                                                       
HELP030  CLC   FLAG,0(R1)                                                       
         BNE   HELP040                                                          
         TM    1(R1),X'80'         DDS PANEL ?                                  
         BNO   HELP031                                                          
         TM    HLPFLG,X'80'                                                     
         BNO   HELP040                                                          
HELP031  MVC   HELPNUM,2(R1)                                                    
         MVC   HELPPAG,HLPPAG                                                   
         SR    RF,RF                                                            
         TM    1(R1),X'40'         EXTRA TEXT ?                                 
         BNO   HELP032                                                          
*        L     RF,?                                                             
HELP032  L     R1,QHDR                                                          
         OI    6(R1),X'40'         SET CURSOR                                   
         GOTO1 VGETHELP,DMCB,(X'50',HELPKEY),QHDR,(C'B',0),(RF),0               
         DC    H'0'                GETHELP EXITS TO MONITOR                     
HELP040  LA    R1,L'HELPTAB(R1)                                                 
         CLI   0(R1),0                                                          
         BNE   HELP030                                                          
         B     INFO4                                                            
         EJECT                                                                  
*************************************************************                   
*                  INFO MESSAGES                            *                   
*************************************************************                   
         SPACE 1                                                                
INFO2    LA    RF,154              END OF REPORT                                
         B     INFOX                                                            
INFO4    LA    RF,4                HELP UNAVAILABLE                             
         L     R1,QHDR                                                          
         OI    6(R1),X'40'         SET CURSOR                                   
         B     INFOX                                                            
INFOX    XC    DMCB(24),DMCB                                                    
         GOTO1 VGETTXT,DMCB,(RF),0,(C'I',0)                                     
         B     EXIT                                                             
*************************************************************                   
*                       ERROR MESSAGES                      *                   
*************************************************************                   
ERR0     LA    RE,SREMBC           MUST BE CONNECTED                            
         B     ERRX                                                             
ERR1     LA    RE,SREMIF           MISSING INPUT FIELD                          
         B     ERRX                                                             
ERR2     LA    RE,SREIIF           INVALID INPUT FIELD                          
         B     ERRX                                                             
ERR10    LA    RE,57               INVALID SUB ACTION                           
         B     ERRX                                                             
ERRX     XC    DMCB,DMCB                                                        
         GOTO1 VGETTXT,DMCB,(RE),0,(C'E',0),0,0                                 
         B     EXITERR                                                          
         EJECT                                                                  
*************************************************************                   
*                 READ IN SAVED STORAGE                     *                   
*************************************************************                   
         SPACE 1                                                                
READSTR  NTR1                                                                   
         L     R5,ATIA             TIA WORK AREA                                
         USING SRSD,R5                                                          
         LA    R2,SRPAGENO         READ IN TWA11                                
         SLL   R2,32-8                                                          
         ICM   R2,3,TRM                                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMREAD'),=C'TEMPSTR',(R2),SRSD            
         CLI   8(R1),0             CHECK DMGR RETURN OK                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    DMCB(24),DMCB       REMOVE THE SPECIAL BITS                      
         CLC   SRCOMWRK(4),=C'$GLO'  SEE IF GLOBAL SR SAVED                     
         BE    READ10                                                           
         LA    RE,SAVEDSTR         IF NOT INITIALISE AREA                       
         LA    RF,SAVEDL                                                        
         XCEF                                                                   
         MVI   SVELCODE,1          SET FIRST ELEMNT CODE TO 1                   
         B     READX                                                            
*                                                                               
READ10   LA    R0,SAVEDSTR         MOVE SAVED DATA FORM TIA TO WORKD            
         LA    RE,SRCOMWRK                                                      
         L     R1,=A(SAVEDL)                                                    
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
READX    EQU   *                                                                
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
*************************************************************                   
*               WRITE OUT SAVED STORAGE                     *                   
*************************************************************                   
WRITESTR NTR1                                                                   
         L     R5,ATIA             TIA WORK AREA                                
         USING SRSD,R5                                                          
         MVC   IDENT,=C'$GLO'      MARK GLOBAL SR DATA                          
         LA    RE,SAVEDSTR                                                      
         LA    R0,SRCOMWRK                                                      
         L     R1,=A(SAVEDL)                                                    
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE SAVED DATA FROM WORKD TO TIA            
         LA    R2,SRPAGENO         WRITE DATA TO TWA#11                         
         SLL   R2,32-8                                                          
         ICM   R2,3,TRM                                                         
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',(R2),SRSD                     
         CLI   8(R1),0             CHECK DMGR RETURN OK                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* SCAN SCREEN INPUT FIELDS FOR HELP REQUEST                           *         
***********************************************************************         
HELPSCAN NTR1                                                                   
         LA    R4,64(RA)           R4=A(FIRST FIELD)                            
         L     R6,ATIOB                                                         
         USING TIOBD,R6                                                         
         SR    R2,R2               CLEAR FIELD COUNT                            
HS01     SR    R0,R0                                                            
         ICM   R0,1,0(R4)                                                       
         BZ    HSX                                                              
         TM    1(R4),X'20'         TEST PROT                                    
         BNO   HS03                                                             
         AR    R4,R0               NEXT FIELD                                   
         B     HS01                                                             
HS02     LTR   R0,R0                                                            
         BZ    HS02A                                                            
         TM    HLPFLG,X'80'                                                     
         BNO   *+6                                                              
         BCTR  R1,0                                                             
         LR    R5,R0                                                            
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)                                                    
HS02A    SR    R0,R0                                                            
         ICM   R0,1,0(R4)                                                       
         AR    R4,R0                                                            
         B     HS01                                                             
*                                                                               
HS03     EQU   *                   UNPROT FOUND                                 
         LA    R2,1(R2)            INC FIELD COUNT                              
         LA    R1,8(R4)                                                         
         SH    R0,=H'8'                                                         
         SR    R5,R5               POS COUNT ZERO                               
HS04     CLI   0(R1),C'?'                                                       
         BE    HS05                HELP REQUIRED                                
         CLI   TIOBAID,1           CHECK FOR PFKEY 1                            
         BNE   HS04A                                                            
         SR    RF,RF               CHECK IF CURSOR AT THIS FIELD                
         LH    RF,TIOBCURD                                                      
         AR    RF,RA                                                            
         CR    RF,R4                                                            
         BE    HS05                                                             
HS04A    LA    R5,1(R5)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,HS04                                                          
         B     HS02                NEXT FIELD                                   
*                                                                               
HS05     XC    HELP,HELP           CLEAR HELP                                   
         ST    R4,QHDR             SAVE ADDR                                    
         STC   R2,HLPFLD           SET FIELD NUM                                
         STC   R5,HLPPOS           POSITION                                     
         STC   R5,5(R4)            AND NEW FIELD LENGTH                         
         TM    DDSFLAG,DDSTRM                                                   
         BNO   HS06                                                             
         CLI   1(R1),C'*'          DDS CAN ENTER ?*                             
         BNE   HS06                                                             
         OI    HLPFLG,X'80'        AND GET DDS HELP                             
         LA    R1,1(R1)                                                         
HS06     SR    R3,R3               CHECK FOR PAGE NO                            
         TM    1(R1),X'F0'                                                      
         BNO   HS02                                                             
         LA    R3,1(R3)                                                         
         TM    2(R1),X'F0'                                                      
         BNO   HS07                                                             
         LA    R3,1(R3)                                                         
         TM    3(R1),X'F0'                                                      
         BNO   HS07                                                             
         LA    R3,1(R3)                                                         
*                                                                               
HS07     BCTR  R3,0                CONVERT PAGE NO                              
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,1(0,R1)                                                      
         CVB   R3,DUB                                                           
         STC   R3,HLPPAG                                                        
         B     HS02                                                             
*                                                                               
HSX      XIT1                                                                   
         DROP  R6                                                               
         SPACE 2                                                                
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*                       CONSTANTS                           *                   
*************************************************************                   
*                                                                               
* SCREEN DISPLAY GLOBAL DATA LISTING LINE COUNT                                 
*                                                                               
LINENUM  EQU   (SRVXXXH-SRVSACTH)/(SRVLIN2-SRVLIN1)                             
*                                                                               
*  HELP INDEX 2 BYTES ACTION-INDEX 00=NO ACTION FF=ANY ACTION                   
*                                                                               
HLPIND1  DC    X'FF010000'                                                      
*                                                                               
HLPIND2  DC    X'FF020000'                                                      
*                                                                               
HLPIND3  DC    X'FF030000'                                                      
*                                                                               
HLPIND4  DC    X'FF040000'                                                      
         EJECT                                                                  
*  FLAGS       X'80'                   DDS ONLY PANEL                           
*              X'40'                   EXTRA TEXT BLOCK IN PQSAVE               
*                                                                               
HELPTAB  DS    0CL3                    INDEX/FLAGS/PANEL                        
         DC    X'01',X'00',X'01'                                                
         DC    X'02',X'00',X'02'                                                
         DC    X'03',X'00',X'03'                                                
         DC    X'04',X'00',X'04'                                                
         DC    X'00'                                                            
*                                                                               
HELPID   DC    XL10'0115FF00000000000000'                                       
         EJECT                                                                  
*                                                                               
*        ACTION TABLE                                                           
*        LA    RF,S(TEXT),ACTNUM,A(ROUTINE),FLAGS                               
*                                                                               
         DS    0F                                                               
ACTTAB   DS    0XL10                                                            
         DC    X'41F0',S(SR@DSP),AL1(1),AL4(DISPREP),X'00'                      
         DC    X'41F0',S(SR@UPD),AL1(2),AL4(UPDREP),X'00'                       
         DC    X'41F0',S(SR@LAST),AL1(3),AL4(LASTACT),X'00'                     
         DC    X'41F0',S(SR@NEXT),AL1(4),AL4(NEXTACT),X'00'                     
         DC    X'41F0',S(SR@PURGE),AL1(5),AL4(PURGACT),X'00'                    
         DC    X'41F0',S(SR@CLEAR),AL1(6),AL4(CLEARACT),X'00'                   
         DC    X'0000'                                                          
*                                                                               
*        SUB-ACTION TABLE                                                       
*        LA    RF,S(TEXT),SUBNUM,MIN LEN-1                                      
*                                                                               
SUBTAB   DC    X'41F0',S(SR@UPD),AL1(1),X'00'                                   
         DC    X'41F0',S(SR@PURGE),AL1(2),X'00'                                 
         DC    X'0000'                                                          
*                                                                               
*        OPTION TABLE                                                           
*        LA    RF,S(TEXT),A(ROUTINE),X'FLAGS'                                   
*                                                                               
         DS    0F                                                               
OPTTAB   DC    X'41F0',S(DC@SWIT),A(OPSWIT),X'00000000'                         
         DC    X'41F0',S(DC@CODE),A(OPCODE),X'00000000'                         
         DC    X'41F0',S(DC@DDS),A(OPDDS),X'00000000'                           
         DC    X'41F0',S(DC@USR),A(OPUSR),X'00000000'                           
         DC    X'0000'                                                          
         SPACE 1                                                                
DDDCLST  DS    0C                                                               
*                                                                               
         DCDDL SR#UPD,9,L                                                       
         DCDDL SR#DSP,9,L                                                       
         DCDDL SR#RLEAS,9,L                                                     
         DCDDL SR#UNKNW,9,L                                                     
         DCDDL SR#PURGE,9,L                                                     
         DCDDL SR#LAST,9,L                                                      
         DCDDL SR#NEXT,9,L                                                      
         DCDDL SR#CLEAR,9,L                                                     
*                                                                               
DC@CODE  DC    CL8'CODE'                                                        
DC@DDS   DC    CL8'DDS'                                                         
DC@USR   DC    CL8'U'                                                           
DC@SWIT  DC    CL8'SWITCH'                                                      
*                                                                               
         EJECT                                                                  
*                                                                               
*        GLOBAL ELEMENT NAME TABLE                                              
*                                                                               
         CNOP  2,4                                                              
GLOTXT   DC    AL2(GLOTXT1-GLOTXT0)  ENTRY LENGTH                               
         DC    A(GLOTXTX-1)        END ADDRESS (NOT RELOCATED)                  
*                                                                               
*        ENTRIES: AL1(ELEMENT CODE),CL12'SHORT NAME',CL80'LONG NAME'            
*                                                                               
GLOTXT0  DC    AL1(GLVSPID)                                                     
         DC    CL12'PQ REPORT ID'                                               
         DC    CL80'LAST REPORT ADDED TO PRINT QUEUE'                           
*                                                                               
GLOTXT1  DC    AL1(GLVDQU)                                                      
         DC    CL12'DQU SV FLAG '                                               
         DC    CL80'DQU SAVED AREA INITIALISED FLAG'                            
*                                                                               
         DC    AL1(GLVPGM)                                                      
         DC    CL12'PROGRAM NAME'                                               
         DC    CL80'FIRST THREE BYTES OF PROGRAM NAME'                          
*                                                                               
         DC    AL1(GLVXCTL)                                                     
         DC    CL12'XCTL VAR'                                                   
         DC    CL80'TRANSFER CONTROL VARIABLE'                                  
*                                                                               
         DC    AL1(GLVXREC)                                                     
         DC    CL12'XCTL RECORD'                                                
         DC    CL80'TRANSFER CONTROL RECORD NAME'                               
*                                                                               
         DC    AL1(GLVXACT)                                                     
         DC    CL12'XCTL ACTION'                                                
         DC    CL80'TRACNSFER CONTROL ACTION NAME'                              
*                                                                               
         DC    AL1(GLVNOTE)                                                     
         DC    CL12'IP NOTIFY'                                                  
         DC    CL80'INTER-PROGRAM NOTIFY'                                       
*                                                                               
         DC    AL1(GLVPFM)                                                      
         DC    CL12'PFM'                                                        
         DC    CL80'PFM FILE/DISKADDR/KEY'                                      
*                                                                               
*&&US                                                                           
         DC    AL1(GLVSPMD)                                                     
         DC    CL12'SPOT MEDIA  '                                               
         DC    CL80'SPOT SYSTEM MEDIA'                                          
*                                                                               
         DC    AL1(GLVSPCLT)                                                    
         DC    CL12'SPOT CLIENT '                                               
         DC    CL80'SPOT SYSTEM CLIENT'                                         
*                                                                               
         DC    AL1(GLVSPPRD)                                                    
         DC    CL12'SPOT PRODUCT'                                               
         DC    CL80'SPOT SYSTEM PRODUCT'                                        
*                                                                               
         DC    AL1(GLVSPMKT)                                                    
         DC    CL12'SPOT MARKET '                                               
         DC    CL80'SPOT SYSTEM MARKET'                                         
*                                                                               
         DC    AL1(GLVSPSTA)                                                    
         DC    CL12'SPOT STATION'                                               
         DC    CL80'SPOT SYSTEM STATION'                                        
*                                                                               
         DC    AL1(GLVSPEST)                                                    
         DC    CL12'SPOT EST.   '                                               
         DC    CL80'SPOT SYSTEM ESTIMATE'                                       
*                                                                               
         DC    AL1(GLVSPPER)                                                    
         DC    CL12'SPOT PERIOD '                                               
         DC    CL80'SPOT SYSTEM PERIOD(BROADCAST MONTH)'                        
*                                                                               
         DC    AL1(GLVSPREQ)                                                    
         DC    CL12'SPOT REQU.  '                                               
         DC    CL80'SPOT SYSTEM REQUESTOR'                                      
*                                                                               
         DC    AL1(GLVSPPAY)                                                    
         DC    CL12'SPOT MED EXT'                                               
         DC    CL80'SPOT SYSTEM MEDIA EXTENSION FOR $PAY'                       
*                                                                               
         DC    AL1(GLVSPORD)                                                    
         DC    CL12'DARE ORD NUM'                                               
         DC    CL80'DARE ORDER NUMBER'                                          
*                                                                               
         DC    AL1(GLVSPRTE)                                                    
         DC    CL12'DARE RT CODE'                                               
         DC    CL80'DARE ROUTING CODE'                                          
*                                                                               
         DC    AL1(GLVSPBYR)                                                    
         DC    CL12'DARE ORD NUM'                                               
         DC    CL80'DARE BUYER CODE'                                            
*                                                                               
         DC    AL1(GLVSPBUY)                                                    
         DC    CL12'SPOT BUYLINE'                                               
         DC    CL80'SPOT BUYLINE NUMBER'                                        
*                                                                               
         DC    AL1(GLVSPPRD)                                                    
         DC    CL12'DARE PRODS'                                                 
         DC    CL80'DARE PRODUCT CODES'                                         
*                                                                               
         DC    AL1(GLVSPCON)                                                    
         DC    CL12'CONTRACT NUM'                                               
         DC    CL80'CONTRACT NUMBER'                                            
*                                                                               
         DC    AL1(GLVSPMKG)                                                    
         DC    CL12'MAKEGOOD GRP'                                               
         DC    CL80'MAKEGOOD GROUP CODE'                                        
*&&                                                                             
*&&US                                                                           
         DC    AL1(GLVPRMD)                                                     
         DC    CL12'PRINT MEDIA'                                                
         DC    CL80'PRINT MEDIA'                                                
*                                                                               
         DC    AL1(GLVPRCLT)                                                    
         DC    CL12'PRINT CLIENT'                                               
         DC    CL80'PRINT CLIENT'                                               
*                                                                               
         DC    AL1(GLVPRPRD)                                                    
         DC    CL12'PRNT PRODUCT'                                               
         DC    CL80'PRINT PRODUCT'                                              
*                                                                               
         DC    AL1(GLVPRPUB)                                                    
         DC    CL12'PRINT PUB'                                                  
         DC    CL80'PRINT PUB'                                                  
*                                                                               
         DC    AL1(GLVPRPAY)                                                    
         DC    CL12'PRINT PAYEE'                                                
         DC    CL80'PRINT PAYEE TYPE/CODE'                                      
*                                                                               
         DC    AL1(GLVPREST)                                                    
         DC    CL12'PRINT EST.'                                                 
         DC    CL80'PRINT ESTIMATE'                                             
*                                                                               
         DC    AL1(GLVPRPER)                                                    
         DC    CL12'PRINT PERIOD'                                               
         DC    CL80'PRINT PERIOD'                                               
*                                                                               
         DC    AL1(GLVPRMAT)                                                    
         DC    CL12'PRINT INV MK'                                               
         DC    CL80'PRINT INVOICE MATCH KEY'                                    
*                                                                               
         DC    AL1(GLVPRGRS)                                                    
         DC    CL12'PRINT INV PSG'                                              
         DC    CL80'PRINT INVOICE DOLLARS FOR PAY SWITCH GROSS'                 
*                                                                               
         DC    AL1(GLVPRNET)                                                    
         DC    CL12'PRINT INV PSN'                                              
         DC    CL80'PRINT INVOICE DOLLARS FOR PAY SWITCH NET'                   
*                                                                               
         DC    AL1(GLVPRDTA)                                                    
         DC    CL12'PRINT MBC'                                                  
         DC    CL80'PRINT MBC DATA TYPES'                                       
*                                                                               
         DC    AL1(GLVPRRTN)                                                    
         DC    CL12'PRINT RTC'                                                  
         DC    CL80'PRINT RETURN CODE'                                          
*&&                                                                             
*&&UK                                                                           
         DC    AL1(GLVMEDEM)                                                    
         DC    CL12'DEMPRO NUM'                                                 
         DC    CL80'MEDLINE DEMPRO NUMBER'                                      
*                                                                               
         DC    AL1(GLVMERTN)                                                    
         DC    CL12'MEDLINE RTC'                                                
         DC    CL80'MEDLINE RETURN CODE'                                        
*&&                                                                             
*&&US                                                                           
         DC    AL1(GLRCONNO)                                                    
         DC    CL12'REP CONTRACT'                                               
         DC    CL80'REP CONTRACT NUMBER'                                        
*                                                                               
         DC    AL1(GLRPFKEY)                                                    
         DC    CL12'REP PF KEY'                                                 
         DC    CL80'REP PF KEY'                                                 
*                                                                               
         DC    AL1(GLRSTAT)                                                     
         DC    CL12'REP STATUS'                                                 
         DC    CL80'REP STATUS WORD'                                            
*                                                                               
         DC    AL1(GLRORDDA)                                                    
         DC    CL12'REP DARE ODA'                                               
         DC    CL80'REP DARE AGENCY RDER DISK ADDRESS'                          
*                                                                               
         DC    AL1(GLRDARE)                                                     
         DC    CL12'REP DARE'                                                   
         DC    CL80'REP WE CAME FROM DARE'                                      
*&&                                                                             
*                                                                               
         DC    AL1(GLVMBREC)                                                    
         DC    CL12'MB RECORD   '                                               
         DC    CL80'MEDIABASE RECORD'                                           
*                                                                               
         DC    AL1(GLVMBKEY)                                                    
         DC    CL12'MB KEY      '                                               
         DC    CL80'MEDIABASE KEY'                                              
*                                                                               
         DC    AL1(GLVMBSEL)                                                    
         DC    CL12'MB SELECT   '                                               
         DC    CL80'MEDIABASE SELECT'                                           
*                                                                               
         DC    AL1(GLVMBDAT)                                                    
         DC    CL12'MB DATE     '                                               
         DC    CL80'MEDIABASE DATE'                                             
*                                                                               
         DC    AL1(GLVMBACT)                                                    
         DC    CL12'MB ACTION   '                                               
         DC    CL80'MEDIABASE ACTION'                                           
*                                                                               
         DC    AL1(GLVMBDSC)                                                    
         DC    CL12'MB DESC'                                                    
         DC    CL80'MEDIABASE DESCRIPTION'                                      
*                                                                               
         DC    AL1(GLVMBOWN)                                                    
         DC    CL12'MB OWNER'                                                   
         DC    CL80'MEDIABASE OWNER'                                            
*                                                                               
         DC    AL1(GLVMBMTY)                                                    
         DC    CL12'MB MEDIA TYP'                                               
         DC    CL80'MEDIABASE MEDIA TYPE'                                       
*                                                                               
         DC    AL1(GLVMBLST)                                                    
         DC    CL12'MB LIST'                                                    
         DC    CL80'MEDIABASE LIST'                                             
*                                                                               
         DC    AL1(GLVMBCRK)                                                    
         DC    CL12'MB CRKEY'                                                   
         DC    CL80'MEDIABASE CALLING RECORD''S KEY'                            
*                                                                               
*                                                                               
         DC    AL1(GLVCTPID)                                                    
         DC    CL12'PERSONAL ID'                                                
         DC    CL80'CONTROL SECURITY SYSTEM PERSONAL ID'                        
*                                                                               
GLOTXTX  DC    X'FFFFFFFF'                                                      
         EJECT                                                                  
*************************************************************                   
*                    WORKING STORAGE                        *                   
*************************************************************                   
         SPACE 1                                                                
WRKD     DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
SAVERE   DS    F                                                                
DMCB     DS    6F                                                               
ACTION   DS    X                   ACTION CODE                                  
RECLEN   DS    H                   TEMPSTR RECORD LENGTH                        
MVSTIME  DS    F                   IBM TIME BINARY 100THS SECS.                 
MVSDATE  DS    F                   IBM DATE JULIAN                              
RELO     DS    A                   RELOCATION FACTOR                            
ROUTINE  DS    A                   ADDRESS ACTION ROUTINE                       
ATIA     DS    A                   ATIA                                         
APARMS   DS    A                   ADDRESS INPUT PARAMETERS                     
ATIOB    DS    A                   ATIOB                                        
ATSKGLOB DS    A                   ADDRESS                                      
*                                                                               
VGETFACT DS    A                   ADDRESSES OF EXTERNAL ROUTINES               
VGLOBBER DS    A                                                                
VHEXOUT  DS    A                                                                
VDICTATE DS    A                                                                
VGETTXT  DS    A                                                                
VSCANNER DS    A                                                                
VSQUASH  DS    A                                                                
VGETHELP DS    A                                                                
VDATCON  DS    A                                                                
VTIMEOUT DS    A                                                                
VNUMVAL  DS    A                                                                
DATAMGR  DS    A                                                                
*                                                                               
CURSOR   DS    A                   ADDRESS TWA FIELD FOR CURSOR                 
*                                                                               
DDSFLAG  DS    X                   TERMINAL STATUS FLAG                         
DDSTRM   EQU   X'20'                                                            
DDSACT   EQU   X'20'                                                            
DDSPUB   EQU   X'08'                                                            
DDSNEW   EQU   X'04'                                                            
*                                                                               
QHDR     DS    A                   HELP ROUTINE WORK AREAS                      
HELPKEY  DS    0CL10                                                            
         DS    XL3                                                              
HELPNUM  DS    XL1                                                              
HELPPAG  DS    XL1                                                              
         DS    XL5                                                              
HELP     DS    0CL4                                                             
HLPFLD   DS    C                                                                
HLPPOS   DS    C                                                                
HLPPAG   DS    C                                                                
HLPFLG   DS    C                                                                
FLAG     DS    C                                                                
*                                                                               
PUBLICID DS    AL2                                                              
         DS    AL2                                                              
LOGONID  DS    XL2                                                              
USERID   DS    CL8                                                              
TRMTYP   DS    XL2                 UTL INFO SAVE                                
TRMTYP1  DS    X                                                                
TRMUSER  DS    XL2                                                              
TRM      DS    XL2                                                              
*                                                                               
WORK     DS    CL32                                                             
MSG      DS    CL80                                                             
EXT      DS    CL132               EXTRA TEXT FOR GETTXT CALLS                  
SAVE     DS    360C                                                             
*                                                                               
SCANBLK  DS    256C                                                             
*                                                                               
*                                  $GLOBAL SAVE DATA AREA                       
*                                                                               
SAVEDSTR DS    0F                  SAVED STORAGE VARIABLES (COPY)               
IDENT    DS    CL4                 $GLO INDENTIFIER                             
SVELCODE DS    X                   FIRST ELEMENT CODE SAVE                      
SWITCH1  DS    X                   OPTION SWITCH SAVE                           
SWINAS   EQU   X'02'               NOT ASSIGNED ELEMENT DISPLAY SWITCH          
SWINUL   EQU   X'04'               EMPTY ELEMNT DISPLAY SWITCH                  
SVTAB    DS    (LINENUM+1)XL2      DISPLAY LINE INFO SAVE DATA TABLE            
*                                                                               
* 2 BYTE ENTRY IS:  1 BYTE=ELEMENT CODE + 1 BYTE=SUB-ACTION CODE                
*                 OR H'00' FOR EMPTY ENTRY AND END OF TABLE                     
SAVEDL   EQU   *-SAVEDSTR                                                       
SVTABL   EQU   *-SVTAB                                                          
*                                                                               
DDDSLST  DS    0C                                                               
         DSDDL                                                                  
*                                                                               
WRKX     EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*                    OTHER DSECTS                           *                   
*************************************************************                   
         SPACE 1                                                                
REPLINE  DSECT                     DISPLAY GLOBAL REPORT LINE DSECT             
RLINE    DS    0CL(SRVLIN2-SRVLIN1)                                             
RLSAHDR  DS    CL8                                                              
RLSACT   DS    CL3                 SUB-ACTION                                   
RLLNHDR  DS    CL8                                                              
RLCODE   DS    CL4                 ELEMENT CODE                                 
         DS    CL1                                                              
RLNAME   DS    CL12                ELEMENT SHORT NAME                           
RLVAHDR  DS    CL8                                                              
RLVALU   DS    CL56                ELEMENT DATA VALUE                           
         SPACE 1                                                                
GLOBD    DSECT                     GLOBAL AREA DSECT                            
*                                  GLOBAL STATUS AREA                           
GLOBDATE DS    PL4                 DATE OF LAST SAVE OF GLOBALS                 
GLOBTIME DS    CL4                 TIME OF LAST SAVE OF GLOBALS                 
GLOBINFO DS    XL3                 'U' UPDATED,'I' INITIALISED                  
GLOBSWS  DS    XL1                 GLOBBER SWITCHES                             
GLOBREAD EQU   X'80'               SET IF GLOBALS READ FROM DISK                
GLOBUPDT EQU   X'40'               SET IF GLOBALS UPDATED                       
GLOBCLR  EQU   X'20'               SET TO CLEAR GLOBALS                         
*                                                                               
GLOBELS  DS    XL500               GLOBAL DATA AREA                             
*                                                                               
GLOBELEN EQU   *-GLOBELS           LENGTH OF GLOBAL ELEMENTS                    
GLOBALEN EQU   *-GLOBD             LENGTH OF GLOBAL DATA AREA                   
         SPACE 1                                                                
*FADSECTS                                                                       
SRGLOFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE SRGLOFFD                                                       
         EJECT                                                                  
* FADSECTS                                                                      
* FACHKPT                                                                       
* DDCOMFACS                                                                     
* SRDDEQUS                                                                      
* SRERREQUS                                                                     
* DDGLOBEQUS                                                                    
* CTGENFILE                                                                     
       ++INCLUDE FADSECTS                                                       
       ++INCLUDE FACHKPT                                                        
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SRDDEQUS                                                       
       ++INCLUDE SRERREQUS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SRGLO00   05/01/02'                                      
         END                                                                    
