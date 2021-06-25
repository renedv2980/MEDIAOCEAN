*          DATA SET TAREP53    AT LEVEL 035 AS OF 03/05/16                      
*PHASE T70353C,*                                                                
*                                                                               
         TITLE 'T70353 - JOB INTERFACE REPORT'                                  
T70353   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70353,R6                                                      
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         LA    R7,BUFF             R7=A(LOCAL W/S)                              
         LA    R7,8(R7)                                                         
         USING TJBD,R7                                                          
         EJECT                                                                  
*                                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         CLI   MODE,VALKEY         IF MODE TO VALIDATE                          
         BNE   *+12                                                             
         BAS   RE,VKEY             VALIDATE IT                                  
         B     XIT                                                              
*                                                                               
         CLI   MODE,PRINTREP       IF MODE TO PRINT REPORT                      
         BNE   XIT                                                              
         BAS   RE,OPENDATA         OPEN INPUT DATA                              
         BAS   RE,INIT             INITIALIZE VALUES                            
*                                                                               
         TM    JBOPTS,JBSAAT       IF OPTION SAATCHI,                           
         BNO   JOB050                                                           
         MVI   RCSUBPRG,1                                                       
         BAS   RE,PREPS            PROCESS SAATCHI REPORT                       
         B     JOB110                                                           
*                                                                               
JOB050   TM    JBOPTS,JBJWT        IF OPTION JWT,                               
         BNO   JOB060                                                           
         MVI   RCSUBPRG,2                                                       
         BAS   RE,PREPJ            PROCESS JWT REPORT                           
         B     JOB110                                                           
*                                                                               
JOB060   TM    JBOPTS,JBBBDO       IF OPTION BBDO,                              
         BNO   JOB100                                                           
         MVI   RCSUBPRG,3                                                       
         BAS   RE,PREPB            PROCESS BBDO REPORT                          
         B     JOB110                                                           
*                                                                               
JOB100   MVI   RCSUBPRG,1          PROCESS THE REPORT                           
         BAS   RE,PREP             PROCESS THE REPORT                           
JOB110   BAS   RE,CLOSDATA         CLOSE                                        
         GOTO1 SORTER,DMCB,=C'END' END THE SORT                                 
         CLI   SORTFRST,C'Y'       IF SORT WAS NOT ACTIVE                       
         BNE   JOB200                                                           
         XC    HEADHOOK,HEADHOOK   CLEAR HEADLINE HOOK                          
         XC    SPECS,SPECS         AND SPECS                                    
*                                                                               
JOB200   BAS   RE,PRNTIT           AND PRINT TOTAL ADDED TO FILE                
         EDIT  RECCNT,(8,P+1),ALIGN=LEFT,ZERO=NOBLANK                           
         LR    R2,R0                                                            
         LA    R2,P+2(R2)                                                       
         MVC   0(25,R2),=CL25'JOB RECORDS ADDED TO FILE'                        
         BAS   RE,PRNTIT                                                        
         TM    JBOPTS,JBBBDO                                                    
         BZ    XIT                                                              
         EDIT  ERRCNT,(8,P+1),ALIGN=LEFT,ZERO=NOBLANK                           
         LR    R2,R0                                                            
         LA    R2,P+2(R2)                                                       
         MVC   0(25,R2),=CL25'ERRORS WITH JOB RECORDS'                          
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO INITIALIZE SOME VALUES                                
         SPACE 1                                                                
INIT     NTR1                                                                   
         XC    SVSRTKEY,SVSRTKEY   CLEAR SAVED SORT RECORD VALUES               
         MVI   PROCDATA,C'N'       NOT DATA PROCESSING                          
         ZAP   RECCNT,=P'0'        COUNTER OF JOB RECORDS ADDED                 
         ZAP   ERRCNT,=P'0'        COUNTER OF ERRORS                            
         L     R1,=A(JOBREC)                                                    
         ST    R1,AJOBREC          SET A(NEW RECORD)                            
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK         SET A(HEADHOOK)                              
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS            SET A(SPECS)                                 
         B     XIT                                                              
         SPACE 2                                                                
YES      SR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALIDATE THE KEY                                                 
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LA    R2,SPLOPTH          R2=A(OPTIONS)                                
         MVI   JBOPTS,0                                                         
         CLI   5(R2),0             IF ANY OPTIONS                               
         BE    VKEYX                                                            
*                                                                               
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
*                                                                               
VK10     CLC   =C'TRACE',SCDATA1   TRACE                                        
         BNE   VK20                                                             
         CLI   SCDATA2,C'N'                                                     
         BE    VK50                                                             
         CLI   SCDATA2,C'Y'                                                     
         BNE   INVERR                                                           
         OI    JBOPTS,JBTRACE      SET TRACE ON                                 
         B     VK50                                                             
*                                                                               
VK20     CLC   =C'SAATCHI',SCDATA1 SAATCHI AGENCIES                             
         BNE   VK30                                                             
         OI    JBOPTS,JBSAAT       SET SAATCHI OPTION ON                        
         TM    JBOPTS,JBJWT+JBBBDO CAN'T JWT AND BBDO OPTIONS                   
         BZ    VK50                                                             
         B     INVERR                                                           
*                                                                               
VK30     CLC   =C'JWT',SCDATA1     JWT AGENCIES                                 
         BNE   VK40                                                             
         OI    JBOPTS,JBJWT        SET JWT OPTION ON                            
         TM    JBOPTS,JBSAAT+JBBBDO  CAN'T SAATCHI AND BBDO OPTIONS             
         BZ    VK50                                                             
         B     INVERR                                                           
*                                                                               
VK40     CLC   =C'BBDO',SCDATA1    BBDO AGENCIES                                
         BNE   INVERR                                                           
         OI    JBOPTS,JBBBDO       SET BBDO OPTION ON                           
         TM    JBOPTS,JBSAAT+JBJWT CAN'T HAVE SAATCHI AND JWT OPTIONS           
         BZ    VK50                                                             
         B     INVERR                                                           
*                                                                               
VK50     LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VK10             AND CONTINUE                                 
*                                                                               
VKEYX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              PROCESS REPORT                                                   
         SPACE                                                                  
PREP     NTR1                                                                   
         MVC   AIO,AJOBREC         SET A(NEW JOB RECORD)                        
         LA    R2,SORTREC          R2=A(SORT RECORD)                            
         USING SORTRECD,R2                                                      
*                                                                               
PREP10   BAS   RE,GETDATA          GET FIRST/(NEXT) RECORD                      
*                                                                               
         LA    R5,DATAREC                                                       
         USING DATARECD,R5                                                      
         CLI   0(R5),X'FF'         IF NOT END OF DATA                           
         BE    PREP50                                                           
         CLC   0(3,R5),=C'SE*'                                                  
         BE    PREP50                                                           
         CLI   PROCDATA,C'Y'       IF DATA FLAG NOT SET                         
         BE    PREP30                                                           
         CLC   0(3,R5),=C'ST*'     KEEP LOOK FOR LAST LINE OF HEADER            
         BNE   PREP10                                                           
         MVI   PROCDATA,C'Y'       FOUND IT, SET DATA COMING                    
         B     PREP10                                                           
*                                                                               
PREP30   CLC   DATAREC,SPACES      IF ALL SPACES                                
         BE    PREP10              SKIP IT                                      
         XC    SORTREC,SORTREC                                                  
         MVC   SRTCLI(L'DATACLI),DATACLI                                        
         MVC   SRTPRD(L'DATAPRD),DATAPRD                                        
         MVC   SRTJOB(L'DATAJOB),DATAJOB                                        
         MVC   SRTAGY(L'DATAAGY),DATAAGY                                        
         OC    SORTREC,SPACES                                                   
         BAS   RE,PUTSORT          PUT RECORD TO SORTER                         
         B     PREP10                                                           
*                                                                               
PREP50   CLI   SORTFRST,C'Y'       IF SORT WAS ACTIVE                           
         BE    XIT                                                              
*                                                                               
PREP60   BAS   RE,GETSORT          GET FIRST/(NEXT) SORT RECORD                 
         LTR   R2,R2               R2=A(SORT RECORD)                            
         BZ    PREPX                                                            
         MVC   SORTREC,0(R2)                                                    
         TM    JBOPTS,JBTRACE                                                   
         BZ    PREP65                                                           
         MVC   P+1(L'SORTREC),SORTREC                                           
         MVC   P+1+L'SORTREC+1(18),=CL18' --- GET FROM SORT'                    
         BAS   RE,PRNTIT                                                        
*                                                                               
PREP65   OC    SVSRTKEY,SVSRTKEY   IF NOT FIRST RECORD                          
         BZ    PREP80                                                           
*                                                                               
         CLC   SVAGY,SRTAGY        IF SAME AGENCY, CLIENT, & PRODUCT            
         BNE   PREP70                                                           
         CLC   SVCLI,SRTCLI                                                     
         BNE   PREP70                                                           
         CLC   SVPRD,SRTPRD                                                     
         BNE   PREP70                                                           
         BAS   RE,ADDJBEL          ADD JOB TO EXISTING RECORD                   
         B     PREP60                                                           
*                                                                               
PREP70   BAS   RE,ADDIT            ADD JOB RECORD AND PRINT                     
         CLC   SVAGY,SRTAGY        IF IT IS AN AGENCY CHANGE                    
         BE    PREP80                                                           
         MVI   FORCEHED,C'Y'       SET NEW PAGE                                 
*                                                                               
PREP80   BAS   RE,SVVALS           SAVE NEW VALUES                              
         BAS   RE,SETNREC          SET UP NEW RECORD W/FIRST ELE                
         B     PREP60              AND GET NEXT                                 
*                                                                               
PREPX    CP    RECCNT,=P'0'        IF ADDED RECORDS                             
         BE    XIT                                                              
         BAS   RE,ADDIT            ADD LAST JOB RECORD AND PRINT                
         B     XIT                                                              
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              PROCESS JWT REPORT                                               
*---------------------------------------------------------------------          
PREPJ    NTR1                                                                   
         MVC   AIO,AJOBREC         SET A(NEW JOB RECORD)                        
*                                                                               
PREPJ10  BAS   RE,GETDATA          GET FIRST/(NEXT) RECORD                      
*                                                                               
         LA    R5,DATAREC                                                       
         USING JWTRECD,R5                                                       
         CLI   0(R5),X'FF'         END OF DATA?                                 
         BE    PREPJ50                                                          
*                                                                               
PREPJ30  CLC   DATARECS,SPACES     IF ALL SPACES                                
         BNH   PREPJ10             SKIP IT                                      
*                                                                               
         BRAS  RE,BLDJWTJB         BUILD JWT JOB AND ADD IT                     
*                                                                               
         USING PRNTD,R2                                                         
         LA    R2,P                PRINT IT                                     
         BNE   PREPJ40             NOPE, ON FILE BEFORE, GET NEXT ONE           
*                                                                               
         MVI   P,C'+'                                                           
PREPJ40  MVC   PRJBUNIT,JWTBUS                                                  
         MVC   PRJPRJID,JWTPRJID                                                
         MVC   PRJCSTID,JWTCSTID                                                
         MVC   PRJPRD,JWTPROD                                                   
         BAS   RE,PRNTIT                                                        
         B     PREPJ10             GET NEXT ONE                                 
*                                                                               
PREPJ50  B     XIT                                                              
         DROP  R5,R2                                                            
         EJECT                                                                  
*---------------------------------------------------------------------          
*              PROCESS BBDO REPORT                                              
*---------------------------------------------------------------------          
PREPB    NTR1                                                                   
         MVC   AIO,AJOBREC         SET A(NEW JOB RECORD)                        
         USING PRNTD,R2                                                         
         LA    R2,P                PRINT IT                                     
*                                                                               
PREPB10  BAS   RE,GETDATA          GET FIRST/(NEXT) RECORD                      
*                                                                               
         LA    R5,DATAREC                                                       
         USING BBDORECD,R5                                                      
         CLI   0(R5),X'FF'         END OF DATA?                                 
         BE    PREPB99                                                          
*                                                                               
         CLC   =C'ProjectID',BBDOPROJ    SKIP FIRST LINE OF FILE                
         BE    PREPB10                                                          
         CLC   BBDOPROJ+16(10),SPACES    IF MORE THAN 16 CHARS, ERROR           
         BH    PREPB45                                                          
*                                                                               
PREPB39  BRAS  RE,BLDBBDJB         BUILD BBD JOB AND ADD IT                     
         BNE   PREPB40             NOPE, ON FILE BEFORE, GET NEXT ONE           
*                                                                               
         MVI   P,C' '                                                           
PREPB40  MVC   PRBPRJID,BBDOPROJ                                                
         B     PREPB90                                                          
*                                                                               
PREPB45  MVC   PRBPRJID,BBDOPROJ                                                
         MVC   PRBPRJID+20(36),=C'**ERROR - JOB IS LONGER THAN 16 CHAR'         
         AP    ERRCNT,=P'1'                                                     
*                                                                               
PREPB90  BAS   RE,PRNTIT                                                        
         B     PREPB10             GET NEXT ONE                                 
*                                                                               
PREPB99  B     XIT                                                              
         DROP  R5,R2                                                            
         EJECT                                                                  
*---------------------------------------------------------------------          
*              PROCESS SAATCHI REPORT                                           
*---------------------------------------------------------------------          
         SPACE                                                                  
PREPS    NTR1                                                                   
         MVC   AIO,AJOBREC         SET A(NEW JOB RECORD)                        
         LA    R2,SORTREC          R2=A(SORT RECORD)                            
         USING SORTRECD,R2                                                      
*                                                                               
PREPS10  BAS   RE,GETDATA          GET FIRST/(NEXT) RECORD                      
*                                                                               
         LA    R5,DATARECS                                                      
         USING SAATRECD,R5                                                      
         CLI   0(R5),X'FF'         END OF DATA?                                 
         BE    PREPS50                                                          
*                                                                               
PREPS30  CLC   DATARECS,SPACES     IF ALL SPACES                                
         BE    PREPS10             SKIP IT                                      
         XC    SORTREC,SORTREC                                                  
         CLC   SAATTYP,=C'ADM'     SKIP, IF TYPE IS ADM                         
         BE    PREPS10                                                          
         MVC   SRTCLI(L'SAATCLI),SAATBUS                                        
         MVC   SRTCLI+3(L'SAATCLI),SAATCLI                                      
         MVC   SRTPRD(L'SAATPRD),SAATPRD                                        
         MVC   SRTJOB(L'SAATJOB),SAATJOB                                        
         MVC   SRTBUS(L'SAATBUS),SAATBUS                                        
*                                                                               
         LA    R3,AGYTAB                                                        
         USING AGYTABD,R3                                                       
PREPS35  CLI   0(R3),X'FF'         IF NO MATCH ON BUSINESS UNIT, SKIP           
         BE    PREPS10                                                          
         CLC   SAATBUS,AGYBUS      BUSINESS UNIT                                
         BE    PREPS40                                                          
         LA    R3,AGYLNQ(R3)                                                    
         B     PREPS35                                                          
PREPS40  MVC   SRTAGY,AGYAGY       NON-PRINT AGENCY                             
         CLC   SAATTYP,=C'PRI'     IF PROJECT TYPE IS PRINT                     
         BNE   *+10                                                             
         MVC   SRTAGY,AGYPAGY      USE PRINT AGENCY                             
*                                                                               
         OC    SORTREC,SPACES                                                   
         BAS   RE,PUTSORT          PUT RECORD TO SORTER                         
         B     PREPS10                                                          
*                                                                               
PREPS50  CLI   SORTFRST,C'Y'       IF SORT WAS ACTIVE                           
         BE    XIT                                                              
*                                                                               
PREPS60  BAS   RE,GETSORT          GET FIRST/(NEXT) SORT RECORD                 
         LTR   R2,R2               R2=A(SORT RECORD)                            
         BZ    PREPSX                                                           
         MVC   SORTREC,0(R2)                                                    
         TM    JBOPTS,JBTRACE                                                   
         BZ    PREPS65                                                          
         MVC   P+1(L'SORTREC),SORTREC                                           
         MVC   P+1+L'SORTREC+1(18),=CL18' --- GET FROM SORT'                    
         BAS   RE,PRNTIT                                                        
*                                                                               
PREPS65  OC    SVSRTKEY,SVSRTKEY   IF NOT FIRST RECORD                          
         BZ    PREPS80                                                          
*                                                                               
         CLC   SVAGY,SRTAGY        IF SAME AGENCY, CLIENT, & PRODUCT            
         BNE   PREPS70                                                          
         CLC   SVCLI,SRTCLI                                                     
         BNE   PREPS70                                                          
         CLC   SVPRD,SRTPRD                                                     
         BNE   PREPS70                                                          
         BAS   RE,ADDJBEL          ADD JOB TO EXISTING RECORD                   
         B     PREPS60                                                          
*                                                                               
PREPS70  BAS   RE,ADDIT            ADD JOB RECORD AND PRINT                     
         CLC   SVAGY,SRTAGY        IF IT IS AN AGENCY CHANGE                    
         BE    PREPS80                                                          
         MVI   FORCEHED,C'Y'       SET NEW PAGE                                 
*                                                                               
PREPS80  BAS   RE,SVVALS           SAVE NEW VALUES                              
         BAS   RE,SETNREC          SET UP NEW RECORD W/FIRST ELE                
         BAS   RE,ADDBUEL          ADD BUSINESS UNIT ELEMENT                    
         B     PREPS60             AND GET NEXT                                 
*                                                                               
PREPSX   CP    RECCNT,=P'0'        IF ADDED RECORDS                             
         BE    XIT                                                              
         BAS   RE,ADDIT            ADD LAST JOB RECORD AND PRINT                
         B     XIT                                                              
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE TO SAVE CURRENT SORT RECORD VALUES                       
         SPACE                                                                  
SVVALS   NTR1                                                                   
         MVC   SVAGY,SRTAGY                                                     
         MVC   SVCLI,SRTCLI                                                     
         MVC   SVPRD,SRTPRD                                                     
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO SET PRELIMINARY JOB RECORD                            
SETNREC  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLJBD,R4                                                         
         MVI   TLJBCD,TLJBCDQ      SET RECORD CODE                              
         MVC   TLJBAGY,SRTAGY      SET AGENCY                                   
*                                                                               
         CLC   TLJBAGY,=C'2071  '  IF AGENCY IS 2071                            
         BNE   *+10                                                             
         MVC   TLJBAGY,=C'4937  '  LOOK FOR JOB UNDER 4937                      
*                                                                               
         MVC   TLJBCLI,SRTCLI      SET CLIENT CODE                              
         MVC   TLJBPRD,SRTPRD      SET PRODUCT CODE                             
         MVC   TLJBDTE,TGNXTBUS    SET PWOS NEXT BUSINESS DAY                   
         XC    TLJBDTE,XFFS        COMPLEMENT DATE                              
         SPACE 1                                                                
         L     R4,AIO              R4=A(NEW RECORD)                             
         USING TLJBD,R4                                                         
         MVC   TLJBKEY,KEY         SET KEY IN IOAREA                            
         MVC   TLJBLEN,DATADISP    SET START LENGTH                             
         XC    TLJBSTAT(10),TLJBSTAT                                            
*                                                                               
         BAS   RE,ADDJBEL          ADD FIRST ELEMENT                            
*                                                                               
         GOTO1 ACTVIN,DMCB,0       ADD ACTIVITY ELEMENT                         
         AP    RECCNT,=P'1'        ADD TO RECORD COUNT                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD JOB ELEMENT                                       
         SPACE                                                                  
ADDJBEL  NTR1                                                                   
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAGLD,R4                                                         
         MVI   TAGLEL,TAGLELQ                                                   
         MVI   TAGLLEN,TAGLLNQ+6                                                
         MVC   TAGLDATA(L'SRTJOB),SRTJOB                                        
         OC    TAGLDATA(L'SRTJOB),SPACES                                        
*                                                                               
         LA    R3,TAGLLEN          CHECK NOT DUPLICATE                          
         LA    R3,1(R3)                                                         
         MVI   ELCODE,TAGLELQ                                                   
         GOTO1 GETL,DMCB,(7,(R3))                                               
         BE    XIT                                                              
*                                                                               
         L     R4,AIO              R4=A(RECORD)                                 
         USING TLJBD,R4                                                         
         LH    R1,TLJBLEN          IF MAXIMUM RECORD LENGTH REACHED             
         CH    R1,=H'9600'                                                      
         BL    *+6                                                              
         DC    H'0'                NEED TO INCREASE NEW RECORD AREA             
*                                  CURRENTLY 4 RECS AT 300 JOBS PER REC         
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,ELEMENT,0                           
         CLI   12(R1),5                                                         
         BE    XIT                                                              
         CLI   12(R1),0                                                         
         BE    XIT                                                              
         DC    H'00'                                                            
         SPACE 2                                                                
*              ROUTINE TO ADD BUSINESS UNIT ELEMENT                             
         SPACE                                                                  
ADDBUEL  NTR1                                                                   
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TANUD,R4                                                         
         MVI   TANUEL,TANUELQ                                                   
         MVI   TANULEN,TANULNQ+L'SRTBUS                                         
         MVI   TANUTYPE,TANUBUSI                                                
         MVC   TANUMBER(L'SRTBUS),SRTBUS    BUSINESS UNIT                       
*                                                                               
         L     R4,AIO              R4=A(RECORD)                                 
         USING TLJBD,R4                                                         
         LH    R1,TLJBLEN          IF MAXIMUM RECORD LENGTH REACHED             
         CH    R1,=H'5700'                                                      
         BL    *+6                                                              
         DC    H'0'                NEED TO INCREASE NEW RECORD AREA             
*                                  CURRENTLY 3 RECS AT 300 JOBS PER REC         
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,ELEMENT,0                           
         CLI   12(R1),5                                                         
         BE    XIT                                                              
         CLI   12(R1),0                                                         
         BE    XIT                                                              
         DC    H'00'                                                            
         SPACE 2                                                                
*              ROUTINE TO PRINT OUT AND ADD RECORD                              
         SPACE                                                                  
ADDIT    NTR1                                                                   
         L     R4,AJOBREC                                                       
         USING TLJBD,R4                                                         
         BAS   RE,PLINE            PRINT THE RECORD OUT FIRST                   
*                                                                               
         BAS   RE,SPLIT            SPLIT/ADD/WRITE RECORD(S)                    
         B     XIT                                                              
         EJECT                                                                  
*            ROUTINE TO PRINT NEW RECORD INFORMATION                            
         SPACE 1                                                                
PLINE    NTR1                                                                   
         LA    R2,P                R2=A(PRINT LINE)                             
         USING PRNTD,R2                                                         
*                                                                               
         MVC   PRTCLI,TLJBCLI      CLIENT                                       
         MVC   PRTPRD,TLJBPRD      PRODUCT                                      
*                                                                               
         L     R3,AIO2             R3=A(UNSCAN BLOCK)                           
         XR    R0,R0               R0=N'JOBS TO UNSCAN                          
         MVI   JOBCONT,C'N'        PRE-CLEAR JOB CONTINUE FLAG                  
*                                                                               
         USING TAGLD,R4                                                         
         MVI   ELCODE,TAGLELQ      R4=A(FIRST JOB)                              
         BAS   RE,GETEL                                                         
         BE    PLINE5                                                           
         DC    H'0'                SHOULD BE AT LEAST ONE                       
*                                                                               
PLINE5   MVC   0(20,R3),SPACES     PRE-CLEAR ENTRY                              
         ZIC   R1,TAGLLEN                                                       
         SH    R1,=AL2(TAGLLNQ+1)                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),TAGLDATA                                                 
         LA    R3,20(R3)           BUMP TO NEXT ENTRY                           
         AHI   R0,1                ADD TO COUNT                                 
         CH    R0,=AL2(2000/20)    IF NO MORE ROOM IN IOAREA                    
         BL    PLINE8                                                           
         MVI   JOBCONT,C'Y'        SET MORE ELEMENTS TO PROCESS                 
         B     *+12                                                             
PLINE8   BAS   RE,NEXTEL           ELSE, LOOK FOR MORE ELEMENTS                 
         BE    PLINE5                                                           
         MVC   DMCB,AIO2                                                        
         STC   R0,DMCB                                                          
*                                                                               
PLINE10  CLI   DMCB,0              TEST ANYTHING (LEFT) TO PRINT                
         BE    PLINE20                                                          
         MVC   BLOCK(80),SPACES                                                 
         GOTO1 UNSCAN,DMCB,,(C'C',BLOCK),0,0                                    
         MVC   PRTJOBS,BLOCK                                                    
         L     R0,DMCB             SAVE                                         
         BAS   RE,PRNTIT                                                        
         ST    R0,DMCB             RESTORE                                      
         B     PLINE10                                                          
*                                                                               
PLINE20  CLI   JOBCONT,C'Y'        IF MORE JOBS TO PROCESS                      
         BNE   XIT                                                              
         MVI   JOBCONT,C'N'        RESET INDICATOR                              
         XR    R0,R0                     COUNTER                                
         L     R3,AIO2                   UNSCAN INPUT AREA                      
         B     PLINE8                                                           
         EJECT                                                                  
*              ROUTINE ADDS RECORDS TO FILE                                     
*              IF NECESSARY, SPLITS 'BIG' RECORDS INTO SMALLER ONES             
         SPACE 1                                                                
SPLIT    NTR1                                                                   
         L     R4,AJOBREC          R4=A(RECORD TO ADD)                          
         L     R3,AIO2                                                          
         ST    R3,AIO                                                           
         USING TLJBD,R3                                                         
         MVC   TLJBKEY,0(R4)       SET KEY IN RECORD                            
         XC    TLJBSTAT(10),TLJBSTAT                                            
*                                                                               
SPL1     LA    R4,TLJBELEM-TLJBD(R4) R4=A(FIRST ELEMENT)                        
         MVI   ELCODE,0            SET TO LOOP THROUGH ALL ELEMS                
*                                                                               
SPL2     MVC   TLJBLEN,DATADISP    INIT RECORD LENGTH                           
         XC    TLJBELEM(2),TLJBELEM                                             
*                                                                               
SPL4     ZIC   R1,1(R4)            R1=L'ELEMENT                                 
         AH    R1,TLJBLEN             +L'RECORD                                 
         CH    R1,=H'1930'         WILL RECORD BECOME TOO LONG                  
         BH    SPL10               YES - GO ADD IT                              
         MVC   ELEMENT,0(R4)       NO - MOVE ELEMENT TO W/S                     
         GOTO1 ADDELEM,DMCB,,,,0   AND ADD TO NEW REC                           
*                                                                               
SPL8     BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    SPL4                                                             
*                                                                               
SPL10    BRAS  RE,DOADD            ADD/WRITE THE RECORD                         
         ZIC   R1,TLJBSEQ          BUMP SEQUENCE NUMBER IN KEY OF REC           
         LA    R1,2(R1)                                                         
         STC   R1,TLJBSEQ                                                       
         CLI   0(R4),0             IF THERE ARE MORE ELS. TO PROCESS            
         BNE   SPL2                GO BACK                                      
         BRAS  RE,DELETE           DELETE ANY REMAINING RECORDS                 
         MVC   AIO,AJOBREC         RESET IOAREA                                 
         B     XIT                                                              
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*              DATA ROUTINES                                                    
         SPACE 2                                                                
*              OPEN DATA FOR READS                                              
         SPACE 1                                                                
OPENDATA NTR1                                                                   
         L     R2,=A(TAPEIN)                                                    
         TM    JBOPTS,JBSAAT                                                    
         BNO   *+8                                                              
         L     R2,=A(TAPEINS)    FOR OPTION SAATCHI                             
         TM    JBOPTS,JBJWT                                                     
         BNO   *+8                                                              
         L     R2,=A(TAPEINJ)    FOR OPTION JWT                                 
         TM    JBOPTS,JBBBDO                                                    
         BNO   *+8                                                              
         L     R2,=A(TAPEINB)    FOR OPTION BBDO                                
         OPEN  ((2),INPUT)                                                      
         B     XIT                                                              
         SPACE 2                                                                
*              CLOSE DATA                                                       
         SPACE 1                                                                
CLOSDATA NTR1                                                                   
         L     R2,=A(TAPEIN)                                                    
         TM    JBOPTS,JBSAAT                                                    
         BNO   *+8                                                              
         L     R2,=A(TAPEINS)    FOR OPTION SAATCHI                             
         TM    JBOPTS,JBJWT                                                     
         BNO   *+8                                                              
         L     R2,=A(TAPEINJ)    FOR OPTION JWT                                 
         TM    JBOPTS,JBBBDO                                                    
         BNO   *+8                                                              
         L     R2,=A(TAPEINB)    FOR OPTION BBDO                                
         CLOSE ((2))                                                            
         B     XIT                                                              
         SPACE 2                                                                
*              GET FIRST(/NEXT) DATA                                            
         SPACE 1                                                                
GETDATA  NTR1                                                                   
         LA    R0,DATAREC                                                       
         L     R1,=A(TAPEIN)                                                    
*                                                                               
         TM    JBOPTS,JBSAAT                                                    
         BNO   *+12                                                             
         LA    R0,DATARECS                                                      
         L     R1,=A(TAPEINS)    FOR OPTION SAATCHI                             
*                                                                               
         TM    JBOPTS,JBJWT                                                     
         BNO   *+12                                                             
         LA    R0,DATAREC                                                       
         L     R1,=A(TAPEINJ)    FOR OPTION JWT                                 
*                                                                               
         TM    JBOPTS,JBBBDO                                                    
         BNO   *+12                                                             
         LA    R0,DATAREC                                                       
         L     R1,=A(TAPEINB)    FOR OPTION BBDO                                
*                                                                               
         GET   (1),(0)                                                          
         B     XIT                                                              
         SPACE 2                                                                
*              SET END OF DATA                                                  
         SPACE 1                                                                
DATAEOF  MVI   DATAREC,X'FF'       SET END OF DATA                              
         B     XIT                                                              
         EJECT                                                                  
*              SET END OF SAATCHI DATA                                          
         SPACE 1                                                                
DATAEOFS MVI   DATARECS,X'FF'      SET END OF DATA                              
         B     XIT                                                              
         EJECT                                                                  
*              VARIOUS SORT ROUTINES                                            
         SPACE 2                                                                
*              ROUTINE TO PUT RECORD TO SORTER                                  
         SPACE 1                                                                
PUTSORT  NTR1                                                                   
         CLI   SORTFRST,C'Y'                                                    
         BNE   PUTSORT2                                                         
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         MVI   SORTFRST,C'N'                                                    
         SPACE 1                                                                
PUTSORT2 GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO GET A RECORD FROM SORT                                
*                                  XIT - R2=A(SORT RECORD)                      
         SPACE 1                                                                
GETSORT  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R2,15,4(R1)         R2=A(SORT RECORD)                            
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              HEADLINE ROUTINE (HEADHOOK)                                      
         SPACE                                                                  
HOOK     NTR1                                                                   
         GOTO1 DATCON,DMCB,(1,TGNXTBUS),(8,H4+9)                                
*                                                                               
         CLI   RCSUBPRG,1                                                       
         BNE   XIT                                                              
         L     R4,AIO                                                           
         USING TLJBD,R4                                                         
         MVC   H3+9(L'TGAGY),TLJBAGY                                            
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*              PRINT A LINE                                                     
         SPACE                                                                  
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
INVERR   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS , ETC..                                                
*                                                                               
SORTFRST DC    C'Y'                                                             
XFFS     DC    6X'FF'                                                           
SORTCARD DC    CL80'SORT FIELDS=(1,27,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=27'                                    
*                                                                               
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GM),EODAD=DATAEOF,        X        
               RECFM=FB,LRECL=140                                               
*                                                                               
TAPEINS  DCB   DDNAME=TAPEINS,DSORG=PS,MACRF=(GM),EODAD=DATAEOFS,      X        
               RECFM=FB,LRECL=132                                               
*                                                                               
TAPEINJ  DCB   DDNAME=TAPEINJ,DSORG=PS,MACRF=(GM),EODAD=DATAEOF,       X        
               RECFM=FB,LRECL=39                                                
*                                                                               
TAPEINB  DCB   DDNAME=TAPEINB,DSORG=PS,MACRF=(GM),EODAD=DATAEOF,       X        
               RECFM=FB,LRECL=140                                               
*                                                                               
         EJECT                                                                  
*              AGENCY TABLE FOR SAATCHI                                         
*                                                                               
AGYTAB   DS    0CL15                                                            
         DC    CL3'402',CL6'DWNY',CL6'9032'                                     
         DC    CL3'405',CL6'DWNY',CL6'DWNY'                                     
         DC    CL3'407',CL6'DWNY',CL6'DWNY'                                     
         DC    CL3'408',CL6'DWNY',CL6'DWNY'                                     
         DC    CL3'409',CL6'DWNY',CL6'DWNY'                                     
         DC    CL3'410',CL6'DWLA',CL6'DWLA'                                     
         DC    CL3'411',CL6'DWNY',CL6'DWNY'                                     
         DC    CL3'412',CL6'8351',CL6'9548'                                     
         DC    CL3'413',CL6'8351',CL6'9548'                                     
         DC    CL3'414',CL6'8351',CL6'9548'                                     
         DC    CL3'415',CL6'DWNY',CL6'DWNY'                                     
         DC    CL3'416',CL6'8351',CL6'9548'                                     
         DC    CL3'417',CL6'DWNY',CL6'DWNY'                                     
         DC    CL3'418',CL6'DWNY',CL6'DWNY'                                     
         DC    CL3'420',CL6'TELA',CL6'TELA'                                     
         DC    CL3'421',CL6'TELA',CL6'TELA'                                     
         DC    CL3'422',CL6'TELA',CL6'TELA'                                     
         DC    CL3'424',CL6'TELA',CL6'TELA'                                     
         DC    CL3'425',CL6'TELA',CL6'TELA'                                     
         DC    CL3'428',CL6'8351',CL6'9548'                                     
         DC    CL3'429',CL6'8351',CL6'9548'                                     
         DC    CL3'431',CL6'8351',CL6'9548'                                     
         DC    CL3'432',CL6'8351',CL6'9548'                                     
         DC    CL3'441',CL6'DWFL',CL6'DWFL'                                     
         DC    CL3'442',CL6'4937',CL6'9077'                                     
         DC    CL3'443',CL6'4937',CL6'9077'                                     
         DC    CL3'444',CL6'4937',CL6'4937'                                     
         DC    CL3'444',CL6'2071',CL6'2071'                                     
         DC    CL3'445',CL6'DWNY',CL6'DWNY'                                     
         DC    CL3'448',CL6'DWNY',CL6'DWNY'                                     
         DC    CL3'449',CL6'DWNY',CL6'DWNY'                                     
         DC    CL3'450',CL6'0138',CL6'0138'                                     
         DC    CL3'456',CL6'0138',CL6'0138'                                     
         DC    CL3'457',CL6'0138',CL6'0138'                                     
         DC    CL3'464',CL6'0138',CL6'0138'                                     
         DC    CL3'065',CL6'0138',CL6'0138'                                     
         DC    CL3'088',CL6'0138',CL6'0138'                                     
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        SPECS                                                                  
*                                                                               
MYSPECS  DS    0H                                                               
         SPROG 0,1,2                                                            
         SSPEC H1,2,RUN                                                         
         SSPEC H1,99,REPORT                                                     
         SSPEC H1,119,PAGE                                                      
         SSPEC H2,99,REQUESTOR                                                  
         SSPEC H1,57,C'JOB REPORT'                                              
         SSPEC H2,57,10X'BF'                                                    
         SSPEC H4,2,C'DATE'                                                     
*                                                                               
         SPROG 1                                                                
         SSPEC H3,2,C'AGENCY'                                                   
         SSPEC H7,2,C'CLIENT PRODUCT JOBS'                                      
*                                                                               
         SPROG 2                                                                
         SSPEC H3,2,C'J WALTER'                                                 
         SSPEC H7,2,C'BUNIT PROJECT CUSTID PRD'                                 
*                                                                               
         SPROG 3                                                                
         SSPEC H3,2,C'BBDO'                                                     
         SSPEC H7,2,C'ProjectID'                                                
*                                                                               
         DC    X'00'                                                            
*                                                                               
         DC    C'**JOB**'                                                       
JOBREC   DS    10000C              LOGICAL HOLD RECORD AREA                     
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO ADD/WRITE BACK A SINGLE RECORD                        
*---------------------------------------------------------------------          
DOADD    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 MYTRACE,DMCB,=C'RECORD',AIO,0                                    
*                                                                               
         L     R3,AIO              MOVE KEY FROM RECORD TO KEY                  
         USING TLJBD,R3            R3=A(A RECORD TO BE WRITTEN)                 
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLJBKEY),TLJBKEY                                           
         GOTO1 HIGH                SEE IF RECORD ALREADY ON FILE                
         CLC   KEY(L'TLJBKEY),KEYSAVE IS RECORD ALREADY ON FILE                 
         BE    DOADD30                                                          
*                                                                               
         MVC   KEY,KEYSAVE         NO, SO RESTORE SAVED KEY                     
         GOTO1 ADDREC              AND ADD THE RECORD                           
         B     DOADDX                                                           
*                                                                               
DOADD30  MVC   AIO,AIO1            RECORD EXISTS - READ IT INTO IO1             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO2            RESET AIO TO A(NEW RECORD)                   
         GOTO1 PUTREC              WRITE BACK NEW FILE RECORD                   
*                                                                               
DOADDX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO ADD/WRITE BACK A SINGLE RECORD                        
*---------------------------------------------------------------------          
BLDJWTJB NTR1  BASE=*,LABEL=*                                                   
         LA    R5,DATAREC                                                       
         USING JWTRECD,R5                                                       
*                                                                               
         MVC   AIO,AIO2                                                         
         USING TLJBD,R3                                                         
         L     R3,AIO2             SET KEY IN RECORD                            
         XC    TLJBKEY,TLJBKEY                                                  
         MVI   TLJBCD,TLJBCDQ                                                   
         MVI   TLJBSPCL,TLJBSPJW   SPECIAL JOB FOR JWT                          
         MVC   TLJBPRJI,JWTPRJID   PROJECT ID                                   
         MVC   TLJBDATE,TGNXTBUS   DATE VALID                                   
         XC    TLJBDATE,=X'FFFFFF' COMPLEMENT                                   
         MVC   TLJBBUNT,JWTBUS     BUSINESS UNIT                                
         MVC   TLJBCSTI,JWTCSTID   CUSTOMER ID                                  
         MVC   TLJBPRDI,JWTPROD    PRODUCT                                      
         MVC   TLJBLEN,DATADISP    SET START LENGTH                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(TLJBDATE-TLJBD),0(R3)                                        
         GOTO1 HIGH                                                             
         CLC   KEY(TLJBDATE-TLJBD),0(R3)     THIS PROJECT ID EXIST?             
         BNE   BLDJW30                       NO, ADD IT                         
         LA    R3,KEY              CHECK IF THE OTHER FIELDS ARE SAME           
         CLC   TLJBBUNT,JWTBUS     BUSINESS UNIT                                
         BNE   BLDJW30                                                          
         CLC   TLJBCSTI,JWTCSTID   CUSTOMER ID                                  
         BNE   BLDJW30                                                          
         CLC   TLJBPRDI,JWTPROD    PRODUCT                                      
         BNE   BLDJW30                                                          
*                                                                               
         MVC   DYOLD25X,TGNXTBUS                                                
         GOTO1 DATCON,DMCB,(1,DYOLD25X),(0,DYOLD25)                             
         GOTO1 ADDAY,DMCB,DYOLD25,WORK,-25                                      
         MVC   DYOLD25,WORK                                                     
         GOTO1 DATCON,DMCB,(0,DYOLD25),(1,DYOLD25X)                             
         MVC   WORK(3),DYOLD25X                                                 
         XC    WORK(3),=X'FFFFFF'                                               
         CLC   TLJBDATE,WORK       IF 25 OR MORE DAYS OLD, ADD IT               
         BNL   BLDJW30                                                          
         B     BLDJWNO             ALL THE SAME, LEAVE                          
*                                                                               
BLDJW30  BRAS  RE,DOADD                                                         
         AP    RECCNT,=P'1'        ADD TO RECORD COUNT                          
*                                                                               
BLDJWYES SR    RC,RC               SET CONDITION CODE                           
BLDJWNO  LTR   RC,RC                                                            
BLDJWTJX XIT1                                                                   
*                                                                               
         DROP  R3,R5                                                            
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE TO ADD/WRITE BACK A SINGLE RECORD                        
*---------------------------------------------------------------------          
BLDBBDJB NTR1  BASE=*,LABEL=*                                                   
         LA    R5,DATAREC                                                       
         USING BBDORECD,R5                                                      
*                                                                               
         MVC   AIO,AIO2                                                         
         USING TLJBD,R3                                                         
         L     R3,AIO2             SET KEY IN RECORD                            
         XC    TLJBKEY,TLJBKEY                                                  
         MVI   TLJBCD,TLJBCDQ                                                   
         MVI   TLJBSPCL,TLJBSPBD   SPECIAL JOB FOR BBDO                         
         MVC   TLJBPROJ,BBDOPROJ   PROJECT ID                                   
         MVC   TLJBDAT,TGNXTBUS    DATE VALID                                   
         XC    TLJBDAT,=X'FFFFFF'  COMPLEMENT                                   
         MVC   TLJBLEN,DATADISP    SET START LENGTH                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(TLJBDAT-TLJBD),0(R3)                                         
         GOTO1 HIGH                                                             
         CLC   KEY(TLJBDAT-TLJBD),0(R3)     THIS PROJECT ID EXIST?              
         BNE   BLDBD30                       NO, ADD IT                         
*                                                                               
         MVC   DYOLD25X,TGNXTBUS                                                
         GOTO1 DATCON,DMCB,(1,DYOLD25X),(0,DYOLD25)                             
         GOTO1 ADDAY,DMCB,DYOLD25,WORK,-25                                      
         MVC   DYOLD25,WORK                                                     
         GOTO1 DATCON,DMCB,(0,DYOLD25),(1,DYOLD25X)                             
         MVC   WORK(3),DYOLD25X                                                 
         XC    WORK(3),=X'FFFFFF'                                               
         CLC   TLJBDAT,WORK        IF 25 OR MORE DAYS OLD, ADD IT               
         BNL   BLDBD30                                                          
         B     BLDBDNO             ALL THE SAME, LEAVE                          
*                                                                               
BLDBD30  BRAS  RE,DOADD                                                         
         AP    RECCNT,=P'1'        ADD TO RECORD COUNT                          
*                                                                               
BLDBDYES SR    RC,RC               SET CONDITION CODE                           
BLDBDNO  LTR   RC,RC                                                            
BLDBDTJX XIT1                                                                   
*                                                                               
         DROP  R3,R5                                                            
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
*              ROUTINE DELETES SUBSEQUENT JOB RECORDS                           
*---------------------------------------------------------------------          
DELETE   NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIO              R3=A(FILE RECORD)                            
         USING TLJBD,R3                                                         
*                                                                               
DEL2     GOTO1 HIGH                RE-READ RECORD WE JUST WROTE                 
         NI    DMINBTS,X'F7'       TURN OFF READ DELETED                        
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 SEQ                 GET NEXT                                     
*                                                                               
         CLC   KEY(TLJBSEQ-TLJBD),KEYSAVE  TEST STILL SAME RECORD               
         BNE   DELX                                                             
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC              GET THE RECORD                               
         OI    TLJBSTAT,X'80'      MARK IT DELETED                              
         GOTO1 PUTREC              AND WRITE IT BACK                            
*                                                                               
         OI    KEY+TLDRSTAT-TLDRD,X'80'  MARK DIRECTORY DELETED                 
         GOTO1 WRITE                     AND WRITE IT BACK                      
         OI    DMINBTS,X'08'       SET TO READ DELETED                          
         B     DEL2                LOOK FOR MORE                                
*                                                                               
DELX     XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  R3                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*              SET INFO FOR TRACE ROUTINE                                       
*---------------------------------------------------------------------          
MYTRACE  NTR1  BASE=*,LABEL=*                                                   
         TM    JBOPTS,JBTRACE      IF TRACE ON                                  
         BZ    MYTRACEX                                                         
         LM    R2,R3,0(R1)         R2=A(LITERAL), R3=A(I/O AREA)                
         ZIC   R4,0(R1)            R4=L'LITERAL                                 
         GOTO1 TRACE,DMCB,(R3),0,(R2),(R4)                                      
MYTRACEX XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
*                                                                               
TJBD     DSECT                                                                  
         DS    0A                                                               
AJOBREC  DS    A                   A(NEW RECORD)                                
*                                                                               
RECCNT   DS    PL4                 COUNTER OF JOB RECORDS ADDED                 
ERRCNT   DS    PL4                 COUNTER OF ERRORS                            
JBOPTS   DS    XL1                                                              
JBTRACE  EQU   X'80'               TRACE RECORDS                                
JBSAAT   EQU   X'40'               SAATCHI OPTION                               
JBJWT    EQU   X'20'               JWT OPTION                                   
JBBBDO   EQU   X'10'               BBDO OPTION                                  
*                                                                               
PROCDATA DS    CL1                 FLAG SAYS OKAY TO PROCESS DATA               
JOBCONT  DS    CL1                 CONTINUE JOB UNSCAN                          
DYOLD25  DS    CL6                 25 DAYS AGO FROM NEXTBDAY                    
DYOLD25X DS    XL3                 25 DAYS AGO FROM NEXTBDAY HEX                
*                                                                               
SVSRTKEY DS    0CL(SRTKLNQ)                                                     
SVAGY    DS    CL(L'SRTAGY)        SAVED SORT AGENCY                            
SVCLI    DS    CL(L'SRTCLI)        SAVED SORT CLIENT                            
SVPRD    DS    CL(L'SRTPRD)        SAVED SORT PRODUCT                           
*                                                                               
         DS    0D                                                               
DATAREC  DS    CL(DATARLNQ)        DATA RECORD                                  
DATARECS DS    CL(SAATRLNQ)        SAATCHI DATA RECORD                          
*                                                                               
         DS    0D                  SORT RECORD                                  
SORTREC  DS    CL(SRTRLNQ)                                                      
         EJECT                                                                  
*              DSECT TO COVER INPUT DATA                                        
         SPACE                                                                  
DATARECD DSECT                                                                  
DATACLI  DS    CL4                 CLIENT                                       
DATAPRD  DS    CL2                 PRODUCT                                      
DATAJOB  DS    CL3                 JOB                                          
DATAAGY  DS    CL4                 AGENCY                                       
DATARLNQ EQU   *-DATARECD                                                       
         SPACE 2                                                                
*              DSECT TO COVER SAATCHI INPUT DATA                                
         SPACE                                                                  
SAATRECD DSECT                                                                  
SAATBUS  DS    CL3                 BUSINESS UNIT                                
         DS    CL3                                                              
SAATCLI  DS    CL3                 CLIENT                                       
SAATPRD  DS    CL3                 PRODUCT                                      
SAATJOB  DS    CL6                 JOB                                          
SAATDAT  DS    CL8                 MIN EFFECTIVE DATE                           
SAATSTA  DS    CL1                 STATUS                                       
SAATTYP  DS    CL3                 PROJECT TYPE                                 
SAATRLNQ EQU   *-SAATRECD                                                       
*                                                                               
*              DSECT TO COVER JWT INPUT DATA                                    
         SPACE                                                                  
JWTRECD  DSECT                                                                  
JWTBUS   DS    CL5                 BUSINESS UNIT                                
JWTPRJID DS    CL7                 PROJECT ID                                   
         DS    CL8                                                              
JWTCSTID DS    CL6                 COSTUMER ID                                  
         DS    CL9                                                              
JWTPROD  DS    CL3                 PRODUCT                                      
JWTRLNQ  EQU   *-JWTRECD                                                        
         SPACE 2                                                                
*                                                                               
*              DSECT TO COVER BBDO INPUT DATA                                   
         SPACE                                                                  
BBDORECD DSECT                                                                  
BBDOPROJ DS    CL16                PROJECT ID                                   
BBDOLNQ  EQU   *-BBDORECD                                                       
         SPACE 2                                                                
*              DSECT TO COVER SORTREC                                           
         SPACE                                                                  
SORTRECD DSECT                                                                  
SRTAGY   DS    CL6                 AGENCY                                       
SRTCLI   DS    CL6                 CLIENT                                       
SRTPRD   DS    CL6                 PRODUCT                                      
SRTKLNQ  EQU   *-SORTRECD                                                       
SRTBUS   DS    CL3                 BUSINESS UNIT (SAATCHI)                      
SRTJOB   DS    CL6                 JOB                                          
SRTRLNQ  EQU   *-SORTRECD                                                       
         SPACE 2                                                                
*              DSECT TO COVER AGENCY TABLE FOR SAATCHI AGENCIES                 
         SPACE 3                                                                
AGYTABD  DSECT                                                                  
AGYBUS   DS    CL3               BUSINESS UNIT                                  
AGYAGY   DS    CL6               AGENCY                                         
AGYPAGY  DS    CL6               PRINT AGENCY                                   
AGYLNQ   EQU   *-AGYTABD                                                        
         SPACE 2                                                                
*              DSECT TO COVER PRINT LINE                                        
         SPACE                                                                  
PRNTD    DSECT                                                                  
         DS    CL1                                                              
PRTCLI   DS    CL6                 CLIENT                                       
         DS    CL1                                                              
PRTPRD   DS    CL6                 PRODUCT                                      
         DS    CL2                                                              
PRTJOBS  DS    0CL80               JOBS                                         
*                                                                               
         ORG   PRTCLI                                                           
PRJBUNIT DS    CL5                 JWT BUSINESS UNIT                            
         DS    CL1                                                              
PRJPRJID DS    CL7                 JWT PROJECT ID                               
         DS    CL1                                                              
PRJCSTID DS    CL6                 JWT CUST ID                                  
         DS    CL1                                                              
PRJPRD   DS    CL3                 JWT PRODUCT                                  
*                                                                               
         ORG   PRTCLI                                                           
PRBPRJID DS    CL16                BBDO PROJECT ID                              
         EJECT                                                                  
*TAREPFFD                                                                       
*TAREPE2D                                                                       
*DDGENTWA                                                                       
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*TAGENFILE                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*TAREPWORKD                                                                     
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         ORG CONTAGH                                                            
       ++INCLUDE TAREPE2D                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035TAREP53   03/05/16'                                      
         END                                                                    
