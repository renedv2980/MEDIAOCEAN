*          DATA SET RESFM1D    AT LEVEL 229 AS OF 02/11/03                      
*PHASE T8181DA,*                                                                
         TITLE 'T8181D - RESFM1D - BUY TEMPLATE LIST/MAINT'                     
*********************************************************************           
*                                                                   *           
*        RESFM1D (T8181D) --- BUY TEMPLATE LIST/MAINT               *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* JUN07/01 (RHV) --- VOILA!                                         *           
* FEB11/03 (HQ ) --- PUTREC INSTEAD OF DUMP ON ADDREC DUPLICATE KEY *           
*                                                                   *           
*                    *****  END TOMBSTONE  *****                    *           
*********************************************************************           
*                                                                               
T8181D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**181D**,RR=R3                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         SPACE                                                                  
*                                                                               
         LA    RF,HHOOK                                                         
         ST    RF,HEADHOOK                                                      
         LA    RF,HEDSPECS                                                      
         ST    RF,SPECS                                                         
*                                                                               
         OI    GENSTAT1,OKADDEL                                                 
         OI    GENSTAT4,CONFDEL                                                 
*                                                                               
         GOTO1 (RFGETTAB,REPFACS),DMCB,('RTTMPFLD',0)                           
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         MVC   AFLDTAB,0(R1)       SAVE A(FIELD TABLE)                          
*                                                                               
         OI    CONSERVH+6,X'81'                                                 
*                                                                               
         CLI   MODE,NEWSCR         NEW SCREEN LOADED                            
         BE    NSCR                                                             
*                                                                               
         CLI   ACTNUM,ACTDEL       DEL ACTION?                                  
         BE    MAIN020                                                          
         CLI   ACTNUM,ACTSCOPY     COPY ACTION?                                 
         BE    MAIN020                                                          
         CLI   ACTNUM,ACTADD       ADD ACTION?                                  
         BNE   MAIN030                                                          
MAIN020  CLI   BTMSUB,C'B'                                                      
         BE    MAIN030                                                          
         CLI   TWASCR,X'9E'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =A(SCRN9B),RR=Y     MUST USE 9B SCREEN                           
*                                                                               
MAIN030  DS    0H                                                               
         CLI   ACTNUM,ACTSEL       SELECT ACTION NOW?                           
         BNE   MAIN050                                                          
         OI    GENSTAT2,RETEQSEL                                                
         CLI   PFKEY,12                                                         
         BNE   MAIN050                                                          
         NI    GENSTAT2,X'FF'-RETEQSEL                                          
         OI    GENSTAT2,NEXTSEL                                                 
         B     EXIT                                                             
*                                                                               
MAIN050  DS    0H                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    DEL                                                              
         CLI   MODE,PRINTREP       REPORT?                                      
         BE    PREP                                                             
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
*              NEW SCREEN LOADED                               *                
****************************************************************                
NSCR     DS    0H                                                               
         CLI   TWASCR,X'9E'                                                     
         BNE   EXITOK                                                           
         GOTO1 =A(SCRN9B),RR=Y     DEFAULT SUBSCREEN                            
         B     EXITOK                                                           
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
VKEY     DS    0H                                                               
         CLI   ACTNUM,ACTLIST      LIST ACTION?                                 
         BE    VKEYX                                                            
         XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING RTMPREC,R4                                                       
         MVC   RTMPKTYP,=X'1507'                                                
         MVC   RTMPKREP,AGENCY                                                  
         MVI   RTMPKLIN,0                                                       
*                                                                               
         LA    R2,BTMNAMH                                                       
         CLI   5(R2),0                                                          
         BNE   VKEY030                                                          
         MVI   ERROR,MISSING                                                    
         B     ERREND                                                           
VKEY030  DS    0H                                                               
         MVC   RTMPKTMP,8(R2)                                                   
         OC    RTMPKTMP,SPACES                                                  
*                                                                               
         CLI   ACTNUM,ACTSCOPY                                                  
         BE    VKEY050                                                          
         XC    COPYKEY,COPYKEY                                                  
         B     VKEYX                                                            
*                                                                               
VKEY050  DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY                       
         OC    COPYKEY,COPYKEY     HAVE A COPY SOURCE?                          
         BNZ   VKEY060                                                          
         MVI   ERROR,NOTFOUND                                                   
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         MVC   COPYKEY,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFIL',KEY+28,AIO,DMWORK             
         CLI   8(R1),0                                                          
         BE    VKEYX                                                            
         DC    H'0'                                                             
*                                                                               
VKEY060  DS    0H                  ALREADY HAVE COPY SOURCE                     
         MVI   ERROR,DUPLICAT                                                   
         CLC   KEY(27),KEYSAVE         KEY IS TARGET                            
         BE    ERREND              MUST NOT BE ON FILE                          
         MVC   KEY,KEYSAVE                                                      
*                                                                               
VKEYX    DS    0H                                                               
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE RECORD ROUTINE                                *         
***********************************************************************         
* NOTE: THIS ROUTINE HANDLES THE VALIDATION OF BOTH THE 9B/9C SCREENS *         
*       FIRST WE HANDLE THE GENERIC STUFF COMMON TO BOTH SCREENS,     *         
*       THEN WE DIRECT TO THE APPROPRIATE SECTION FOR EITHER SCREEN   *         
***********************************************************************         
***********************************************************************         
VREC     DS    0H                                                               
         CLI   ACTNUM,ACTSCOPY    COPY ACTION?                                  
         BNE   VREC010                                                          
         CLC   COPYKEY,KEY        CURRENT KEY IS DEST KEY?                      
         BE    DREC               THIS IS 1ST PASS DISPLAY CALL                 
         B     VREC020                                                          
*                                                                               
VREC010  DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VREC020                                                          
         CLI   PFKEY,2                                                          
         BE    DREC                                                             
*                                                                               
VREC020  DS    0H                                                               
         L     R5,AIO1             A(HEADER REC)                                
         USING RTMPREC,R5                                                       
*                                                                               
         XC    ATIMFLD,ATIMFLD                                                  
         XC    ADAYFLD,ADAYFLD                                                  
*                                                                               
         CLI   ACTNUM,ACTSCOPY                                                  
         BE    VREC030                                                          
         CLI   ACTNUM,ACTADD       ADD ACTION?                                  
         BNE   VREC050             NO                                           
VREC030  XC    RTMPREC(35+RTMPHLQ),RTMPREC                                      
         MVC   RTMPKEY,KEY         YES - BUILD NEW RECORD                       
         MVI   RTMPLEN+1,35+RTMPHLQ                                             
*                                                                               
         MVI   RTMPHCD,X'01'                                                    
         MVI   RTMPHLN,RTMPHLQ                                                  
         MVI   RTMPHFLG,0                                                       
*                                                                               
* GENERIC HEADER VALIDATION                                                     
*                                                                               
VREC050  DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,RTMPHUPD)                                   
*                                                                               
         LA    R2,BTMACVH          'ACTIVE' FIELD                               
         MVI   ERROR,INVALID                                                    
         NI    RTMPHFLG,X'FF'-X'80'                                             
         CLI   8(R2),C'Y'                                                       
         BE    VREC060                                                          
         CLI   8(R2),C'N'                                                       
         BNE   ERREND                                                           
         OI    RTMPHFLG,X'80'                                                   
*                                                                               
VREC060  DS    0H                                                               
         LA    R2,BTMDSCH          'DESC' FIELD                                 
         MVC   RTMPHDSC,8(R2)                                                   
*                                                                               
* DIRECT TO TEMPLATE HEADER/DETAIL VALREC                                       
*                                                                               
VREC100  DS    0H                                                               
         CLI   BTMSUB,C'B'         HEADER SCREEN?                               
         BE    VR200                                                            
         CLI   BTMSUB,C'C'         DETAIL SCREEN?                               
         BE    VR500                                                            
         DC    H'0'                                                             
*                                                                               
* TEMPLATE HEADER VALIDATE                                                      
*                                                                               
VR200    DS    0H                                                               
         GOTO1 =A(VALNUM),RR=Y     VALIDATE LINES FIELD & TABLE TARGETS         
*                                                                               
         L     R4,AFLDTAB                                                       
         MVC   HALF,0(R4)          TABLE ENTRY LENGTH                           
         LA    R4,4(R4)            START OF TABLE                               
*                                                                               
         LA    R2,BTMFLD1H         1ST SCREEN FIELD                             
*                                                                               
VR210    DS    0H                                                               
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    VR360               YES                                          
*                                                                               
         CLI   0(R4),0             NUM FIELD                                    
         BNE   VR215                                                            
         AH    R4,HALF                                                          
         B     VR210                                                            
*                                                                               
VR215    DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               T/C FIELD                                    
         GOTO1 =A(VFLD),DMCB,(0(R4),AIO),(R4),RR=Y VALIDATE THIS FIELD          
*                                                                               
         ZIC   R1,0(R2)            DATA FIELD                                   
         AR    R2,R1                                                            
         ZIC   R1,0(R2)            NEXT LABEL                                   
         AR    R2,R1                                                            
         AH    R4,HALF             NEXT TABLE ENTRY                             
         B     VR210                                                            
         DROP  R5                                                               
*                                                                               
VR360    DS    0H                  NOW APPLY BULK CHANGES TO LINES              
         LA    R5,LINAREA          LIST OF LINES TO CHANGE                      
         USING LINTAB,R5                                                        
VR370    DS    0H                                                               
         CLI   0(R5),X'FF'                                                      
         BE    VR420               DONE WITH LINES                              
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'REPFILE',LINDA,AIO3, +        
               DMWORK                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   ACTNUM,ACTSCOPY     COPY                                         
         BNE   VR375                                                            
         L     RF,AIO3             SOURCE LINE                                  
         L     RE,AIO1             TARGET HEADER                                
         MVC   0(26,RF),0(RE)      REPLACE KEY EXCEPT #                         
         GOTOR MYPUTREC,DMCB,AIO3,AIO2                                          
         B     VR415                                                            
*        GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'REPFILE',LINDA,AIO3,DMWORK            
*        CLI   8(R1),0                                                          
*        BE    VR415                                                            
*        DC    H'0'                                                             
*                                                                               
VR375    DS    0H                                                               
         L     R4,AFLDTAB          FIELD TABLE                                  
         MVC   HALF,0(R4)                                                       
         LA    R4,4(R4)                                                         
*                                                                               
VR380    DS    0H                                                               
         CLI   0(R4),X'FF'         DONE                                         
         BE    VR410               YES                                          
         CLI   0(R4),0             NUM FIELD?                                   
         BE    VR400               YES - SKIP                                   
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',=C'REPFIL'),(X'20',AIO1),(1,0(R4)),0            
         CLI   12(R1),0                                                         
         BNE   VR400               NO CHANGE TO THIS FIELD                      
         L     R3,12(R1)           A(ELEM)                                      
         CLI   1(R3),RTMPFOLQ                                                   
         BNH   VR400               DON'T CONSIDER EMPTY ELEMS                   
         GOTO1 HELLO,DMCB,(C'D',=C'REPFIL'),(X'20',AIO3),(1,0(R4)),0            
         CLC   =C'*CLEAR',RTMPFOLQ(R3)  SPECIAL CASE?                           
         BE    VR400               DON'T REWRITE ANY ELEM                       
         ZIC   R1,1(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   ELEM(0),0(R3)                                                    
         NI    ELEM+(RTMPFFLG-RTMPFLD),X'FF'-X'80'                              
         GOTO1 HELLO,DMCB,(C'P',=C'REPFIL'),AIO3,ELEM,0                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR400    DS    0H                                                               
         AH    R4,HALF             NEXT FIELD                                   
         B     VR380                                                            
*                                                                               
VR410    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'REPFIL',FULL,AIO3,DMWORK              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR415    DS    0H                                                               
         LA    R5,L'LINTAB(R5)                                                  
         B     VR370               NEXT LINE TO UPDATE                          
         DROP  R5                                                               
*                                                                               
VR420    DS    0H                  RESTORE GENCON GET-PUT SEQ                   
         CLI   ACTNUM,ACTADD       ADD ACTION?                                  
         BE    VR450               NO REC TO RESTORE YET                        
         CLI   ACTNUM,ACTSCOPY     COPY ACTION?                                 
         BE    VR450               NO REC TO RESTORE YET                        
         L     R1,AIO1                                                          
         MVC   KEY(27),0(R1)                                                    
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'REPDIR',KEY,KEY,0                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'REPFILE',KEY+28,AIO2,+        
               DMWORK                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR450    DS    0H                                                               
         CLI   ACTNUM,ACTSCOPY     COPY ACTION?                                 
         BNE   VR470               IF SO, WE HANDLE I/O                         
         GOTOR MYPUTREC,DMCB,AIO1,AIO2                                          
*        GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'REPFILE',KEY+28,AIO1,DMWORK           
*        CLI   8(R1),0                                                          
*        BE    VR470                                                            
*        DC    H'0'                                                             
*                                                                               
VR470    DS    0H                                                               
         CLI   ACTNUM,ACTADD       ADD ACTION?                                  
         BNE   VRX                                                              
         MVC   CONACT,=CL8'CHANGE' SWITCH TO CHANGE FOR FURTHER EDITS           
         OI    CONACTH+6,X'80'                                                  
         B     VRX                                                              
*                                                                               
* TEMPLATE DETAIL VALIDATE                                                      
*                                                                               
* HERE'S HOW THIS WORKS: LINTAB IS PREBUILT WITH D/A OF EACH LINE IN            
* TEMPLATE AND LINOLD=LINNEW. WE FIRST LOOP THRU THE SCREEN AND APPLY           
* ALL DATA CHANGES TO ACTUAL RECORDS, AND UPDATE THE LINNEW SETTING OF          
* EACH TABLE ENTRY AS WE DETECT THE NEED FOR RECORD MOVING/RENUMBERING          
* AFTER WE'VE VALIDATE AND CHANGED ALL NECESSARY LINES, WE LOOP THRU            
* LINTAB AGAIN TO MOVE/ADD/DELETE ANY RECORDS AS DIRECTED BY THEIR              
* NEWNUM.                                                                       
*                                                                               
VR500    DS    0H                                                               
         ZIC   R3,LINNUM           # OF TEMPLATES LINES SCREEN                  
         L     R2,S9CSTART                                                      
         AR    R2,RA                                                            
         LA    R4,LINAREA                                                       
         USING LINTAB,R4                                                        
*                                                                               
VR520    DS    0H                                                               
         CLI   0(R4),X'FF'         NO MORE RECORDS IN TABLE                     
         BE    VR550                                                            
         TM    LINSTAT,X'80'       ON SCREEN?                                   
         BO    VR550                                                            
         LA    R4,L'LINTAB(R4)                                                  
         B     VR520                                                            
VR550    DS    0H                                                               
         GOTO1 =A(VALLIN),RR=Y                                                  
*                                                                               
         CLI   0(R4),X'FF'                                                      
         BE    *+8                                                              
         LA    R4,L'LINTAB(R4)     NEXT LINE IN TABLE                           
         AH    R2,LINLEN           NEXT SELECT FIELD                            
         BCT   R3,VR520                                                         
         B     VR600                                                            
*                                                                               
VR600    DS    0H                  PROCESS RECORD RE-ARRANGING                  
         GOTO1 =A(RECUPD),RR=Y                                                  
*                                                                               
VRX      B     DREC                REDISPLAY RECORD                             
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
****************************************************************                
*                                                                               
         EJECT                                                                  
****************************************************************                
*              DISPLAY KEY ROUTINE                             *                
****************************************************************                
DKEY     DS    0H                                                               
         L     R4,AIO              A(HEADER REC)                                
         USING RTMPREC,R4                                                       
*                                                                               
         LA    R2,BTMNAMH                                                       
         MVC   8(L'BTMNAM,R2),RTMPKTMP                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
DREC     DS    0H                                                               
         GOTO1 =A(DREC00),RR=Y                                                  
         B     EXITOK                                                           
*                                                                               
PREP     DS    0H                                                               
         GOTO1 =A(PREP00),RR=Y                                                  
         B     DREC                                                             
*                                                                               
***********************************************************************         
* DELETE ALL LINE RECORDS ASSOCIATED WITH A TEMPLATE                            
***********************************************************************         
DEL      DS    0H                                                               
         L     R6,AIO1                                                          
         MVC   KEY(27),0(R6)                                                    
         MVI   KEY+26,1                                                         
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMRDHI'),=C'REPDIR',KEY,KEY,0             
         B     DEL30                                                            
DEL20    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMRSEQ'),=C'REPDIR',KEY,KEY,0             
DEL30    DS    0H                                                               
         CLC   KEY(26),KEYSAVE                                                  
         BNE   DEL50                                                            
         OI    KEY+27,X'80'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'REPDIR',0,KEY,0                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'REPFIL',KEY+28,AIO3, +        
               DMWORK                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO3                                                          
         OI    29(R1),X'80'                                                     
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'REPFIL',KEY+28,AIO3,DMWORK            
         CLI   8(R1),0                                                          
         BE    DEL20                                                            
         DC    H'0'                                                             
DEL50    DS    0H                  RESTORE GENCON GET-PUT SEQ                   
         L     R1,AIO1                                                          
         MVC   KEY(27),0(R1)                                                    
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'REPDIR',KEY,KEY,0                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'REPFILE',KEY+28,AIO2,+        
               DMWORK                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
****************************************************************                
*                        LIST AND PRINT ROUTINE                *                
****************************************************************                
LIST     DS    0H                                                               
         OC    KEY(27),KEY                                                      
         BNZ   LR05                                                             
         XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING RTMPREC,R4                                                       
         MVC   RTMPKTYP,=X'1507'                                                
         MVC   RTMPKREP,AGENCY                                                  
         MVC   RTMPKTMP,BTMNAM                                                  
         MVI   RTMPKLIN,0                                                       
         DROP  R4                                                               
LR05     GOTO1 HIGH                                                             
         B     LR15                                                             
LR10     GOTO1 SEQ                                                              
LR15     CLC   KEY(14),KEYSAVE     CORRECT REP                                  
         BNE   LRX                                                              
         LA    R4,KEY                                                           
         USING RTMPREC,R4                                                       
         CLI   RTMPKLIN,0          HEADER REC?                                  
         BNE   LR10                                                             
*                                                                               
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LA    R2,LISTAR                                                        
         USING LISTD,R2                                                         
         MVC   LISTAR,P                                                         
         MVC   LNAME,RTMPKTMP                                                   
         MVC   LDESC,RTMPHDSC                                                   
         GOTO1 DATCON,DMCB,(2,RTMPHUPD),(11,LUPDATE)                            
         MVI   LACTIVE,C'Y'                                                     
         TM    RTMPHFLG,X'80'                                                   
         BZ    *+8                                                              
         MVI   LACTIVE,C'N'                                                     
         GOTO1 LISTMON                                                          
*                                                                               
         LA    R4,KEY                                                           
         MVI   RTMPKLIN,X'FF'      SKIP TO NEXT HEADER                          
         B     LR05                                                             
*                                                                               
LRX      B     EXIT                                                             
         DROP  R2,R4                                                            
*                                                                               
***********************************************************************         
* HHOOK - HEADHOOK ROUTINE                                                      
***********************************************************************         
HHOOK    NTR1                                                                   
         L     R6,AIO                                                           
         USING RTMPREC,R6                                                       
         MVC   H4+10(L'RTMPKTMP),RTMPKTMP                                       
         MVC   H4+42(L'RTMPHDSC),RTMPHDSC                                       
         XIT1                                                                   
         DROP  R6                                                               
*                                                                               
***********************************************************************         
* HEADSPECS                                                                     
***********************************************************************         
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,1,AGYNAME                                                     
         PSPEC H2,1,AGYADD                                                      
         PSPEC H1,42,C'BUY TEMPLATE'                                            
         PSPEC H2,42,C'------------'                                            
         PSPEC H1,76,RUN                                                        
         PSPEC H1,103,PAGE                                                      
         PSPEC H2,76,REQUESTOR                                                  
         PSPEC H3,1,SPACES                                                      
         PSPEC H4,1,C'TEMPLATE:'                                                
         PSPEC H4,30,C'DESCRIPTION:'                                            
         PSPEC H5,1,SPACES                                                      
         DC    X'00'                                                            
***********************************************************************         
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
MYEND    MVC   CONHEAD(9),=C'* ERROR *'                                         
         MVI   ERROR,X'FE'         USING MY OWN ERROR MESSAGE                   
         MVI   GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
ERREND   GOTO1 ERREX                                                            
*                                                                               
DUPERR   MVC   RERROR,=AL2(554)    NEW CODE=OLD CODE ERROR                      
*                                                                               
MYERR    GOTO1 MYERROR                                                          
*                                                                               
UNWIND   DS    0H            CALL ERROR MSG & UNWIND TRANSACTION I/O            
         XC    DMCB,DMCB                                                        
         MVC   DMCB+2(2),RERROR                                                 
         GOTO1 GETTXT,DMCB,,0,(C'E',0),0,0,0                                    
         OI    6(R2),X'40'+X'80'   CURSOR & XMIT                                
         DC    H'0',C'$ABEND'      UNWIND THIS TRANSACTION                      
*                                                                               
RELO     DS    A                                                                
         SPACE 2                                                                
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,X'FF'             SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
EXIT     XIT1                                                                   
*                                                                               
ACTSCOPY EQU   9                                                                
         LTORG                                                                  
*                                                                               
LISTD    DSECT                                                                  
LNAME    DS    CL12                                                             
         DS    CL2                                                              
LDESC    DS    CL34                                                             
         DS    CL3                                                              
LUPDATE  DS    CL8                                                              
         DS    CL7                                                              
LACTIVE  DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RESFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFM9ED                                                       
         ORG   BTMLAST                                                          
       ++INCLUDE RESFM9BD                                                       
         ORG   BTMLAST                                                          
       ++INCLUDE RESFM9CD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFM9DD                                                       
         EJECT                                                                  
*                                                                               
* HERE IS A LITTLE TWA STORAGE AT THE END OF THE RESFM9C SCREEN AREA            
* FOR SAVING BETWEEN TRANSACTIONS ON 9C SCREEN                                  
* WARNING: KEEP THIS UNDER 100 BYTES                                            
*                                                                               
         ORG   CONHEADH+3404                                                    
LINSTART DS    X                   DISPLAY LINE START #                         
LINNEXT  DS    X                   START REC FOR NEXT PAGE                      
COPYKEY  DS    CL27                SOURCE COPY ACTION KEY                       
*                                                                               
         DSECT                                                                  
       ++INCLUDE REGENTMP                                                       
         EJECT                                                                  
       ++INCLUDE DDFH                                                           
         EJECT                                                                  
         DSECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         EJECT                                                                  
         ORG   SYSSPARE                                                         
*                                                                               
MYWORK   DS    0F                  **WORK AREA**                                
MYFLAGS  DS    X                                                                
AFLDTAB  DS    A                   A(FIELD TABLE FROM REPTABS)                  
ATIMFLD  DS    A                                                                
ADAYFLD  DS    A                                                                
LOCALREC DS    A                   A(CURRENT ROUTINE RECORD)                    
MYDA     DS    F                                                                
BLDSTART DS    F                   DISPL TO 1ST FLD ON REPORT LINE              
S9CSTART DS    F                   DISPL TO 1ST SEL FLD ON 9C SCREEN            
S9CPFK   DS    F                   DISPL TO PFKEY LINE ON 9C SCREEN             
SVLIN    DS    F                                                                
SVTAB    DS    F                                                                
REPCOUNT DS    X                   REPORT LINE COUNTER                          
LINLEN   DS    H                   DISPL LIN LEN (SEL TO SEL FIELD)             
MYCOL    DS    X                                                                
MYROW    DS    X                                                                
MYSTAT   DS    X                                                                
*              X'80'               HEADER BASED ADD IN PROGRESS                 
*                                                                               
SELACT   DS    C                   SELECT ACTION                                
SELNUM   DS    X                   SELECT NUMBER (REPEATS)                      
*                                                                               
LINELNQ  EQU   74                  DISPLAY LINE WIDTH                           
LINENUMQ EQU   15                  # OF DISPLAY LINES                           
LINSIZE  DS    X                   # OF SCREEN LINES OF EACH ENTRY              
LINNUM   DS    X                   # OF ENTRIES ON FORMATTED SCREEN             
LINDEF   DS    24XL1               TEMPLATE LINE DISPLAY DEFINITION             
*                                  MAX 24 1-BYTE FIELD CODES                    
*                                                                               
LINAREA  DS    257XL7              LINE PROCESSING TABLE AREA                   
                                                                                
*                                                                               
MYWORKX  EQU   *                                                                
MYWORKLQ EQU   *-MYWORK                                                         
*                                                                               
LINTAB   DS    0XL7                LINE PROCESSING TABLE                        
LINOLD   DS    X                   ORIGINAL LINE NUMBER                         
LINNEW   DS    X                   NEW LINE NUMBER                              
LINSTAT  DS    X                   X'80'=ON SCREEN, X'40'=PROCESSED             
LINDA    DS    XL4                 ORIGINAL LINE D/A                            
*                                                                               
T8181D   CSECT                                                                  
*****************************************************************               
* SCRN9B - LOAD & PREFORMAT BUYTMP REC 9B SCREEN                                
*****************************************************************               
SCRN9B   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 CALLOV,DMCB,(X'9B',BTMLAST),0  LOAD SUBSCREEN                    
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   BTMSUB,C'B'         REMEMBER WHAT'S LOADED                       
*                                                                               
         L     R4,AFLDTAB                                                       
         MVC   HALF,0(R4)          TABLE ENTRY LENGTH                           
         LA    R4,4(R4)            START OF TABLE                               
*                                                                               
         LA    R2,BTMFLD1H         1ST SCREEN FIELD                             
*                                                                               
S9B010   DS    0H                                                               
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    S9B050              YES                                          
*                                                                               
         CLI   0(R4),0             NUM FIELD NOT ON THIS SCREEN                 
         BNE   S9B020                                                           
         AH    R4,HALF                                                          
         B     S9B010                                                           
*                                                                               
S9B020   DS    0H                                                               
         MVC   8(8,R2),1(R4)     FIELD LABEL                                    
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               TO T/C FIELD                                 
         NI    1(R2),X'FF'-X'20'   UNPROTECT                                    
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               TO VALUE FIELD                               
         NI    1(R2),X'FF'-X'20'   UNPROTECT                                    
*                                                                               
         AH    R4,HALF             NEXT TABLE ENTRY                             
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               NEXT SCREEN FIELD                            
         B     S9B010                                                           
*                                                                               
S9B050   DS    0H                                                               
         XC    COPYKEY,COPYKEY                                                  
         XIT1                                                                   
         LTORG                                                                  
*****************************************************************               
* SCRN9C - LOAD & PREFORMAT BUYTMP REC 9C SCREEN                                
*****************************************************************               
*                                                                               
* NOTE: AIO MUST ADDRESS TEMPLATE HEADER REC!!                                  
*                                                                               
SCRN9C   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 CALLOV,DMCB,(X'9C',BTMLAST),0  LOAD SUBSCREEN                    
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   BTMSUB,C'C'         REMEMBER WHAT'S LOADED                       
*                                                                               
         XC    S9CSTART,S9CSTART   INITIALIZE                                   
         XC    LINLEN,LINLEN       INITIALIZE                                   
         MVI   LINNEXT,0                                                        
         MVI   LINSTART,0                                                       
*                                                                               
         L     R6,AIO                                                           
         CLC   =X'1507',0(R6)      JUST CHECKING!                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* LOOP THRU FIELD TABLE, OPTIMIZE FIELD ARRANGEMENT, BUILD DEFINITION           
*                                                                               
         L     R4,AFLDTAB                                                       
         MVC   HALF,0(R4)          TABLE ENTRY LENGTH                           
         LA    R4,4(R4)            START OF TABLE                               
*                                                                               
         LA    R2,LINDEF           DEFINITION TABLE                             
         MVI   0(R2),X'FF'         INITIALIZE                                   
*                                                                               
         MVI   LINSIZE,1           INIT SIZE OF ENTRY                           
*                                                                               
         LA    R3,LINELNQ          REMAINING ON LINE                            
*                                                                               
S9C100   DS    0H                  LOOP START                                   
         CLI   0(R4),X'FF'         END OF FIELD TABLE?                          
         BE    S9C200                                                           
*                                                                               
         GOTO1 =A(ADDDEF),RR=Y     ADD (R4)DEFINITION (UPDATE R3)               
         BE    S9C190              SUCESSFUL ADD                                
         LA    R3,LINELNQ          NEW LINE                                     
         ZIC   RE,LINSIZE                                                       
         LA    RE,1(RE)            INCREMENT SIZE OF ENTRY                      
         STC   RE,LINSIZE                                                       
         GOTO1 =A(ADDDEF),RR=Y     TRY AGAIN                                    
         BE    *+6                                                              
         DC    H'0'                CAN'T FAIL WITH FRESH LINE                   
*                                                                               
S9C190   DS    0H                                                               
         AH    R4,HALF             NEXT FIELD                                   
         B     S9C100              LOOP                                         
*                                                                               
* NOW BUILD SCREEN FIELDS                                                       
*                                                                               
S9C200   DS    0H                                                               
         SR    R2,R2                                                            
         LA    R3,LINENUMQ         LINES/SCREEN                                 
         LA    R3,1(R3)            SPACE BETWEEN ENTRIES FENCEPOST              
         ZIC   R1,LINSIZE          LINES/ENTRY                                  
         LA    R1,1(R1)            SPACE BETWEEN ENTRIES                        
         DR    R2,R1                                                            
         STC   R3,LINNUM           ENTRIES/SCREEN                               
*                                                                               
         MVI   MYROW,9             INIT STARTING DISPLAY ROW                    
*                                                                               
* BUILD THE SELECT FIELD                                                        
*                                                                               
S9C210   DS    0H                                                               
         MVI   MYCOL,2                                                          
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(1),MYROW                                                  
         MVC   DMCB+7(1),MYCOL                                                  
         GOTO1 =A(ADDFLD),DMCB,3,,RR=Y                                          
*                                                                               
         OC    S9CSTART,S9CSTART   1ST TIME ROUND?                              
         BNZ   S9C215              NO                                           
         LR    RF,R2               YES - SAVE DISPL OF 1ST FIELD                
         SR    RF,RA                                                            
         ST    RF,S9CSTART                                                      
         B     S9C218                                                           
S9C215   DS    0H                                                               
         OC    LINLEN,LINLEN       2ND TIME ROUND?                              
         BNZ   S9C218              NO                                           
         LR    RF,R2               YES - SAVE LEN OF LINE                       
         SR    RF,RA                                                            
         L     RE,S9CSTART                                                      
         SR    RF,RE                                                            
         STH   RF,LINLEN                                                        
*                                                                               
S9C218   DS    0H                                                               
         LA    R5,LINDEF           LINE DEFINITION TABLE                        
         MVI   MYCOL,6                                                          
S9C220   DS    0H                  BUILD SINGLE DISPLAY ENTRY                   
         CLI   0(R5),X'FF'         DONE?                                        
         BE    S9C280              YES                                          
*                                                                               
         L     R4,AFLDTAB          FIND FIELD DEFINITION ENTRY                  
         MVC   HALF,0(R4)                                                       
         LA    R4,4(R4)                                                         
         CLC   0(1,R5),0(R4)                                                    
         BE    *+18                                                             
         AH    R4,HALF                                                          
         CLI   0(R4),X'FF'                                                      
         BNE   *-18                                                             
         DC    H'0'                BAD FIELD EQU                                
*                                                                               
         LA    R1,0(R4)            FIELD LABEL-1                                
         LA    R2,9(R1)            MAX LABEL LEN                                
         BCTR  R2,0                                                             
         CLI   0(R2),C' '                                                       
         BE    *-6                                                              
         SR    R2,R1               LABEL LEN                                    
*                                                                               
         LA    R2,1(R2)            +SPACE                                       
*                                                                               
         ZIC   R1,17(R4)           FIELD LEN                                    
         AR    R2,R1                                                            
*                                                                               
         LA    RF,80                                                            
         ZIC   RE,MYCOL                                                         
         SR    RF,RE               ROOM LEFT ON LINE                            
*                                                                               
         CR    R2,RF                                                            
         BNH   S9C250              FITS                                         
*                                                                               
         ZIC   R1,MYROW                                                         
         LA    R1,1(R1)            NEXT ROW                                     
         STC   R1,MYROW                                                         
         MVI   MYCOL,6                                                          
*                                                                               
* WRITE FIELDS TO TWA                                                           
*                                                                               
S9C250   DS    0H                  ADD LABEL FIELD                              
         LA    R1,0(R4)            FIELD LABEL-1                                
         LA    R6,9(R1)            MAX LABEL LEN                                
         BCTR  R6,0                                                             
         CLI   0(R6),C' '                                                       
         BE    *-6                                                              
         SR    R6,R1               LABEL LEN                                    
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(1),MYROW                                                  
         MVC   DMCB+7(1),MYCOL                                                  
         GOTO1 =A(ADDFLD),DMCB,(R6),,RR=Y                                       
         OI    1(R2),X'20'         PROTECTED                                    
         ZIC   RE,MYCOL                                                         
         LA    RE,1(R6,RE)                                                      
         STC   RE,MYCOL            INCR COLUMN LABEL LEN + 1 SPACE              
         BCTR  R6,0                                                             
         EX    R6,*+4                                                           
         MVC   8(0,R2),1(R4)       LABEL TEXT                                   
*                                                                               
*                                  ADD DATA FIELD                               
         ZIC   R6,17(R4)           DATA LEN                                     
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(1),MYROW                                                  
         MVC   DMCB+7(1),MYCOL                                                  
         GOTO1 =A(ADDFLD),DMCB,(R6),,RR=Y                                       
         ZIC   RE,MYCOL                                                         
         AR    RE,R6                                                            
         CHI   RE,80                                                            
         BNL   *+8                                                              
         LA    RE,1(RE)            ALLOW 1 SPACE BEFORE NEXT FIELD              
         STC   RE,MYCOL            INCR COLUMN DATA LEN                         
*                                                                               
         LA    R5,1(R5)            NEXT FIELD                                   
         B     S9C220                                                           
*                                                                               
S9C280   DS    0H                  DONE INDIVIDUAL ENTRY                        
         ZIC   R1,MYROW                                                         
         LA    R1,2(R1)            NEXT ROW + 1 BLANK                           
         STC   R1,MYROW                                                         
         BCT   R3,S9C210           NEXT ENTRY DISPLAY                           
*                                                                               
         GOTO1 =A(ADDFLD),DMCB,60,(24,3),RR=Y                                   
         OI    1(R2),X'20'         PROTECT FIELD                                
         SR    R2,RA                                                            
         ST    R2,S9CPFK                                                        
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
***********************************************************************         
* ADDFLD - DYNAMICALLY ADD SCREEN FIELD TO TWA (DEFAULT ATTRIBUTES)             
***********************************************************************         
*                                                                               
*  P1 : LENGTH                                                                  
*                                                                               
*  P2 : BYTE 0 : ROW                                                            
*       BYTE 3 : COL                                                            
*                                                                               
*  ON RETURN R2=FLD HEADER                                                      
*                                                                               
ADDFLD   NTR1  BASE=*,LABEL=*                                                   
         L     R2,ATWA     FIND LAST FLD ON SCREEN                              
         LA    R2,64(R2)                                                        
ADDFLD1  DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    ADDFLD2                                                          
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     ADDFLD1                                                          
ADDFLD2  DS    0H                                                               
         USING FHD,R2              GENERATE NEW FIELD                           
         XC    0(8,R2),0(R2)                                                    
         ZIC   R3,4(R1)            ROW                                          
         ZIC   R4,7(R1)            COL                                          
         BCTR  R3,0                                                             
         BCTR  R4,0                                                             
         MH    R3,=H'80'                                                        
         AR    R3,R4                                                            
         STCM  R3,3,FHAD           SCREEN ADDRESS                               
         OI    FHOI,X'80'          XMIT                                         
         ZIC   R3,3(R1)            FIELD LEN                                    
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   FHDA(0),SPACES      BLANK FIELD                                  
         LA    R3,9(R3)            +HDR                                         
         STC   R3,FHLN                                                          
         AR    R3,R2                                                            
         MVC   0(3,R3),=X'000101'                                               
*                                                                               
         LA    RF,CONHEADH+3400                                                 
         CR    R3,RF                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
         XIT1  REGS=(R2)                                                        
         DROP  R2                                                               
         LTORG                                                                  
***********************************************************************         
* VALLIN - VALIDATE INDIVIDUAL TEMPLATE LINE                                    
***********************************************************************         
* R2= A("ACT" FIELD HEADER)                                                     
* R4= LINTAB ENTRY FOR CURRENT REC, OR X'FF' END OF TABLE FOR NEW FLD           
*                                                                               
* IO AREA USAGE SCHEME:                                                         
*    IO1 = ALREADY CONTAINS HEADER RECORD                                       
*    IO2 = OLD DETAIL RECORD (CHA,DEL)                                          
*    IO3 = NEW DETAIL RECORD (ADD,CHA)                                          
*                                                                               
***********************************************************************         
VALLIN   NTR1  BASE=*,LABEL=*                                                   
         NI    MYSTAT,X'FF'-X'80'  RESET THIS FLAG                              
*                                                                               
* VALIDATE ACTION FIELD                                                         
*                                                                               
         USING LINTAB,R4                                                        
         ST    R4,SVTAB                                                         
         ST    R2,SVLIN                                                         
*                                                                               
         CLI   5(R2),0                                                          
         BE    VLINX               NO ACTION ON THIS SELECTION                  
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),8(R2)                                                    
         OC    WORK,SPACES                                                      
         CLC   WORK(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                MUST HAVE INPUT                              
         LA    R1,WORK                                                          
         CLI   0(R1),C' '          STRIP PRECEEDING SPACES                      
         BNE   *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
*                                                                               
         MVC   RERROR,=AL2(INVALID)                                             
         SR    R5,R5                                                            
         LA    R3,3(R1)                                                         
VLIN020  BCTR  R3,0                                                             
         CR    R3,R1                                                            
         BL    VLIN025                                                          
         LTR   R5,R5                                                            
         BNZ   *+12                                                             
         CLI   0(R3),C' '                                                       
         BE    VLIN020                                                          
         CLI   0(R3),X'F0'                                                      
         BL    VLIN025                                                          
         LR    R5,R3                                                            
         B     VLIN020                                                          
VLIN025  LA    R3,1(R3)                                                         
         SR    R3,R1                                                            
         BZ    VLINERR                                                          
*                                                                               
         MVI   SELNUM,1            DEFAULT                                      
         LTR   R5,R5                                                            
         BZ    VLIN040             NO NUMERIC ENTRY                             
         ZIC   RF,5(R2)                                                         
         SR    RF,R3               LEN OF NUMERIC PORTION                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R5)                                                      
         CVB   RF,DUB                                                           
         STC   RF,SELNUM                                                        
*                                                                               
VLIN040  DS    0H                  VALIDATE ALPHA                               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),=C'CHA'                                                  
         BNE   VLIN050                                                          
         MVI   SELACT,C'C'                                                      
         CLI   SELNUM,1                                                         
         BNE   VLINERR                                                          
         CLI   LINTAB,X'FF'                                                     
         BE    VLINERR             SOURCE REC REQ'D FOR CHANGE                  
         B     VLIN100                                                          
*                                                                               
VLIN050  EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),=C'ADD'                                                  
         BNE   VLIN060                                                          
         MVI   SELACT,C'A'                                                      
         B     VLIN100                                                          
*                                                                               
VLIN060  EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),=C'DEL'                                                  
         BNE   VLINERR                                                          
         MVI   SELACT,C'D'                                                      
         CLI   SELNUM,1                                                         
         BNE   VLINERR                                                          
         CLI   LINTAB,X'FF'                                                     
         BE    VLINERR             SOURCE REC REQ'D FOR DELETE                  
*                                                                               
VLIN100  DS    0H                  EXECUTE ACTIONS                              
         ZIC   R3,SELNUM                                                        
VLIN110  DS    0H                                                               
         CLI   LINTAB,X'FF'                                                     
         BNE   VLIN120                                                          
         GOTO1 =A(MOVEREC),DMCB,AIO1,AIO2,RR=Y                                  
         OI    MYSTAT,X'80'        REMEMBER WE'RE WORKING OFF HEADER            
         B     VLIN200                                                          
VLIN120  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFIL',LINDA,AIO2,DMWORK             
         CLI   8(R1),0                                                          
         BE    VLIN200                                                          
         DC    H'0'                                                             
*                                                                               
* BUILD NEW ADD/CHA REC IN IO3                                                  
*                                                                               
VLIN200  DS    0H                  BUILD NEW REC IN IO3                         
         L     R6,AIO3                                                          
         USING RTMPREC,R6                                                       
         GOTO1 =A(MOVEREC),DMCB,AIO2,RTMPREC,RR=Y                               
*                                                                               
*                                  DELETE EXISTING ELEMENTS                     
         GOTO1 HELLO,DMCB,(C'D',=C'REPFIL'),(X'20',RTMPREC),0,0                 
         TM    12(R1),X'FF'-X'06'                                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 HELLO,DMCB,(C'D',=C'REPFIL'),(X'01',RTMPREC),0,0                 
         TM    12(R1),X'FF'-X'06'                                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 HELLO,DMCB,(C'P',=C'REPFIL'),RTMPREC,=X'0102',0                  
         CLI   12(R1),0  DUMMY 01 ELEM FOR MIN REC LEN                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,LINDEF           LINE DEFINITION TO LOOP THRU                 
VLIN300  DS    0H                                                               
         CLI   0(R5),X'FF'                                                      
         BE    VLIN600             DONE                                         
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               LABEL FIELD                                  
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               DATA FIELD                                   
*                                                                               
         CLI   0(R5),0             NUM FIELD?                                   
         BE    VLIN400             YES                                          
*                                                                               
         GOTO1 =A(VFLD),DMCB,(0(R5),RTMPREC),0,RR=Y VAL DATA FIELD              
         CLI   SELACT,C'A'                                                      
         BNE   VLIN500                                                          
         TM    MYSTAT,X'80'        HEADER-BASED ADD?                            
         BZ    VLIN500             NO                                           
         GOTO1 HELLO,DMCB,(C'G',=C'REPFIL'),(X'20',RTMPREC),(1,0(R5)),0         
         CLI   12(R1),0                                                         
         BE    VLIN500             FOUND - OK                                   
         TM    12(R1),X'06'        NOT FOUND?                                   
         BO    *+6                                                              
         DC    H'0'                                                             
         GOTO1 HELLO,DMCB,(C'G',=C'REPFIL'),(X'20',AIO2),(1,0(R5)),0            
         CLI   12(R1),0                                                         
         BNE   VLIN500             NOT FOUND - OK                               
         L     R1,12(R1)           A(ELEM)                                      
         TM    RTMPFFLG-RTMPFLD(R1),X'80'                                       
         BO    VLIN500                                                          
         CLI   1(R1),RTMPFOLQ                                                   
         BNH   VLIN500                                                          
         CLC   =C'*CLEAR',RTMPFOLQ(R1)                                          
         BE    VLIN500                                                          
         ZIC   RF,1(R1)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   ELEM(0),0(R1)                                                    
         NI    ELEM+(RTMPFFLG-RTMPFLD),X'FF'-X'80'                              
         GOTO1 HELLO,DMCB,(C'P',=C'REPFIL'),RTMPREC,ELEM,0                      
         CLI   12(R1),0                                                         
         BE    VLIN500                                                          
         DC    H'0'                                                             
*                                                                               
* HANDLE NUMBER FIELD & RENUMBERING TABLE                                       
*                                                                               
VLIN400  DS    0H                                                               
         CLI   SELACT,C'D'         ACTION DELETE                                
         BNE   VLIN410                                                          
         MVI   BYTE,0              MAKE LINE NUM = 0                            
         GOTO1 =A(RENUM),RR=Y      UPDATE LINE NUMBERS                          
         B     VLINX               DONE HERE                                    
*                                                                               
VLIN410  DS    0H                                                               
         CLI   SELACT,C'A'         ADD ACTION?                                  
         BNE   VLIN430             NO                                           
         CLI   LINTAB,X'FF'        AT END OF TABLE                              
         BE    *+12                                                             
         LA    R4,L'LINTAB(R4)                                                  
         B     *-12                                                             
         MVI   LINOLD,0            CREATE VIRGIN ENTRY AT TABLE END             
         MVI   LINNEW,0                                                         
         MVI   LINSTAT,0                                                        
         XC    LINDA,LINDA                                                      
         MVI   LINTAB+L'LINTAB,X'FF' NEW END MARK                               
*                                                                               
         LA    R1,LINAREA          FIND NEXT AVAILABLE LIN# FOR ENTRY           
VLIN425  CLI   0(R1),X'FF'         FIND HIGHEST ORIGINAL                        
         BE    VLIN428                                                          
         CLC   LINOLD,0(R1)                                                     
         BH    *+10                                                             
         MVC   LINOLD,0(R1)                                                     
         LA    R1,L'LINTAB(R1)                                                  
         B     VLIN425                                                          
*                                                                               
VLIN428  DS    0H                                                               
         ZIC   RF,LINOLD                                                        
         LA    RF,1(RF)            USE HIGHEST # +1                             
         STC   RF,LINOLD                                                        
         STC   RF,RTMPKLIN         WRITE TO DISK WITH THIS NUM                  
*                                                                               
VLIN430  DS    0H                  VALIDATE NUM FIELD ENTRY                     
         CLI   5(R2),0                                                          
         BNE   VLIN440                                                          
         CLI   SELACT,C'A'                                                      
         BE    VLIN450             USE NEXT AVAILABLE NUMBER                    
         B     VLIN500             NO NUM CHANGE, NOT ADD                       
VLIN440  DS    0H                                                               
         TM    4(R2),X'08'                                                      
         BO    VLIN480             VALID NUMBER INPUT                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'END'                                                  
         BNE   VLINERR                                                          
VLIN450  DS    0H                  FIND END NUMBER                              
         LA    R1,LINAREA                                                       
         MVI   BYTE,0                                                           
VLIN460  CLI   0(R1),X'FF'         FIND HIGHEST NEXT NEW NUM AVAILABLE          
         BE    VLIN470                                                          
         CLC   BYTE,1(R1)                                                       
         BH    *+10                                                             
         MVC   BYTE,1(R1)                                                       
         LA    R1,L'LINTAB(R1)                                                  
         B     VLIN460                                                          
VLIN470  ZIC   R1,BYTE                                                          
         LA    R1,1(R1)            USE HIGHEST +1                               
         STC   R1,BYTE                                                          
         B     VLIN490                                                          
VLIN480  DS    0H                  VALIDATE NEW NUM                             
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         CHI   R1,254                                                           
         BH    VLINERR                                                          
         STC   R1,BYTE                                                          
VLIN490  DS    0H                  PUT NEW LINE NUMBER INTO TABLE               
         GOTO1 =A(RENUM),RR=Y      UPDATE LINE NUMBERS                          
*                                                                               
VLIN500  DS    0H                  NEXT FIELD IN DEFINITION                     
         LA    R5,1(R5)                                                         
         B     VLIN300                                                          
*                                                                               
VLIN600  DS    0H                  WRITE UPDATES TO RECORD                      
         GOTO1 =A(MYPUTREC),DMCB,AIO3,AIO2,RR=Y                                 
         MVC   LINDA,MYDA          UPDATE D/A                                   
*                                                                               
         L     R4,SVTAB                                                         
         L     R2,SVLIN                                                         
         GOTO1 =A(MOVEREC),DMCB,AIO3,AIO2,RR=Y RE-USE AS OLD REC                
         BCT   R3,VLIN200          CASE OF MULTIPLE ADDS                        
*                                                                               
VLINX    XIT1                                                                   
VLINERR  GOTO1 MYERROR                                                          
         DROP  R4,R6                                                            
         LTORG                                                                  
***********************************************************************         
* MYPUTREC - INTELLIGENT RECORD ADD/PUT                                         
***********************************************************************         
* P1 = A(RECORD TO WRITE)                                                       
* P2 = A(IO BUFFER)                                                             
***********************************************************************         
* ROUTINE WILL ADD/PUT/RESTORE RECORD SPECIFIED IN P1 AS APPROPRIATE            
* NOTE KEY & IO BUFFER ARE USED                                                 
*                                                                               
***********************************************************************         
MYPUTREC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,0(R1)            REC                                          
         L     R3,4(R1)            WORK AREA                                    
         MVC   KEY,0(R2)                                                        
         GOTO1 DATAMGR,DMCB,(X'88',=C'DMREAD'),=C'REPDIR',KEY,KEY               
         TM    8(R1),X'10'         NOT FOUND                                    
         BO    MYP100                                                           
         MVC   MYDA,KEY+28         D/A                                          
         TM    8(R1),X'02'         DELETED                                      
         BO    MYP050                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MYP050   DS    0H                  RESTORE/OVERWITE REC                         
         CLC   KEY+27(1),29(R2)    STATUS BYTE MATCH OLD/NEW                    
         BE    MYP070              YES                                          
         MVC   KEY+27(1),29(R2)    NO - UPDATE KEY                              
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'REPDIR',0,KEY                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MYP070   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'88',=C'GETREC'),=C'REPFIL',MYDA,(R3),   +        
               DMWORK                                                           
         TM    8(R1),X'FF'-X'02'                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   =X'1507',0(R2)      SAFETY CHECK                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'REPFIL',KEY,(R2),DMWORK               
         CLI   8(R1),0                                                          
         BE    MYPX                                                             
         DC    H'0'                                                             
*                                                                               
MYP100   DS    0H                                                               
         MVC   KEY,0(R2)                                                        
         CLC   =X'1507',0(R2)      SAFETY CHECK                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'REPFIL',KEY,(R2),DMWORK               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,8(R1)                                                         
         MVC   MYDA,0(RF)                                                       
MYPX     XIT1                                                                   
*                                                                               
         LTORG                                                                  
***********************************************************************         
* DISLIN - DISPLAY INDIVIDUAL TEMPLATE LINE                                     
***********************************************************************         
* R2= A("ACT" FIELD HEADER)                                                     
* IO2 = TEMPLATE RECORD                                                         
*                                                                               
***********************************************************************         
DISLIN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,LINDEF           LINE DEFINITION TO LOOP THRU                 
         L     R4,AIO2                                                          
         USING RTMPREC,R4                                                       
DLIN320  DS    0H                                                               
         CLI   0(R5),X'FF'                                                      
         BE    DLINX               DONE                                         
*                                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               LABEL FIELD                                  
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               DATA FIELD                                   
*                                                                               
         CLI   0(R5),0             NUM FIELD?                                   
         BNE   DLIN350                                                          
         EDIT  RTMPKLIN,(3,8(R2)),ALIGN=LEFT                                    
         STC   R0,5(R2)                                                         
         B     DLIN380                                                          
*                                                                               
DLIN350  DS    0H                  DISPLAY THIS FIELD                           
         GOTO1 HELLO,DMCB,(C'G',=C'REPFIL'),(X'20',RTMPREC),(1,0(R5)),0         
         CLI   12(R1),0                                                         
         BE    DLIN360                                                          
         TM    12(R1),X'06'        NOT FOUND?                                   
         BO    DLIN380                                                          
         DC    H'0'                                                             
DLIN360  DS    0H                  DISPLAY THIS FIELD                           
         L     R3,12(R1)           A(ELEM)                                      
         USING RTMPFLD,R3                                                       
         ZIC   R1,1(R3)            LEN                                          
         AHI   R1,-(RTMPFOLQ)                                                   
         ZIC   RF,0(R2)                                                         
         AHI   RF,-8                                                            
         CR    R1,RF                                                            
         BNH   *+6                                                              
         LR    R1,RF               MAKE SURE WE DON'T OVERLOAD SCREEN           
         STC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   8(0,R2),RTMPFVAL                                                 
         DROP  R3                                                               
*                                                                               
DLIN380  DS    0H                  NEXT FIELD IN DEFINITION                     
         LA    R5,1(R5)                                                         
         B     DLIN320                                                          
*                                                                               
DLINX    XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
***********************************************************************         
* CLRS9C - CLEAR 9C SCREEN DISPLAY LINES                                        
***********************************************************************         
CLRS9C   NTR1  BASE=*,LABEL=*                                                   
         L     R4,S9CSTART                                                      
         AR    R4,RA                                                            
         ZIC   R3,LINNUM                                                        
CLIN100  DS    0H                  CLEAR A WHOLE LINE                           
         LR    R2,R4                                                            
         ZIC   R1,0(R2)                                                         
         AHI   R1,-8                                                            
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         XC    8(0,R2),8(R2)       CLEAR SELECT FIELD                           
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
         LA    R5,LINDEF           LINE DEFINITION TO LOOP THRU                 
CLIN320  DS    0H                                                               
         CLI   0(R5),X'FF'                                                      
         BE    CLIN400             DONE W/LINE                                  
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               LABEL FIELD                                  
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               DATA FIELD                                   
         ZIC   R1,0(R2)                                                         
         AHI   R1,-8                                                            
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         XC    8(0,R2),8(R2)       CLEAR DATA FIELD                             
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
         LA    R5,1(R5)                                                         
         B     CLIN320                                                          
CLIN400  DS    0H                  NEXT FIELD IN DEFINITION                     
         AH    R4,LINLEN           NEXT DISPLAY LINE                            
         BCT   R3,CLIN100          GO CLEAR IT                                  
CLINX    XIT1                                                                   
         LTORG                                                                  
***********************************************************************         
* VFLD - VALIDATE AN INDIVIDUAL TEMPLATE DATA FIELD                             
***********************************************************************         
*   P1: BYTE 0       REPTABS FIELD EQUATE ID#                                   
*       BYTES 1-3    A(TEMPLATE RECORD) TO UPDATE                               
*                                                                               
*   P2: A(REPTABS FIELD ENTRY) HEADER REC ONLY, OR NULLS                        
*                                                                               
*   R2: FIELD TO VALIDATE                                                       
*       NOTE: IF VALIDATING HEADER REC (P2 NOT NULL) ROUTINE ASSUMES R2         
*             WILL POINT TO A "T/C" FIELD PRECEEDING THE DATA FIELD             
*             WHICH ALSO IS VALIDATED                                           
***********************************************************************         
VFLD     NTR1  BASE=*,LABEL=*                                                   
         L     RF,0(R1)                                                         
         ST    RF,LOCALREC         SAVE RECORD WE'RE WORKING ON                 
         MVC   BYTE,0(R1)          SAVE FIELD EQUATE                            
         L     R4,4(R1)            REPTABS ENTRY OR NULL                        
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFIL'),(X'20',LOCALREC),(1,BYTE),0         
*                                                                               
         LTR   R4,R4               HEADER REC VALIDATION?                       
         BZ    VF230               NO                                           
*                                                                               
         TM    18(R4),X'80'        REQUIRED FIELD?                              
         BZ    VF220                                                            
         MVC   RERROR,=AL2(894)    MSG REQUIRED FIELD                           
         CLI   5(R2),0                                                          
         BNE   VF215                                                            
         GOTO1 MYERROR                                                          
VF215    CLI   8(R2),C'N'                                                       
         BNE   VF220                                                            
         GOTO1 MYERROR                                                          
VF220    DS    0H                                                               
         CLI   8(R2),C'N'                                                       
         BE    VF250                                                            
*                                                                               
VF230    DS    0H                                                               
         XC    ELEM,ELEM           BUILD FRESH FIELD ELEM                       
         LA    R6,ELEM                                                          
         USING RTMPFLD,R6                                                       
         MVI   RTMPFCD,X'20'                                                    
         MVI   RTMPFLN,RTMPFOLQ                                                 
         MVC   RTMPFTYP,BYTE                                                    
         MVI   RTMPFFLG,0                                                       
         LTR   R4,R4               HEADER REC VALIDATION?                       
         BZ    VF258               NO                                           
         CLI   8(R2),C'C'          CONTRACT ONLY?                               
         BNE   *+12                                                             
         OI    RTMPFFLG,X'80'                                                   
         B     VF250                                                            
         MVC   RERROR,=AL2(INVALID)                                             
         CLI   8(R2),C'T'                                                       
         BE    VF250                                                            
         GOTO1 MYERROR                                                          
*                                                                               
VF250    DS    0H                                                               
         LR    R3,R2               SAVE T/C FIELD                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               VALUE FIELD                                  
*                                                                               
VF255    DS    0H                  HEADER REC INPUT CHECK                       
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VF260               YES - VALIDATE IT                            
         CLI   8(R3),C'N'                                                       
         BE    VFX                 SKIP ELEM WRITE FOR HIDDEN FIELDS            
         B     VF330                                                            
*                                                                               
VF258    DS    0H                  NON-HEADER REC INPUT CHECK                   
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VFX                 NO - SKIP ELEM WRITE                         
         B     VF265               YES - VALIDATE DATA                          
*                                                                               
VF260    DS    0H                  VALIDATE FIELD VALUE INPUT                   
         MVC   RERROR,=AL2(895)    FIELD NOT ON TEMPLATE                        
         CLI   8(R3),C'T'                                                       
         BE    VF265                                                            
         GOTO1 MYERROR                                                          
*                                                                               
VF265    DS    0H                  VALIDATE FIELD VALUE INPUT                   
         LTR   R4,R4                                                            
         BZ    VF268                                                            
         CLC   =C'*CLEAR',8(R2)    SPECIAL CASE ON HEADER ONLY                  
         BE    VF320                                                            
*                                                                               
VF268    DS    0H                                                               
         CLI   BYTE,1             DAY FIELD VALIDATION                          
         BNE   VF270                                                            
         ST    R2,ADAYFLD          SAVE FOR FUTURE USE                          
         GOTO1 (RKVDAYTM,REKFACS),DMCB,(R2),0,0,ACOMFACS,0                      
         BE    VF270                                                            
         MVC   RERROR,DMCB+2                                                    
         GOTO1 MYERROR                                                          
*                                                                               
VF270    DS    0H                  VALIDATE TIME FIELD                          
         CLI   BYTE,2                                                           
         BNE   VF280                                                            
         ST    R2,ATIMFLD          SAVE FOR FUTURE USE                          
         GOTO1 (RKVDAYTM,REKFACS),DMCB,ADAYFLD,(R2),0,ACOMFACS,0                
         BE    VF280                                                            
         MVC   RERROR,DMCB+2                                                    
         GOTO1 MYERROR                                                          
*                                                                               
VF280    DS    0H                  VALIDATE LENGTH FIELD                        
         CLI   BYTE,3                                                           
         BNE   VF290                                                            
         GOTO1 (RKVBLEN,REKFACS),DMCB,(R2),0,0,ACOMFACS,0                       
         BE    VF290                                                            
         MVC   RERROR,DMCB+2                                                    
         GOTO1 MYERROR                                                          
*                                                                               
VF290    DS    0H                  VALIDATE DATES FIELD                         
         CLI   BYTE,4                                                           
         BNE   VF300                                                            
         GOTO1 (RKVBDATE,REKFACS),DMCB,(R2),0,0,ACOMFACS,0                      
         BE    VF300                                                            
         MVC   RERROR,DMCB+2                                                    
         GOTO1 MYERROR                                                          
*                                                                               
VF300    DS    0H                                                               
         CLI   BYTE,5                                                           
         BNE   VF310                                                            
         GOTO1 (RKVBSPT,REKFACS),DMCB,(R2),0,0,ACOMFACS,0                       
         BE    VF310                                                            
         MVC   RERROR,DMCB+2                                                    
         GOTO1 MYERROR                                                          
*                                                                               
VF310    DS    0H                                                               
*                                                                               
*                                                                               
VF320    DS    0H                  VALUE IS VALID, WRITE TO ELEMENT             
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   RTMPFVAL(0),8(R2)   INPUT VALUE                                  
         ZIC   R3,RTMPFLN                                                       
         LA    R3,1(R1,R3)         UPDATE ELEM LEN                              
         STC   R3,RTMPFLN                                                       
*                                                                               
VF330    DS    0H                  WRITE COMPLETED ELEMENT                      
         GOTO1 HELLO,DMCB,(C'P',=C'REPFIL'),LOCALREC,ELEM,0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
VFX      XIT1                                                                   
VFERR    GOTO1 =A(UNWIND),RR=Y     NEED TO UNWIND W/ERROR EXIT                  
         LTORG                                                                  
***********************************************************************         
* MOVEREC - ROUTINE TO MOVE RECORD FROM 1 AREA TO ANOTHER                       
***********************************************************************         
*                                                                               
*  P1 = A(FROM RECORD AREA)                                                     
*  P2 = A(TO RECORD AREA)                                                       
*                                                                               
***********************************************************************         
MOVEREC  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            'FROM' ADDRESS                               
         LH    R3,27(R2)           'FROM' LENGTH                                
         L     RE,4(R1)            'TO' ADDRESS                                 
         LR    RF,R3               'TO' LENGTH = 'FROM' LENGTH                  
         MVCL  (RE),(R2)                                                        
         L     R2,4(R1)                                                         
         LH    R3,27(R2)                                                        
         AR    R2,R3                                                            
         MVI   0(R2),0                                                          
         XIT1                                                                   
***********************************************************************         
* RENUM - ROUTINE TO FACILITATE CHANGING/INSERTING/DELETING TMP RECS            
***********************************************************************         
*                                                                               
*  R4 = A(LINTAB ENTRY)                                                         
*  BYTE = NEW TEMPLATE LINE#                                                    
*                                                                               
***********************************************************************         
RENUM    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,LINAREA                                                       
         USING LINTAB,R3                                                        
RN100    DS    0H                                                               
         CLI   0(R3),X'FF'                                                      
         BE    RN250                                                            
         CR    R3,R4               TARGET LINE?                                 
         BE    RN200               YES - SKIP FOR NOW                           
*                                                                               
         CLI   LINNEW,0            THIS ENTRY IN SEQUENCE?                      
         BE    RN200               NO - DON'T WORRY ABOUT IT                    
*                                                                               
         CLI   1(R4),0             TARGET ORIGINAL NUMBER =0                    
         BE    RN150               YES - NOTHING BEING REMOVED                  
         CLC   LINNEW,1(R4)        THIS ENTRY VS. TARGET ORIG NUMBER            
         BNH   RN150                                                            
         ZIC   RF,LINNEW           HIGHER - NEED TO BACK IT UP -1               
         AHI   RF,-1                                                            
         STC   RF,LINNEW                                                        
*                                                                               
RN150    DS    0H                                                               
         CLI   BYTE,0              TARGET NEW NUMBER =0                         
         BE    RN200               YES - NOTHING BEING INSERTED                 
         CLC   LINNEW,BYTE         THIS ENTRY VS. TARGET NEW NUMBER             
         BL    RN200                                                            
         ZIC   RF,LINNEW           NOT LOWER - NEED TO BUMP IT +1               
         CHI   RF,254              CHECK WE DON'T EXCEED LINE #254              
         BNL   RNERR                                                            
         AHI   RF,1                                                             
         STC   RF,LINNEW                                                        
*                                                                               
RN200    DS    0H                                                               
         LA    R3,L'LINTAB(R3)                                                  
         B     RN100                                                            
*                                                                               
RN250    DS    0H                                                               
         MVC   1(0,R4),BYTE        UPDATE TARGET LINE NUMBER                    
         XIT1                      DONE                                         
         DROP  R3                                                               
*                                                                               
RNERR    MVC   RERROR,=AL2(898)    MAX LINES/TEMPLATE EXCEEDED                  
         GOTO1 =A(UNWIND),RR=Y     NEED TO UNWIND W/ERROR EXIT                  
*                                                                               
         LTORG                                                                  
***********************************************************************         
* RECUPD - ROUTINE TO REWRITE TMPREC WITH NEW KEYS AFTER RENUMBERING            
***********************************************************************         
RECUPD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
RU050    DS    0H                                                               
         USING LINTAB,R4                                                        
         LA    R4,LINAREA                                                       
RU060    CLI   0(R4),X'FF'                                                      
         BE    RUX                                                              
         TM    2(R4),X'40'         PROCESSED ALREADY?                           
         BZ    RU100               NO                                           
         LA    R4,L'LINTAB(R4)     CHECK NEXT ENTRY                             
         B     RU060                                                            
*                                                                               
RU100    DS    0H                                                               
         OI    2(R4),X'40'         PROCESSED NOW                                
         CLC   0(1,R4),1(R4)       OLD VS. NEW NUMS                             
         BE    RU050               NO CHANGE                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'REPFIL',LINDA,AIO2,  +        
               DMWORK                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =A(MOVEREC),DMCB,AIO2,AIO3,RR=Y                                  
*                                                                               
         L     R6,AIO2                                                          
         OI    29(R6),X'80'        DELETE ORIGINAL REC                          
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'REPFIL',0,AIO2,DMWORK                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   KEY(27),0(R6)                                                    
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMREAD'),=C'REPDIR',KEY,KEY               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+27,X'80'                                                     
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMWRT'),=C'REPDIR',0,KEY                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RU120    DS    0H                                                               
         CLI   1(R4),0             DELETING LINE?                               
         BE    RU050               NO NEED TO WRITE BACK                        
*                                                                               
         L     R6,AIO3                                                          
         MVC   26(1,R6),1(R4)      UPDATE LIN#                                  
*                                                                               
         GOTO1 =A(MYPUTREC),DMCB,AIO3,AIO2,RR=Y                                 
         CLI   1(R4),0             ANY NEW NUM?                                 
         BE    RU050               NO                                           
*                                                                               
         MVC   BYTE,1(R4)          NUM OF REC WE OVERWROTE                      
         LA    R4,LINAREA                                                       
         B     *+8                                                              
RU170    DS    0H                  FIND REC WE OVERWROTE IN TABLE               
         LA    R4,L'LINTAB(R4)                                                  
         CLI   0(R4),X'FF'         END                                          
         BE    RU050                                                            
         CLC   BYTE,0(R4)                                                       
         BNE   RU170                                                            
         TM    2(R4),X'40'         PROCESSED ALREADY?                           
         BO    RU050               YES                                          
         CLI   1(R4),0             BEING DELETED ANYWAY                         
         BNE   RU180               NO                                           
         OI    2(R4),X'40'         YES - ALL SET NOW                            
         B     RU050                                                            
RU180    DS    0H                  NEED TO PROCESS THIS REC IMMEDIATELY         
         GOTO1 =A(MOVEREC),DMCB,AIO2,AIO3,RR=Y   USE OLD COPY                   
         OI    2(R4),X'40'         PROCESSING NOW                               
         CLC   0(1,R4),1(R4)       HAVE A UNIQUE LINE # FOR IT                  
         BNE   RU120                                                            
         DC    H'0'                SHOULDN'T BE POSSIBLE HERE                   
*                                                                               
RUX      XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
***********************************************************************         
* VALNUM - VALIDATE 'LINES' FIELD, BUILD TARGET LINE TABLE IN LINTAB            
***********************************************************************         
VALNUM   NTR1  BASE=*,LABEL=*                                                   
         USING LINTAB,R3                                                        
         LA    R3,LINAREA         BUILD TARGET LINE LIST HERE                   
         MVI   0(R3),X'FF'                                                      
*                                                                               
         L     R5,AIO1                                                          
         MVC   KEY(27),0(R5)                                                    
         CLI   ACTNUM,ACTSCOPY     COPY?                                        
         BNE   *+14                                                             
         MVC   KEY,COPYKEY                                                      
         B     VNUM025             YES, DEFAULT TO COPY ALL LINES               
*                                                                               
         LA    R2,BTMLINH                                                       
         CLI   5(R2),0             ANY TARGETS ENTERED?                         
         BE    VNUMX                                                            
*                                                                               
         MVC   RERROR,=AL2(INVALID)                                             
         CLC   =C'ALL',8(R2)                                                    
         BNE   VNUM050                                                          
         CLI   5(R2),3                                                          
         BNE   VNUMERR                                                          
VNUM025  MVI   KEY+26,1                                                         
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY,0                     
         CLC   KEY(26),KEYSAVE                                                  
         BE    VNUM040                                                          
         B     VNUMERR                                                          
VNUM030  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEY,KEY,0                     
         CLC   KEY(26),KEYSAVE                                                  
         BNE   VNUMX                                                            
VNUM040  DS    0H                                                               
         MVC   LINOLD,KEY+26                                                    
         MVC   LINDA,KEY+28                                                     
         LA    R3,L'LINTAB(R3)                                                  
         MVI   LINTAB,X'FF'                                                     
         B     VNUM030                                                          
*                                                                               
VNUM050  DS    0H                                                               
         GOTO1 SCANNER,DMCB,(R2),AIO2,C',=,-'                                   
         CLI   4(R1),0                                                          
         BE    VNUMERR                                                          
         ZIC   R4,4(R1)            # OF SCANNER LINES                           
         L     R5,AIO2             SCANNER OUTPUT                               
*                                                                               
VNUM060  DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    VNUMERR                                                          
         TM    2(R5),X'80'         1ST FIELD NUMERIC                            
         BZ    VNUMERR                                                          
         ICM   R6,15,4(R5)         BINARY 1ST FLD                               
         C     R6,=F'255'                                                       
         BH    VNUMERR                                                          
         LTR   R6,R6                                                            
         BZ    VNUMERR                                                          
         CLI   1(R5),0             2ND FIELD                                    
         BNE   VNUM070             YES                                          
         STC   R6,KEY+26                                                        
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'REPDIR',KEY,KEY,0                     
         CLI   8(R1),0                                                          
         BNE   VNUMERR                                                          
         LA    RE,LINAREA          CHECK FOR DUPLICATE ENTRIES                  
VNUM065  CLI   0(RE),X'FF'         END OF LIST                                  
         BE    VNUM066                                                          
         CLC   0(1,RE),KEY+26      IN LIST ALREADY?                             
         BE    VNUMERR             DON'T DUPLICATE ENTRY                        
         LA    RE,L'LINTAB(RE)                                                  
         B     VNUM065                                                          
VNUM066  MVC   LINOLD,KEY+26                                                    
         MVC   LINDA,KEY+28                                                     
         LA    R3,L'LINTAB(R3)                                                  
         MVI   LINTAB,X'FF'                                                     
         B     VNUM090                                                          
*                                                                               
VNUM070  DS    0H                                                               
         TM    3(R5),X'80'         2ND FIELD NUMERIC                            
         BZ    VNUMERR                                                          
         ICM   R0,15,8(R5)                                                      
         C     R0,=F'255'                                                       
         BH    VNUMERR                                                          
         CR    R0,R6                                                            
         BNH   VNUMERR                                                          
VNUM080  DS    0H                                                               
         STC   R6,KEY+26                                                        
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'REPDIR',KEY,KEY,0                     
         CLI   8(R1),0                                                          
         BNE   VNUMERR                                                          
         LA    RE,LINAREA          CHECK FOR DUPLICATE ENTRIES                  
VNUM085  CLI   0(RE),X'FF'         END OF LIST                                  
         BE    VNUM086                                                          
         CLC   0(1,RE),KEY+26      IN LIST ALREADY?                             
         BE    VNUMERR             DON'T DUPLICATE ENTRY                        
         LA    RE,L'LINTAB(RE)                                                  
         B     VNUM085                                                          
VNUM086  MVC   LINOLD,KEY+26                                                    
         MVC   LINDA,KEY+28                                                     
         LA    R3,L'LINTAB(R3)                                                  
         MVI   LINTAB,X'FF'                                                     
         CR    R6,R0                                                            
         BNL   VNUM090                                                          
         LA    R6,1(R6)                                                         
         B     VNUM080                                                          
*                                                                               
VNUM090  DS    0H                                                               
         LA    R5,32(R5)                                                        
         BCT   R4,VNUM060          NEXT SCANNER LINE                            
VNUMX    XIT1                                                                   
VNUMERR  GOTO1 MYERROR                                                          
         DROP  R3                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
*              DISPLAY RECORD ROUTINE                                 *         
***********************************************************************         
* NOTE: THIS ROUTINE HANDLES THE DISPLAY OF BOTH THE 9B/9C SCREENS    *         
*       FIRST WE HANDLE THE GENERIC STUFF COMMON TO BOTH SCREENS,     *         
*       THEN WE DIRECT TO THE APPROPRIATE SECTION FOR EITHER SCREEN   *         
***********************************************************************         
DREC00   NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO              A(HEADER REC)                                
         USING RTMPREC,R4                                                       
*                                                                               
         CLI   ACTNUM,ACTSCOPY                                                  
         BE    DREC100                                                          
*                                                                               
         CLI   PFKEY,0                                                          
         BE    DREC100                                                          
         CLI   PFKEY,2                                                          
         BE    DREC010                                                          
         B     DREC100                                                          
*                                                                               
DREC010  DS    0H                  TOGGLE DISPLAY SUBSCREENS                    
         SR    RF,RF                                                            
         CLI   BTMSUB,C'B'                                                      
         BNE   *+8                                                              
         L     RF,=A(SCRN9C)                                                    
         CLI   BTMSUB,C'C'                                                      
         BNE   *+8                                                              
         L     RF,=A(SCRN9B)                                                    
         GOTO1 (RF),RR=Y                                                        
*                                                                               
* GENERIC HEADER DISPLAY                                                        
*                                                                               
DREC100  DS    0H                                                               
         LA    R2,BTMACVH          'ACTIVE' FIELD                               
         MVI   8(R2),C'Y'                                                       
         OI    6(R2),X'80'                                                      
         TM    RTMPHFLG,X'80'      INACTIVE?                                    
         BZ    *+8                                                              
         MVI   8(R2),C'N'                                                       
*                                                                               
         LA    R2,BTMUPDH          'LAST UPDATED' FIELD                         
         GOTO1 DATCON,DMCB,(2,RTMPHUPD),(11,8(R2))                              
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,BTMDSCH          'DESC' FIELD                                 
         MVC   8(L'BTMDSC,R2),RTMPHDSC                                          
         OI    6(R2),X'80'                                                      
*                                                                               
* DIRECT TO TEMPLATE HEADER/DETAIL SCREEN DISPLAY 'DREC'                        
*                                                                               
         CLI   BTMSUB,C'B'         HEADER SCREEN?                               
         BE    DR200                                                            
         CLI   BTMSUB,C'C'         DETAIL SCREEN?                               
         BE    DR500                                                            
         DC    H'0'                                                             
*                                                                               
* TEMPLATE HEADER DISPLAY                                                       
*                                                                               
DR200    DS    0H                                                               
         L     R4,AFLDTAB                                                       
         MVC   HALF,0(R4)          TABLE ENTRY LENGTH                           
         LA    R4,4(R4)            START OF TABLE                               
*                                                                               
         LA    R2,BTMFLD1H         1ST SCREEN FIELD                             
*                                                                               
DR210    DS    0H                                                               
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    DR300               YES                                          
*                                                                               
         CLI   0(R4),0             NUM FIELD?                                   
         BNE   DR220                                                            
         AH    R4,HALF                                                          
         B     DR210                                                            
*                                                                               
DR220    DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               T/C FIELD HDR                                
         MVI   8(R2),C'N'          DEFAULT VAULE                                
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         LR    R3,R2               SAVE A(T/C FIELD)                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               A(VALUE FIELD)                               
         MVI   5(R2),0                                                          
         XC    8(L'BTMVAL1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',=C'REPFIL'),(X'20',AIO),(1,0(R4)),0             
         CLI   12(R1),0                                                         
         BE    DR250                                                            
         TM    12(R1),X'06'        NOT FOUND?                                   
         BO    DR280                                                            
         DC    H'0'                                                             
*                                                                               
DR250    DS    0H                                                               
         L     R6,12(R1)                                                        
         USING RTMPFLD,R6                                                       
*                                                                               
         MVI   8(R3),C'T'                                                       
         TM    RTMPFFLG,X'80'                                                   
         BZ    *+8                                                              
         MVI   8(R3),C'C'                                                       
*                                                                               
         ZIC   R1,RTMPFLN          EL LENGTH                                    
         AHI   R1,-RTMPFOLQ        -OVERHEAD                                    
*                                                                               
         ZIC   R5,0(R2)            CHECK DATA NOT TOO BIG FOR SCREEN            
         AHI   R5,-8                                                            
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         AHI   R5,-8                                                            
         CR    R1,R5                                                            
         BNH   *+6                                                              
         LR    R1,R5                                                            
*                                                                               
         LTR   R1,R1                                                            
         BZ    DR280                                                            
         STC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   8(0,R2),RTMPFVAL                                                 
         DROP  R6                                                               
*                                                                               
DR280    DS    0H                                                               
         ZIC   R1,0(R2)            NEXT FIELD                                   
         AR    R2,R1                                                            
         AH    R4,HALF             NEXT TABLE ENTRY                             
         B     DR210                                                            
*                                                                               
DR300    DS    0H                                                               
         CLI   ACTNUM,ACTSCOPY                                                  
         BNE   DR330                                                            
         CLC   COPYKEY,KEY                                                      
         BNE   DR310                                                            
         XC    BTMNAM,BTMNAM                                                    
         MVI   BTMNAMH+5,0                                                      
         OI    BTMNAMH+6,X'80'                                                  
         OI    BTMNAMH+1,X'01'                                                  
         GOTO1 GETTXT,DMCB,148,0,(C'I',0),0,0,0    NEW KEY?                     
         B     DRX                                                              
*                                                                               
DR310    DS    0H                                                               
         GOTO1 GETTXT,DMCB,149,0,(C'I',0),0,0,0    COPIED?                      
         B     DRX                                                              
*                                                                               
DR330    DS    0H                                                               
         LA    R2,BTMPFK                                                        
         XC    BTMPFK,BTMPFK                                                    
         OI    BTMPFKH+6,X'80'                                                  
         MVC   0(18,R2),=C'PF2=Template Lines'                                  
         LA    R2,20(R2)                                                        
         CLI   ACTNUM,ACTSEL                                                    
         BNE   DR350                                                            
         MVC   0(11,R2),=C'PF12=Return'                                         
         LA    R2,13(R2)                                                        
DR350    DS    0H                                                               
         B     DRX                                                              
*                                                                               
* TEMPLATE DETAIL DISPLAY                                                       
*                                                                               
DR500    DS    0H                  BUFFER ALL RECORD #'S & D/A'S                
         LA    R5,LINAREA                                                       
         MVC   KEY,RTMPKEY                                                      
         MVI   KEY+26,1                                                         
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY                       
DR510    DS    0H                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(26),KEYSAVE                                                  
         BNE   DR520                                                            
         MVC   0(1,R5),KEY+26      ORIG NUMBER                                  
         MVC   1(1,R5),KEY+26      NEW NUMBER (DEFAULT SAME)                    
         MVI   2(R5),0             STATUS DEFAULT                               
         MVC   3(4,R5),KEY+28      D/A                                          
         LA    R5,L'LINTAB(R5)                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEY,KEY                       
         B     DR510                                                            
*                                                                               
DR520    DS    0H                                                               
         MVI   0(R5),X'FF'         END OF TABLE                                 
*                                                                               
         CLI   PFKEY,6             PG DOWN HIT?                                 
         BNE   DR525                                                            
***      CLI   LINNEXT,0                                                        
***      BE    DR525                                                            
         MVC   LINSTART,LINNEXT    START AT TOP OF NEXT PAGE                    
         XC    BTBLIN,BTBLIN       CLEAR LINE FIELD                             
         MVI   BTBLINH+5,0                                                      
         OI    BTBLINH+6,X'80'                                                  
         B     DR540                                                            
*                                                                               
DR525    DS    0H                                                               
         LA    R2,BTBLINH          LINE FIELD                                   
         MVC   RERROR,=AL2(INVALID)                                             
         CLI   5(R2),0                                                          
         BE    DR550                                                            
         TM    4(R2),X'08'         NUMERIC                                      
         BZ    DR530               NO                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         CHI   R1,254                                                           
         BH    DRERROR                                                          
         STC   R1,LINSTART                                                      
         B     DR540                                                            
DR530    DS    0H                                                               
         CLC   =C'END',8(R2)                                                    
         BNE   DRERROR                                                          
         MVI   LINSTART,X'FF'                                                   
*                                                                               
DR540    DS    0H                  VALIDATE FILTERS                             
*                                                                               
DR550    DS    0H                                                               
         GOTO1 =A(CLRS9C),RR=Y     CLEAR DISPLAY LINES                          
*                                                                               
DR555    DS    0H                                                               
         MVI   LINNEXT,0                                                        
         L     R2,S9CSTART                                                      
         AR    R2,RA                                                            
         ZIC   R3,LINNUM                                                        
         LA    R5,LINAREA                                                       
         USING LINTAB,R5                                                        
DR560    DS    0H                                                               
         CLI   0(R5),X'FF'         END OF TABLE                                 
         BNE   DR570               NO - CONTINUE                                
         CLI   LINSTART,X'FF'      JUMP TO END OF RECORDS?                      
         BNE   DR600               NO                                           
         SHI   R5,L'LINTAB         YES - DISPLAY LAST RECORD                    
         LA    RF,LINAREA                                                       
         CR    R5,RF                                                            
         BL    DR600               CASE OF EMPTY TABLE                          
         MVC   LINSTART,0(R5)                                                   
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFIL',LINDA,AIO2,DMWORK             
         CLI   8(R1),0                                                          
         BE    DR580                                                            
         DC    H'0'                                                             
DR570    DS    0H                                                               
         CLC   LINSTART,0(R5)                                                   
         BH    DR575               REJECT RECORD                                
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFIL',LINDA,AIO2,DMWORK             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
***>>>   FILTER RECORDS                                                         
*                                                                               
         B     DR580               KEEP RECORD                                  
*                                                                               
DR575    DS    0H                  REJECT RECORD                                
         LA    R5,L'LINTAB(R5)                                                  
         B     DR560                                                            
*                                                                               
DR580    DS    0H                                                               
         GOTO1 =A(DISLIN),RR=Y                                                  
         OI    LINSTAT,X'80'       ONSCREEN                                     
         AH    R2,LINLEN           USED A DISPLAY LINE                          
         LA    R5,L'LINTAB(R5)                                                  
         BCT   R3,DR560            DISPLAY LINE COUNTER -1                      
         MVC   LINNEXT,0(R5)       NO MORE ROOM ON SCREEN                       
*                                                                               
DR600    DS    0H                                                               
*                                                                               
DR800    DS    0H                                                               
         L     R2,S9CPFK                                                        
         AR    R2,RA                                                            
         LA    R2,8(R2)                                                         
         MVC   0(19,R2),=C'PF2=Template Header'                                 
         LA    R2,21(R2)                                                        
         CLI   LINNEXT,0                                                        
         BE    DR820                                                            
         MVC   0(10,R2),=C'PF6=PgDown'                                          
         LA    R2,12(R2)                                                        
DR820    CLI   ACTNUM,ACTSEL                                                    
         BNE   DR900                                                            
         MVC   0(11,R2),=C'PF12=Return'                                         
         LA    R2,13(R2)                                                        
*                                                                               
DR900    DS    0H                  RESTORE HEADER GET-PUT SEQUENCE              
         L     R6,AIO1                                                          
         MVC   KEY(27),0(R6)       HEADER KEY                                   
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMREAD'),=C'REPDIR',KEY,KEY               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'REPFIL',KEY+28,AIO3, +        
               DMWORK                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DRX      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
DRERROR  GOTO1 MYERROR                                                          
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* REPORT - GENERATES REPORT OF CONTRACT MOVE RECORDS                            
***********************************************************************         
PREP00   NTR1  BASE=*,LABEL=*                                                   
         MVI   REPCOUNT,0          INITIALIZE                                   
         XC    BLDSTART,BLDSTART   INITIALIZE                                   
         XC    LINLEN,LINLEN       INITIALIZE                                   
*                                                                               
         OC    KEY(27),KEY         READ HEADER REC                              
         BNZ   PR010                                                            
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RTMPREC,R4                                                       
         MVC   RTMPKTYP,=X'1507'                                                
         MVC   RTMPKREP,AGENCY                                                  
         MVC   RTMPKTMP,BTMNAM                                                  
         OC    RTMPKTMP,SPACES                                                  
         MVI   RTMPKLIN,0                                                       
         GOTO1 READ                                                             
PR010    DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,BLDREP           BUILD REPORT LINE DEFINITION                 
*                                                                               
         MVI   KEY+26,1            START READING TEMPLATE LINES                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY                       
         B     PR120                                                            
PR100    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEY,KEY                       
PR120    DS    0H                                                               
         CLC   KEY(26),KEYSAVE                                                  
         BNE   PREP200                                                          
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFIL',KEY+28,AIO2,DMWORK            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO2                                                          
         ZIC   R1,REPCOUNT                                                      
         LA    R1,1(R1)                                                         
         STC   R1,REPCOUNT                                                      
         BAS   RE,REPLINE          OUTPUT THIS TEMPLATE LINE                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR100                                                            
*                                                                               
PREP200  DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,P+12                                                          
         EDIT  REPCOUNT,(3,(R2)),ALIGN=LEFT,ZERO=NOBLANK                        
         LA    R2,1(R2)                                                         
         MVC   1(15,R2),=C'LINES DISPLAYED'                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R4,AIO                                                           
         MVC   KEY(27),0(R4)       RESTORE HEADER KEY                           
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,KEY                       
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   BTMSUB,C'C'                                                      
         BNE   PREPX                                                            
         GOTO1 =A(SCRN9C),RR=Y                                                  
*                                                                               
PREPX    XIT1                                                                   
*                                                                               
* LOOP THRU FIELD TABLE, OPTIMIZE FIELD ARRANGEMENT, BUILD DEFINITION           
*                                                                               
BLDREP   NTR1                                                                   
         L     R4,AFLDTAB                                                       
         MVC   HALF,0(R4)          TABLE ENTRY LENGTH                           
         LA    R4,4(R4)            START OF TABLE                               
*                                                                               
         LA    R2,LINDEF           DEFINITION TABLE                             
         MVI   0(R2),X'FF'         INITIALIZE                                   
*                                                                               
         MVI   LINSIZE,1           INIT SIZE OF ENTRY                           
*                                                                               
         LA    R3,108              REMAINING ON LINE                            
*                                                                               
BLD100   DS    0H                  LOOP START                                   
         CLI   0(R4),X'FF'         END OF FIELD TABLE?                          
         BE    BLDX                                                             
*                                                                               
         GOTO1 =A(ADDDEF),RR=Y     ADD (R4)DEFINITION (UPDATE R3)               
         BE    BLD190              SUCESSFUL ADD                                
         LA    R3,108              NEW LINE                                     
         ZIC   RE,LINSIZE                                                       
         LA    RE,1(RE)            INCREMENT SIZE OF ENTRY                      
         STC   RE,LINSIZE                                                       
         GOTO1 =A(ADDDEF),RR=Y     TRY AGAIN                                    
         BE    *+6                                                              
         DC    H'0'                CAN'T FAIL WITH FRESH LINE                   
*                                                                               
BLD190   DS    0H                                                               
         AH    R4,HALF             NEXT FIELD                                   
         B     BLD100              LOOP                                         
*                                                                               
BLDX     DS    0H                                                               
         MVC   ALLOWLIN,LINSIZE                                                 
         XIT1                                                                   
*                                                                               
* REPLINE - REPORT OUTPUT TEMPLATE LINE                                         
*                                                                               
REPLINE  NTR1                                                                   
         SR    R2,R2                                                            
RL010    DS    0H                                                               
*                                                                               
         LA    R5,LINDEF           LINE DEFINITION TABLE                        
         MVI   MYCOL,0                                                          
RL020    DS    0H                  BUILD SINGLE DISPLAY ENTRY                   
         CLI   0(R5),X'FF'         DONE?                                        
         BE    RL080               YES                                          
*                                                                               
         L     R4,AFLDTAB          FIND FIELD DEFINITION ENTRY                  
         MVC   HALF,0(R4)                                                       
         LA    R4,4(R4)                                                         
         CLC   0(1,R5),0(R4)                                                    
         BE    *+18                                                             
         AH    R4,HALF                                                          
         CLI   0(R4),X'FF'                                                      
         BNE   *-18                                                             
         DC    H'0'                BAD FIELD EQU                                
*                                                                               
         LA    R1,0(R4)            FIELD LABEL-1                                
         LA    R2,9(R1)            MAX LABEL LEN                                
         BCTR  R2,0                                                             
         CLI   0(R2),C' '                                                       
         BE    *-6                                                              
         SR    R2,R1               LABEL LEN                                    
*                                                                               
         LA    R2,1(R2)            +SPACE                                       
*                                                                               
         ZIC   R1,17(R4)           FIELD LEN                                    
         AR    R2,R1                                                            
*                                                                               
         LA    RF,107                                                           
         ZIC   RE,MYCOL                                                         
         SR    RF,RE               ROOM LEFT ON LINE                            
*                                                                               
         CR    R2,RF                                                            
         BNH   RL050               FITS                                         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   MYCOL,0                                                          
*                                                                               
* WRITE FIELDS TO TWA                                                           
*                                                                               
RL050    DS    0H                  ADD LABEL FIELD                              
         LA    R1,0(R4)            FIELD LABEL-1                                
         LA    R6,9(R1)            MAX LABEL LEN                                
         BCTR  R6,0                                                             
         CLI   0(R6),C' '                                                       
         BE    *-6                                                              
         SR    R6,R1               LABEL LEN                                    
         ZIC   RF,MYCOL                                                         
         LA    R2,P(RF)                                                         
         ZIC   RE,MYCOL                                                         
         LA    RE,1(R6,RE)                                                      
         STC   RE,MYCOL            INCR COLUMN LABEL LEN + 1 SPACE              
         BCTR  R6,0                                                             
         EX    R6,*+4                                                           
         MVC   0(0,R2),1(R4)       LABEL TEXT                                   
*                                                                               
*                                  ADD DATA FIELD                               
         ZIC   R6,17(R4)           DATA LEN                                     
         ZIC   RF,MYCOL                                                         
         LA    R2,P(RF)                                                         
         CLI   0(R4),0             NUMBER FIELD?                                
         BNE   RL055                                                            
         EDIT  (1,KEY+26),(3,(R2)),ALIGN=LEFT,ZERO=NOBLANK                      
         B     RL060                                                            
RL055    DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'G',=C'REPFIL'),(X'20',AIO2),(1,0(R4)),0            
         CLI   12(R1),0                                                         
         BNE   RL060               NO DATA FOR THIS FIELD                       
         L     RE,12(R1)           A(ELEM)                                      
         ZIC   R1,1(RE)            ELEM LEN                                     
         AHI   R1,-(RTMPFOLQ)                                                   
         CR    R1,R6                                                            
         BL    *+6                                                              
         LR    R1,R6                                                            
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R2),RTMPFOLQ(RE)                                             
RL060    ZIC   RE,MYCOL                                                         
         AR    RE,R6                                                            
         CHI   RE,107                                                           
         BNL   *+8                                                              
         LA    RE,1(RE)            ALLOW 1 SPACE BEFORE NEXT FIELD              
         STC   RE,MYCOL            INCR COLUMN DATA LEN                         
*                                                                               
         LA    R5,1(R5)            NEXT FIELD                                   
         B     RL020                                                            
*                                                                               
RL080    DS    0H                  DONE INDIVIDUAL ENTRY                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***  ADDDEF     ON RETURN: CC NOT= NO ROOM ON LINE                              
ADDDEF   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* CHECK IF FIELD ALREADY IN DEFINITION                                          
*                                                                               
         LA    R2,LINDEF                                                        
ADF010   CLI   0(R2),X'FF'                                                      
         BE    ADF020                                                           
         CLC   0(1,R4),0(R2)       IN DEFINITION ALREADY?                       
         BE    ADFXOK              YES                                          
         LA    R2,1(R2)                                                         
         B     ADF010                                                           
*                                                                               
* CHECK HEADER RECORD IF FIELD BELONGS IN DEFINITION                            
*                                                                               
ADF020   DS    0H                                                               
         CLI   0(R4),0             NUM FIELD ALWAYS IN DEFINITION               
         BE    ADF050                                                           
         GOTO1 HELLO,DMCB,(C'G',=C'REPFIL'),(X'20',AIO),(1,0(R4)),0             
         CLI   12(R1),0                                                         
         BE    ADF040                                                           
         TM    12(R1),X'06'        NOT FOUND?                                   
         BO    ADFXOK                                                           
         DC    H'0'                                                             
*                                                                               
ADF040   DS    0H                                                               
         L     R6,12(R1)                                                        
         USING RTMPFLD,R6                                                       
         TM    RTMPFFLG,X'80'      FIELD IN CONTRACT ONLY                       
         BO    ADFXOK              YES                                          
*                                                                               
* NOW DECIDE IF WE HAVE ROOM ON LINE FOR THIS FIELD                             
*                                                                               
ADF050   DS    0H                                                               
         LA    R1,0(R4)            FIELD LABEL-1                                
         LA    R2,9(R1)            MAX LABEL LEN                                
         BCTR  R2,0                                                             
         CLI   0(R2),C' '                                                       
         BE    *-6                                                              
         SR    R2,R1               LABEL LEN                                    
*                                                                               
         LA    R2,1(R2)            +SPACE                                       
*                                                                               
         ZIC   R1,17(R4)           FIELD LEN                                    
         AR    R2,R1                                                            
*                                                                               
         CR    R2,R3                                                            
         BH    ADF150              DOESN'T FIT                                  
*                                                                               
* FIELD FITS ON LINE - UPDATE LEN & ADD TO DEFINITION TABLE                     
*                                                                               
         SR    R3,R2               UPDATE RUNNING LINE LEN                      
         BZ    *+6                                                              
         BCTR  R3,0                ALLOW 1 SPACE BEFORE NEXT FIELD              
*                                                                               
         LA    R2,LINDEF           FITS - ADD TO DEFINITION                     
         CLI   0(R2),X'FF'         FIND END OF DEFINITION TABLE                 
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         B     *-12                                                             
         MVC   0(1,R2),0(R4)       ADD TO DEFINITION                            
         MVI   1(R2),X'FF'                                                      
         B     ADFXOK                                                           
*                                                                               
* FIELD DOESN'T FIT, TRY FOR SMALLER FIELD & RECURSIVELY CALL THIS ROUT         
*                                                                               
ADF150   DS    0H                                                               
         AH    R4,HALF             NEXT FIELD IN TABLE                          
         CLI   0(R4),X'FF'         NO MORE                                      
         BE    ADFXL                                                            
*                                                                               
         BAS   RE,ADDDEF                                                        
         BNE   ADFXL               NO FIELDS FIT, GET OUT                       
         B     ADF150              FIT A SMALLER ONE, KEEP TRYING               
*                                                                               
ADFXH    CLI   *,0                 SET CC HIGH                                  
         B     ADFX                                                             
ADFXL    CLI   *,X'FF'             SET CC LOW                                   
         B     ADFX                                                             
ADFXOK   CR    RB,RB               SET CC EQUAL                                 
ADFX     XIT1  REGS=(R3)                                                        
         LTORG                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'229RESFM1D   02/11/03'                                      
         END                                                                    
