*          DATA SET SPSFM1E    AT LEVEL 058 AS OF 08/11/11                      
*PHASE T2171EA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T2171E - SIR NSID/DETAIL RECORD REPORT                *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T21700 (SFM CONTROLLER)                    *         
*                                                                     *         
*  INPUTS       SCREEN T217CE (NSID REPORT)                           *         
*                                                                     *         
*  OUTPUTS      NSID/DETAIL PRINTED REPORT                            *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- GETEL                                           *         
*               R7 -- SECOND BASE                                     *         
*               R8 -- SPOOLD                                          *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 - NSID RECORD                                     *         
*               IO2 - DETAIL RECORDS                                  *         
*               IO3 - MISC.                                           *         
*                                                                     *         
*  COMMENTS                                                           *         
*                                                                     *         
*  THIS PROGRAM PRODUCES A REPORT OF ALL DETAIL RECORDS FOR A GIVEN   *         
*  AGENCY/MEDIA/SCHEME/YEAR.  THE NSID RECORDS ARE READ FIRST TO      *         
*  OBTAIN THE SEQUENCE NUMBERS FOR THE DETAIL RECORDS.  BECAUSE THE   *         
*  KEYS OF THE NSID RECORDS ARE NOT SORTED IN THE ORDER OF THE        *         
*  REPORT COLUMNS, ALL KEYS FOR A STATION ARE READ AND SORTED WITH    *         
*  XSORT BEFORE READING THE DETAIL RECORDS.                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2171E  SIR RECORDS - INVENTORY REPORT'                         
T2171E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2171E                                                         
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING T2171E,RB,R7                                                     
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       LA    R2,=X'0900000000010000E3'                                        
         GOTO1 VALIMED             FAKE THE MEDIA FIELD TO 'T'                  
*                                                                               
         LA    R2,SRLSCHH          SCHEME                                       
         XC    SCHEME,SCHEME                                                    
         CLI   SRLSCHH+5,0                                                      
         BNE   VK2                 SCHEME IS GIVEN                              
*                                                                               
         XC    WORK,WORK           READ SID PROFILE FOR AGENCY/MEDIA            
         MVC   WORK+16(4),=C'S0SI'                                              
         MVC   WORK+20(2),AGENCY                                                
         MVC   WORK+22(1),QMED                                                  
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         OC    WORK(16),WORK       TEST PROFILE FOUND                           
         BZ    VK1                 NO -- NO SCHEME IS OK                        
         CLI   WORK+2,C'N'         DEFAULT ALLOWED?                             
         BNE   VK1                 YES                                          
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VK1      MVC   SRLSCH,=C'ALL'                                                   
         OI    SRLSCHH+6,X'80'     XMIT                                         
         B     VK3                                                              
*                                                                               
VK2      CLC   =C'ALL',SRLSCH      TEST SCHEME FOR ENTIRE AGENCY                
         BE    VK3                                                              
         OC    SRLSCH,=C'   '                                                   
         GOTO1 CLPACK,DMCB,SRLSCH,SCHEME                                        
         CLI   DMCB,0              TEST VALID SCHEME CODE                       
         BE    VK3                 YES                                          
         MVI   ERROR,INVSCH                                                     
         B     TRAPERR                                                          
*                                                                               
VK3      MVI   COMPUSER,C'N'       ASSUME USER DOESN'T USE COMPETITION          
         MVI   REPMUSER,C'N'       ASSUME USER DOESN'T USE REP MARKETS          
         XC    WORK,WORK           READ SID PROFILE                             
         MVC   WORK+16(4),=C'S0SI'                                              
         MVC   WORK+20(2),AGENCY                                                
         MVC   WORK+22(1),QMED                                                  
         CLC   SRLSCH,=C'ALL'                                                   
         BE    *+10                                                             
         MVC   WORK+23(3),SRLSCH                                                
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         OC    WORK(16),WORK       TEST PROFILE FOUND                           
         BZ    *+16                NO                                           
         MVC   COMPUSER,WORK       SAVE PROFILE VALUES                          
         MVC   REPMUSER,WORK+1                                                  
*                                                                               
         XC    BMKTSTA,BMKTSTA     MARKET/STATION                               
         LA    R2,SRLSTAH                                                       
         CLI   SRLSTAH+5,0         TEST FILTER IS GIVEN                         
         BNE   VK5                 YES - VALIDATE THE MARKET OR STATION         
*                                                                               
         CLI   WHEN,X'40'          TEST 'NOW' PRINT OPTION                      
         BE    NOTNOW              CAN'T PRINT 'NOW' WITHOUT MARKET             
         B     VK20                                                             
*                                                                               
VK5      TM    SRLSTAH+4,X'08'     IS IT NUMERIC (MKT NUMBER)                   
         BZ    VK10                                                             
*                                                                               
         CLI   REPMUSER,C'Y'       TEST READ REP MARKETS                        
         BNE   VK7                 NO                                           
         GOTO1 VALIRMKT            YES                                          
         B     VK20                                                             
VK7      GOTO1 VALIMKT             IT'S A MARKET NUMBER                         
         B     VK20                                                             
*                                                                               
VK10     CLI   REPMUSER,C'Y'       TEST READ REP MARKETS                        
         BNE   VK15                NO                                           
         GOTO1 VALIRSTA            YES                                          
         B     VK20                                                             
VK15     GOTO1 VALISTA             IT'S A STATION                               
*                                                                               
VK20     MVI   DAYPART,0           DAYPART                                      
         LA    R2,SRLDPTH                                                       
         CLI   SRLDPTH+5,0                                                      
         BE    *+10                                                             
         MVC   DAYPART,SRLDPT                                                   
*                                                                               
         MVI   PROGTYP,0           PROGTYPE                                     
         LA    R2,SRLPRGH                                                       
         CLI   SRLPRGH+5,0                                                      
         BE    *+10                                                             
         MVC   PROGTYP,SRLPRG                                                   
*                                                                               
         MVI   YEAR,0              YEAR                                         
         LA    R2,SRLYRH                                                        
         CLI   SRLYRH+5,0                                                       
         BE    VK30                                                             
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 PERVAL,DMCB,(SRLYRH+5,SRLYR),('PVINSGLO',WORK)                   
*                                                                               
         CLI   DMCB+4,PVRCOK       YEAR WAS OK?                                 
         BE    *+12                                                             
         MVI   ERROR,INVYEAR                                                    
         B     TRAPERR             NO                                           
*                                                                               
         LA    RF,WORK             PERVAL OUTPUT AREA                           
         MVC   YEAR,PVALBSTA-PERVALD(RF)  BINARY YEAR                           
         XI    YEAR,X'FF'          YEAR IN ONE'S COMPLEMENT FORM                
*                                                                               
VK30     LA    R4,KEY              CREATE THE SCHEME KEY                        
         XC    KEY,KEY                                                          
         USING SIRKEY,R4                                                        
         MVI   SIRKTYPE,SIRKTYPQ   SIR RECORD TYPE                              
         MVC   SIRKAM,BAGYMD       AGY/MED                                      
         MVC   SIRKCODE,SCHEME     SCHEME CODE                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST SCHEME EXISTS                           
         BE    *+16                                                             
         LA    R2,SRLSCHH                                                       
         MVI   ERROR,NOSCHM                                                     
         B     TRAPERR                                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              GET THE SCHEME RECORD                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,EDYCODEQ     LOOK FOR DEFAULT YEAR ELEMENT                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING EDYELEM,R6                                                       
         MVC   CURYEAR,EDYYEAR     GET THE YEAR                                 
         DROP  R6                                                               
*                                                                               
         CLI   YEAR,0              TEST YEAR IS ABSENT                          
         BNE   VK50                NO, WE HAVE ONE                              
         MVC   YEAR,CURYEAR        USE THE CURRENT YEAR                         
         MVC   FULL(1),CURYEAR                                                  
         MVC   FULL+1(2),=X'0101'  PRETEND IT'S JAN01                           
         GOTO1 DATCON,DMCB,(3,FULL),(20,DUB)                                    
         MVC   SRLYR,DUB+2         PRINTABLE YY                                 
         OI    SRLYRH+6,X'80'      XMIT THE CURRENT YEAR                        
         XI    YEAR,X'FF'          YEAR IN ONE'S COMPLEMENT FORM                
         B     VK60                                                             
*                                                                               
VK50     ZIC   R1,YEAR             COMPARE GIVEN YEAR TO CURRENT YEAR           
         X     R1,=F'255'          MAKE YEAR POSITIVE                           
         ZIC   R0,CURYEAR                                                       
         SR    R1,R0                                                            
         LPR   R1,R1               MUST BE WITHIN ONE YEAR OF CURRENT           
         CH    R1,=H'1'                                                         
         BNH   VK60                                                             
         LA    R2,SRLYRH                                                        
         MVI   ERROR,INVYEAR                                                    
         B     TRAPERR                                                          
*                                                                               
VK60     CLI   DAYPART,0           TEST ANY DAYPART GIVEN                       
         BE    VK80                NO                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,EDCCODEQ     LOOK FOR DAYPART CODES                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'8'            R1 = NO. OF DAYPARTS                         
         LA    R6,2(R6)            FIRST DAYPART                                
*                                                                               
VK70     CLC   DAYPART,0(R6)       TEST MATCH ON DAYPART                        
         BE    VK80                                                             
         LA    R6,8(R6)            BUMP TO NEXT DAYPART                         
         BCT   R1,VK70                                                          
         LA    R2,SRLDPTH                                                       
         MVI   ERROR,INVDPT                                                     
         B     TRAPERR                                                          
*                                                                               
VK80     CLI   PROGTYP,0           TEST NO PROGTYPE GIVEN                       
         BE    VK100                                                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,EPCCODEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   VK100               PROGTYP IS OPTIONAL                          
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'8'            R1 = NO. OF PROGTYPES                        
         LA    R6,2(R6)            FIRST PROGTYPE                               
*                                                                               
VK90     CLC   PROGTYP,0(R6)       LOOK FOR THE GIVEN PROGTYPE                  
         BE    VK100                                                            
         LA    R6,8(R6)            NEXT PROGTYPE                                
         BCT   R1,VK90                                                          
         LA    R2,SRLPRGH                                                       
         MVI   ERROR,INVPRGTP                                                   
         B     TRAPERR                                                          
*                                                                               
VK100    L     R6,AIO                                                           
         MVI   ELCODE,EPNCODEQ     PERIOD NAMES ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    PERNAMES,PERNAMES                                                
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'            2 FOR OVERHEAD, 1 FOR EX                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PERNAMES(0),2(R6)   SAVE ALL PERIOD NAMES AND NUMBERS            
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'5'            R1 = NO. OF PERIODS                          
         STC   R1,NUMPERS                                                       
*                                                                               
         MVC   SIRKYEAR,YEAR       NOW WE HAVE PERIOD KEY                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR PERIOD KEY                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+12                                                             
         MVI   ERROR,NOPERREC                                                   
         B     TRAPERR                                                          
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              GET THE PERIOD RECORD                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,EPDCODEQ     LOOK FOR PERIOD ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EPDELEM,R6                                                       
         XC    PERDATES,PERDATES   SAVE ALL PERIOD DATES (MMMDD)                
         LA    R3,PERDATES                                                      
         XC    WORK,WORK                                                        
*                                                                               
VK110    GOTO1 DATCON,DMCB,(3,EPDSTART),(5,WORK)                                
         GOTO1 DATCON,DMCB,(3,EPDEND),(5,WORK+8)                                
         MVC   0(5,R3),WORK                                                     
         MVC   5(5,R3),WORK+8                                                   
         LA    R3,10(R3)           BUMP PERIOD TABLE                            
         BAS   RE,NEXTEL                                                        
         BE    VK110                                                            
         DROP  R6                                                               
*                                                                               
         MVI   PERNUM,0            VALIDATE PERIOD NAME                         
         LA    R2,SRLPERH                                                       
         CLI   SRLPERH+5,0                                                      
         BE    VK130                                                            
*                                                                               
         LA    R3,PERNAMES                                                      
         ZIC   R1,NUMPERS                                                       
         OC    SRLPER,=C'    '                                                  
VK120    CLC   SRLPER,1(R3)                                                     
         BE    VK125               PERIOD NAME IS FOUND                         
         LA    R3,5(R3)                                                         
         BCT   R1,VK120                                                         
         MVI   ERROR,INVBUYP                                                    
         B     TRAPERR                                                          
*                                                                               
VK125    MVC   PERNUM,0(R3)        SAVE THE PERIOD NUMBER                       
*                                                                               
VK130    MVI   DSFLAG,C'N'         ASSUME NO DOUBLE SPACE                       
         LA    R2,SRLOPTSH         OPTIONS FIELD                                
         CLI   5(R2),0                                                          
         BE    VKX                                                              
*                                                                               
         CLI   5(R2),3             TEST 'S=2' OPTION                            
         BNE   INVOPT                                                           
         CLC   =C'S=2',8(R2)                                                    
         BNE   INVOPT                                                           
         MVI   DSFLAG,C'Y'         YES - WE'LL DOUBLE SPACE                     
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
* PRINT REPORT                                                                  
*                                                                               
PR       LA    R1,HEDSPECS         INITIALIZE                                   
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVI   HDHOOKOK,C'Y'                                                    
         MVI   RECFOUND,C'N'                                                    
         MVI   MORESTA,C'Y'                                                     
         XC    SVCSTAD,SVCSTAD                                                  
         XC    SRTSPARE,SRTSPARE                                                
         MVC   MKTSTA,BMKTSTA                                                   
*                                                                               
         CLI   REPMUSER,C'Y'       READ REP MARKETS?                            
         BNE   PR10                NO                                           
         CLI   OFFLINE,C'Y'        OFFLINE?                                     
         BNE   PR10                NO                                           
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'           BUILD ID RECORD KEY                          
         MVC   WORK+23(2),TWAORIG  USERID NUMBER                                
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',WORK,AIO3                 
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         L     RE,AIO3                                                          
         LA    RE,28(RE)           FIND SYS AUTHORIZATION ELEMENT               
*                                                                               
PR5      CLI   0(RE),X'21'         AUTH ELEMENT?                                
         BNE   *+12                NO                                           
         CLI   2(RE),X'08'         IS IT FOR REP?                               
         BE    PR7                 YES                                          
*                                                                               
         ZIC   R0,1(RE)            BUMP TO NEXT ELEMENT                         
         AR    RE,R0                                                            
         CLI   0(RE),0             END OF RECORD                                
         BNE   PR5                 NO                                           
         DC    H'0'                NO X'21' ELEMENT FOR REP                     
*                                                                               
PR7      MVC   SVREPSYS,3(RE)      GET REP SENUM FOR THIS USERID                
         L     R3,TWAMASTC         GET A(MASTC)                                 
         L     R3,MCUTL-MASTD(R3)  GET A(UTL)                                   
         MVC   SVSPTSYS,4(R3)      SAVE SPOT SENUM (FROM UTL)                   
         MVC   4(1,R3),SVREPSYS    PUT REP SENUM IN UTL                         
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',REPFLIST,AIO3                    
         MVC   4(1,R3),SVSPTSYS    SWITCH BACK TO SPOT SYSTEM                   
*                                                                               
PR10     LA    R3,SORTTAB          CLEAR THE XSORT TABLE                        
         XC    0(256,R3),0(R3)                                                  
         XC    256(256,R3),256(R3)                                              
         MVI   SORTTABL,0                                                       
*                                                                               
         LA    R4,KEY                                                           
         USING SIRKEY,R4           BUILD NSID KEY                               
         XC    KEY,KEY                                                          
         MVI   SIRKTYPE,SIRKTYPQ                                                
         MVC   SIRKAM,BAGYMD                                                    
         MVC   SIRKCODE,SCHEME                                                  
         MVC   SIRKMS,MKTSTA                                                    
         MVC   SIRKDPT,DAYPART                                                  
         MVC   SIRKYEAR,YEAR                                                    
         MVI   SIRKMON,X'81'                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR FIRST NSID KEY                      
         MVC   MKTSTA,SIRKMS                                                    
         B     PR30                                                             
*                                                                               
PR20     MVC   KEYSAVE,KEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                 NEXT RECORD                                  
*                                                                               
PR30     CLC   KEY(4),KEYSAVE      TEST SAME AGY/MED/SCHEME                     
         BE    *+12                YES                                          
         MVI   MORESTA,C'N'        NO MORE STATIONS                             
         B     PR50                                                             
*                                                                               
         OC    SIRKMS,SIRKMS       TEST THERE'S A MKT OR STA                    
         BZ    PR20                                                             
         CLI   SIRKDPT,0           TEST THERE'S A DAYPART                       
         BE    PR20                                                             
         CLI   SIRKSEQ,0           TEST THERE'S NO SEQUENCE NO.                 
         BNE   PR20                                                             
*                                                                               
         OC    BMKT,BMKT           TEST MARKET WAS GIVEN                        
         BZ    *+14                                                             
         CLC   SIRKMKT,BMKT        TEST SAME MARKET                             
         BNE   PR20                                                             
*                                                                               
         OC    BSTA,BSTA           TEST STATION WAS GIVEN                       
         BZ    *+14                                                             
         CLC   SIRKSTA,BSTA        TEST SAME STATION                            
         BNE   PR20                                                             
*                                                                               
         CLC   SIRKMS,MKTSTA       TEST CHANGE ON MKT/STA                       
         BNE   PR50                                                             
*                                                                               
         CLI   DAYPART,0           TEST DAYPART WAS GIVEN                       
         BE    *+14                                                             
         CLC   SIRKDPT,DAYPART     TEST SAME DAYPART                            
         BNE   PR20                                                             
*                                                                               
         CLC   YEAR,SIRKYEAR       TEST SAME YEAR                               
         BNE   PR20                                                             
*                                                                               
         TM    SIRKMON,SIRKBUYQ    TEST LAST BYTE IS A PERIOD NUMBER            
         BZ    PR20                                                             
*                                                                               
         CLI   PERNUM,0            TEST PERIOD WAS GIVEN                        
         BE    PR40                                                             
         MVC   BYTE,PERNUM                                                      
         OI    BYTE,SIRKBUYQ                                                    
         CLC   BYTE,SIRKMON        TEST SAME PERIOD                             
         BNE   PR20                                                             
*                                                                               
PR40     MVC   0(1,R3),SIRKMON     PUT PER/DPT INTO XSORT TABLE                 
         MVC   1(1,R3),SIRKDPT                                                  
         ZIC   R1,SORTTABL         INCREMENT TABLE LENGTH                       
         LA    R1,1(R1)                                                         
         STC   R1,SORTTABL                                                      
         LA    R3,2(R3)            BUMP TABLE POINTER                           
         B     PR20                NEXT RECORD                                  
*                                                                               
PR50     CLI   SORTTABL,0          TEST ANY RECORDS FOR THIS STATION            
         BE    PR310                                                            
         ZIC   R3,SORTTABL         NUMBER OF TABLE ENTRIES                      
         GOTO1 XSORT,DMCB,(0,SORTTAB),(R3),2,2,0                                
         LA    R3,SRTSPARE                                                      
*                                                                               
PR60     MVC   BYTE,0(R3)          LAST PERIOD PRINTED                          
         LA    R3,2(R3)            BUMP PER/DPT TABLE                           
         OC    0(2,R3),0(R3)       TEST END OF TABLE                            
         BE    PR310                                                            
         CLC   BYTE,0(R3)          TEST CHANGE OF PERIOD                        
         BE    PR140               NO                                           
*                                                                               
         MVI   ALLOWLIN,2          MUST BE ROOM FOR THE PERIOD DESCRIP.         
         OC    ABOX,ABOX           TEST WE HAVE BOXES                           
         BZ    PR70                NO                                           
*                                                                               
         L     R5,ABOX             A(BOX DSECT)                                 
         USING BOXD,R5                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)    PRINT HORIZONTAL LINE                        
         MVI   0(R1),C'M'                                                       
         MVI   BOXINIT,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R5                                                               
*                                                                               
PR70     LA    R1,PERNAMES         SAVED PERIOD NAMES                           
         LA    R2,PERDATES         SAVED PERIOD DATES                           
         ZIC   R0,NUMPERS          NUMBER OF PERIODS                            
*                                                                               
PR80     MVC   BYTE,0(R3)                                                       
         NI    BYTE,X'FF'-SIRKBUYQ                                              
         CLC   BYTE,0(R1)          TEST MATCH ON PERIOD                         
         BE    *+18                                                             
         LA    R1,5(R1)            TRY NEXT PERIOD                              
         LA    R2,10(R2)                                                        
         BCT   R0,PR80                                                          
         DC    H'0'                BETTER BE THERE                              
*                                                                               
         MVC   PRTPER(4),1(R1)     PERIOD NAME                                  
         MVC   PRTPER+6(5),0(R2)   PERIOD START DATE                            
         MVI   PRTPER+11,C'-'                                                   
         MVC   PRTPER+12(5),5(R2)  PERIOD END DATE                              
*                                                                               
         XC    KEY,KEY             BUILD STATION DEFAULT KEY                    
         MVI   SIRKTYPE,SIRKTYPQ                                                
         MVC   SIRKAM,BAGYMD                                                    
         MVC   SIRKCODE,SCHEME                                                  
         MVC   SIRKMS,MKTSTA                                                    
         MVC   SIRKMON,0(R3)                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST STATION DEFAULT FOUND                   
         BE    PR90                YES                                          
*                                                                               
         XC    KEY,KEY             NO - TRY MARKET DEFAULT UPGRADE              
         MVI   SIRKTYPE,SIRKTYPQ                                                
         MVC   SIRKAM,BAGYMD                                                    
         MVC   SIRKCODE,SCHEME                                                  
         MVC   SIRKMKT,MKTSTA                                                   
         MVC   SIRKMON,0(R3)                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PR110               NOT THERE - TRY PERIOD UPGRADE               
*                                                                               
PR90     MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              STATION UPGRADE IN IO1                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,EUPCODEQ     UPGRADE ELEMENT                              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING EUPELEM,R6                                                       
*                                                                               
         MVC   WORK,SPACES         BUILD UPGRADE EXPRESSION                     
         CLI   EUPUPGRD,SPUPTPUT   TEST HUT OR PUT                              
         BNE   PR100               NO, NEITHER ONE                              
         CLI   EUPUPGRD+1,C'P'     TEST PUT                                     
         BNE   PR100               NO, IT'S A HUT                               
*                                                                               
         MVC   WORK(4),=C'PUT/'                                                 
         ZIC   R0,CURYEAR          CURRENT YEAR FROM SCHEME RECORD              
         SR    R1,R1                                                            
         ICM   R1,8,EUPUPGRD+2     RELATIVE PUT BOOK YEAR                       
         SRA   R1,24                                                            
         AR    R1,R0               DETERMINE ABSOLUTE YEAR                      
         STC   R1,EUPUPGRD+2                                                    
         GOTO1 DATCON,DMCB,(3,EUPUPGRD+2),(6,WORK+4)                            
         MVC   WORK+7(2),WORK+8   GET RID OF '/'                                
         MVI   WORK+9,C' '                                                      
         B     *+10                                                             
*                                                                               
PR100    MVC   WORK(16),EUPUPINP                                                
*                                                                               
         OC    EUPUPFBK,EUPUPFBK   TEST ANY SHARE BOOK                          
         BZ    PR130                                                            
         ZIC   R0,CURYEAR          CURRENT YEAR FROM SCHEME RECORD              
         SR    R1,R1                                                            
         ICM   R1,8,EUPUPFBK       RELATIVE SHARE BOOK YEAR                     
         SRA   R1,24                                                            
         AR    R1,R0               DETERMINE ABSOLUTE YEAR                      
         STC   R1,EUPUPFBK                                                      
*                                                                               
         LA    R5,WORK+17                                                       
         CLI   0(R5),C' '          BACK UP TO NON-BLANK                         
         BNE   *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C','          EDIT IN COMMA                                
         LA    R5,2(R5)                                                         
         MVC   0(3,R5),=C'BK='                                                  
         GOTO1 DATCON,DMCB,(3,EUPUPFBK),(6,3(R5))                               
         B     PR130                                                            
         DROP  R6                                                               
*                                                                               
PR110    XC    KEY,KEY             BUILD PERIOD KEY                             
         MVI   SIRKTYPE,SIRKTYPQ                                                
         MVC   SIRKAM,BAGYMD                                                    
         MVC   SIRKCODE,SCHEME                                                  
         MVC   SIRKYEAR,YEAR                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                BETTER BE THERE                              
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              PERIOD RECORD IN IO1                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,EPDCODEQ     LOOK FOR PERIOD ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EPDELEM,R6                                                       
PR120    MVC   BYTE,0(R3)          PERIOD NUMBER                                
         NI    BYTE,X'FF'-SIRKBUYQ                                              
         CLC   BYTE,EPDNUM         LOOK FOR THE GIVEN PERIOD                    
         BE    *+14                                                             
         BAS   RE,NEXTEL                                                        
         BE    PR120                                                            
         DC    H'0'                                                             
*                                                                               
         MVC   WORK,SPACES                                                      
         OC    EPDUPINP,EPDUPINP   CANADIAN UPGRADE?                            
         BNZ   PR125               NO                                           
         GOTO1 DATCON,DMCB,(3,EPDUPFBK),DUB                                     
         MVC   WORK(2),DUB+2       MONTH NUMBER                                 
         MVC   WORK+2(2),DUB       YEAR                                         
*                                                                               
         LA    R2,WORK+4           BUMP PAST THIS BOOK                          
         LA    R0,3                MAXIMUM OF 3 MORE BOOKS                      
         LA    R5,EPDUPMBK         EXTRA BOOKS FOR CANADA                       
PR122    OC    0(2,R5),0(R5)       ANY MORE?                                    
         BZ    PR130                                                            
         MVI   0(R2),C'/'          BOOK DELIMITER                               
         GOTO1 DATCON,DMCB,(3,0(R5)),DUB                                        
         MVC   1(2,R2),DUB+2       MONTH NUMBER                                 
         MVC   3(2,R2),DUB         YEAR                                         
         LA    R2,5(R2)                                                         
         LA    R5,2(R5)                                                         
         BCT   R0,PR122                                                         
         B     PR130                                                            
*                                                                               
PR125    MVC   WORK(16),EPDUPINP   BUILD PERIOD UPGRADE EXPRESSION              
         OC    EPDUPFBK,EPDUPFBK   TEST ANY SHARE BOOK                          
         BZ    PR130                                                            
         LA    R2,WORK+17                                                       
         CLI   0(R2),C' '          BACK UP TO NON-BLANK                         
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C','          EDIT IN COMMA                                
         LA    R2,2(R2)                                                         
         MVC   0(3,R2),=C'BK='                                                  
         GOTO1 DATCON,DMCB,(3,EPDUPFBK),(6,3(R2))                               
*                                                                               
PR130    MVC   PRTPER+132,WORK     PUT UPGRADE IN P2                            
         DROP  R6                                                               
*                                                                               
PR140    XC    KEY,KEY             BUILD NSID KEY                               
         MVI   SIRKTYPE,SIRKTYPQ                                                
         MVC   SIRKAM,BAGYMD                                                    
         MVC   SIRKCODE,SCHEME                                                  
         MVC   SIRKMS,MKTSTA                                                    
         MVC   SIRKDPT,1(R3)                                                    
         MVC   SIRKYEAR,YEAR                                                    
         MVC   SIRKMON,0(R3)                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     WE FOUND IT BEFORE, SO IT HAD BETTER         
         BE    *+6                  BE THERE NOW                                
         DC    H'0'                                                             
*                                                                               
         MVC   NSIDKEY,KEY         SAVE NSID KEY                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              NSID RECORD IN IO1                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,EDPCODEQ     LOOK FOR DAY/TIME ELEMENTS                   
         BAS   RE,GETEL                                                         
         BNE   PR60                RECORD CONTAINS NO INVENTORY                 
*                                                                               
PR150    ST    R6,SVELEMAD         REMEMBER WHERE WE ARE IN NSID                
         USING EDPELEM,R6                                                       
         XC    KEY,KEY             BUILD DETAIL KEY                             
         MVC   KEY(13),NSIDKEY                                                  
         MVC   SIRKSEQ,EDPSEQ      USE SEQUENCE NUMBER                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO2            USE IO2 FOR DETAIL RECORDS                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         BAS   RE,GETEL            GET DAY/TIME ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING EDPELEM,R6                                                       
         CLI   PROGTYP,0           TEST PROGRAM TYPE FILTER GIVEN               
         BE    *+14                NO                                           
         CLC   EDPPROG,PROGTYP     TEST MATCH ON PROGRAM TYPE                   
         BNE   PR290               NO - TRY NEXT DETAIL RECORD                  
         MVC   DAY,EDPDAY          HANG ON TO DAY                               
         MVC   TIME,EDPTIME        HANG ON TO TIME                              
         MVC   PPRGTYP,EDPPROG     PRINT PROGRAM TYPE                           
         DROP  R6                                                               
*                                                                               
         GOTO1 UNDAY,DMCB,DAY,WORK                                              
         MVC   PDAY,WORK           PRINT DAY/TIME                               
         GOTO1 UNTIME,DMCB,TIME,PTIME                                           
         MVC   PDPT,SIRKDPT        PRINT DAYPART                                
         MVC   PDPT+1(2),=C'30'    PRINT DEFAULT SPOT LENGTH                    
*                                                                               
         CLI   COMPUSER,C'Y'       TEST COMPETITION USER                        
         BNE   PR165               NO                                           
*        TM    MKTSTA+2,X'F0'      LOCAL CABLE HEADEND?                         
*        BO    PR165               YES -- FORGET COMPETITION                    
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BE    *+12                YES                                          
         CLI   MKTSTA+2,X'E8'      LOCAL CABLE HEADEND?                         
         BNL   PR165               YES -- FORGET COMPETITION                    
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY              BUILD COMPETITION KEY                        
         USING COMPRECD,R6                                                      
         MVI   CMPKTYP,CMPKTYPQ    RECORD TYPE                                  
         MVI   CMPKFIL,CMPKFILQ                                                 
         MVC   CMPKAM,BAGYMD       A/M                                          
         ZIC   R1,YEAR                                                          
         X     R1,=X'000000FF'     MAKE YEAR POSITIVE                           
         CVD   R1,DUB                                                           
         MVC   CMPKYM,DUB+7        ONE'S DIGIT OF YEAR                          
         MVN   CMPKYM,0(R3)        PERIOD NUMBER                                
         MVC   CMPKCODE,SCHEME     SCHEME                                       
         MVC   CMPKMKT,MKTSTA      MARKET                                       
         MVC   CMPKDAY,DAY         DAY CODE                                     
         MVC   CMPKSTIM,TIME       START TIME                                   
         MVC   CMPKETIM,TIME+2     END TIME                                     
         DROP  R6                                                               
*                                                                               
         MVI   STAINDEX,0          NO COMPETITION RECORD FOUND YET              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                LOOK FOR COMPETITION KEY                     
         CLC   KEY(13),KEYSAVE     TEST RECORD IS THERE                         
         BNE   PR165               NO                                           
*                                                                               
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              GET COMPETITION RECORD                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO              BEGINNING OF COMPETITION RECORD              
         MVI   ELCODE,CMSCODEQ     LOOK FOR STATION ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',MKTSTA),WORK,WORK+4                           
         CLI   WORK+8,C' '         PUT STATION CALL LETTERS IN WORK             
         BH    *+8                                                              
         MVI   WORK+8,C'T'                                                      
         LA    RF,1                LOOK THROUGH UP TO 20 STATIONS               
         LA    R6,2(R6)            BUMP TO FIRST STATION                        
*                                                                               
PR155    CLC   WORK+4(5),0(R6)     TEST MATCH ON STATION                        
         BE    PR160               YES                                          
         LA    R6,5(R6)            BUMP TO NEXT STATION                         
         LA    RF,1(RF)            RF = STATION INDEX NUMBER                    
         CH    RF,=H'21'           TEST AGAINST MAXIMUM                         
         BNE   PR155               NOT THERE YET                                
         MVC   AIO,AIO2            RESTORE AIO (DETAIL RECORD)                  
         B     PR165               STATION NOT IN ELEMENT - USE DETAIL          
*                                                                               
PR160    STC   RF,STAINDEX         STATION INDEX NUMBER                         
         L     R6,AIO              BEGINNING OF COMPETITION RECORD              
         MVI   ELCODE,CMPCODEQ     LOOK FOR PROGRAMMING ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMPEL,R6                                                         
PR162    MVC   BYTE,CMPSEQ         STATION INDEX                                
         NI    BYTE,X'7F'          TURN OFF OVERRIDE BIT IF IT'S ON             
         CLC   BYTE,STAINDEX       TEST PROGRAM FOR THIS STATION                
         BE    *+14                YES                                          
         BAS   RE,NEXTEL           NO - TRY NEXT                                
         BE    PR162                                                            
         DC    H'0'                NO PROGRAM FOR THIS STATION                  
*                                                                               
         MVC   AIO,AIO2            RESTORE AIO                                  
         CLC   CMPPROG,=17X'FF'    TEST ELEMENT WAS NEVER INITIALIZED           
         BE    PR165               RIGHT - USE DETAIL                           
         MVC   PPROG,CMPPROG                                                    
         B     PR168                                                            
         DROP  R6                                                               
*                                                                               
PR165    L     R6,AIO                                                           
         USING EPRELEM,R6                                                       
         MVI   ELCODE,EPRCODEQ     PROGRAMMING ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   PPROG,EPRTEXT                                                    
         DROP  R6                                                               
*                                                                               
PR168    MVI   ELCODE,EECCODEQ                                                  
         OC    SVCSTAD,SVCSTAD     DO WE NEED TO LOOK FOR A(COST)               
         BZ    *+12                YES                                          
         L     R6,SVCSTAD                                                       
         B     *+16                                                             
*                                                                               
         L     R6,AIO              LOOK FOR COST ELEMENTS                       
         BAS   RE,GETEL                                                         
         BNE   PR175                                                            
*                                                                               
         USING EECELEM,R6                                                       
         EDIT  EECSLN,(3,PSLN),ALIGN=LEFT                                       
*                                                                               
         SR    R0,R0               COST1                                        
         ICM   R1,15,EECCOST1                                                   
         D     R0,=F'100'                                                       
         EDIT  (R1),(5,PCST),ZERO=NOBLANK                                       
*                                                                               
         OC    EECDATE2,EECDATE2   DATE2                                        
         BZ    PR170                                                            
         GOTO1 DATCON,DMCB,(3,EECDATE2),(5,WORK)                                
         MVC   PDATE+132,WORK                                                   
         SR    R0,R0               COST2                                        
         ICM   R1,15,EECCOST2                                                   
         D     R0,=F'100'                                                       
         EDIT  (R1),(5,PCST+132),ZERO=NOBLANK                                   
*                                                                               
         OC    EECDATE3,EECDATE3   DATE3                                        
         BZ    PR170                                                            
         GOTO1 DATCON,DMCB,(3,EECDATE3),(5,WORK)                                
         MVC   PDATE+264,WORK                                                   
         SR    R0,R0               COST3                                        
         ICM   R1,15,EECCOST3                                                   
         D     R0,=F'100'                                                       
         EDIT  (R1),(5,PCST+264),ZERO=NOBLANK                                   
*                                                                               
         OC    EECDATE4,EECDATE4   DATE4                                        
         BZ    PR170                                                            
         GOTO1 DATCON,DMCB,(3,EECDATE4),(5,WORK)                                
         MVC   PDATE+396,WORK                                                   
         SR    R0,R0               COST4                                        
         ICM   R1,15,EECCOST4                                                   
         D     R0,=F'100'                                                       
         EDIT  (R1),(5,PCST+396),ZERO=NOBLANK                                   
         DROP  R6                                                               
*                                                                               
PR170    BAS   RE,NEXTEL           ARE THERE MORE COST ELEMENTS                 
         BE    *+14                YES                                          
         XC    SVCSTAD,SVCSTAD                                                  
         B     PR175                                                            
         ST    R6,SVCSTAD          SAVE A(NEXT COST ELEMENT)                    
*                                                                               
PR175    L     R6,AIO                                                           
         MVI   ELCODE,EUPCODEQ     UPGRADE ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   PR205                                                            
*                                                                               
         MVC   WORK,SPACES         BUILD UPGRADE EXPRESSION                     
         USING EUPELEM,R6                                                       
         MVC   WORK(4),=C'UPX='                                                 
         MVC   WORK+2(1),EUPUPFIL                                               
         MVC   WORK+4(16),EUPUPINP                                              
*                                                                               
         OC    EUPUPINP,EUPUPINP   CANADIAN UPGRADE EXPRESSION?                 
         BNZ   *+18                                                             
         LA    R5,WORK+4           YES                                          
         MVC   0(3,R5),=C'BK/'                                                  
         B     PR177                                                            
*                                                                               
         LA    R5,WORK+21                                                       
         OC    EUPUPFBK,EUPUPFBK   TEST ANY SHARE BOOK                          
         BZ    PR180                                                            
         BAS   RE,SETCOMMA                                                      
         MVC   0(3,R5),=C'BK='                                                  
*                                                                               
PR177    GOTO1 DATCON,DMCB,(3,EUPUPFBK),(6,3(R5))                               
         LA    R5,9(R5)                                                         
         CLI   EUPLEN,EUPLENXQ     NEW VERSION OF ELEMENT?                      
         BL    PR180               NO                                           
*                                                                               
         LA    R0,3                MAXIMUM OF 3 MORE BOOKS                      
         LA    R2,EUPUPMBK         EXTRA BOOKS FOR CANADA                       
PR178    OC    0(2,R2),0(R2)       ANY MORE?                                    
         BZ    PR180                                                            
         MVI   0(R5),C'/'          BOOK DELIMITER                               
         GOTO1 DATCON,DMCB,(3,0(R2)),(6,1(R5))                                  
         LA    R5,7(R5)                                                         
         LA    R2,2(R2)                                                         
         BCT   R0,PR178                                                         
         DROP  R6                                                               
*                                                                               
PR180    L     R6,AIO                                                           
         MVI   ELCODE,EOVCODEQ     OVERRIDE ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   PR200                                                            
*                                                                               
         USING EOVELEM,R6                                                       
         OC    EOVUPDAY,EOVUPDAY   TEST DAY/TIME OVERRIDE                       
         BZ    PR190                                                            
         BAS   RE,SETCOMMA                                                      
         MVC   0(3,R5),=C'DT='                                                  
         GOTO1 UNDAY,DMCB,EOVUPDAY,3(R5)                                        
         LA    R5,11(R5)                                                        
*                                                                               
         BAS   RE,SETCOMMA                                                      
         BCTR  R5,0                                                             
         MVI   0(R5),C'/'                                                       
         GOTO1 UNTIME,DMCB,EOVUPTIM,1(R5)                                       
         LA    R5,14(R5)                                                        
*                                                                               
PR190    OC    EOVUPSTA,EOVUPSTA   TEST STATION OVERRIDE                        
         BZ    PR200                                                            
         BAS   RE,SETCOMMA                                                      
         MVC   0(3,R5),=C'ST='                                                  
         MVC   3(4,R5),EOVUPSTA                                                 
         DROP  R6                                                               
*                                                                               
PR200    MVC   PUPG,WORK           EBCDIC UPGRADE EXPRESSION                    
         LA    R2,PUPG+132         USE P2 THROUGH P4                            
         B     *+8                                                              
*                                                                               
PR205    LA    R2,PUPG                                                          
         CLI   COMPUSER,C'Y'       TEST COMPETITION USER                        
         BNE   PR210                                                            
*        TM    MKTSTA+2,X'F0'      LOCAL CABLE HEADEND?                         
*        BO    PR210               YES -- FORGET COMPETITION                    
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BE    *+12                YES                                          
         CLI   MKTSTA+2,X'E8'      LOCAL CABLE HEADEND?                         
         BNL   PR210               YES -- FORGET COMPETITION                    
         CLI   STAINDEX,0          TEST COMPETITION RECORD WAS FOUND            
         BE    PR210               NO                                           
*                                                                               
         ZIC   R1,STAINDEX         STATION INDEX NUMBER                         
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,4(R1)            R1 = DISP TO DEMO FOR STATION                
         STH   R1,STADISP                                                       
*                                                                               
         L     R6,AIO3             BEGINNING OF COMP RECORD                     
         MVI   ELCODE,CMDCODEQ     LOOK FOR DEMO ELEMENTS                       
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST BE AT LEAST ONE                         
         DC    H'0'                                                             
*                                                                               
         LA    R5,DEMBUILD         WORK AREA FOR DEMOS                          
         XC    DEMBUILD,DEMBUILD                                                
*                                                                               
         BAS   RE,CNVRTCMP         CONVERT TO PRINTABLE FORMAT                  
         BAS   RE,NEXTEL           LOOK FOR MORE DEMOS                          
         BE    *-8                                                              
         B     PR215               DISPLAY ALL DEMOS                            
*                                                                               
PR210    L     R6,AIO              DEMO OVERRIDE ELEMENTS                       
         MVI   ELCODE,EDOCODEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   PR250                                                            
*                                                                               
         LA    R5,DEMBUILD         WORK AREA FOR DEMOS                          
         XC    DEMBUILD,DEMBUILD                                                
*                                                                               
         BAS   RE,CNVRT            CONVERT CODES TO NAMES                       
         BAS   RE,NEXTEL                                                        
         BE    *-8                                                              
*                                                                               
PR215    LA    R5,DEMBUILD                                                      
         OC    0(10,R5),0(R5)      ANY DEMO OVERRIDES                           
         BZ    PR250               NO                                           
*                                                                               
PR220    LA    R1,L'PUPG(R5)       R5 - START OF BLOCK AREA                     
         CLI   0(R1),C','          R1 - END OF BLOCK AREA                       
         BNE   PR230                                                            
         MVI   0(R1),C' '                                                       
         B     PR240                                                            
*                                                                               
PR230    BCTR  R1,0                                                             
         CR    R5,R1               R1 MUST NOT BE LESS THEN R5                  
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   0(R1),C','                                                       
         BNE   PR230                                                            
         MVI   0(R1),C' '                                                       
*                                                                               
PR240    LR    RE,R1               R1 - END OF BLOCK AREA                       
         SR    RE,R5               R5 - BEG OF BLOCK AREA                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R5)       PRINT A ROW OF DEMO OVERRIDES                
*                                                                               
         LA    R2,132(R2)          BUMP PRINT LINE                              
         MVC   0(L'PUPG,R2),SPACES                                              
*                                                                               
         LA    R5,1(R1)            SET R5 TO BEG OF NEXT BLOCK AREA             
         OC    0(7,R5),=7X'40'                                                  
         CLC   0(7,R5),=7X'40'                                                  
         BE    PR250               NO MORE DATA                                 
*                                                                               
         LA    R0,PUPG+528         A(END OF PRINT LINES)                        
         CR    R2,R0               TEST ANY MORE PRINT LINES                    
         BL    PR220               YES                                          
*                                                                               
PR250    L     R6,AIO                                                           
         MVI   ELCODE,ECOCODEQ     COMMENTS ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   PR270                                                            
*                                                                               
         LA    R0,PUPG+528         A(END OF PRINT LINES)                        
         CR    R2,R0               TEST ANY MORE PRINT LINES                    
         BL    PR260               YES                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,PUPG                                                          
*                                                                               
         USING ECOELEM,R6                                                       
PR260    MVC   WORK,SPACES                                                      
         ZIC   R1,ECOLEN                                                        
         SH    R1,=H'3'            INCLUDE BCTR AND OVERHEAD                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ECOMMENT                                                 
         MVC   0(L'PUPG,R2),WORK                                                
         DROP  R6                                                               
*                                                                               
PR270    MVC   SAVEP2,SPACES                                                    
         CLC   PRTPER+132,SPACES                                                
         BE    PR280               NOT A NEW PERIOD - PRINT THE LINE            
         CLC   PDATE+132,SPACES                                                 
         BNE   PR280               P2 IS NOT EMPTY - PRINT THE LINE             
         CLC   PUPG+132,SPACES                                                  
         BNE   PR280                                                            
*                                                                               
         MVC   SAVEP2,P2           DON'T PRINT P2 YET                           
         MVC   P2,SPACES                                                        
*                                                                               
PR280    CLI   DSFLAG,C'Y'                                                      
         BNE   *+8                                                              
         MVI   SPACING,2           YES - WE'LL DOUBLE SPACE                     
         MVI   RECFOUND,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CLC   SAVEP2,SPACES       DID WE HAVE TO SAVE P2                       
         BE    *+10                                                             
         MVC   P1,SAVEP2                                                        
*                                                                               
PR290    L     R6,SVELEMAD         A(LAST ELEMENT FOUND IN NSID)                
         MVI   ELCODE,EDPCODEQ     DAY/TIME                                     
         OC    SVCSTAD,SVCSTAD     TEST ANY MORE SPOT LENGTHS TO DO             
         BNZ   PR150               YES - DO ELEMENT AGAIN WITH NEW SLN          
         BAS   RE,NEXTEL           LOOK FOR MORE DAY/TIME ELEMENTS              
         BE    PR150                                                            
*                                                                               
         CLC   P1,SPACES           TEST ANYTHING LEFT HERE                      
         BE    PR300               NO                                           
         GOTO1 SPOOL,DMCB,(R8)     YES - PRINT IT                               
*                                                                               
PR300    MVC   AIO,AIO1            RESTORE AIO                                  
         B     PR60                NEXT NSID RECORD                             
*                                                                               
PR310    CLI   MORESTA,C'N'        TEST ANY MORE STATIONS TO PROCESS            
         BE    PR320                                                            
*                                                                               
         MVI   FORCEHED,C'Y'       NEW STATION - EJECT PAGE                     
         SR    R1,R1                                                            
         ICM   R1,7,MKTSTA+2                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,7,MKTSTA+2       TRY NEXT POSSIBLE STATION                    
         B     PR10                                                             
*                                                                               
PR320    CLI   RECFOUND,C'Y'       TEST REPORT HAS SOMETHING IN IT              
         BE    PRX                 YES                                          
         MVI   HDHOOKOK,C'N'                                                    
         MVC   PRTPER(16),=C'NO RECORDS FOUND'                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
***                                                                             
* TURN OFF PREVIOUSLY VALIDATED ON STATION FIELD BECAUSE IF WE SWITCH           
* TO NSID/DIS AND LEAVE ALL THE FIELDS FILLED IN IT WILL SKIP VALKEY            
* AND DIE IN VALREC.  AK 8/5/03                                                 
***                                                                             
PRX      NI    SRLSTAH+4,X'FF'-X'20'                                            
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* HEADLINE ROUTINES                                                             
*                                                                               
HOOK     NTR1                                                                   
*                                                                               
         CLI   HDHOOKOK,C'N'       TEST OK TO DO HEADHOOK                       
         BE    HOOKX                                                            
*                                                                               
         MVI   H3,0                NEED THIS TO SKIP LINES                      
         MVI   H6,0                                                             
*                                                                               
         OC    ABOX,ABOX           TEST WE HAVE BOXES                           
         BZ    HOOK10              NO                                           
*                                                                               
         L     R4,ABOX             A(BOX DSECT)                                 
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+7,C'T'                                                   
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+58,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+20,C'C'                                                  
         MVI   BOXCOLS+25,C'C'                                                  
         MVI   BOXCOLS+33,C'C'                                                  
         MVI   BOXCOLS+45,C'C'                                                  
         MVI   BOXCOLS+49,C'C'                                                  
         MVI   BOXCOLS+67,C'C'                                                  
         MVI   BOXCOLS+73,C'C'                                                  
         MVI   BOXCOLS+79,C'C'                                                  
         MVI   BOXCOLS+131,C'R'                                                 
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  R4                                                               
*                                                                               
HOOK10   GOTO1 MSUNPK,DMCB,(X'80',MKTSTA),H4+10,H5+10                           
         MVC   H4+58(3),SRLSCH     SCHEME                                       
         MVI   H4+61,C'/'                                                       
         MVC   H4+62(2),SRLYR      YEAR                                         
*                                                                               
         MVC   WORK(13),KEY        SAVE NSID KEY IN WORK                        
*                                                                               
         CLI   REPMUSER,C'Y'       TEST READ REP MARKETS                        
         BNE   HOOK40              NO                                           
*                                                                               
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   HOOK20                                                           
         L     R5,TWAMASTC         GET A(MASTC)                                 
         L     R5,MCUTL-MASTD(R5)  GET A(UTL)                                   
         MVC   SVSPTSYS,4(R5)      SAVE SPOT SENUM                              
         MVC   4(1,R5),SVREPSYS    PUT REP SENUM IN UTL                         
         B     HOOK25                                                           
*                                                                               
HOOK20   GOTO1 SWITCH,DMCB,=C'REP',0                                            
         CLI   DMCB+4,2            TEST REP SYSTEM IS STARTED                   
         BNE   *+12                YES                                          
         MVI   ERROR,REPISOFF                                                   
         B     TRAPERR                                                          
         CLI   DMCB+4,0            TEST SWITCH WAS SUCCESSFUL                   
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
HOOK25   LA    R5,KEY              READ MARKET KEY                              
         USING RMKTRECD,R5                                                      
         XC    KEY,KEY                                                          
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKREP,AGENCY                                                  
         MVC   RMKTKMKT,H4+10                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'REPDIR',KEY,KEY,0                 
         CLI   8(R1),0             TEST VALID MARKET                            
         BE    *+14                YES                                          
         MVC   H4+16(24),=CL24'*** UNKNOWN ***'                                 
         B     HOOK30                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'REPFIL',KEY+28,          +        
               AIO3,DMWORK                                                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,AIO3             DISPLAY MARKET NAME                          
         MVC   H4+16(L'RMKTNAME),RMKTNAME                                       
         DROP  R5                                                               
*                                                                               
HOOK30   CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   HOOK35                                                           
         L     R5,TWAMASTC         GET A(MASTC)                                 
         L     R5,MCUTL-MASTD(R5)  GET A(UTL)                                   
         MVC   4(1,R5),SVSPTSYS    SWITCH BACK TO SPOT SYSTEM                   
         B     HOOK50                                                           
*                                                                               
HOOK35   GOTO1 SWITCH,DMCB,=C'SPOT',0                                           
         CLI   DMCB+4,0            TEST SWITCH WAS SUCCESSFUL                   
         BE    HOOK50              YES                                          
         DC    H'0'                                                             
*                                                                               
HOOK40   LA    R5,KEY              BUILD MARKET RECORD KEY                      
         USING MKTRECD,R5                                                       
         XC    KEY,KEY                                                          
         MVC   MKTKEY(MKTKEYLN),=C'000000000000000'                             
         MVI   MKTKTYPE,C'M'                                                    
         MVI   MKTKMED,C'T'        MEDIA 'T'                                    
         MVC   MKTKMKT,H4+10       MARKET NUMBER                                
         MVC   MKTKAGY,AGENCY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO3                 
         LA    R1,=CL24'*** UNKNOWN ***'                                        
         L     R5,AIO3                                                          
         CLC   KEY(MKTKEYLN),0(R5)                                              
         BNE   *+8                                                              
         LA    R1,MKTNAME                                                       
         MVC   H4+16(24),0(R1)     PRINT MARKET NAME                            
         DROP  R5                                                               
*                                                                               
         CLI   H5+10,X'F0'         CABLE HEADEND?                               
         BL    *+12                                                             
         MVI   H5+14,C'/'          YES                                          
         B     HOOK50              NO NEED TO READ STATION RECORD               
*                                                                               
         USING STARECD,R5                                                       
         LA    R5,KEY              BUILD STATION RECORD KEY                     
         XC    KEY,KEY                                                          
         MVC   STAKEY(STAKEYLN),=C'000000000000000'                             
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'T'        MEDIA 'T'                                    
         MVC   STAKCALL,H5+10      STATION CALL LETTERS                         
         MVI   STAKCALL+4,C'T'                                                  
         MVC   STAKAGY,AGENCY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO3                 
         L     R5,AIO3                                                          
         CLC   KEY(STAKEYLN),0(R5)                                              
         BNE   HOOK50                                                           
         CLI   STAKCALL,X'F0'      IS THIS CABLE?                               
         BNL   HOOK50                                                           
         MVC   H5+16(3),SNETWRK    PRINT NETWORK AFFILIATION                    
         DROP  R5                                                               
*                                                                               
HOOK50   XC    KEY,KEY             RESTORE NSID KEY                             
         MVC   KEY(13),WORK                                                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                RESTORE DATAMGR SEQUENCE                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
HOOKX    B     XIT                                                              
         SPACE 3                                                                
REPFLIST DC    CL8'NREPFILE'                                                    
         DC    CL8'NREPDIR'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         SPACE 3                                                                
HEDSPECS SSPEC H1,1,C'MEDIA     SPOT T.V.'                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C'DETAIL REPORT'                                           
         SSPEC H2,52,C'-------------'                                           
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,1,C'MARKET    '                                               
         SSPEC H5,1,C'STATION   '                                               
         SSPEC H4,52,C'SCHEME '                                                 
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H5,112,PAGE                                                      
         SSPEC H9,2,C'BUYING PERIOD       DPT    DAY'                           
         SSPEC H9,38,C'TIME     PGM    PROGRAM(S)     EFFEC  COST'              
         SSPEC H9,81,C'UPGRADE VALUES'                                          
         SSPEC H10,2,C'DEFAULT UPGRADE     SLN'                                 
         SSPEC H10,47,C'TYP                   DATE        COMMENTS'             
         DC    X'00'                                                            
         EJECT                                                                  
INVOPT   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVOPTM),INVOPTM                                       
         B     SOLONG                                                           
INVOPTM  DC    C'* ERROR * INVALID OPTION *'                                    
*                                                                               
NOTNOW   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOTNOWM),NOTNOWM                                       
         B     SOLONG                                                           
NOTNOWM  DC    C'* ERROR * FIELD REQUIRED WHEN PRINTING ''NOW'' *'              
*                                                                               
TRAPERR  CLI   OFFLINE,C'Y'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 ERREX                                                            
*                                                                               
SOLONG   CLI   OFFLINE,C'Y'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 ERREX2                                                           
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
SETCOMMA CLI   0(R5),C' '          BACK UP TO NON-BLANK/NULL                    
         BH    *+8                                                              
         BCT   R5,SETCOMMA                                                      
         MVI   1(R5),C','          EDIT IN COMMA                                
         LA    R5,2(R5)                                                         
         BR    RE                                                               
         EJECT                                                                  
CNVRT    NTR1                      CONVERT DEMO CODE/VALUE                      
*                                                                               
         LR    R4,R6                                                            
         USING EDOELEM,R4                                                       
*                                                                               
         LA    RE,BLOCK                                                         
         USING DBLOCK,RE                                                        
         XC    BLOCK(256),BLOCK                                                 
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELSRC,C'A'                                                    
         MVI   DBSELMED,C'T'                                                    
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   *+8                                                              
         MVI   DBSELMED,C'C'                                                    
         DROP  RE                                                               
*                                                                               
         LA    R3,DEMWRK                                                        
         XC    0(3,R3),0(R3)       EDODEMO=2 BYTES/BUT                          
         MVC   1(2,R3),EDODEMO     DEMOCON NEEDS 3                              
         GOTO1 DEMOCON,DMCB,(0,(R3)),(2,(R5)),(C'S',BLOCK)  7 CHAR              
*                                                                               
CNV10    CLI   0(R5),C' '          LOOK FOR END OF DEMNAME                      
         BNH   *+12                                                             
         LA    R5,1(R5)                                                         
         B     CNV10                                                            
*                                                                               
         MVI   0(R5),C'='          GOT IT - MOVE IN '='                         
         LA    R5,1(R5)                                                         
*                                                                               
         EDIT  (2,EDOVALUE),(6,(R5)),1,ALIGN=LEFT                               
*                                                                               
CNV20    CLI   0(R5),C' '          LOOK FOR END OF VALUE                        
         BNH   *+12                                                             
         LA    R5,1(R5)                                                         
         B     CNV20                                                            
*                                                                               
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         DROP  R4                                                               
*                                                                               
         XIT1  REGS=(R5)           R5-NEXT POSSIBLE NAME=VALUE AREA             
         EJECT                                                                  
CNVRTCMP NTR1                      CONVERT DEMO CODE FROM COMPETITION           
*                                                                               
         LA    RE,BLOCK                                                         
         USING DBLOCK,RE                                                        
         XC    BLOCK(256),BLOCK                                                 
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELSRC,C'A'                                                    
         MVI   DBSELMED,C'T'                                                    
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   *+8                                                              
         MVI   DBSELMED,C'C'                                                    
         DROP  RE                                                               
*                                                                               
         LH    R4,STADISP          DISP TO STATION IN ELEMENT                   
         AR    R4,R6               R4 = A(STATION INDEX)                        
         TM    0(R4),X'80'         TEST DEMO IS AN OVERRIDE                     
         BZ    CNVCMPX5            NO - IGNORE                                  
*                                                                               
         MVC   BYTE,0(R4)          STATION INDEX                                
         NI    BYTE,X'7F'          TURN OFF OVERRIDE BIT                        
         CLC   STAINDEX,BYTE       BETTER BE FOR THE RIGHT STATION              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =X'FFFF',1(R4)      TEST ANY VALUE FOR THIS STATION              
         BE    CNVCMPX5            NO - IGNORE                                  
*                                                                               
         LA    R3,DEMWRK                                                        
         XC    0(3,R3),0(R3)       CMDTYP=2 BYTES/BUT                           
         MVC   1(2,R3),2(R6)       DEMOCON NEEDS 3                              
         GOTO1 DEMOCON,DMCB,(0,(R3)),(2,(R5)),(C'S',BLOCK)  7 CHAR              
*                                                                               
CNVCMP10 CLI   0(R5),C' '          LOOK FOR END OF DEMNAME                      
         BNH   *+12                                                             
         LA    R5,1(R5)                                                         
         B     CNVCMP10                                                         
*                                                                               
         MVI   0(R5),C'='          GOT IT - MOVE IN '='                         
         LA    R5,1(R5)                                                         
         LH    R4,STADISP                                                       
         LA    R4,1(R6,R4)         R4 = A(DEMOVALUE)                            
         EDIT  (2,(R4)),(6,(R5)),1,ALIGN=LEFT                                   
*                                                                               
CNVCMP20 CLI   0(R5),C' '          LOOK FOR END OF VALUE                        
         BNH   *+12                                                             
         LA    R5,1(R5)                                                         
         B     CNVCMP20                                                         
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
*                                                                               
CNVCMPX5 XIT1  REGS=(R5)           R5-NEXT POSSIBLE NAME=VALUE AREA             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMCED                                                       
         PRINT OFF                                                              
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
SVELEMAD DS    A                   A(CURRENT NSID ELEMENT)                      
SVCSTAD  DS    A                   A(NEXT COST ELEMENT)                         
STADISP  DS    H                   DISP TO STATION IN COMPETITION ELEMS         
SRTSPARE DS    XL2                                                              
SORTTAB  DS    512X                XSORT TABLE OF PER/DPT                       
SORTTABL DS    X                   NUMBER OF ENTRIES IN XSORT TABLE             
SCHEME   DS    XL2                 SCHEME CODE                                  
SEQNUM   DS    X                   SEQUENCE NUMBER                              
PERNUM   DS    X                   PERIOD                                       
YEAR     DS    X                   YEAR                                         
CURYEAR  DS    X                   CURRENT YEAR                                 
DAYPART  DS    C                   DAYPART CODE                                 
PROGTYP  DS    C                   PROGRAM CODE (IN FILTER)                     
DAYTIME  DS    0XL5                                                             
DAY      DS    X                   DAY                                          
TIME     DS    XL4                 TIME                                         
MKTSTA   DS    XL5                 SAVE MKT/STA                                 
NUMPERS  DS    X                   NUMBER OF PERIODS                            
PERNAMES DS    CL60                PERIOD NAMES                                 
PERDATES DS    CL120               PERIOD DATES (MMMDD)                         
DEMWRK   DS    CL40                WORK AREA FOR DEMOVAL                        
NSIDKEY  DS    XL13                SAVE NSID KEY                                
SAVEP2   DS    CL132                                                            
DEMBUILD DS    CL200               PRINTABLE DEMOS                              
DSFLAG   DS    C                   'Y' = DOUBLE SPACE REPORT                    
MORESTA  DS    C                   'Y' = MORE STATIONS TO PROCESS               
RECFOUND DS    C                   'Y' = THE REPORT IS NON-EMPTY                
HDHOOKOK DS    C                   'Y' = OK TO DO HEADHOOK                      
COMPUSER DS    C                   'Y' = COMPETITION USER                       
REPMUSER DS    C                   'Y' = REP MARKET RECORD USER                 
STAINDEX DS    X                   STATION INDEX IN COMPETITION RECORDS         
SVSPTSYS DS    X                   SAVED SPOT SENUM BEFORE FASWITCH             
SVREPSYS DS    X                   SAVED REP SENUM                              
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDREPMASTD                                                     
DBLKD    DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPDEMUPD                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
RMKTRECD DSECT                                                                  
       ++INCLUDE REGENMKT                                                       
COMPRECD DSECT                                                                  
       ++INCLUDE SPGENCOMP                                                      
         PRINT ON                                                               
         EJECT                                                                  
SIRRECD  DSECT                                                                  
       ++INCLUDE SPGENSIR                                                       
         EJECT                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL1                                                              
PRTPER   DS    CL19                BUYING PERIOD/DEFAULT UPGRADE                
         DS    CL1                                                              
PDPT     DS    CL1                 DAYPART CODE                                 
PSLN     DS    CL3                 SPOT LENGTH                                  
         DS    CL1                                                              
PDAY     DS    CL7                 DAY(S)                                       
         DS    CL1                                                              
PTIME    DS    CL11                TIME(S)                                      
         DS    CL2                                                              
PPRGTYP  DS    CL1                 PROGRAM TYPE CODE                            
         DS    CL2                                                              
PPROG    DS    CL17                PROGRAM(S)                                   
         DS    CL1                                                              
PDATE    DS    CL5                 EFFECTIVE DATE                               
         DS    CL1                                                              
PCST     DS    CL5                 COST                                         
         DS    CL1                                                              
PUPG     DS    CL51                UPGRADE/OVERRIDES/COMMENTS                   
         DS    CL1                                                              
         SPACE 5                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058SPSFM1E   08/11/11'                                      
         END                                                                    
