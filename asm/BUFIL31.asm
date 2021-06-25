*          DATA SET BUFIL31    AT LEVEL 006 AS OF 05/01/02                      
*PHASE T50231A                                                                  
*INCLUDE EDITOR                                                                 
         TITLE 'T50231 - BUDGET CONTROL LFM - DATA TRANSFER UTILITY'            
T50231   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FITR**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R5,ANODBLK          R5=A(NODIO BLOCK)                            
         USING NODBLKD,R5                                                       
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE                                                        
*                                                                               
DTR1     L     RF,=V(EDITOR)       RELOCATE INCLUDED MODULES                    
         AR    RF,R2                                                            
         ST    RF,VEDITOR                                                       
*                                                                               
         GOTO1 VSETADD                                                          
*                                                                               
DTR2     CLI   MODE,VALKEY         VALIDATE KEY FIELDS                          
         BE    DTR4                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    DTR6                                                             
         B     DTRX                                                             
*                                                                               
DTR4     MVI   TERMSW,NO                                                        
         BAS   RE,VALHED           VALIDATE HEADLINE FIELDS                     
         B     DTRX                                                             
*                                                                               
DTR6     BAS   RE,INITPRT          INITIALIZE FOR PRINTING                      
         CLI   TERMSW,YES          TEST IF REPORT TERMINATED                    
         BE    DTR8                                                             
*                                                                               
         BAS   RE,GENMON           GENERATE PERIOD MONTH TABLE                  
         BAS   RE,RDPL             READ THE PLAN/PRINT REPORT                   
         B     DTRX                                                             
*                                                                               
DTR8     LA    R2,P                REPORT TERMINATION PROCESSING                
         USING PRTD,R2             DELAYED TO LET GENCON DO SCREEN              
         MVC   PRTNAME(L'SVERRMSG),SVERRMSG                                     
         GOTO1 SPOOL,PARAS,(R8)                                                 
         GOTO1 (RF),(R1),(R8)                                                   
         MVC   PRTNAME(L'TERMMSG),TERMMSG                                       
         GOTO1 (RF),(R1),(R8)                                                   
*                                                                               
DTRX     B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE HEADLINE KEY FIELDS                                   
*                                                                               
* VALIDATE CLIENT                                                               
*                                                                               
VALHED   NTR1                                                                   
         GOTO1 VVALCLT,PARAS,DTRCLTH,0                                          
         MVC   DTRCLN,CLTNAM                                                    
         OI    DTRCLNH+6,X'80'     XMIT                                         
*                                                                               
* VALIDATE PRODUCT                                                              
*                                                                               
VALHED2  GOTO1 VVALPRD,PARAS,DTRPRDH,0                                          
         MVC   DTRPRN,PRDNAM                                                    
         OI    DTRPRNH+6,X'80'     XMIT                                         
*                                                                               
* VALIDATE PLAN                                                                 
*                                                                               
VALHED4  XC    DTRPLN,DTRPLN       CLEAR AND XMIT PLAN NAME ETC                 
         OI    DTRPLNH+6,X'80'                                                  
         GOTO1 VVALPLAN,PARAS,DTRPLAH,0                                         
*                                                                               
         MVC   DTRPLN(L'PLANNAM),PLANNAM                                        
         GOTO1 VPEROUT,PARAS,(1,PLANST),WORK                                    
         LA    R2,DTRPLN+L'PLANNAM+1                                            
         MVC   0(13,R2),WORK       DISPLAY PLAN START-END                       
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         MVC   PLANKEY,NDLVKEY     EXTRACT PLAN KEY                             
*                                                                               
* SAVE PLAN SNAPSHOT LIST                                                       
*                                                                               
VALHED5  MVI   NSNAPS,0                                                         
         XC    SNAPLIST,SNAPLIST                                                
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         MVI   ELCODE,BUSNELQ      GET PLAN SNAPSHOT ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   VALHED6             NONE FOUND-ERROR                             
         USING BUSND,R6                                                         
         ZIC   R1,BUSNLEN                                                       
         SH    R1,=Y(BUSNAPS-BUSND+1)                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SNAPLIST(0),BUSNAPS SAVE SNAPSHOT LIST                           
*                                                                               
         LA    R1,1(R1)            RESTORE L'SNAPSHOT LIST                      
         SR    R0,R0                                                            
         LA    RE,L'BUSNAPS                                                     
         DR    R0,RE               R1=N'SNAPSHOTS                               
         STC   R1,NSNAPS                                                        
*                                                                               
* VALIDATE OUTLINE                                                              
*                                                                               
VALHED6  XC    DTROUTN,DTROUTN     CLEAR AND XMIT OUTLINE NAME                  
         OI    DTROUTNH+6,X'80'                                                 
         GOTO1 VGETFLD,PARAS,(X'FF',DTROUTH)                                    
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BE    VALHED8             NO                                           
         MVC   OUTCODE,FLD                                                      
         GOTO1 VFINDOUT,PARAS,OUTCODE,NDIOA                                     
         BNE   SPERR                                                            
         GOTO1 VTRACE                                                           
         GOTO1 VGETVAL                                                          
         MVC   DTROUTN,OUTNAME     DISPLAY OUTLINE NAME                         
*                                                                               
* VALIDATE FROM DATA TYPE(,SNAPSHOT DATE)                                       
*                                                                               
VALHED8  LA    R2,DTRFRDTH                                                      
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA         LOOK FOR COMMA                               
         GOTO1 VFVAL                                                            
*                                                                               
         CLI   FLDH+5,0            TEST FOR NOTHING FOUND                       
         BNE   VALHED9                                                          
         MVI   ERROR,MISSING                                                    
         CLI   FSTOP,COMMA         TEST IF STOP CHARACTER FOUND                 
         BNE   *+14                                                             
         MVI   ERROR,INVALID                                                    
         MVC   XTRA(20),=C'START WITH DATA TYPE'                                
         B     SPERR                                                            
*                                                                               
VALHED9  GOTO1 DTVAL,0                                                          
         BNE   VALHEDX             ERROR FOUND-REPORT WILL TERMINATE            
*                                                                               
         XC    FTERM,FTERM         GET REST OF FIELD                            
         GOTO1 VFVAL                                                            
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BE    VALHED11                                                         
*                                                                               
         MVI   ERROR,INVDATE       EDIT FOR SNAPSHOT DATE                       
         GOTO1 DATVAL,DMCB,FLD,DUB                                              
         OC    0(4,R1),0(R1)                                                    
         BZ    SPERR                                                            
         CLC   FLDH+5(1),3(R1)     TEST THAT DATE MAKES UP WHOLE FIELD          
         BNE   SPERR                                                            
         GOTO1 DATCON,DMCB,DUB,(3,FROMASAT)                                     
*                                                                               
         SR    R1,R1               R1=N'SNAPSHOTS                               
         ICM   R1,1,NSNAPS                                                      
         BZ    VALHED10            NO SNAPSHOTS-SO ITS AN ERROR                 
         LA    RE,SNAPLIST         RE=SNAPSHOT DATE POINTER                     
*                                                                               
         CLC   FROMASAT,0(RE)      TEST IF DATE IS IN LIST                      
         BE    VALHED11            YES                                          
         LA    RE,L'BUSNAPS(RE)                                                 
         BCT   R1,*-14                                                          
*                                                                               
VALHED10 MVC   WORK,SPACES         SNAPSHOT DATE ERROR                          
         MVC   WORK(L'SNAPMSG),SNAPMSG                                          
         MVI   ERROR,SUPPLIED                                                   
         CLI   OFFLINE,NO                                                       
         BE    SPERR                                                            
         MVI   TERMSW,YES                                                       
         MVC   SVERRMSG,WORK                                                    
         B     VALHEDX                                                          
*                                                                               
VALHED11 MVC   WORK,SPACES                                                      
         MVC   WORK(L'FRHEAD1),FRHEAD1                                          
         MVC   WORK+L'FRHEAD1+1(L'FRHEAD2),FRHEAD2                              
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   DTRFRDN,WORK        DISPLAY FROM DATATYPE NAME ON SCREEN         
         OI    DTRFRDNH+6,X'80'                                                 
*                                                                               
* VALIDATE TO DATA TYPE                                                         
*                                                                               
VALHED12 GOTO1 VGETFLD,PARAS,DTRTODTH                                           
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BE    SPERR                                                            
*                                                                               
         GOTO1 DTVAL,1             EDIT TO DATA TYPE                            
         BNE   VALHEDX             FOUND AN ERROR OFF-LINE                      
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'TOHEAD1),TOHEAD1                                          
         MVC   WORK+L'TOHEAD1+1(L'TOHEAD2),TOHEAD2                              
         GOTO1 SQUASHER,DMCB,WORK,L'WORK                                        
         MVC   DTRTODN,WORK        DISPLAY DATA TYPE NAME ON SCREEN             
         OI    DTRTODNH+6,X'80'                                                 
*                                                                               
* VALIDATE PERIOD                                                               
*                                                                               
VALHED14 XC    DTRPERN,DTRPERN     CLEAR DISPLAYED PERIOD                       
         OI    DTRPERNH+6,X'80'                                                 
         GOTO1 VGETFLD,PARAS,DTRPERH                                            
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BE    SPERR                                                            
*                                                                               
         XC    DMCB+8(4),DMCB+8                                                 
         MVC   DMCB+8(1),PLANST+1  FISCAL YEAR START MONTH                      
         MVC   DMCB+9(1),CLTTYPE   FISCAL MONTH TYPE                            
         GOTO1 VMONVAL,DMCB,FLD,PLANST                                          
         MVI   ERROR,INVDATE                                                    
         OC    4(4,R1),4(R1)       TEST FOR ERROR                               
         BZ    SPERR                                                            
         MVC   PERIOD,4(R1)        EXTRACT PERIOD RETURNED BY MONVAL            
*                                                                               
         GOTO1 VPEROUT,PARAS,PERST,WORK                                         
         MVC   DTRPERN(6),WORK     DISPLAY START MONTH                          
         CLC   PERST,PEREND        TEST FOR SINGLE MONTH                        
         BE    VALHED16            YES-ALL DONE                                 
         MVI   DTRPERN+6,DASH      TACK ON -END MONTH                           
         GOTO1 VPEROUT,PARAS,PEREND,WORK                                        
         MVC   DTRPERN+7(6),WORK                                                
*                                                                               
* VALIDATE TEST OPTION                                                          
*                                                                               
VALHED16 MVI   TEST,NO                                                          
         GOTO1 VGETFLD,PARAS,DTRTESTH                                           
         CLI   FLDH+5,0                                                         
         BE    VALHED18                                                         
         MVI   ERROR,INVALID                                                    
         MVC   TEST,FLD                                                         
         CLI   TEST,YES                                                         
         BE    VALHED18                                                         
         CLI   TEST,NO                                                          
         BNE   SPERR                                                            
*                                                                               
* VALIDATE PRINT DETAILS OPTION                                                 
*                                                                               
VALHED18 MVI   DETAILS,YES                                                      
         GOTO1 VGETFLD,PARAS,DTRDETH                                            
         CLI   FLDH+5,0                                                         
         BE    VALHED20                                                         
         MVC   DETAILS,FLD                                                      
         CLI   DETAILS,YES                                                      
         BE    VALHED20                                                         
         CLI   DETAILS,NO                                                       
         BNE   SPERR                                                            
*                                                                               
* CHECK IF REQUEST ALLOWED TO RUN 'SOON'                                        
*                                                                               
VALHED20 TM    WHEN,X'20'          TEST IF REQUESTED 'SOON'                     
         BZ    VALHEDX             NO                                           
         MVI   ERROR,SOONERR                                                    
         LA    R2,CONWHENH                                                      
         CLI   TEST,NO             TEST FOR LIVE REQUEST                        
         BE    TRAPERR             YES-REJECT IT SOON                           
*                                                                               
VALHEDX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO BUILD A MONTH TABLE FOR REPORT PERIOD                          
*                                                                               
GENMON   ST    RE,SAVERE                                                        
         LA    RE,MONTAB           RE=A(MONTH TABLE ENTRY)                      
         MVC   0(2,RE),PERST       FIRST ENTRY IS PERIOD START                  
         MVC   HALF,PERST          HALF=NEXT MONTH WORK AREA                    
         ZIC   R1,PERST+1          R1=NEXT MONTH NUMBER                         
         MVI   BYTE,12             12 MONTHS FOR ALL FISCAL YEARS               
         CLI   CLTTYPE,10          EXCEPT TYPE 10 (444)                         
         BNE   *+8                                                              
         MVI   BYTE,13                                                          
         LA    RE,2(RE)            POINT RE AT NEXT ENTRY                       
*                                                                               
GENMON2  LA    R1,1(R1)            INCREMENT MONTH                              
         CLM   R1,1,BYTE           TEST IF PAST YEAR END                        
         BNH   GENMON3             NO                                           
         LA    R1,1                RESET MONTH TO ONE                           
         ZIC   RF,HALF                                                          
         LA    RF,1(RF)            INCREMENT YEAR                               
         STC   RF,HALF                                                          
*                                                                               
GENMON3  STC   R1,HALF+1           SET NEW MONTH                                
         CLC   HALF,PEREND         TEST IF PAST PERIOD END                      
         BH    GENMON4             YES-TABLE IS DONE                            
         MVC   0(2,RE),HALF                                                     
         LA    RE,2(RE)                                                         
         B     GENMON2                                                          
*                                                                               
GENMON4  LA    RF,MONTAB                                                        
         SR    RE,RF               RE=L'TABLE                                   
         SRL   RE,1                COMPUTES N'TABLE ENTRIES                     
         STC   RE,NMONTHS                                                       
*                                                                               
GENMONX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE A DATA TYPE CODE                                      
*                                                                               
* AT ENTRY, R1 = 0 FOR FROM DATA TYPE, 1 FOR TO DATA TYPE                       
*           FLD CONTAINS CODE TO VALIDATE                                       
* ON EXIT, CC=EQ FOR DATA TYPE CODE VALID, NEQ IF INVALID                       
*                                                                               
DTVAL    NTR1                                                                   
         STC   R1,BYTE             SAVE PARAMETER VALUE                         
         MVI   ERROR,LENERR                                                     
         CLI   FLDH+5,L'BUDTYP     TEST IF CODE IS TOO LONG                     
         BH    SPERR                                                            
*                                                                               
         MVC   AIO,AIO3            READ DATA TYPE RECORDS INTO IO3              
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BURECD,R4                                                        
         MVC   BUKEY,PLANKEY       SET PLAN KEY                                 
         MVI   BUDSUB,BUDSUBQ                                                   
         MVC   BUDTYP,FLD          SET DATA TYPE CODE                           
*                                                                               
DTVAL2   GOTO1 HIGH                                                             
         CLC   BUKEY,KEYSAVE       TEST IF DATA TYPE FOUND                      
         BE    DTVAL4                                                           
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'DTNFMSG),DTNFMSG                                          
         MVI   ERROR,SUPPLIED                                                   
         CLI   OFFLINE,NO          TEST IF ON-LINE                              
         BE    SPERR                                                            
         MVI   TERMSW,YES                                                       
         MVC   SVERRMSG,WORK                                                    
         B     DTVALX                                                           
*                                                                               
DTVAL4   GOTO1 GETREC                                                           
         MVI   ELCODE,BUPOLELQ                                                  
         L     R4,AIO              LOOK FOR POLISH FORMULA ELEMENT              
         BAS   RE,GETEL                                                         
         BNE   DTVAL6              NO-NOT A FORMULA DATA TYPE                   
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'DTFRMMSG),DTFRMMSG                                        
         MVI   ERROR,SUPPLIED                                                   
         CLI   OFFLINE,NO                                                       
         BE    SPERR                                                            
         MVI   TERMSW,YES          SET REPORT TERMINATED SWITCH                 
         MVC   SVERRMSG,WORK       SAVE THE ERROR MESSAGE                       
         B     DTVALX                                                           
*                                                                               
DTVAL6   BAS   RE,GETDT            EXTRACT VALUES FROM RECORD                   
         LA    RE,FRDTVALS         RE=A(DATA TYPE VALUES)                       
         LA    RF,FROMDT           RF=A(DATA TYPE CODE)                         
         CLI   BYTE,0              TEST FOR FROM DATA TYPE                      
         BE    *+12                                                             
         LA    RE,TODTVALS                                                      
         LA    RF,TODT                                                          
         MVC   0(DTVALLN,RE),DTVALS SLOT DATA TYPE VALUES                       
         MVC   0(L'BUDTYP,RF),FLD  SAVE DATA TYPE CODE                          
         B     DTVALX                                                           
*                                                                               
DTVALX   MVC   AIO,AIO1            RESTORE IO AREA POINTER                      
         CLI   TERMSW,NO           SET CC ON EXIT                               
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO GET VALUES FROM DATA TYPE RECORD                               
*                                                                               
GETDT    NTR1                                                                   
         L     R4,AIO                                                           
         XC    DTVALS(DTVALLN),DTVALS CLEAR DATA TYPE VALUES                    
         MVC   DTCODE,BUDTYP       DATA TYPE CODE                               
         MVC   DTHEAD1,SPACES                                                   
         MVC   DTHEAD2,SPACES                                                   
         MVI   ELCODE,BUDHELQ      FIRST HEADING                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUDHD,R6                                                         
         ZIC   R1,BUDHLEN                                                       
         SH    R1,=Y(BUDHEAD-BUDHD+1)                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DTHEAD1(0),BUDHEAD                                               
*                                                                               
         SR    R0,R0                                                            
*                                                                               
GETDT2   IC    R0,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
         CLI   0(R6),0             TEST FOR EOR                                 
         BE    GETDT3              YES                                          
         CLI   0(R6),BUDHELQ       TEST FOR HEADING ELEMENT                     
         BNE   GETDT2                                                           
*                                                                               
         ZIC   R1,BUDHLEN                                                       
         SH    R1,=Y(BUDHEAD-BUDHD+1)                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DTHEAD2(0),BUDHEAD                                               
*                                                                               
GETDT3   MVI   ELCODE,BUDTELQ      GET DATA TYPE ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUDTD,R6                                                         
*                                                                               
GETDT4   MVC   DTEX,BUDTEX                                                      
         MVC   DTCOL,BUDTCOL                                                    
         MVC   DTSC,BUDTSC                                                      
         MVC   DTDEC,BUDTDEC                                                    
         MVC   DTFORM,BUDTFORM     EXTRACT FORMAT OPTIONS                       
*                                                                               
GETDTX   B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO READ OUTLINE RECORDS AND PRINT REPORT                          
*                                                                               
* AT ENTRY, ASSUMES THAT READ ON PLAN OR OUTLINE HAS BEEN DONE                  
*                                                                               
RDPL     NTR1                                                                   
         LA    R1,RDPL2                                                         
         ST    R1,NDHOOK                                                        
         CLI   NDLEV,3             TEST IF PLAN WAS READ LAST                   
         BE    RDPL1               RE-READ OUTLINE                              
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
*                                                                               
RDPL1    GOTO1 VNODIO,DMCB,NODBLKD,=C'LSEQ',NODKEY,0                            
*                                                                               
RDPLX    B     XIT                                                              
         SPACE 2                                                                
* NODIO HOOK ROUTINE FOR READING OUTLINES                                       
*                                                                               
RDPL2    NTR1                                                                   
         CLI   NDMODE,NDPROC                                                    
         BNE   RDPLX                                                            
*                                                                               
         GOTO1 VGETVAL                                                          
*                                                                               
         MVI   PARENTSW,NO         FIND OUT IF OUTLINE IS PARENT                
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         CLI   OUTLEV,MAXOUTS      TEST IF AT LOWEST LEVEL                      
         BE    RDPL4               YES-CANNOT BE A PARENT                       
         LA    R3,NDLVTABL(R3)     POINT TO NEXT LOWER OUTLINE                  
         OC    NDLVNOD,NDLVNOD                                                  
         BZ    *+8                                                              
         MVI   PARENTSW,YES                                                     
*                                                                               
RDPL4    LA    R2,P                                                             
         USING PRTD,R2                                                          
         ZIC   R1,OUTLEV                                                        
         BCTR  R1,0                DEVELOP INDEX FOR OUTLINE LEVEL              
         SLL   R1,1                X 2 FOR INDENTATION                          
         LA    RE,PRTCODE(R1)                                                   
         MVC   0(L'OUTCODE,RE),OUTCODE                                          
         OC    PRTCODE,SPACES                                                   
         LA    RE,PRTNAME(R1)                                                   
         MVC   0(L'OUTNAME,RE),OUTNAME                                          
         GOTO1 SPOOL,PARAS,(R8)                                                 
*                                                                               
RDPL5    CLI   PARENTSW,YES        TEST IF OUTLINE IS PARENT                    
         BE    RDPLX               YES-EXIT                                     
         MVI   ELCODE,BUPOLELQ     SEARCH FOR POLISH FORMULA ELEM               
         L     R4,NDIOA                                                         
         BAS   RE,GETEL            SKIP HANDLING DATA RECORDS                   
         BE    RDPLX               IF OUTLINE HAS ROW FORMULA                   
*                                                                               
RDPL6    L     R3,NDLEVPTR         RESTORE LEVEL POINTER                        
         LA    R4,MONTAB           R4=A(MONTH)                                  
         ZIC   R0,NMONTHS          R0=LOOP COUNTER                              
*                                                                               
RDPL7    GOTO1 GETDATA,PARAS,NDLVKEY,(R4)                                       
         CLI   TEST,YES            SKIP REPLACE FOR TEST MODE                   
         BE    RDPL8                                                            
         GOTO1 REPLACE,PARAS,NDLVKEY,(R4)                                       
*                                                                               
RDPL8    CLI   DETAILS,NO          TEST FOR SUPPRESSING DETAILS                 
         BE    RDPL10              YES                                          
         GOTO1 PRTDET,PARAS,(R4)                                                
*                                                                               
RDPL10   LA    R4,L'MONTAB(R4)                                                  
         BCT   R0,RDPL7                                                         
*                                                                               
         GOTO1 SPOOL,PARAS,(R8)    SKIP A LINE AFTER DETAIL                     
*                                                                               
         B     RDPLX                                                            
         EJECT                                                                  
* HOOK ROUTINE FOR HEADLINE PRINTING                                            
*                                                                               
HOOK     NTR1                                                                   
         MVC   H4+10(3),CLTCODE                                                 
         OC    H4+10(3),SPACES                                                  
         MVC   H4+15(L'CLTNAM),CLTNAM                                           
*                                                                               
         MVC   H5+10(3),PRDCODE                                                 
         OC    H5+10(3),SPACES                                                  
         MVC   H5+15(L'PRDNAM),PRDNAM                                           
*                                                                               
         MVC   H6+10(3),PLANCODE                                                
         OC    H6+10(3),SPACES                                                  
         MVC   H6+15(L'PLANNAM),PLANNAM                                         
*                                                                               
HOOK1    MVC   H6+74(8),=C'TRANSFER'                                            
         GOTO1 VPEROUT,PARAS,PERST,WORK                                         
         MVC   H6+83(6),WORK       START OF PERIOD                              
         LA    R2,H6+90            R2=A(NEXT OUTPUT POSITION)                   
         CLC   PERST,PEREND        TEST START=END                               
         BE    HOOK2                                                            
         MVI   H6+89,DASH                                                       
         GOTO1 VPEROUT,PARAS,PEREND,WORK                                        
         MVC   H6+90(6),WORK       END OF PERIOD                                
         LA    R2,H6+97                                                         
*                                                                               
HOOK2    CLI   TEST,YES            TEST MODE                                    
         BNE   *+10                NO                                           
         MVC   0(4,R2),=C'TEST'                                                 
*                                                                               
HOOK3    LA    R2,H9+(PRTFROM-PRTD)                                             
         MVC   0(L'PRTFROM,R2),FRHEAD1                                          
         GOTO1 CENTER,PARAS,(R2),L'PRTFROM                                      
         LA    R2,L'P(R2)          NEXT PRINT LINE                              
         MVC   0(L'PRTFROM,R2),FRHEAD2                                          
         GOTO1 CENTER,PARAS,(R2),L'PRTFROM                                      
*                                                                               
         OC    FROMASAT,FROMASAT   TEST FOR SNAPSHOT DATE                       
         BZ    HOOK4                                                            
         LA    R2,L'P(R2)          NEXT PRINT LINE                              
         MVC   0(4,R2),=C'ASAT'    SHOW ASAT MMMDD/YY                           
         GOTO1 DATCON,DMCB,(3,FROMASAT),(5,8(R2))                               
*                                                                               
HOOK4    LA    R2,H9+(PRTTO-PRTD)                                               
         MVC   0(L'PRTTO,R2),TOHEAD1                                            
         GOTO1 CENTER,PARAS,(R2),L'PRTTO                                        
         LA    R2,L'P(R2)                                                       
         MVC   0(L'PRTTO,R2),TOHEAD2                                            
         GOTO1 CENTER,PARAS,(R2),L'PRTTO                                        
*                                                                               
HOOK6    ICM   R3,15,ABOX                                                       
         BZ    HOOKX                                                            
         USING BOXD,R3                                                          
*                                                                               
         MVI   BOXROWS+7,C'T'      SET UP FOR BOXES                             
         LA    R2,BOXROWS+10       POSITION FOR MIDDLE BOX LINE                 
         OC    FROMASAT,FROMASAT   TEST IF ASAT GIVEN                           
         BZ    *+8                                                              
         LA    R2,BOXROWS+11       YES-SO NEED ANOTHER HEAD LINE                
         MVI   0(R2),C'M'                                                       
         MVI   BOXROWS+58,C'B'                                                  
         LA    R2,BOXCOLS                                                       
         MVI   PRTLBOX-PRTD(R2),C'L'                                            
         MVI   PRTBOX1-PRTD(R2),C'C'                                            
         MVI   PRTBOX2-PRTD(R2),C'C'                                            
         MVI   PRTBOX3-PRTD(R2),C'C'                                            
         MVI   PRTRBOX-PRTD(R2),C'R'                                            
*                                                                               
HOOK8    MVI   BOXWT,1                                                          
         MVI   BOXYORN,YES                                                      
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
*                                                                               
HOOKX    B     XIT                                                              
         DROP  R3                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO INITIALIZE FOR PRINTING                                        
*                                                                               
INITPRT  LA    R1,HEDSPECS         SET UP FOR PRINTING                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO READ FROM AND TO DATA RECORDS FOR A MONTH                      
*                                                                               
* AT ENTRY,  P1 = A(OUTLINE KEY)                                                
*            P2 = A(MONTH)                                                      
* ON EXIT,   AIO2=A(FROM RECORD),AIO3=A(TO RECORD)                              
*            FROMNUM=FROM RECORD VALUE, TONUM=TO RECORD VALUE                   
*                                                                               
GETDATA  NTR1                                                                   
         LM    R2,R3,0(R1)         R2=A(OUTLINE KEY),R3=A(MONTH)                
         ZAP   FROMNUM,=P'0'                                                    
         MVI   FROMNUM,0                                                        
         ZAP   TONUM,=P'0'                                                      
         MVI   TONUM,0                                                          
*                                                                               
GETDATA1 XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BURECD,R4                                                        
         MVC   BUKEY,0(R2)                                                      
         MVI   BUVSUB,BUVSUBQ                                                   
         MVC   BUVDTYP,FROMDT      FROM DATA TYPE                               
         MVC   BUVPER,0(R3)        MONTH                                        
*                                                                               
GETDATA2 GOTO1 HIGH                                                             
         CLC   BUKEY,KEYSAVE                                                    
         BNE   GETDATA4                                                         
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         GOTO1 FINDNUM,PARAS,(C'F',AIO)                                         
*                                                                               
GETDATA4 XC    KEY,KEY                                                          
         MVC   BUKEY,0(R2)                                                      
         MVI   BUVSUB,BUVSUBQ                                                   
         MVC   BUVDTYP,TODT                                                     
         MVC   BUVPER,0(R3)                                                     
*                                                                               
GETDATA5 GOTO1 HIGH                                                             
         CLC   BUKEY,KEYSAVE                                                    
         BNE   GETDATAX                                                         
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         GOTO1 FINDNUM,PARAS,(C'T',AIO)                                         
*                                                                               
GETDATAX MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO FIND DATA ELEMENT ON A RECORD                                  
*                                                                               
* AT ENTRY, P1 BYTE   0  = F(FROM RECORD),T=(TO RECORD)                         
*              BYTES 1-3 = A(DATA RECORD)                                       
*                                                                               
FINDNUM  NTR1                                                                   
         MVC   BYTE,0(R1)                                                       
         L     R4,0(R1)                                                         
         USING BURECD,R4                                                        
         XC    FULL(3),FULL                                                     
         CLI   BYTE,C'T'           TEST FOR TO RECORD                           
         BE    FINDNUM2            YES                                          
*                                                                               
         MVC   FULL(3),FROMASAT                                                 
         OC    FULL(3),FULL        TEST FOR CURRENT DATA REQUESTED              
         BZ    FINDNUM2            YES                                          
         XC    FULL(3),EFFS        NO-COMPLEMENT DATE BEFORE SEARCH             
*                                                                               
FINDNUM2 LA    R6,BUFRSTEL-BUKEY(R4)                                            
         USING BUDAD,R6                                                         
         SR    R0,R0                                                            
         LA    R3,FROMNUM          R3=A(RECORD VALUE)                           
         CLI   BYTE,C'F'                                                        
         BE    *+8                                                              
         LA    R3,TONUM                                                         
*                                                                               
FINDNUM4 CLI   0(R6),0                                                          
         BE    FINDNUMX                                                         
         CLI   BUDAEL,BUDAELQ                                                   
         BNE   FINDNUM5                                                         
         CLC   BUDADATE,FULL                                                    
         BE    FINDNUM6                                                         
*                                                                               
FINDNUM5 IC    R0,BUDALEN                                                       
         AR    R6,R0                                                            
         B     FINDNUM4                                                         
*                                                                               
FINDNUM6 MVC   0(L'BUDAPREC,R3),BUDAPREC                                        
         MVC   L'BUDAPREC(L'BUDATA,R3),BUDATA                                   
*                                                                               
FINDNUMX B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* SUB-ROUTINE TO TRANSFER (REPLACE) DATA                                        
*                                                                               
* AT ENTRY,  P1 = A(OUTLINE KEY)                                                
*            P2 = A(MONTH)                                                      
*                                                                               
REPLACE  NTR1                                                                   
         LM    R2,R3,0(R1)         R2=A(OUTLINE KEY),R3=A(MONTH)                
         L     R4,AIO2             R4=A(BUPPER RECORD)                          
         USING BURECD,R4                                                        
         LR    RE,R4                                                            
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO2                                    
*                                                                               
REPLACE2 MVC   BUKEY,0(R2)         START WITH OUTLINE KEY                       
         MVI   BUVSUB,BUVSUBQ                                                   
         MVC   BUVDTYP,TODT                                                     
         MVC   BUVPER,0(R3)                                                     
         LA    R6,BUFRSTEL                                                      
         USING BUDAD,R6                                                         
         MVI   BUDAEL,BUDAELQ                                                   
         MVI   BUDALEN,BUDALNQ                                                  
         MVC   BUDAPREC(L'FROMNUM),FROMNUM SET FROM RECORD NUMBER               
         LA    R1,(BUFRSTEL-BUKEY)+BUDALNQ+1                                    
         STH   R1,BURLEN                                                        
*                                                                               
REPLACE4 XC    BUPBLOCK,BUPBLOCK                                                
         LA    RE,BUPBLOCK                                                      
         USING BUPBLKD,RE                                                       
         MVC   BUPADMGR,DATAMGR                                                 
         MVC   BUPAHELO,HELLO                                                   
         LR    R1,R4               BUPPER ASSUMES RECORD HEADER                 
         SH    R1,=H'4'                                                         
         ST    R1,BUPAREC                                                       
         MVI   BUPORIG,BUACTTRA    UPDATE ORIGIN IS TRANSFER                    
         MVC   BUPBDATE,BTODAY                                                  
         MVI   BYTE,BUPPUT                                                      
         GOTO1 VBUPPER,DMCB,(BYTE,BUPBLOCK)                                     
*                                                                               
REPLACEX B     XIT                                                              
         DROP  R6,RE                                                            
         EJECT                                                                  
* SUB-ROUTINE TO PRINT A DETAIL LINE                                            
*                                                                               
* AT ENTRY,  P1 = A(MONTH)                                                      
*            FROMNUM AND TONUM CONTAIN NUMBERS TO POST                          
*                                                                               
PRTDET   NTR1                                                                   
         L     R2,0(R1)            SAVE A(MONTH)                                
         LA    R3,P                R3=A(PRINT LINE)                             
         USING PRTD,R3                                                          
         GOTO1 VPEROUT,PARAS,(R2),WORK                                          
         MVC   PRTPER,WORK                                                      
*                                                                               
PRTDET2  XC    EBLOCK,EBLOCK                                                    
         MVI   EBLIN,L'BUDAPREC+L'BUDATA                                        
         MVI   EBTIN,C'S'          SCALED NUMBER                                
         MVI   EBOPT,EMINUS+EZERO                                               
         MVC   EBDECS,FRDEC                                                     
         MVC   EBSCOUT,FRSC                                                     
         LA    RE,FROMNUM          FROM NUMBER FIRST                            
         ST    RE,EBAIN                                                         
         MVC   EBSCIN,0(RE)        EXTRACT SCALE BYTE FROM VALUE                
         LA    RE,PRTFROM                                                       
         ST    RE,EBAOUT                                                        
         MVI   EBLOUT,L'PRTFROM                                                 
         GOTO1 VEDITOR,PARAS,EBLOCK                                             
*                                                                               
PRTDET4  MVC   EBDECS,TODEC                                                     
         MVC   EBSCOUT,TOSC                                                     
         LA    RE,TONUM            NOW EDIT OUT TO NUMBER                       
         ST    RE,EBAIN                                                         
         MVC   EBSCIN,0(RE)                                                     
         LA    RE,PRTTO                                                         
         ST    RE,EBAOUT                                                        
         GOTO1 VEDITOR,PARAS,EBLOCK                                             
*                                                                               
         GOTO1 SPOOL,PARAS,(R8)                                                 
*                                                                               
PRTDETX  B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* COMMON ROUTINES                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 1                                                                
SPERR    GOTO1 VCURSERR,PARAS,0                                                 
         SPACE 1                                                                
GETEL    LR    R0,RE                                                            
         GOTO1 HELLO,PARAS,(C'G',SYSFIL),(ELCODE,(R4)),0,0                      
         L     R6,12(R1)                                                        
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BR    RE                                                               
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
EFFS     DC    3X'FF'                                                           
DTNFMSG  DC    C'** ERROR- DATA TYPE IS NOT ON FILE **'                         
DTFRMMSG DC    C'** ERROR - DATA TYPE HAS A FORMULA **'                         
SNAPMSG  DC    C'** ERROR - NO SNAPSHOT TAKEN ON DATE INPUT **'                 
TERMMSG  DC    C'** REPORT ENDED DUE TO ERROR **'                               
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
*                                                                               
*&&US*&& SSPEC H1,2,C'ABC - ACCOUNT BUDGET SYSTEM'                              
*&&UK*&& SSPEC H1,2,C'ABS - ACCOUNT BUDGET SYSTEM'                              
         SSPEC H1,2,C'ABC - ACCOUNT BUDGET SYSTEM'                              
         SSPEC H2,2,C'---------------------------'                              
         SSPEC H1,45,C'DATA TRANSFER REPORT'                                    
         SSPEC H2,45,C'--------------------'                                    
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H5,2,C'PRODUCT'                                                  
         SSPEC H6,2,C'PLAN'                                                     
         SSPEC H4,75,REPORT                                                     
         SSPEC H4,87,RUN                                                        
         SSPEC H5,75,REQUESTOR                                                  
         SSPEC H5,93,PAGE                                                       
         SSPEC H10,16,C'OUTLINE CODE'                                           
         SSPEC H10,36,C'OUTLINE NAME'                                           
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER REPORT REQUEST SCREEN                                          
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILB2D                                                       
         EJECT                                                                  
* WORKING STORAGE VALUES                                                        
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
MYBASE   DS    A                                                                
VEDITOR  DS    V                   V(EDITOR)                                    
*                                                                               
TERMSW   DS    C                   Y=TERMINATE REPORT                           
TEST     DS    C                   TEST OPTION (Y/N)                            
DETAILS  DS    C                   PRINT DETAILS OPTION (Y/N)                   
*                                                                               
PARENTSW DS    C                   OUTLINE IS A PARENT (Y/N)                    
*                                                                               
PLANKEY  DS    CL(L'BUKEY)         PLAN DIRECTORY KEY                           
*                                                                               
FROMDT   DS    CL(L'BUDTYP)                                                     
FROMASAT DS    XL3                 FROM DATA TYPE AS AT DATE                    
TODT     DS    CL(L'BUDTYP)                                                     
*                                                                               
PERIOD   DS    0XL4                                                             
PERST    DS    XL2                 PERIOD START                                 
PEREND   DS    XL2                 PERIOD END                                   
NMONTHS  DS    X                   N'MONTHS IN PERIOD                           
MONTAB   DS    13XL2               REPORT MONTH TABLE                           
*                                                                               
DTVALS   DS    0C                  DATA TYPE RECORD VALUES                      
DTCODE   DS    CL(L'BUDTYP)                                                     
DTHEAD1  DS    CL20                HEADING 1+2 (SPACE PADDED)                   
DTHEAD2  DS    CL20                                                             
DTEX     DS    X                   EXTRACT TYPE                                 
DTCOL    DS    X                   COLUMN WIDTH                                 
DTSC     DS    X                   SCALE                                        
DTDEC    DS    X                   N'DECIMAL PLACES                             
DTFORM   DS    XL2                 FORMAT OPTIONS                               
DTVALLN  EQU   *-DTVALS            L'DATA TYPE RECORD VALUES                    
*                                                                               
FRDTVALS DS    0CL(DTVALLN)        FROM DATA TYPE VALUES                        
FRCODE   DS    CL(L'BUDTYP)                                                     
FRHEAD1  DS    CL20                HEADING 1+2 (SPACE PADDED)                   
FRHEAD2  DS    CL20                                                             
FREX     DS    X                   EXTRACT TYPE                                 
FRCOL    DS    X                   COLUMN WIDTH                                 
FRSC     DS    X                   SCALE                                        
FRDEC    DS    X                   N'DECIMAL PLACES                             
FRFORM   DS    XL2                 FORMAT OPTIONS                               
*                                                                               
TODTVALS DS    0CL(DTVALLN)        TO DATA TYPE VALUES                          
TOCODE   DS    CL(L'BUDTYP)                                                     
TOHEAD1  DS    CL20                HEADING 1+2 (SPACE PADDED)                   
TOHEAD2  DS    CL20                                                             
TOEX     DS    X                   EXTRACT TYPE                                 
TOCOL    DS    X                   COLUMN WIDTH                                 
TOSC     DS    X                   SCALE                                        
TODEC    DS    X                   N'DECIMAL PLACES                             
TOFORM   DS    XL2                 FORMAT OPTIONS                               
*                                                                               
FROMNUM  DS    CL7                 FROM RECORD NUMBER                           
TONUM    DS    CL7                 TO RECORD NUMBER                             
*                                                                               
       ++INCLUDE DDEBLOCK                                                       
*                                                                               
         DS    0D                                                               
BUPBLOCK DS    CL(BUPBLKL)         BUPPER BLOCK AREA                            
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
*                                                                               
         ORG   TWA1USER            SAVE AREA                                    
SVERRMSG DS    CL(L'WORK)          SAVED ERROR MESSAGE                          
NSNAPS   DS    X                   N'SNAPSHOTS                                  
SNAPLIST DS    XL255               SNAPSHOT LIST                                
*                                                                               
         SPACE 2                                                                
* DSECT TO COVER PRINT LINE                                                     
*                                                                               
PRTD     DSECT                                                                  
         DS    CL14                SPARE                                        
PRTLBOX  DS    C                   LEFT BOX                                     
PRTCODE  DS    CL18                                                             
         DS    C                   SPARE                                        
PRTBOX1  DS    C                   BOX COLUMN                                   
PRTNAME  DS    CL30                                                             
         DS    C                   SPARE                                        
PRTBOX2  DS    C                   BOX COLUMN                                   
PRTFROM  DS    CL13                FROM DATA                                    
         DS    C                   SPARE                                        
PRTBOX3  DS    C                   BOX COLUMN                                   
PRTTO    DS    CL13                TO DATA                                      
PRTRBOX  DS    C                   RIGHT BOX                                    
         ORG   PRTNAME+6                                                        
PRTPER   DS    CL6                 PERIOD EXPRESSION                            
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
COMMA    EQU   C','                                                             
EMINUS   EQU   X'40'               MINUS=YES                                    
EZERO    EQU   X'20'               ZERO=NOBLANK                                 
         SPACE 2                                                                
* BUPPERD                                                                       
         PRINT OFF                                                              
       ++INCLUDE BUPPERD                                                        
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006BUFIL31   05/01/02'                                      
         END                                                                    
