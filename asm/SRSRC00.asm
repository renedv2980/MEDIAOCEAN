*          DATA SET SRSRC00    AT LEVEL 029 AS OF 07/07/09                      
*PHASE T12A00A                                                                  
*                                                                               
* AHYD LVL 029 - EXTEND BITLIST STORAGE FROM 36 TO 72K                          
* AHYD LVL 027 - EXTEND BITLIST STORAGE FROM 30 TO 36K                          
* PCAS LVL 026 - EXTEND BITLIST STORAGE FROM 26 TO 30K                          
* PCAS LVL 025 - EXTEND BITLIST STORAGE FROM 24 TO 26K                          
* PCAS LVL 023 - EXTEND BITLIST STORAGE FROM 18 TO 24K                          
* PCAS LVL 022 - EXTEND BITLIST STORAGE FROM 16 TO 18K                          
*               (COPIED FROM UK VERSION LEVEL 30 CHANGED)                       
* CBLA LVL 021 - GESRCHBLKA-> GESRCHBLKD                                        
* DCHA LVL 020 - EXTEND BITLIST STORAGE FROM 12 TO 16K                          
* CBLA LVL 019 - STOP MULTI-SELECTS FROM SEARCH DISPLAY SCREEN                  
* CMOL LVL 018 - PASS LIMACC BLOCK TO SRCHEXEC IF REQUIRED                      
* TCLE LVL 017 - BUG AT LVL 15. FAILED TO SET TSFDATAL ON EXIT                  
* TCLE LVL 016 - SUPPORT =SEARCH/T=A,N                                          
* TCLE LVL 015 - ADD SUPPORT FOR TSFLEN                                         
* RMOR LVL 014 - REWRITE S/R VAALIDATION TO ALLOW FOR STEREO DATA               
* RMOR LVL 013 - FIX CHKPT DISPLACEMENTS FOR 18K TEMPSTRS                       
* TCLE LVL 012 - MOVE RETURN TO SERVICE SYS SO ALWAYS DONE ON EXIT              
* TCLE LVL 011 - SWITCH AREA NEEDS MORE SPACE                                   
* TCLE LVL 010 - SWITCH BACK TO CORRECT SYSTEM , NOT OVSYS                      
* DPEA LVL 009 - SWITCH BACK TO SYSTEM AFTER DICTATE CALL IF NECESSERY          
*                                                                               
         TITLE '$SEARCH - GENERALISED SEARCH CONTROLLER'                        
SRCH     CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKL,*$SEARCH,RA,RR=R2,CLEAR=YES                                 
         USING WRKD,RC             RC=A(W/S)                                    
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         ST    R2,RELO                                                          
         ST    R1,APARMS                                                        
         MVC   PARMS(PARMSL),0(R1)                                              
         USING TWAD,R8             R8=A(TWA)                                    
         L     R8,ATWA                                                          
         USING UTLD,R2             R2=A(UTL) DURING INIT ONLY                   
         L     R2,AUTL                                                          
*                                                                               
* EXTRACT SSB INFO AND LOCATE TCB                                               
*                                                                               
         L     R1,ASYSFAC                                                       
         L     R1,VSSB-SYSFACD(,R1)                                             
         MVC   DATEB,SSBDATEB-SSBD(R1)                                          
*                                                                               
         MVC   RECLEN,SSBTWAL-SSBD(R1)                                          
         MVC   CHKDSP,=Y(CHKPTDSP)                                              
         CLC   RECLEN,=H'14336'                                                 
         BNE   *+10                                                             
         MVC   CHKDSP,=H'12800'    CHECKPOINT STARTS AT 12.5K FOR 1.5K          
*                                                                               
         L     R1,SSBTKADR-SSBD(,R1) R1=A(TCB)                                  
         ST    R1,ATCB                                                          
*                                                                               
* SET ADDRESS OF SEQLIST IN TIA FOR FAST ACCESS                                 
*                                                                               
         L     R1,ATIA                                                          
         LA    R1,SEQLIST-TIAD(,R1)                                             
         ST    R1,ASEQLIST                                                      
*                                                                               
* CLEAR REPORT BLOCK                                                            
*                                                                               
         XC    REPTBLK,REPTBLK                                                  
*                                                                               
* INITIALIZE SEARCH BLOCK                                                       
*                                                                               
         USING SEARCHD,R9          R9=A(SRCHBLK)                                
         LA    R9,SRCHBLK                                                       
         XC    SEARCHD(SBSAVE-SEARCHD),SEARCHD CLEAR SEARCH BLOCK               
         XC    SBSAVE(SBLSAVE),SBSAVE                                           
         MVC   SBUSER,TUSER        CONNECTED USERID                             
         MVC   SBPASSWD,TPASSWD    PASSWORD ID (IF USED)                        
         MVC   SBAGY,TAGY          AGENCY ALPHA ID                              
         MVI   SBSVCREQ,1          INDICATE SERVICE REQUEST                     
         XC    FACAREA,FACAREA     CLEAR FACILITIES AREA                        
         LA    RE,FACAREA                                                       
         ST    RE,SBAPPFAC         PASS ADDRESS OF AREA TO SEARCH               
         MVC   SBOVSYS,TOVSYS      COPY CONNECTED SYSTEM                        
         MVC   SBCOMFAC,ACOMFACS                                                
         MVC   SBATWA,ATWA                                                      
         MVC   SBCTRY,TAGCTRY                                                   
         MVC   SBLANG,TLANG                                                     
*                                                                               
* LOCATE COMFACS ROUTINES                                                       
*                                                                               
         L     RE,ACOMFACS                                                      
         L     RF,CSEARCH-COMFACSD(,RE)                                         
         ST    RF,VSRCHXEC                                                      
         L     RF,CDICTATE-COMFACSD(,RE)                                        
         ST    RF,VDICTATE                                                      
         L     RF,CSWITCH-COMFACSD(,RE)                                         
         ST    RF,VSWITCH                                                       
*                                                                               
*        GET LANGUAGE DEPENDENT WORDS ETC.                                      
*                                                                               
         L     RF,CUNSCAN-COMFACSD(,RE)                                         
         MVC   WORK(40),SPACES                                                  
         MVI   WORK,C'X'                                                        
         MVI   WORK+20,C'X'                                                     
         GOTO1 (RF),DMCB,(2,WORK),(C'C',EXTAREA),C',=  '                        
         MVC   COMMA,EXTAREA+1                                                  
         MVI   EQUALS,C'='                                                      
*                                                                               
         GOTO1 VDICTATE,DMCB,C'LL  ',DDLCTBL,DDLCLST                            
         GOTO1 (RF),(R1),C'LU  ',DDUCTBL,DDUCLST                                
*                                                                               
* PRESET ADDRESS OF LAST FIELD TO TRANSMIT, NO CURSOR LOCATION                  
* AND PRESET MODE AND FIRST TIME INDICATORS                                     
*                                                                               
         LA    RE,SRVIDLH                                                       
         ST    RE,LASTFLDH                                                      
         XC    CURSORH,CURSORH                                                  
         MVI   MODE,MODESTAQ       RUNNING STAND ALONE                          
         MVI   FIRST,FIRSTQ        FIRST TIME IN                                
*                                                                               
* MUST BE CONNECTED TO SOMETHING SO SEARCH CAN DECIDE IF ACCESS TO              
* REQUIRED SYSTEM IS AUTHORISED.                                                
*                                                                               
         NC    TUSER,TUSER         MUST BE CONNECTED TO SOMETHING               
         BZ    ENCT                IF USING SEARCH                              
*                                                                               
* READ SERVICE SAVE PAGE INTO TIA AND COPY OUT LAST TIME VALUES.                
* NO NEED TO LOCK AT THIS POINT, WE'LL REREAD IT LATER, AS WE CAN'T             
* SPARE THE STORAGE TO HOLD IT                                                  
*                                                                               
GETSRPAG LA    R1,SRPAGENO         READ TEMPSTR SERVICE REQUEST PAGE            
         L     RF,ATIA             INTO TIA                                     
         BAS   RE,TEMPGET                                                       
         MVC   SVDATA,SR$SEAR-SRSD(RF) COPY SEARCH SAVED DATA                   
*                                                                               
* READ TWA 0 INTO DYNAMIC STORAGE. WE NEED IT FOR SYSTEM SWITCH DATA            
* AND MAY NEED TO LOOK AT AN INPUT FIELD IF XCTLED                              
*                                                                               
         CLC   =Y(TCBSWLEN*TCBSWMAX),=Y(L'SWAREA-1) TEST SWITCH AREA            
         BNH   *+6                 SIZE                                         
         DC    H'0'                DIE IF GROWN BIGGER THAN WORK AREA           
*                                                                               
         LH    R1,RECLEN           SIZE OF TWA 0                                
         BAS   RE,GETWRK           GET STORAGE FOR IT                           
         LR    R7,R0               R7=STORAGE AQUIRED                           
*                                                                               
         XR    R1,R1               READ TEMPSTR PAGE ZERO                       
         LR    RF,R7               INTO AQUIRED STORAGE                         
         BAS   RE,TEMPGET                                                       
*                                                                               
         XC    SWAREA(256),SWAREA  CLEAR SWITCH DATA AREA                       
         XC    SWAREA+256(L'SWAREA-256),SWAREA+256                              
         LR    RF,R7               COPY CHECKPOINTED SYSTEM SWITCH DATA         
         AH    RF,CHKDSP                                                        
         USING CHKPTD,RF           RF=A(TWA0 CHECKPOINT AREA)                   
         XR    R0,R0                                                            
         ICM   R0,1,CHTCBNUM       R0=N'SYSTEM SWITCH ENTRIES                   
         BZ    EAUT                NOT AUTHORISED IF NONE                       
         L     R1,ATCB                                                          
         STC   R0,TCBSWNUM-TCBD(,R1) STORE FOR SWITCH                           
         LA    RE,SWAREA           SWITCH TABLE WORK AREA                       
         ST    RE,SBASYSL          PASS ADDRESS TO SEARCH                       
         STC   R0,0(,RE)                                                        
         LA    RE,1(,RE)                                                        
         USING TCBSWTAB,RE         RE=A(SWITCH TABLE ENTRY)                     
         LA    RF,CHTCBTAB                                                      
         USING CHTCBTAB,RF         RF=A(TWA CHKPT TABLE)                        
SWTAB    NC    CHTCBAGB(5),CHTCBAGB IF AGENCY BIN AND ACCS ZERO,                
         BZ    SWTABNXT            ITS NOT FROM AUTHORISED SYSTEM LIST          
         MVC   TCBSWSYS,CHTCBSYS   COPY SWITCH ENTRIES FROM CHECKPOINT          
         MVC   TCBSWSOV,CHTCBSOV   AREA TO WORK AREA                            
         MVC   TCBSWAGB(5),CHTCBAGB                                             
SWTABNXT LA    RE,TCBSWLEN(RE)                                                  
         LA    RF,CHTCBLEN(RF)                                                  
         BCT   R0,SWTAB                                                         
         DROP  RE,RF                                                            
         EJECT                                                                  
* DETERMINE OPERATING MODE.                                                     
*                                                                               
* IF TTRCNT SAME AS SAVED TTRCNT+1, THIS IS A CONTINUATION, IE WE WERE          
* HERE LAST TIME, (FIRST=NEXTQ). ELSE ITS FIRST TIME (FIRST=FIRSTQ)             
*                                                                               
* IF FIRST TIME, CHECK SERVICE REQUEST FIELD TO SEE IF IT CONTAINS A            
* TEMPSTR PAGE NUMBER. IF IT DOES, WE HAVE BEEN INVOKED FROM AN                 
* APPLICATION PROGRAM (MODE=MODEXCTQ), OTHERWISE WE WERE INVOKED AS             
* A NORMAL STAND-ALONE SERVICE REQUEST, (MODE=MODESTAQ).                        
*                                                                               
* IF NOT FIRST TIME, DECIDE MODE ACCORDING TO WHETHER WE HAD A TEMPSTR          
* PAGE NUMBER LAST TIME                                                         
*                                                                               
         USING TIAD,R3             R3=A(TIA) DURING 'HOW' SECTION ONLY          
HOW      L     R3,ATIA                                                          
         LA    RE,SRVACT1H                                                      
         ST    RE,LASTFLDH                                                      
*                                                                               
         LH    R1,SVTTRCNT         GET LAST TERMINAL TRANSACTION COUNT          
         LA    R1,1(,R1)           ADD ONE                                      
         MVI   FIRST,NEXTQ         PRESET CONTINUATION FROM LAST TIME           
         CH    R1,TTRCNT           IF NOT SAME AS CURRENT COUNT                 
         BE    HOWNEXT                                                          
         MVI   FIRST,FIRSTQ        ITS NOT CONTINUATION SO SET FIRST            
         XC    SVDATA,SVDATA       AND CLEAR SAVED DATA                         
HOWNEXT  MVC   SVTTRCNT,TTRCNT     SAVE CURRENT COUNT                           
*                                                                               
HOWSRV   LA    RE,SRVSRV+7         SEE IF TWA PAGE NUMBER SUPPLIED              
         CLC   0(2,RE),=C'/A'      =SEARCH,N IS NORMAL FORMAT                   
         BNE   *+8                                                              
         LA    RE,2(,RE)           BUT ALLOW =SEARCH/A,N                        
         CLI   0(RE),C','                                                       
         BNE   HOWPAGE                                                          
         CLI   1(RE),C'0'          N MUST BE 1 TO 4                             
         BL    HOWPAGE                                                          
         CLI   1(RE),C'4'                                                       
         BH    HOWPAGE                                                          
         IC    R1,1(,RE)                                                        
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         MVC   0(2,RE),=C'  '      CLEAR ,N PAGE NUMBER                         
         CLI   FIRST,FIRSTQ                                                     
         BNE   HOWPAGE             USE SAVED VALUE IF NOT FIRST TIME            
         STC   R1,SVPAGENO                                                      
         MVI   FIRST,FIRSTQ        SET FIRST TIME IF ,N ENTERED                 
*                                                                               
* IF NON-ZERO PAGENUMBER, READ PAGE INTO TIA                                    
*                                                                               
HOWPAGE  CLI   SVPAGENO,0          IF NO TEMPSTR PAGE                           
         BE    HOWFIRST                                                         
         XR    R1,R1                                                            
         IC    R1,SVPAGENO         READ SPECIFIED TEMPSTR PAGE                  
         LR    RF,R3               INTO TIA                                     
         BAS   RE,TEMPGET                                                       
         CLC   TSIDENT,=AL4(TSIDENTQ) TEST IF VALID IDENT IN PAGE               
         BE    HOWFIRST            YES, SKIP                                    
         CLI   FIRST,FIRSTQ        TEST IF FIRST TIME IN                        
         BE    *+6                                                              
         DC    H'0'                NO, SO MUST HAVE A VALID IDENT               
         MVC   WORK,TIAD           YES, ASSUME OLD STYLE SRCHCALL               
         LA    RE,TSCALL                                                        
         LA    RF,TSCALLNQ                                                      
         XCEF                                                                   
         MVC   TSIDENT,=AL4(TSIDENTQ) WITH VALID IDENT IN PAGE                  
         MVC   TSID(L'TSID+L'TSFIELD),WORK AND ALL DATA PASSED BY OLD           
*                                                                               
* CLEAR SAVED STORAGE IF FIRST TIME                                             
*                                                                               
HOWFIRST CLI   FIRST,FIRSTQ                                                     
         BNE   HOWMODE                                                          
         LA    RE,TSSAVE                                                        
         LA    RF,TSSAVEL                                                       
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
* IF NO SAVED PAGE, WE ARE STAND ALONE                                          
*                                                                               
HOWMODE  CLI   SVPAGENO,0          IF NO TEMPSTR PAGE                           
         BE    HOWSTAL             WE ARE STAND ALONE                           
         EJECT                                                                  
* $SEARCH INVOKED BY XCTL FROM APPLICATION PROGRAM.                             
*                                                                               
* IF TRANSFERRED FROM AN APPLICATION, THE SEARCH ID, AND THE TWA0               
* OFFSET OF A FIELD CONTAINING THE INITIAL SEARCH KEY, ARE PASSED IN            
* THE GIVEN TEMPSTR PAGE. THE REST OF THIS PAGE IS USED BY $SEARCH TO           
* SAVE ITS STATUS BETWEEN SCREENS. THE INITIAL KEY IS USED ONLY FIRST           
* TIME IN, THEREAFTER IT MAY BE MODIFIED ON THE SCREEN.                         
* THE ID FIELD ON THE SCREEN IS NOT REQUIRED FOR THIS MODE, SO IT IS            
* PROTECTED AND USED TO SHOW THE DESCRIPTION OF THE SEARCH ITEM. ITS            
* PRECEDING PROMT FIELD IS ALSO BLANKED OUT.                                    
*                                                                               
HOWXCTL  MVI   MODE,MODEXCTQ                                                    
         OI    SRVIDH+1,X'28'      SET ID FIELD TO PROT, HIGH INT               
         XC    SRVID,SRVID         AND CLEAR IT.                                
*                                                                               
* CHECK IF SYSTEM SWITCH DATA GIVEN, MODIFY SWITCH TAB IF SO                    
*                                                                               
HOWXCSWD CLI   TSSYSWOV,0          TEST SWITCH DATA PRESENT                     
         BE    HOWXCKEY            NO, SKIP                                     
         LA    RE,SWAREA           SWITCH TABLE WORK AREA                       
         XR    R0,R0                                                            
         ICM   R0,1,0(RE)                                                       
         LA    RE,1(,RE)                                                        
         BZ    HOWXCS03            NO ENTRIES                                   
         USING TCBSWTAB,RE         RE=A(SWITCH TABLE ENTRY)                     
HOWXCS01 CLC   TCBSWSOV,TSSYSWOV   LOOK FOR MATCHING ENTRY                      
         BNE   HOWXCS02                                                         
         MVC   TCBSWSYS,TSSYSWSE   REPLACE WITH SWITCH DATA ENTRY               
         MVC   TCBSWAGB(5),TSSYSWAG                                             
         B     HOWXCS04                                                         
HOWXCS02 LA    RE,TCBSWLEN(RE)                                                  
         BCT   R0,HOWXCS01                                                      
HOWXCS03 MVC   TCBSWSOV,TSSYSWOV   BUILD NEW MATCHING ENTRY IF NONE             
         LA    RF,SWAREA                                                        
         IC    R0,0(,RF)                                                        
         AHI   R0,1                                                             
         STC   R0,0(,RF)                                                        
         B     HOWXCS01                                                         
HOWXCS04 XC    TSSYS,TSSYS                                                      
         MVC   TSSYS+L'TSSYS-1(1),TSSYSWOV FORCE GIVEN SYSTEM                   
         DROP  RE                                                               
*                                                                               
* FIRST TIME, GET INITIAL KEY FROM GIVEN TWA0 FIELD INTO SAVED INPUT            
* ALSO COPY FIELD ID TO OURS IF EXTENDED HEADER                                 
*                                                                               
HOWXCKEY CLI   FIRST,FIRSTQ                                                     
         BNE   HOWEND              SKIP IF NOT FIRST TIME                       
         TM    TSOPTS,TSOPTLAQ     LIMIT ACCESS BLOCK PASSED?                   
         BO    *+10                                                             
         XC    TSLIMBLK,TSLIMBLK   NO - CLEAR LIMACC BLOCK TO NULLS             
*                                                                               
         LR    R6,R7               COPY ADDRESS OF TWA0 PAGE                    
         AH    R6,TSFIELD          R6=FIELD CAUSING SEARCH                      
         MVC   TSFRRCC,2(R6)       SAVE ROW/COL FOR GOBACK                      
         XR    R1,R1                                                            
         TM    1(R6),X'02'         EXTENDED HEADER                              
         BZ    HOWXCK01            NO                                           
         IC    R1,0(R6)            GET FIELD LENGTH                             
         SHI   R1,8                                                             
         LA    RE,0(R1,R6)         POINT TO EXTENDED HEADER                     
         MVC   SRVFILTX(1),0(RE)   COPY ID TO OUR FILTER FIELD                  
HOWXCK01 ICM   R1,1,5(R6)          GET HIS INPUT LENGTH                         
         BZ    HOWEND              NOTHING THERE (??)                           
         STC   R1,TSFDATAL         STORE LENGTH AS INPUT                        
         BCTR  R1,0                                                             
         EX    R1,*+8              SAVE DATA AS INPUT                           
         B     *+10                                                             
         MVC   TSFDATA(0),8(R6)    *EXECUTED*                                   
         LA    R1,1(,R1)                                                        
         XR    RE,RE                                                            
         IC    RE,TSFDISP          GET DISP TO SEARCH DATA IN FIELD             
         SR    R1,RE               ADJUST LENGTH BY START DISP                  
         BNP   HOWEND              NO SEARCH DATA (??)                          
         CLI   TSFLEN,0            WAS A LENGTH GIVEN                           
         BE    HOWXCK02            NO, USE ALL OF REMAINDER                     
         CLM   R1,1,TSFLEN         IS GIVEN LENGTH LESS THAN REMAINDER          
         BH    *+12                YES, SKIP                                    
         MVI   TSFLEN,0            IGNORE IT AND USE ALL OF REMAINDER           
         B     HOWXCK02                                                         
         IC    R1,TSFLEN           USE ONLY LENGTH SPECIFIED                    
HOWXCK02 LA    RE,8(RE,R6)         POINT TO START OF DATA                       
         LA    RF,L'TSFILTER-1     GET CAPACITY OF OUR FILTER FIELD             
         CR    R1,RF                                                            
         BNH   *+6                                                              
         LR    R1,RF               IF HIS INPUT TOO LONG, TRUNCATE              
         STC   R1,TSFILTL          SAVE LENGTH                                  
         BCTR  R1,0                                                             
         EX    R1,*+8              COPY CALLERS SEARCH KEY DATA                 
         B     HOWEND                                                           
         MVC   TSFILTER(0),0(RE)   *EXECUTED*                                   
         EJECT                                                                  
* $SEARCH INVOKED DIRECTLY AS A NORMAL SERVICE REQUEST (STAND ALONE)            
*                                                                               
* IF INVOKED STAND ALONE, BOTH THE KEY AND ID INFORMATION COME FROM THE         
* SCREEN. BECAUSE NO TEMPSTR PAGE IS AVAILABLE, ONLY THE LIMITED SPACE          
* IN THE SERVICE REQUEST TEMPSTR PAGE IS AVAILABLE TO SAVE THE STATUS.          
*                                                                               
HOWSTAL  LA    RE,TIAD                                                          
         LA    RF,TSSAVED+TSSAVEL                                               
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVC   TSID,SPACES         CLEAR ID, AS INPUT LATER                     
         MVC   TSIDLANG,SBLANG     ID WILL BE IN CONNECTED LANGUAGE             
         CLI   FIRST,FIRSTQ                                                     
         BE    HOWEND              IF FIRST, DON'T COPY SAVED DATA              
*                                                                               
         MVC   TSNDXS(9),SVNDXS    NEXT, PREVIOUS AND MAX INDEXES               
         XR    R1,R1                                                            
         ICM   R1,1,SVSASAVE       LENGTH SAVED 'OTHERS' FIELD                  
         BZ    HOWST10                                                          
         LR    RF,R1                                                            
         BCTR  RF,0                                                             
         MVC   TSOTHER(0),SVSASAVE+1 COPY SAVED 'OTHERS' FIELD                  
         EX    RF,*-6                                                           
                                                                                
HOWST10  LA    RE,SVSASAVE+1(R1)   POINT TO NEXT BIT                            
         ICM   R1,1,0(RE)          LENGTH SAVED FILTERS                         
         BCTR  R1,0                                                             
         MVC   TSFILTER(0),1(RE)   COPY SAVED FILTERS                           
         EX    R1,*-6                                                           
         MVC   TSFILTL,0(RE)       AND LENGTH                                   
         DROP  R2                  FINISHED WITH UTL                            
*                                                                               
* FREE STORAGE CONTAINING TWA 0 COPY                                            
*                                                                               
HOWEND   BAS   RE,PUTWRK           FREE STORAGE CONTAINING TWA0 COPY            
         EJECT                                                                  
* ANALYSE PF KEY IF USED, AND MOVE CORRESPONDING COMMAND TO FIELD               
* WE RECOGNISE ONLY HELP AND BACKWARD SCROLL, ALL OTHER KEYS WILL               
* GIVE FORWARD SCROLL IF KEY NOT CHANGED.                                       
* HELP IS HANDLED BY BUNGING '?' IN INPUT FIELD ON LINE WHERE CURSOR IS         
*                                                                               
PFCHK    MVC   SBID,TSID           COPY SEARCH ID TO SEARCH BLOCK               
         MVC   SBIDLANG,TSIDLANG                                                
*                                                                               
         L     R2,ATIOB            R2=A(TIOB)                                   
         USING TIOBD,R2                                                         
         ZIC   R0,TIOBAID          COPY PF KEY                                  
         LA    RE,12                                                            
         CR    R0,RE               IS IT PF13-24                                
         BNH   *+6                                                              
         SR    R0,RE               YES, CHANGE TO PF1-12                        
         STC   R0,PFKEY                                                         
         LH    R1,TIOBCURS         ABSOLUTE ADDRESS OF CURSOR                   
         LA    R1,1(,R1)           +1 SO 1ST COLUMN EQUALS 1ST FIELD            
         DROP  R2                                                               
*                                                                               
         CLI   FIRST,FIRSTQ        IGNORE PF KEY IF FIRST TIME                  
         BE    VID                                                              
         CLI   MODE,MODEXCTQ       IF NOT XCTLED MODE,                          
         BNE   PFCHKPRV            IGNORE RETURN                                
         CLI   PFKEY,PFRETURN      RETURN (WITHOUT SELECT)                      
         BE    SELECT              YES, TAKE SELECT/RETURN EXIT                 
*                                                                               
PFCHKPRV LA    RE,SRVACT1H         SCROLL COMMANDS GO HERE                      
         XR    R0,R0                                                            
         IC    R0,SR@PREV                                                       
         CLI   PFKEY,PFPREV        SCROLL BACKWARD                              
         BE    PFSET               YES, SET IT UP                               
*                                                                               
         CLI   PFKEY,PFPRINT       PRINT REQUEST ?                              
         BE    VID                 YES DEAL WITH IT LATER                       
*                                                                               
PFCHKHLP CLI   PFKEY,PFHELP        HELP                                         
         BNE   VID                 NO, ALL ELSE GIVES SCROLL FORWARD            
         LA    R0,C'?'                                                          
         LA    RE,SRVIDH                                                        
         CLM   R1,3,SRVIDLH+2                                                   
         BL    VID                 IGNORE PF1 IF BEFORE ID FIELD LINE           
         CLM   R1,3,SRVFILLH+2                                                  
         BL    PFSET               ID HELP IF ON ID LINE                        
         LA    RE,SRVFILTH                                                      
         CLM   R1,3,SRVACT1H+2                                                  
         BL    PFSET               FILTER HELP IF ON FILT LINE                  
         CLI   SVNDETL,0                                                        
         BE    PFSET               OR IF NO DATA ON SCREEN                      
*                                                                               
         LA    RE,SRVACT1H         FIRST ACTION FIELD                           
         CLI   MODE,MODESTAQ       IF STAND ALONE,                              
         BE    PFSET               ONLY FIRST LINE HAS HELP                     
         LA    RF,TSSCRNDX+L'TSSCRNDX SCREEN INDEX OF SECOND ITEM               
PFCHKACT CLI   0(RF),X'FF'         CURSOR AFTER LAST LINE, USE LAST             
         BE    PFSET                                                            
         CLM   R1,3,3(RF)          CHECK CURSER AGAINST NEXT LINE ADDR          
         BL    PFSET               HELP ON THIS LINE IF LOW                     
         LA    RE,SRVACT2H-SRVACT1H(,RE)                                        
         LA    RF,L'TSSCRNDX(,RF)                                               
         B     PFCHKACT            CHECK EVERY USED LINE                        
*                                                                               
PFSET    XR    R1,R1                                                            
         IC    R1,0(,RE)           LENGTH OF FIELD                              
         SHI   R1,9                FIELD LENGTH ADJUSTED FOR XC                 
         TM    1(RE),X'02'                                                      
         BZ    *+8                                                              
         SHI   R1,8                ADJUST IF EXTENDED HEADER                    
         XC    8(0,RE),8(RE)       CLEAR FIELD                                  
         EX    R1,*-6                                                           
         STC   R0,8(,RE)           SET GENERATED CHARACTER IN FIELD             
         MVI   5(RE),1             SET I/P LENGTH=1                             
         MVI   4(RE),X'80'         SET I/P THIS TIME                            
         DROP  R3                  DROP TIA DSECT FOR TIME BEING                
         EJECT                                                                  
***********************************************************************         
* IF STAND ALONE, ANALYSE SEARCH ID FIELD INTO SEARCH BLOCK                     
* FORMAT IS SYSTEM(1-6)/SEARCH(1-8)/OTHER(0-18)                                 
***********************************************************************         
VID      CLI   MODE,MODESTAQ       IS IT STAND ALONE ?                          
         BNE   VFILT               NO, ID NOT INPUT ON SCREEN                   
*                                                                               
         LA    R2,SRVIDH                                                        
         CLI   5(R2),0             IF ID INPUT                                  
         BNE   VIDHELP             SEE WHAT IT IS                               
         NC    SVDDIRS,SVDDIRS     IF NO ID, AND NONE FROM LAST TIME            
         BZ    EMIF                MISSING INPUT                                
         XC    SBID(L'SBSYSTEM+L'SBSEARCH),SBID  ELSE ASSUME SAME AS            
         MVC   SBDDIRS,SVDDIRS     LAST, AND PASS SYS/SRCH IN DISP FORM         
         B     VFILT                                                            
*                                                                               
VIDHELP  CLI   SRVID,C'?'                                                       
         BNE   VIDSTART                                                         
         XC    SRVID,SRVID                                                      
         MVC   SRVID(HELPIDLQ),HELPID                                           
         OI    SRVIDH+1,X'40'      SET LOWER CASE TEMPORARILY                   
         B     EHLP                                                             
*                                                                               
VIDSTART XR    R4,R4                                                            
         IC    R4,5(R2)            LENGTH OF INPUT                              
         LA    R3,8(R2)            FIRST BYTE OF ID                             
         MVC   SBID,SPACES                                                      
VIDSYS   MVCDD SBOPTTXT(10),SR#SYS                                              
         MVI   GTLTXT-GTBLOCK+SBTXTBLK,10                                       
         LA    RE,SBOPTTXT                                                      
         STCM  RE,B'0111',GTATXT-GTBLOCK+SBTXTBLK                               
         MVI   GTINDX-GTBLOCK+SBTXTBLK,1                                        
         LR    RE,R3               SAVE START OF SYSTEM FIELD                   
VIDSYSA  CLI   0(R3),C'/'          IS THIS A SEPERATOR                          
         BE    VIDSYSB             YES - GO MOVE IN SYSTEM                      
         LA    R3,1(,R3)                                                        
         BCT   R4,VIDSYSA          LOOP TILL END                                
VIDSYSB  LR    RF,R3               COPY END ADDRESS                             
         SR    RF,RE               GET LENGTH                                   
         BZ    VIDSYSC             SYSTEM NOT ENTERED                           
         CH    RF,=Y(L'SBSYSTEM)                                                
         BH    EFTL                TOO LONG                                     
         BCTR  RF,0                                                             
         MVC   SBSYSTEM(0),0(RE)                                                
         EX    RF,*-6                                                           
                                                                                
VIDSYSC  LTR   R4,R4                                                            
         BZ    VFILT               SKIP IF NO MORE                              
         LA    R3,1(,R3)           PASS /                                       
         BCT   R4,VIDSRCH                                                       
         B     VFILT                                                            
*                                                                               
VIDSRCH  MVCDD SBOPTTXT(10),SR#SRCH                                             
         MVI   GTINDX-GTBLOCK+SBTXTBLK,2                                        
         LR    RE,R3               SAVE START OF SYSTEM FIELD                   
VIDSRCHA CLI   0(R3),C'/'          IS THIS A SEPERATOR                          
         BE    VIDSRCHB            YES - GO MOVE IN SEARCH                      
         LA    R3,1(,R3)                                                        
         BCT   R4,VIDSRCHA         LOOP TILL END                                
VIDSRCHB LR    RF,R3               COPY END ADDRESS                             
         SR    RF,RE               GET LENGTH                                   
         BZ    VIDSRCHC            SEARCH NOT ENTERED                           
         CH    RF,=Y(L'SBSEARCH)                                                
         BH    EFTL                TOO LONG                                     
         BCTR  RF,0                                                             
         MVC   SBSEARCH(0),0(RE)                                                
         EX    RF,*-6                                                           
                                                                                
VIDSRCHC XC    GTLTXT-GTBLOCK+SBTXTBLK(4),GTLTXT-GTBLOCK+SBTXTBLK               
         MVI   GTINDX-GTBLOCK+SBTXTBLK,0                                        
         LTR   R4,R4                                                            
         BZ    VFILT               SKIP IF NO MORE                              
         LA    R3,1(,R3)           PASS /                                       
         BCT   R4,VIDOTH                                                        
         B     VFILT                                                            
*                                                                               
VIDOTH   CHI   R4,L'SBOTHER                                                     
         BH    EFTL                TOO LONG                                     
         BCTR  R4,0                                                             
         MVC   SBOTHER(0),0(R3)                                                 
         EX    R4,*-6                                                           
         EJECT                                                                  
**************************                                                      
* VALIDATE FILTER FIELD  *                                                      
**************************                                                      
VFILT    LA    R2,SRVFILTH                                                      
         CLI   PFKEY,PFPRINT       IS IT PRINT REQUEST BY PF KEY                
         BE    VFILTPRT            YES, GO PROCESS IT                           
         CLI   5(R2),0             IF NO INPUT                                  
         BE    VFILTRES            RESTORE WHAT WAS THERE BEFORE                
*                                                                               
* SEE IF PRINT REQUEST, AND DETERMINE ID TO USE FOR REPORT ON PRINT Q           
*        THE DEFAULT REPORT ID IS 'SCH', AND IS USED IF THE PRINT               
*        PF KEY IS USED AND/OR 'PRI(NT)' IS ENTERED IN SRVFILT. TO              
*        OVERRIDE THE DEFAULT, THE USER MUST ENTER 'PRI(NT)=XXX' IN             
*        SRVFILT (WITH OR WITHOUT PRINT PF KEY). THE ID IS TAKEN TO             
*        BE THE NEXT 3 CHARS FOLLOWING THE '=', OR UP TO THE END OF             
*        INPUT OR THE FIRST COMMA IF LESS THAN 3 CHARS.                         
*                                                                               
VFILTPRT LA    R3,L'SR@PRINT       SET UP FOR CLCL SRVFILT(5),=C'PRINT'         
         LA    RE,8(,R2)                                                        
         LR    RF,R3                                                            
         LA    R0,SR@PRINT                                                      
         LR    R1,R3                                                            
         CLCL  RE,R0                                                            
         BE    VFILTP05            1ST N CHARS OF SRVFILT ARE 'PRINT'           
         CHI   RF,3                RF=R3-NUMBER OF EQUAL CHARS (N'EQ)           
         BNL   VFILTP10            MUST BE AT LEAST 3 EQUAL - 'PRI(NT)'         
*                                                                               
VFILTP05 IC    R1,5(,R2)           LENGTH OF DATA INPUT (N'IN)                  
         AR    R1,RF               R1=N'IN-N'EQ+R3                              
         SR    R1,R3               R1=N'IN-N'EQ+(R3-R3), IE LENGTH LEFT         
         BZ    VFILTP40            IF NONE, USE DEFAULT PRINTID.                
         CLC   0(1,RE),EQUALS      RE=FIRST UNEQUAL CHARACTER IN INPUT          
         BE    VFILTP20            MUST BE EQUALS SIGN - 'PRI(NT)=XXX'          
*                                                                               
VFILTP10 CLI   PFKEY,PFPRINT       CAN'T ASSUME PRINT COMMAND ON SCREEN         
         BNE   VFILTEQ             BUT IF PRINT PF KEY USED                     
         B     VFILTP40            DO PRINT WITH DEFAULT ID.                    
*                                                                               
VFILTP20 LA    R0,L'PRINTID        LENGTH OF PQ ID                              
         BCTR  R1,0                LENGTH OF DATA AFTER 'PRI(NT)='              
         CR    R0,R1                                                            
         BNH   *+6                                                              
         LR    R0,R1               USE DATA LENGTH IF < L'PRINTID               
         MVC   WORK(3),=C' ..'                                                  
         LA    RF,WORK                                                          
VFILTP25 CLC   1(1,RE),COMMA       COPY ID UPTO END OF FIELD                    
         BE    VFILTP30                                                         
         MVC   0(1,RF),1(RE)                                                    
         LA    RF,1(,RF)                                                        
         LA    RE,1(,RE)                                                        
         BCT   R0,VFILTP25                                                      
VFILTP30 CLI   WORK,C' '           IF STILL NO ID, USE DEFAULT                  
         BNH   VFILTP40                                                         
         MVC   PRINTID,WORK        ELSE COPY TO PRINTID TO CAUSE PRINT          
         B     VFILTP50                                                         
*                                                                               
VFILTP40 MVC   PRINTID,=C'SCH'     DEFAULT PRINTID=SCH                          
VFILTP50 CLI   SVNDETL,0           IF NO DATA ON SCREEN (NOTHING TO             
         BNE   VFILTRES            PRINT), IGNORE PRINT REQUEST                 
         XC    PRINTID,PRINTID                                                  
*                                                                               
* RESTORE PREVIOUS CONTENTS OF FIELD                                            
*                                                                               
VFILTRES L     R1,ATIA                                                          
         MVC   SRVFILT,TSFILTER-TIAD(R1) COPY WHAT WAS SAVED                    
         MVC   SRVFILTH+5(1),TSFILTL-TIAD(R1)                                   
*                                                                               
* SEE IF FIELD STARTS '='                                                       
*                                                                               
VFILTEQ  CLI   8(R2),C'='                                                       
         BNE   VFILTSRC                                                         
         MVC   8(L'SRVFILT-1,R2),9(R2) SHIFT INPUT ONE LEFT                     
         XR    R1,R1                                                            
         IC    R1,5(,R2)                                                        
         BCTR  R1,0                ADJUST INPUT LENGTH                          
         STC   R1,5(,R2)                                                        
         MVI   SRVFILT+L'SRVFILT-1,0 ZERO LAST BYTE                             
         CLI   5(R2),0                                                          
         BNE   VFILTEQ             GO ROUND AGAIN                               
         B     EMIF                UNLESS NOTHING LEFT                          
*                                                                               
* CALL SEARCH EXEC TO ANALYSE APPLICATION DEPENDENT SEARCH KEY/FILTER           
*        FIELD INTO SEARCH BLOCK. THIS ALSO VALIDATES SEARCH ID WHICH           
*        WE'VE ALREADY PUT IN THE BLOCK. IF STAND ALONE, ECHO FULL              
*        SEARCH ID BACK TO SCREEN                                               
*                                                                               
VFILTSRC MVI   SBSTYLI,SBSTVALQ    STYLE IS VALIDATE                            
         MVI   SBSTYLO,0           OUTPUT STYLE ZERO SO VALIDATE ONLY           
         XC    SBEXHEAD(2),SBEXHEAD REQUEST EXTRACT HEADER                      
         L     RE,ATIA                                                          
         USING TIAD,RE                                                          
         LA    R4,TSLIMBLK         PASS LIMACC BLOCK TO SRCHEXEC                
         DROP  RE                                                               
         GOTO1 VSRCHXEC,DMCB,(R9),(R2),(R4)          VALIDATE CALL              
         BNE   *+12                SKIP IF ERROR                                
         MVI   SBRETURN,0          MAKE SURE NO ERRROR CODE SET                 
         B     VFILTS10                                                         
         CLI   SBRETURN,SBEHLPQ    WAS IT HELP REQUEST                          
         BNE   VFILTS10            NO, SKIP                                     
         CLI   FIRST,FIRSTQ                                                     
         BNE   VFILTS10            SKIP IF NOT FIRST TIME                       
         L     RE,ATIA                                                          
         USING TIAD,RE                                                          
         MVI   TSFILTL,0           CLEAR SAVED STUFF IF FIRST                   
         XC    TSFILTER,TSFILTER   SO DOESN'T REPEAT                            
         MVI   TSNAMES,X'FF'       FORCE MISMATCH ON NAMES NEXT TIME            
         DROP  RE                                                               
*                                                                               
VFILTS10 TM    SBSTYLO,SBSTDFLQ    DID APPLICATION REQUEST DIRECTORY            
         BZ    *+8                 FILTERING? SKIP IF NO                        
         OI    DIRFLAG,FILTERQ     INDICATE DIRECTORY FILTERING                 
*                                                                               
* IF XCTLED, DISPLAY SEARCH TITLE IN ID FIELD                                   
*                                                                               
OID      XC    SRVID,SRVID         EXPAND FULL ID TO SCREEN                     
         MVI   DDFSYS,0            USE SDOVSYS                                  
         GOTO1 DDFTWA,SRVFILTH     SORT OUT ANY RETURNED DICT ITEMS             
         CLI   MODE,MODEXCTQ                                                    
         BNE   OIDSY               SKIP IF STAND ALONE                          
         LA    RF,L'SRVID          COPY TITLE TO ID FIELD                       
         XR    RE,RE                                                            
         ICM   RE,1,SBEXHTIL         LENGTH OF SEARCH TITLE                     
         BZ    KEYCHK                                                           
         CR    RE,RF                                                            
         BNH   *+6                                                              
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         MVC   SRVID(0),SBEXHTIT                                                
         EX    RE,*-6                                                           
         B     KEYCHK                                                           
*                                                                               
* IF STAND ALONE, EXPAND COMPLETE SEARCH ID                                     
*                                                                               
OIDSY    MVC   SRVID(L'SBSYSTEM),SBSYSTEM PUT IN SYSTEM CODE                    
         CLC   SBSEARCH,SPACES                                                  
         BNE   OIDSY1                                                           
         CLC   SBOTHER,SPACES                                                   
         BE    KEYCHK              EXIT IF NO SEARCH OR OTHER ENTERED           
OIDSY1   LA    R3,SRVID+L'SBSYSTEM-1 FIND END OF SYSTEM CODE                    
OIDSY2   CLI   0(R3),C' '                                                       
         BH    OIDSE                                                            
         BCT   R3,OIDSY2                                                        
*                                                                               
OIDSE    MVI   1(R3),C'/'                                                       
         MVC   2(L'SBSEARCH,R3),SBSEARCH PUT IN SEARCH CODE                     
         CLC   SBOTHER,SPACES                                                   
         BE    KEYCHK              EXIT IF NO OTHER ENTERED                     
OIDSE1   LA    R3,L'SBSEARCH-1+2(R3) FIND END OF SYSTEM CODE                    
OIDSE2   CLI   0(R3),C' '                                                       
         BH    OIDOTH                                                           
         BCT   R3,OIDSE2                                                        
*                                                                               
OIDOTH   MVI   1(R3),C'/'                                                       
         MVC   2(L'SBOTHER,R3),SBOTHER PUT IN OTHER                             
         EJECT                                                                  
* SEE IF KEY AND ID ARE SAME AS LAST TIME. IF NOT, FORCE TO FIRST               
* MODE. IF 'FIRST' MODE, SAVE STATUS IN TIA AND START NEW SEARCH                
* IF ONLY USER FILTERS DIFFERENT, NO NEED TO RESEARCH, EXISTING                 
* SEQLIST CAN BE USED.                                                          
*                                                                               
KEYCHK   CLI   SBRETURN,0          ANY ERROR RETURN FROM SEARCH ID/KEY          
         BE    KEYCHK1             NO, SEE IF KEY HAS CHANGED                   
         CLI   MODE,MODESTAQ       IF STAND ALONE MODE,                         
         BNE   *+10                                                             
         XC    SBID,SBID           FORCE KEY NOT EQUAL FOR NEXT TIME            
         B     SRCHERR             GO DISPLAY ERROR                             
*                                                                               
KEYCHK1  L     R3,ATIA                                                          
         USING TIAD,R3                                                          
         CLI   FIRST,FIRSTQ                                                     
         BE    KEYFIRST            SKIP ALL THIS IF ALREADY FIRST TIME          
*                                                                               
         CLI   MODE,MODESTAQ       IF STAND ALONE MODE,                         
         BNE   KEYCHK2                                                          
         CLC   SVDDIRS,SBDDIRS     CHECK IF CHANGED SYSTEM/SEARCH               
         BNE   KEYFIRST                                                         
         CLC   TSOTHER,SBOTHER     OR 'OTHER' DATA                              
         BNE   KEYFIRST                                                         
         CLC   TSFILTL,SRVFILTH+5  OR FILTER DATA AS INPUT                      
         BNE   KEYFIRST                                                         
         CLC   TSFILTER,SRVFILT                                                 
         BNE   KEYFIRST                                                         
         MVC   TSID,SBID           COPY ID, NOT REBUILT STAND ALONE             
         MVC   TSIDLANG,SBIDLANG                                                
         MVC   TSNAMES,SBNAMES                                                  
         MVC   TSASAVE,SBASAVE                                                  
         B     VACTS               CHECK SUBACTION IF ALL SAME AS LAST          
*                                                                               
KEYCHK2  CLI   TSSEQLOK,0          IF NOT STAND ALONE, ENSURE SEQUENCE          
         BE    KEYFIRST            LIST WAS VALID FROM LAST TIME                
         CLC   SBNAMES,TSNAMES     IF YES, SEE IF PARSED NAME REMAINS           
         BNE   KEYFIRST            EFFECTIVELY SAME                             
         MVC   TSFILTL,SRVFILTH+5  SAVE ACTUAL NAME (MAY HAVE CHANGED           
         MVC   TSFILTER,SRVFILT    EVEN IF PARSED NAME SAME)                    
         XR    RE,RE               IF YES, SEE IF JUST FILTERS CHANGED          
         IC    RE,SBLFILT          USER DEFINED FILTER LENGTH                   
         EX    RE,*+8                                                           
         BE    VACTS               CHECK SUBACTIONS IF ALL SAME AS LAST         
         CLC   SBASAVE(0),TSASAVE                                               
         TM    SRCHLFLG-SRCHLSTD+SEQLIST,SRCHLFDQ IF OLD LIST WAS DIR           
         BO    KEYFIRST            FILTERED, ALWAYS DO NEW SEARCH               
         TM    DIRFLAG,FILTERQ     TEST NEW LIST TO BE DIR FILTERED             
         BO    KEYFIRST            YES, ALWAYS DO NEW SEARCH                    
         MVC   TSASAVE,SBASAVE     ELSE SAVE CHANGED FILTERS                    
         B     RESTART             AND RESTART LIST WITH NEW FILTERING          
*                                                                               
KEYFIRST MVC   TSID,SBID           SAVE NEW FILTERS ETC                         
         MVC   TSIDLANG,SBIDLANG                                                
         MVC   TSFILTL,SRVFILTH+5                                               
         MVC   TSFILTER,SRVFILT                                                 
         MVC   TSNAMES,SBNAMES                                                  
         MVC   TSASAVE,SBASAVE                                                  
         XC    TSNDXS,TSNDXS                                                    
         B     SEARCH                                                           
         EJECT                                                                  
* USER HASN'T CHANGED FILTERS, SEE IF ANY SUBACTIONS ENTERED                    
*        ANY SUBACTION NOT RECOGNISED IS IGNORED, WITH NO ERROR MESSAGE         
*                                                                               
VACTS    MVI   NEXTPREV,NEXTQ      SET FORWARDS (DEFAULT)                       
         CLI   PRINTID,0           IF PRINT REQUEST, ALWAYS                     
         BNE   VACTFST             RESET TO FIRST                               
*                                                                               
         CLI   SVNDETL,0           IF NOTHING ON SCREEN LAST TIME               
         BE    VACTX               IGNORE ALL ACTIONS                           
*                                                                               
         LA    R2,SRVACT1H         R2=FIRST ACTION FIELD                        
         LA    R4,TSSCRNDX         R4=FIRST SCREEN INDEX NUMBER                 
*                                                                               
         CLI   5(R2),0             ANYTHING IN FIRST FIELD                      
         BE    VACTNXTL            NO - LOOK AT NEXT FIELD                      
*                                                                               
* CHECK FIRST LINE ACTIONS, FOLLOWING FIRST LINE CODES RECOGNISED -             
*        NNN=GOTO LINE, F=FIRST, P=PREVIOUS, L=LAST                             
*                                                                               
         TM    4(R2),X'08'         IS INPUT NUMERIC                             
         BZ    VACTALPH            NO, SEE IF ALPHA COMMAND                     
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB              R1=NUMBER INPUT                              
         LTR   R1,R1               IF ZERO, TREAT AS FIRST                      
         BZ    VACTFST                                                          
         CLM   R1,B'0111',TSMAXNDX IF GREATER THAN MAX IN LIST                  
         BH    VACTFST             TREAT AS FIRST                               
         CLM   R1,B'0111',TSSCRNDX IF SAME AS VALUE ORIGINALLY IN FIELD         
         BE    VACTNXTL            IGNORE IT                                    
         BCTR  R1,0                ELSE, ITS NEW START POINT                    
         STCM  R1,B'0111',TSNXTNDX                                              
         B     VACTX                                                            
VACTALPH CLC   8(1,R2),SR@1ST      F=FIRST SCREEN                               
         BE    VACTFST                                                          
         CLC   8(1,R2),SR@PREV     P=PREVIOUS SCREEN                            
         BE    VACTPRV                                                          
         CLC   8(1,R2),SR@LAST     L=VERY LAST SCREEN                           
         BNE   VACTHLP             NO, SEE WHAT ELSE                            
         XC    TSPRVNDX,TSPRVNDX   GET LAST BY GOING BACKWARD FROM END          
VACTPRV  MVI   NEXTPREV,PREVQ      SET BACKWARDS                                
         B     VACTX                                                            
VACTFST  XC    TSNXTNDX,TSNXTNDX   SET TO RESTART AT BEGINNING                  
         B     VACTX                                                            
*                                                                               
* CHECK ALL USEABLE ACTION FIELDS FOR                                           
*        ?=HELP                                                                 
*                                                                               
VACTHLP  CLI   8(R2),C'?'          ?=HELP                                       
         BNE   VACTSEL                                                          
VACTERR  MVC   HELPADDR,3(R4)      COPY ACT FIELD ADDRESS FOR HELP              
         CLI   MODE,MODESTAQ                                                    
         BNE   *+10                                                             
         MVC   HELPADDR,2(R2)      BUT IF STAND ALONE, USE LINE 1 ADDR          
         B     VACTERR4                                                         
VACTERR2 MVC   SELEADDR,3(R4)      COPY ACT FIELD ADDRESS FOR HELP              
         CLI   MODE,MODESTAQ                                                    
         BNE   *+10                                                             
         MVC   SELEADDR,2(R2)      BUT IF STAND ALONE, USE LINE 1 ADDR          
VACTERR4 ICM   R4,B'0111',TSPRVNDX PICK UP INDEX OF 1ST ITEM                    
         BZ    *+6                 IF ZERO, WE STARTED AT FIRST                 
         BCTR  R4,0                LESS ONE SO GET NEXT GETS IT                 
         STCM  R4,B'0111',TSNXTNDX SET TO RESTART AT FIRST LINE                 
         B     VACTX                                                            
*                                                                               
* IF NOT STAND ALONE, CHECK ALL ACTION FIELDS FOR                               
*        S=SELECT OR R=RETURN                                                   
*                                                                               
VACTSEL  CLI   MODE,MODESTAQ       IF  STAND ALONE                              
         BE    VACTERR             SELECT/RETURN NOT APPROPRIATE                
         CLC   8(1,R2),SR@RETRN    R=RETURN WITHOUT SELECTING                   
         BE    SELECT              (SELIX=0)                                    
         CLC   8(1,R2),SR@SLECT    S=SELECT ITEM ON THIS LINE                   
         BNE   VACTERR                                                          
         MVC   SELIX+1(3),0(R4)    COPY INDEX FOR SELECT                        
         SR    RE,RE                                                            
         IC    RE,#SELECTS                                                      
         LA    RE,1(RE)                                                         
         STC   RE,#SELECTS                                                      
         CLI   #SELECTS,1                                                       
         BH    VACTERR2            ERROR ON MULTI-SELECTS                       
         B     *+12                                                             
*                                                                               
VACTNXTL CLI   MODE,MODESTAQ       IF  STAND ALONE                              
         BE    VACTX               IGNORE REMAINING ACTION FIELDS               
         LA    R2,SRVACT2H-SRVACT1H(,R2) NEXT ACTION FIELD                      
         LA    R4,L'TSSCRNDX(,R4)  NEXT INDEX NUMBER                            
         CLI   0(R4),X'FF'         END OF NUMBERS ON SCREEN                     
         BE    VACTX               YES                                          
         LA    RE,SRVACTXH         LAST ACTION FIELD                            
         CR    R2,RE                                                            
         BH    VACTX               ALL DONE                                     
         CLI   5(R2),0             ANYTHING IN THIS FIELD                       
         BE    VACTNXTL            NO - LOOK AT NEXT FIELD                      
         TM    4(R2),X'08'         IS INPUT NUMERIC                             
         BO    VACTNXTL            YES, LOOK AT NEXT FIELD                      
         B     VACTHLP             NO, GO SEE WHAT IT IS                        
*                                                                               
         DROP  R3                  DROP TIA                                     
*                                                                               
VACTX    CLI   #SELECTS,1                                                       
         BE    SELECT                                                           
         CLI   MODE,MODESTAQ       IF NOT STAND ALONE, WE SAVED SEQLIST         
         BNE   START               ELSE WE HAVE TO REBUILD IT                   
         EJECT                                                                  
***********************************************************************         
* START A NEW SEARCH, RETURNING A SEQUENCE LIST                                 
***********************************************************************         
SEARCH   L     R3,SBMAXSEQ         GET HIGHEST SEQUENCE NUMBER                  
         LA    R3,7(,R3)           ROUND UP,                                    
         SRL   R3,3                GET BYTES NEEDED FOR BITLIST,                
         LA    R3,SRCHLLNQ(R3)     INCLUDING HEADER,                            
         LA    R3,7(,R3)           ROUND UP TO DOUBLE WORD                      
         SRL   R3,3                                                             
         SLL   R3,3                                                             
         C     R3,=A(72*K)         IS IT TOO BIG TO HANDLE                      
         BH    ECHS                YES                                          
*                                                                               
         LR    R1,R3                                                            
         BAS   RE,GETWRK           AQUIRE STORAGE FOR BITLIST                   
         LR    R2,R0               R2=A(BITLIST)                                
         STH   R3,0(R2)            INIT WITH TOTAL LENGTH                       
*                                                                               
         ST    R2,SBAIN                                                         
         MVI   SBSTYLI,SBSTSRCQ    STYLE IS SEARCH                              
         L     R2,ASEQLIST                                                      
         LH    RE,=Y(SEQLISTL)                                                  
         CLC   RECLEN,=H'6144'     FIX FOR NEW 14K TEMPSTR                      
         BE    *+12                                                             
         AH    RE,RECLEN           IF NOT 6144, ADD ACTUAL LENGTH               
         SHI   RE,6144             AND SUBTRACT 6144                            
         STH   RE,0(R2)            INIT WITH TOTAL LENGTH                       
         ST    R2,SBAOUT                                                        
         MVI   SBSTYLO,SBSTSELQ    OUTPUT STYLE IS SEQUENCE NUMBER LIST         
         TM    DIRFLAG,FILTERQ     DIRECTORY FILTERING REQUESTED ?              
         BZ    *+8                 NO                                           
         OI    SBSTYLO,SBSTDFLQ    REQUEST DIRECTORY FILTERING                  
         GOTO1 VSRCHXEC,DMCB,(R9)  SEARCH CALL                                  
         BNE   SRCHERR             GO ANALYSE ERROR RETURN                      
*                                                                               
         BAS   RE,PUTWRK           RELEASE BITLIST STORAGE                      
         L     RE,ATIA                                                          
         MVI   TSSEQLOK-TIAD(RE),1 INDICATE SEQUENCE LIST BUILT                 
         B     START                                                            
*                                                                               
* RESTART SEARCH AT FIRST ITEM                                                  
*                                                                               
RESTART  MVI   NEXTPREV,NEXTQ      SET FORWARDS                                 
         L     R1,ATIA                                                          
         XC    TSNDXS-TIAD(,R1),TSNDXS-TIAD(R1)                                 
*                                                                               
* START SEARCH AT NEXT ITEM                                                     
*                                                                               
START    TWAXC SRVACT1H,SRVTABH,PROT=Y CLEAR ANY JUNK OUT OF SCREEN             
         CLI   PRINTID,0           IF REPORT REQUESTED, OPEN IT                 
         BE    EXTRACT             ELSE GET FIRST RECORD                        
         EJECT                                                                  
* IF PRINT REQUEST, INITIALIZE REPORT                                           
*                                                                               
PRINTOPN L     RF,ACOMFACS         FIND AND SAVE VREPORT                        
         L     RF,CCALLOV-COMFACSD(,RF)                                         
*&&UK*&& GOTO1 (RF),DMCB,0,X'D9000AFA'                                          
*&&US*&& GOTO1 (RF),DMCB,0,X'D9000AF7'                                          
         MVC   VREPORT,0(R1)                                                    
*                                                                               
         MVC   REPACOM,ACOMFACS                                                 
         LA    R0,LINEWRK                                                       
         ST    R0,REPABUF          A(PRINT LINE BUFFER)                         
         LA    R0,PRINTSPC                                                      
         ST    R0,REPAPHS          A(SPECS)                                     
         LH    R1,RECLEN                                                        
         BAS   RE,GETWRK                                                        
         ST    R0,REPAPQB          A(PRINTQ BUFFER)                             
         MVI   REPWIDTH,REPWREGQ                                                
         MVI   REPHEADN,2                                                       
         MVI   REPHEADI,REPHSPAC                                                
         MVI   REPMIDSN,4                                                       
         MVI   REPMIDSI,REPMSPAC                                                
         MVI   REPPRNTN,1                                                       
         MVI   REPPRNTI,REPPCLRA                                                
         MVC   REPSYSID(4),=C'SRCH'                                             
         MVC   REPDATE,DATEB                                                    
         MVC   REPUSRID,SBUSER                                                  
         MVC   REPSUBID,PRINTID                                                 
         MVI   REPCLASS,C'S'                                                    
         MVI   REPRLH+1,8                                                       
         MVI   REPRDH+1,1                                                       
         MVC   REPDESC(3),=C'SCH'                                               
         MVI   REPDESC+3,C'='                                                   
         MVC   REPDESC+4(3),SBSYSTEM                                            
         MVI   REPDESC+7,C'/'                                                   
         MVC   REPDESC+8(3),SBSEARCH                                            
         MVC   REPCTRY,SBCTRY                                                   
         MVC   REPLANG,SBLANG                                                   
*                                                                               
         MVI   REPACTN,REPAINI                                                  
         GOTO1 VREPORT,REPBLK      INITIALISE REPBLK                            
         MVI   REPACTN,REPAOPN                                                  
         GOTO1 VREPORT,REPBLK      OPEN REPORT                                  
         CLI   REPERRS,0                                                        
         BNE   PRINTEND                                                         
         EJECT                                                                  
* GET NEXT SCREENFUL FROM SEQUENCE LIST, READ IN EITHER DIRECTION               
* AND KEEP GOING UNTIL EITHER END OF LIST, OR ITEM WON'T FIT                    
*                                                                               
EXTRACT  MVI   SCRMAP,X'FF'        INDICATE NO MAP BUILT YET                    
         L     R2,ASEQLIST                                                      
         ST    R2,SBAIN                                                         
         USING SRCHLSTD,R2                                                      
         MVI   SBSTYLI,SBSTSELQ    STYLE IS NEXT SEQUENCE LIST ENTRY            
         L     R1,ATIA                                                          
         MVC   TSMAXNDX-TIAD(,R1),SRCHLNAC SAVE MAX INDEX VALUE                 
         CLI   NEXTPREV,PREVQ                                                   
         BE    EXTBWD              SKIP IF BACKWARDS                            
EXTFWD   MVC   SRCHLNXT,TSNXTNDX-TIAD(R1) SET FORWARD START POINT               
         MVC   TSPRVNDX-TIAD(,R1),SRCHLNXT COPY TO BWD START                    
         B     EXTBOTH                                                          
EXTBWD   OI    SBSTYLI,SBSTPRVQ    SET STYLE TO GET PREVIOUS                    
         MVC   SRCHLNXT,TSPRVNDX-TIAD(R1) SET BACKWARD START POINT              
         MVC   TSNXTNDX-TIAD(,R1),SRCHLNXT COPY TO FWD START                    
         NC    SRCHLNXT,SRCHLNXT   IF ALREADY AT START,                         
         BNZ   EXTBOTH                                                          
         MVI   SRCHLNXT,X'FF'      FORCE TO END                                 
         DROP  R2                                                               
EXTBOTH  LA    R2,EXTAREA                                                       
         ST    R2,SBAOUT                                                        
         MVI   SBSTYLO,SBSTRXQ     OUTPUT STYLE IS RECORD EXTRACT               
*                                                                               
         XC    SBEXHEAD(2),SBEXHEAD ENSURE WE GET THE HEADER                    
         XC    ITEMCNT,ITEMCNT     IDICATE NO ITEMS FOUND THIS TIME             
         LA    RE,SRVACT1H                                                      
         ST    RE,LASTFLDH         SET NEXT AVAILABLE LINE                      
         XC    CURSORH,CURSORH     SET NO ITEMS USED YET                        
         LA    RE,LINEWRK                                                       
         ST    RE,NEXTLWRK         SET ADDRESS OF FIRST LINE                    
         MVI   NEXTLWIX,1          INDEX TO FIRST LINE                          
*                                                                               
EXTNEXT  GOTO1 VSRCHXEC,DMCB,(R9)  EXTRACT NEXT CALL                            
         BE    EXTHEAD             SKIP IF ITEM RETURNED OK                     
*                                                                               
         CLI   PRINTID,0           IF PRINTING SEARCH ITEMS                     
         BNE   PRINTEND            GO CLOSE UP REPORT                           
*                                                                               
         CLI   SBRETURN,SBREOLQ                                                 
         BNE   SRCHERR             GO ANALYSE ERROR IF NOT END OF LIST          
*                                                                               
EXTEOL   CLI   NEXTPREV,PREVQ      IF END OF SEARCH GOING BACKWARDS             
         BE    RESTART             RESTART SEARCH AT BEGINNING                  
*                                                                               
         L     R1,ATIA             IF GOING FORWARDS, CLEAR NEXT                
         XC    TSNXTNDX-TIAD(,R1),TSNXTNDX-TIAD(R1)                             
*                                                                               
         CLC   ITEMCNT,=Y(1)       HOW MANY ITEMS ON SCREEN                     
         BH    BUILDSCR            MORE THAN ONE, ALWAYS DISPLAY                
         BE    EXTEOL1             SKIP IF ONLY ONE                             
         NC    TSNDXS-TIAD(,R1),TSNDXS-TIAD(R1) IF NONE, IXS BOTH ZERO          
         BZ    BUILDSCR            THERE WERE NO HITS AT ALL                    
         B     EXTRACT             ELSE ALL FILTERED OUT, RESTART               
EXTEOL1  CLI   MODE,MODESTAQ                                                    
         BE    BUILDSCR            IF ONLY ONE ITEM ON SCREEN,                  
         CLI   FIRST,FIRSTQ                                                     
         BNE   BUILDSCR            AND XCTLED, AND FIRST TIME IN                
         MVI   SELIX+3,1                                                        
         B     SELECT              SELECT THE ITEM AUTOMATICALLY                
         EJECT                                                                  
* SEARCH NEXT CALL GAVE END OF LIST, OR ERROR WHILE PRINTING REPORT             
*                                                                               
* CLOSE UP REPORT IN PROGRESS.                                                  
* IF END-OF-LIST RETURNED, RESTART AND GET FIRST PAGE ONTO SCREEN               
* OTHERWISE GO RESPOND TO ERROR                                                 
*                                                                               
PRINTEND MVI   REPACTN,REPACLO     CLOSE REPORT                                 
         CLI   REPERRS,0                                                        
         BE    *+8                                                              
         MVI   REPACTN,REPACLOP    CLOSE PURGE IF ANY ERROR                     
         GOTO1 VREPORT,REPBLK                                                   
*                                                                               
PRINTEX  BAS   RE,PUTWRK           FREE REPORT BUFFER                           
*                                                                               
         CLI   SBRETURN,SBREOLQ    TEST RETURN FROM SEARCH                      
         BNE   SRCHERR             GO ANALYSE ERROR IF NOT END OF LIST          
         XC    PRINTID,PRINTID     RESET TO SCREEN DISPLAY                      
         B     RESTART             RESTART LIST ON SCREEN                       
         EJECT                                                                  
* FIRST CALL SHOULD ALSO RETURN HEADER DATA, THIS PROVIDES HEADING              
* DETAIL, AND ALSO DEFINES FORM OF EXTRACT DATA. IT IS USED TO DECIDE           
* HOW THE EXTRACT DATA IS FITTED ON THE SCREEN AND THIS IS DETAIL IS            
* STORED IN SCRMAP                                                              
*                                                                               
EXTHEAD  CLI   SCRMAP,X'FF'        IF MAP BUILT ITS NOT FIRST CALL              
         BNE   EXTCOPY             SO JUST COPY EXTRACT TO LINE                 
         CLI   SBEXHNHD,4          MAX NUMBER OF HEADS REQUIRED                 
         BH    ECHS                TOO MANY, CAN'T HANDLE SEARCH                
*                                                                               
         XR    RE,RE                                                            
         IC    RE,SBEXHTIL         LENGTH OF SEARCH TITLE                       
         LA    R5,SBEXHTIT(RE)     R5=1ST ITEM HEADER ENTRY                     
         LA    R6,16               R6=MAXIMUM ITEMS ALLOWED                     
*                                                                               
         CLI   PRINTID,0           ARE WE IN REPORT MODE                        
         BE    EXTHD10             NO, SKIP                                     
*                                                                               
         L     RF,REPAHEAD         TITLE AND FILTERS TO REPORT HEAD             
         MVC   0(L'SR@LSTOF,RF),SR@LSTOF                                        
         BCTR  RE,0                                                             
         MVC   8(0,RF),SBEXHTIT                                                 
         EX    RE,*-6                                                           
         MVC   L'PLINE(L'SR@FLTRS,RF),SR@FLTRS                                  
         MVI   L'PLINE+L'SR@FLTRS+1(RF),C':'                                    
         MVC   L'PLINE+10(L'SRVFILT,RF),SRVFILT                                 
*                                                                               
         L     R7,REPAMIDS         ADDRESS FIRST MID LINE                       
         BCTR  R7,0                LESS ONE TO ADJUST POSITION                  
         MVI   LINESRQD,1          ALLOW ONLY ONE LINE PER ITEM                 
         B     EXTHD20                                                          
*                                                                               
EXTHD10  MVI   SLNSFREE,NLINES     SET NUMBER OF SCREEN LINES LEFT              
         L     R7,LASTFLDH         1ST AVAILABLE SCREEN LINE                    
*                                                                               
         USING LINED,R7                                                         
EXTHD20  MVI   SCRMAP,0            RELATIVE LINE STARTS AT ZERO                 
         LA    R2,SCRMAP           R2=1ST SCRMAP ENTRY                          
         XR    R3,R3               R3=CURRENT OFFSET INTO SCREEN LINE           
*                                                                               
EXTHDNX  CLI   0(R5),0             IS THIS END OF EXTRACT HEADER                
         BE    EXTHDND             YES, EXIT                                    
         MVC   3(1,R2),1(R5)       COPY OFFSET IN EXTAREA                       
         STC   R3,1(,R2)           STORE OFFSET IN LINE                         
         LA    RF,L'TXT-1          MAX ALLOWED WIDTH                            
         CLI   PRINTID,0                                                        
         BE    *+8                                                              
         LA    RF,L'PLINE-1                                                     
         XR    R4,R4                                                            
         IC    R4,2(,R5)           GET ACTUAL ITEM WIDTH                        
         CR    R4,RF                                                            
         BNH   *+6                                                              
         LR    R4,RF               USE MAX WIDTH IF LESS THAN ACTUAL            
         LR    RE,R4                                                            
         BCTR  RE,0                ADJUST FOR MVC LENGTH                        
         STC   RE,2(,R2)           SET WIDTH OF ITEM IN LINE                    
         AR    R3,R4               LINE OFFSET FOR NEXT ITEM                    
         CR    R3,RF               PAST END OF LINE                             
         BH    EXTHDN10                                                         
         LA    R3,1(,R3)           NO, ALLOW SPACE BEFORE NEXT ITEM             
         B     EXTHDN20                                                         
*                                                                               
EXTHDN10 CLI   PRINTID,0           IF NO ROOM ON LINE,                          
         BNE   EXTHDND2            IGNORE REST IF PRINTING                      
         IC    RF,0(,R2)           ELSE START A NEW LINE                        
         LA    RF,1(,RF)                                                        
         STC   RF,0(R2)                                                         
         MVI   1(R2),0                                                          
         LA    R3,1(,R4)                                                        
         MVI   FLAG,C'H'           INDICATE PUTTING HEADERS                     
         BAS   RE,NXTLINES         GET NEXT SET OF LINES                        
         L     R7,LASTFLDH                                                      
*                                                                               
EXTHDN20 MVC   4(1,R2),0(R2)       COPY LINE NO TO NEXT ENTRY                   
         CLC   SBEXHNHD,3(R5)                                                   
         BL    ECHS                MAKE SURE WITHIN N'HEADS LIMIT               
         LA    RE,4(,R5)           FIRST HEADING ITEM                           
         XR    R0,R0                                                            
         ICM   R0,1,3(R5)          HEAD LINES USED FOR THIS ITEM                
         BZ    EXTHDN40            SKIP IF NONE                                 
         LR    RF,R7               POINT TO START OF 1ST LINE TO USE            
         CLI   PRINTID,0                                                        
         BNE   *+8                 IF NOT PRINTING                              
         LA    RF,TXT-LINED(,RF)   ADJUST FOR SCREEN HDRS                       
         XR    R1,R1                                                            
         IC    R1,1(R2)            OFFSET INTO LINE                             
         AR    RF,R1               POINT TO TARGET POSITION                     
*                                                                               
EXTHDN30 IC    R1,0(,RE)           LENGTH OF HEADER TEXT                        
         BCTR  R1,0                                                             
         MVC   1(0,RF),1(RE)                                                    
         EX    R1,*-6              MOVE TO HEADER                               
         LA    RE,2(RE,R1)         NEXT ITEM                                    
         LA    RF,LINEL(,RF)       NEXT LINE                                    
         CLI   PRINTID,0                                                        
         BE    *+8                                                              
         LA    RF,L'PLINE-LINEL(,RF) BIT FURTHER IF PRINT                       
         BCT   R0,EXTHDN30                                                      
*                                                                               
EXTHDN40 IC    R0,0(,R5)           LENGTH OF EXTHEAD ENTRY                      
         AR    R5,R0               NEXT ENTRY                                   
         CR    R5,RE                                                            
         BNE   ECHS                MAKE SURE WERE IN THE RIGHT PLACE            
         LA    R2,4(,R2)           NEXT MAP ENTRY                               
         BCT   R6,EXTHDNX          BUILD NEXT ENTRY                             
         DROP  R7                                                               
*                                                                               
EXTHDND  IC    R1,0(R2)            HIGHEST RELATIVE LINE NO                     
         LA    R1,1(,R1)                                                        
         STC   R1,LINESRQD         LINES REQUIRED PER ITEM                      
EXTHDND2 MVI   0(R2),X'FF'         TERMINATE SCREEN MAP                         
         CLI   SCRMAP,X'FF'        DID WE GET ANY ENTRIES                       
         BE    ECHS                NO, CAN'T HANDLE THIS SEARCH                 
*                                                                               
         CLI   PRINTID,0                                                        
         BNE   EXTHDNPR            GO TRANSLATE HEADS IF PRINTING               
*                                                                               
         MVI   FLAG,C'L'           INDICATE PUTTING LAST HEADERS                
         BAS   RE,NXTLINES         GET FIRST SET OF DETAIL LINES                
         XR    RF,RF                                                            
         IC    RF,SLNSFREE         RF=LINES LEFT                                
*                                                                               
         MH    RF,=Y(LINEWRKL)     RF=LENGTH OF LINEWRK SPACE RQD               
         LA    RE,LINEWRK                                                       
         LR    R0,RE                                                            
         LA    R1,C' '                                                          
         SLL   R1,24                                                            
         MVCL  RE,R0               CLEAR LINEWRK SPACE TO SPACES                
         ST    RE,LASTLWRK         SET ADDRESS OF END OF LINEWRK                
*                                                                               
         L     R1,ATIA             COPY INDEX OF FIRST ITEM AS START            
         LA    RE,TSPRVNDX-TIAD(,R1) POINT FOR CHANGE OF DIRECTION              
         CLI   NEXTPREV,PREVQ                                                   
         BNE   *+8                                                              
         LA    RE,TSNXTNDX-TIAD(,R1)                                            
         NC    0(3,RE),0(RE)       UNLESS WE STARTED AT ONE END                 
         BZ    EXTCOPY                                                          
         L     R1,ASEQLIST                                                      
         USING SRCHLSTD,R1                                                      
         MVC   0(3,RE),SRCHLNXT                                                 
         DROP  R1                                                               
         B     EXTCOPY             GO MOVE DATA LINE IN                         
*                                                                               
EXTHDNPR XR    R0,R0                                                            
         IC    R0,SBEXHNHD         GET NUMBER OF HEAD LINES                     
         L     R1,REPAMIDS         POINT TO FIRST                               
EXTHDNP1 ICM   R1,8,=AL1(L'PLINE)  GET LENGTH                                   
         MVI   DDFSYS,0            USE SDOVSYS                                  
         BAS   RE,DDFIELD          TRANSLATE LINE                               
         LA    R1,L'PLINE(,R1)                                                  
         BCT   R0,EXTHDNP1         LOOP TILL ALL HEADS DONE                     
         EJECT                                                                  
* MOVE ITEM INTO LINE WORK AREAS. IF NO ROOM LEFT FOR ITEM, GO BUILD            
* SCREEN. WE ALWAYS READ ONE MORE THAN WILL FIT SO WE KNOW IF THERE             
* IS MORE TO COME ON THE NEXT SCREEN.                                           
*                                                                               
EXTCOPY  L     R7,NEXTLWRK         NEXT LINE IF SCREEN OUTPUT                   
         CLI   PRINTID,0                                                        
         BE    *+8                                                              
         L     R7,REPAPRNT         PRINT LINE IF PRINTING                       
         L     R3,LASTLWRK         R3=END OF AVAILABLE LINES                    
         LA    R2,SCRMAP           R2=SCREEN MAP                                
EXTCP10  CLI   0(R2),X'FF'         END OF ITEMS                                 
         BE    EXTCPEND                                                         
         XR    RF,RF                                                            
         IC    RF,3(,R2)           EXTAREA OFFSET                               
         LA    RF,EXTAREA(RF)      SOURCE POSITION                              
         IC    R1,2(,R2)           MVC/CLC LENGTH                               
         EX    R1,EXTCPCLC         CHECK DATA USED                              
         BE    EXTCP30             NO, GET NEXT ITEM FOR RECORD                 
         LR    RE,R7               RE=PRINT LINE IF PRINTING                    
         CLI   PRINTID,0                                                        
         BNE   EXTCP20             SKIP IF PRINT MODE                           
         XR    RE,RE                                                            
         IC    RE,0(,R2)           LINE NO                                      
         MH    RE,=Y(LINEWRKL)                                                  
         LA    RE,LINETXT-LINEWRK+1(R7,RE) RE=LINE WORK AREA                    
         CR    RE,R3               CHECK NOT PASSED END                         
         BH    BUILDSCR            YES, LEAVE CURRENT LINE OUT                  
EXTCP20  XR    R0,R0                                                            
         IC    R0,1(,R2)           LINE OFFSET                                  
         AR    RE,R0               TARGET POSITION                              
         EX    R1,EXTCPMVC         MOVE DATA                                    
EXTCP30  LA    R2,4(,R2)                                                        
         B     EXTCP10             GET NEXT ITEM FOR RECORD                     
EXTCPCLC CLC   0(0,RF),SPACES                                                   
EXTCPMVC MVC   0(0,RE),0(RF)                                                    
*                                                                               
EXTCPEND LH    R1,ITEMCNT          COUNT ITEMS RETURNED                         
         LA    R1,1(,R1)                                                        
         STH   R1,ITEMCNT                                                       
*                                                                               
         CLI   PRINTID,0                                                        
         BE    EXTCPE00            SKIP IF NOT PRINT MODE                       
         LR    R1,R7               POINT TO LINE                                
         ICM   R1,8,=AL1(L'PLINE)  GET LENGTH                                   
         MVI   DDFSYS,0            USE SDOVSYS                                  
         BAS   RE,DDFIELD          TRANSLATE LINE                               
         MVI   REPACTN,REPAPUT                                                  
         GOTO1 VREPORT,REPBLK                                                   
         B     EXTNEXT             PRINT THE LINE                               
*                                                                               
EXTCPE00 MVI   LINE1IX-LINEWRK(R7),0 INDICATE FIRST LINE FOR ITEM               
         L     R1,ATIA             COPY INDEX OF THIS ITEM TO START/END         
         LA    RE,TSNXTNDX-TIAD(,R1)                                            
         CLI   NEXTPREV,PREVQ                                                   
         BNE   *+8                                                              
         LA    RE,TSPRVNDX-TIAD(,R1)                                            
         L     R2,ASEQLIST                                                      
         USING SRCHLSTD,R2                                                      
         MVC   0(3,RE),SRCHLNXT                                                 
         MVC   LINESQIX-LINEWRK(,R7),SRCHLNXT COPY INDEX OF THIS ENTRY          
         DROP  R2                                                               
*                                                                               
         XR    R1,R1                                                            
         IC    R1,LINESRQD         MAX LINES USED FOR ITEM                      
         MH    R1,=Y(LINEWRKL)                                                  
         AR    R1,R7               R1=CURR LINE + MAX LINES ALLOWED             
         CR    R1,R3                                                            
         BNH   *+6                                                              
         LR    R1,R3               OR END OF LINES IF LOWER                     
EXTCPE20 SH    R1,=Y(LINEWRKL)     PREVIOUS LINE                                
         CLC   LINETXT-LINEWRK(L'LINETXT,R1),SPACES                             
         BE    EXTCPE20            LOOK FOR LAST USED LINE                      
         XR    RE,RE                                                            
         IC    RE,NEXTLWIX                                                      
EXTCPE30 AH    R7,=Y(LINEWRKL)     SCAN FORWARD THROUGH CONTINUATION            
         LA    RE,1(,RE)           LINES (IF ANY) TILL PASSED LAST USED         
         CR    R7,R1               LINE                                         
         BH    EXTCPE40                                                         
         MVC   LINE1IX-LINEWRK(,R7),NEXTLWIX SET LINENO OF 1ST LINE TO          
         B     EXTCPE30            SHOW CONTINUATION LINE                       
EXTCPE40 ST    R7,NEXTLWRK                                                      
         STC   RE,NEXTLWIX                                                      
         B     EXTNEXT             NEXT RECORD EVEN IF NO MORE SPACE            
         EJECT                                                                  
* COPY LINE WORK AREA INTO SCREEN                                               
*                                                                               
BUILDSCR LA    R3,LINEWRK          FIRST LINE WORK AREA                         
         L     R4,NEXTLWRK         END OF USED WORK AREA                        
         L     R5,ATIA                                                          
         LA    R5,TSSCRNDX-TIAD(,R5) SCREEN INDEX IN TIA                        
         CR    R3,R4               DID WE USE ANY AT ALL                        
         BE    BUILDEND            NO, NOTHING TO DO                            
         LR    R2,R3               START AT FIRST LINE                          
         CLI   NEXTPREV,NEXTQ                                                   
         BE    BUILD20             IF GOING FORWARD                             
         LR    R2,R4               ELSE START AT END                            
BUILD10  SH    R2,=Y(LINEWRKL)     PREVIOUS LINE                                
         CR    R2,R3               HAVE WE PASSED THE START                     
         BL    BUILDEND            YES, FINISH UP                               
         CLI   LINE1IX-LINEWRK(R2),0 IS THIS A CONTINUATION LINE                
         BNE   BUILD10             YES, LOOK FOR ITS 1ST LINE                   
BUILD20  L     R7,LASTFLDH         POINT TO NEXT SCREEN LINE                    
         USING LINED,R7                                                         
         XR    RF,RF                                                            
         ICM   RF,B'0111',LINESQIX-LINEWRK(R2) GET INDEX NUMBER                 
         STCM  RF,B'0111',0(R5)    COPY TO SCREEN INDEX                         
         MVC   3(2,R5),ACTH+2      COPY SCREEN ADDRESS OF ACTION FIELD          
         NC    HELPADDR,HELPADDR   IS ANY LINE TO CONTAIN HELP                  
         BZ    BUILD22             NO,                                          
         CLC   HELPADDR,ACTH+2     IS THIS LINE TO CONTAIN HELP                 
         BH    BUILD22             NO, PUT SEQUENCE NUMBER IN ACTION            
         BAS   RE,BUILDHLP                                                      
         MVI   HELPADDR,X'FF'      INDICATE HELP DATA DONE                      
         OI    TXTH+1,X'08'        SET HELP TEXT HIGH INTENSITY                 
         NC    CURSORH,CURSORH     IF THIS IS NOT FIRST LINE                    
         BZ    *+8                                                              
         ST    R7,CURSORH          FORCE CURSOR TO THE FIELD                    
         B     BUILD24             LEAVE SEQUENCE BLANK FOR HELP LINE           
BUILD22  NC    SELEADDR,SELEADDR   DOES ANY LINE HAVE MULTISEL ERR              
         BZ    BUILD23             NO,                                          
         CLC   SELEADDR,ACTH+2     IS THIS LINE TO CONTAIN HELP                 
         BH    BUILD23             NO, PUT SEQUENCE NUMBER IN ACTION            
         MVC   ACT(1),SR@SLECT                                                  
         OI    ACTH+6,X'80'        SET HIGH INTENSITY                           
         OI    ACTH+1,X'08'        SET HIGH INTENSITY                           
         MVI   SELEADDR,X'FF'      INDICATE HELP DATA DONE                      
         NC    CURSORH,CURSORH     IF THIS IS NOT FIRST LINE                    
         BZ    *+8                                                              
         ST    R7,CURSORH          FORCE CURSOR TO THE FIELD                    
         B     BUILD24             LEAVE SEQUENCE BLANK FOR HELP LINE           
BUILD23  EDIT  (RF),(3,ACT),ALIGN=LEFT AND SCREEN                               
BUILD24  LA    R5,L'TSSCRNDX(,R5)                                               
         LR    R6,R2               COPY THIS LINE ADDRESS                       
BUILD25  MVC   TXT,LINETXT-LINEWRK(R6) COPY TEXT TO SCREEN                      
         LA    R6,LINEWRKL(,R6)    NEXT LINE                                    
         LA    R7,LINEL(,R7)       NEXT SCREEN LINE                             
         CR    R6,R4               REACHED END YET                              
         BNL   BUILD30             YES                                          
         CLI   LINE1IX-LINEWRK(R6),0 IS THIS A CONTINUATION LINE                
         BNE   BUILD25             YES, BUNG IT IN TOO                          
         DROP  R7                                                               
BUILD30  MVI   FLAG,C'D'           INDICATE DETAIL LINE                         
         BAS   RE,NXTLINES         SORT OUT SCREEN                              
         CLI   NEXTPREV,PREVQ                                                   
         BE    BUILD10             PREVIOUS LINE SET IF BACKWARDS               
         LR    R2,R6               ELSE MOVE FORWARDS                           
         CR    R2,R4               HAVE WE REACHED END                          
         BL    BUILD20             NO, START NEW LINE SET                       
BUILDEND MVI   0(R5),X'FF'         END OF SCREEN INDEX                          
         CLI   REPSUBID,0          DID WE (TRY TO) PRINT A REPORT               
         BNE   ERPT                YES, PREPARE MESSAGE                         
         LA    R2,SRVFILTH         POINT CURSOR AT FILTER AREA                  
         MVI   SBRETURN,SBREOLQ    DUMMY UP END OF LIST RETURN                  
         NC    SELEADDR,SELEADDR   ANY SELECT ERROR                             
         BZ    *+12                NO                                           
         MVI   SBRETURN,SBESELQ                                                 
         B     GETMESS                                                          
         NC    HELPADDR,HELPADDR   ANY HELP ON SCREEN                           
         BZ    GETMESS             NO                                           
         MVI   SBRETURN,SBEHLPQ    YES, SET HELP RETURN CODE                    
         B     GETMESS                                                          
         EJECT                                                                  
*                                                                               
*        BUILD A LINE OF HELP DATA. R2=LINEWRK ENTRY                            
*                                                                               
BUILDHLP NTR1                                                                   
         LA    R3,LINETXT-LINEWRK(,R2) R3=AREA TO BUILD HELP                    
         XC    0(L'LINETXT,R3),0(R3)                                            
         L     R1,ATIA                                                          
         CLC   HELPADDR,TSSCRNDX+3-TIAD(R1) TEST IF 1ST LINE                    
         BH    BUILDHL2            NO, ONLY SELECT HELP APPLIES                 
*                                                                               
         MVCDD 1(30,R3),SR#SRC01   COPY 'N-NNN=GOTO LINE N' MESSAGE             
         GOTO1 ,DMCB,C'SL  ',1(R3)                                              
         MVI   DMCB+2,1            (SERVICE SYSTEM)                             
         GOTO1 VDICTATE                                                         
*                                                                               
         LA    R1,SR@1ST           ADD F=FIRST                                  
         BAS   RE,BUILDHL5                                                      
         L     R1,ATIA                                                          
         NC    TSNDXS-TIAD(,R1),TSNDXS-TIAD(R1) 1ST/LAST INDEXES ZERO           
         BZ    BUILDHL1            IF ALL ON ONE SCREEN                         
*                                                                               
         LA    R1,SR@LAST          ADD L=LAST                                   
         BAS   RE,BUILDHL5                                                      
         LA    R1,SR@PREV          ADD P=PREV                                   
         BAS   RE,BUILDHL5                                                      
*                                                                               
BUILDHL1 CLI   MODE,MODESTAQ       IS IT STAND ALONE MODE                       
         BE    EXIT                YES, ONLY PAGING HELP APPLIES                
*                                                                               
BUILDHL2 LA    R1,SR@SLECT         ADD S=SELECT                                 
         BAS   RE,BUILDHL5                                                      
         LA    R1,SR@RETRN         ADD R=RETURN                                 
         BAS   RE,BUILDHL5                                                      
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
BUILDHL5 LA    RF,L'LINETXT-1(,R3) END OF STRING                                
         LA    R0,L'LINETXT        LENGTH                                       
BUILDHL6 CLI   0(RF),C' '          LOOK FOR LAST NONBLANK                       
         BH    BUILDHL7                                                         
         BCTR  RF,0                                                             
         BCT   R0,BUILDHL6                                                      
         B     *+8                 LEAVE OUT COMMA AT BEGINNING                 
BUILDHL7 MVI   1(RF),C','                                                       
         MVC   2(1,RF),0(R1)       COPY 1ST CHAR                                
         MVI   3(RF),C'='                                                       
         MVC   4(10,RF),0(R1)      COPY WHOLE STRING                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* FIND NEXT FREE LINE ON SCREEN, AND FORMAT ACTION FIELDS                       
*                                                                               
NXTLINES NTR1                                                                   
         L     R7,LASTFLDH         R7=LATEST ACTION FIELD                       
         USING LINED,R7                                                         
         XR    R5,R5                                                            
         IC    R5,SLNSFREE         R5=NUMBER OF LINES LEFT                      
         CLI   FLAG,C'D'                                                        
         BE    NXTL10              SKIP IF NOT HEADING STYLE                    
         NC    TXT,TXT             IS HEADING BLOCK USED                        
         BNZ   NXTL05              YES, SKIP                                    
         CLI   FLAG,C'L'           WAS IT THE LAST BLOCK?                       
         BNE   EXIT                NO, SO WE CAN USE IT FOR THE NEXT            
         CLI   SLNSFREE,NLINES     IS THIS 1ST LINE (NO HEADS AT ALL)           
         BE    NXTL05              YES, IT HAS TO BE A HEADING.                 
         SHI   R7,LINEL            ELSE BACK UP TO PREVIOUS LINE AND            
         LA    R5,1(,R5)           MAKE IT THE LAST LINE INSTEAD                
NXTL05   OI    TXTH+1,X'08'        HIGH INTENSITY ON HEADINGS,                  
         B     NXTL20              AND ACTION FIELD PROTECTED                   
*                                                                               
NXTL10   NC    CURSORH,CURSORH     FOR DETAIL LINES,                            
         BNZ   NXTL15                                                           
         ST    R7,CURSORH          SET CURSOR TO FIRST FIELD HEADER             
         B     NXTL30              AND LEAVE IT UNPROT IN BOTH MODES            
NXTL15   CLI   MODE,MODEXCTQ                                                    
         BE    NXTL30              IF XCTLED, ALL ACTION FIELDS UNPROT          
*                                                                               
NXTL20   OI    ACTH+1,X'20'        PROTECT ACTION FIELD                         
         NI    ACTH+1,255-X'01'    AND UN-MODIFY                                
*                                                                               
NXTL30   MVI   DDFSYS,0            USE SDOVSYS                                  
         GOTO1 DDFTWA,TXTH         SORT OUT ANY DICT ITEMS                      
         LR    R6,R7               SAVE LAST USED LINE ADDRESS                  
         LA    R7,LINEL(,R7)       POINT TO NEXT LINE                           
         ST    R7,LASTFLDH         SET ADDRESS OF NEXT FREE LINE                
         BCTR  R5,0                                                             
         STC   R5,SLNSFREE         COUNT LINES LEFT                             
         LTR   R5,R5               IS THIS END OF SCREEN                        
         BZ    EXIT                YES, EXIT                                    
         NC    TXT,TXT             WAS THIS LINE USED AS CONTINUATION           
         BNZ   NXTL20              YES, PROTECT ACTION AND TRY NEXT             
         CLI   FLAG,C'L'           IS IT LAST LINE OF HEADINGS                  
         BNE   EXIT                NO, EXIT                                     
         OI    ACTH+1-LINED(R6),X'48' SET ACTION HDR HI INT + LWR CASE          
         MVCDD ACT-LINED(3,R6),SR#ACTN PUT ACTION HEADER TO LAST HEAD           
         CLI   MODE,MODESTAQ                                                    
         BNE   EXIT                                                             
         MVCDD ACT-LINED(3,R6),SR#SEQC OR SEQ HEADER IF STAND ALONE             
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
* SELECT/RETURN EXIT.                                                           
*                                                                               
* IF SELIX = ZERO, NO ITEM SELECTED, ELSE ITS INDEX OF SEQLIST ENTRY            
*                                                                               
SELECT   NC    SELIX,SELIX                                                      
         BNZ   SELECT1                                                          
         MVCDD EXTAREA(16),SR#NOITM,S IF NO R SUBACTION, RETURN NO ITEM         
         LA    RF,16               RF=WIDTH OF ITEM                             
         B     SELECT8                                                          
*                                                                               
SELECT1  L     RE,ASEQLIST         EXTRACT DESIRED ITEM                         
         ST    RE,SBAIN                                                         
         USING SRCHLSTD,RE                                                      
         MVI   SBSTYLI,SBSTSELQ    STYLE IS NEXT SEQUENCE LIST ENTRY            
         L     R1,SELIX                                                         
         BCTR  R1,0                                                             
         STCM  R1,B'0111',SRCHLNXT                                              
         DROP  RE                                                               
         LA    RE,EXTAREA                                                       
         ST    RE,SBAOUT                                                        
         MVI   SBSTYLO,SBSTRXQ     OUTPUT STYLE IS RECORD EXTRACT               
         XC    SBEXHEAD(2),SBEXHEAD ENSURE WE GET THE HEADER                    
         GOTO1 VSRCHXEC,DMCB,(R9)  EXTRACT NEXT CALL                            
         BNE   SRCHERR             GO ANALYSE ERROR                             
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,SBEXHDKY       DISPLACEMENT TO KEY ITEM ENTRY               
         BNZ   *+6                                                              
         DC    H'0'                MUST NOT BE ZERO                             
         LA    R1,SBEXHEAD(RF)     R1=ADDRESS KEY ITEM ENTRY                    
         IC    RF,1(,R1)           DISPLACEMENT IN EXTAREA                      
         LA    RE,EXTAREA(RF)      RE=ADDRESS OF ITEM                           
         ICM   RF,1,2(R1)          WIDTH OF ITEM                                
         BNZ   *+6                                                              
         DC    H'0'                MUST NOT BE ZERO                             
         LA    R1,0(RE,RF)         R1=END OF SELECT DATA                        
SELECT4  BCTR  R1,0                LOOK FOR ACTUAL END OF DATA                  
         CLI   0(R1),C' '                                                       
         BH    SELECT5                                                          
         BCT   RF,SELECT4                                                       
         XC    SELIX,SELIX         IF NO KEY DATA, DO ERROR MESSAGE             
         MVCDD EXTAREA(16),SR#IVITM,S                                           
         LA    RF,16               RF=WIDTH OF ITEM                             
         B     SELECT8                                                          
SELECT5  CLI   0(RE),C' '          LOOK FOR START OF DATA                       
         BH    SELECT6                                                          
         LA    RE,1(,RE)                                                        
         BCT   RF,SELECT5                                                       
         DC    H'0'                                                             
SELECT6  L     R3,ATIA                                                          
         USING TIAD,R3                                                          
         XR    R2,R2                                                            
         IC    R2,TSFDISP          R2=DISPLACEMENT IN CALLERS FIELD             
         XR    R1,R1                                                            
         ICM   R1,1,TSFLEN         GET LENGTH OF DATA TO BE REPLACED            
         BZ    SELECT6A            SKIP IF ZERO (ALL TO BE REPLACED)            
         LA    R1,TSFDATA(R1)                                                   
         AR    R1,R2               POINT TO DATA AFTER SEARCH KEY               
         MVC   TEMP(L'TSFDATA),0(R1) SAVE DATA AFTER SEARCH KEY                 
SELECT6A LA    R1,TSFDATA(R2)      POINT TO SEARCH KEY TO BE REPLACED           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)       COPY ITEM TO REPLACE SEARCH KEY              
         LA    R2,1(RF,R2)         R2=TOTAL DATA LENGTH SO FAR                  
         CH    R2,=Y(L'TSFDATA)                                                 
         BL    SELECT6B                                                         
         LA    R2,L'TSFDATA        FORCE TO MAX IF FULL OR TOO BIG              
         B     SELECT6X                                                         
SELECT6B LA    R1,TSFDATA(R2)      POINT TO NEXT BYTE                           
         LA    RE,L'TSFDATA                                                     
         SR    RE,R2               RE=LENGTH LEFT                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)       CLEAR REST OF DATA                           
         XR    RF,RF                                                            
         ICM   RF,1,TSFLEN         GET LENGTH OF DATA TO BE REPLACED            
         BZ    SELECT6X            ALL DONE IF 0 (ALL TO BE REPLACED)           
         XR    RE,RE                                                            
         IC    RE,TSFDISP          GET DISP OF DATA TO BE REPLACED              
         AR    RF,RE               RF=DISP TO REMAINDER                         
         IC    RE,TSFDATAL         GET ORIGINAL LENGTH                          
         SR    RE,RF               RE=LENGTH OF REMAINDER                       
         BNP   SELECT6X                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),TEMP        COPY BACK REST OF DATA                       
         LA    R2,1(RE,R2)         R2=TOTAL DATA LENGTH SO FAR                  
         CH    R2,=Y(L'TSFDATA)                                                 
         BNH   *+8                                                              
         LA    R2,L'TSFDATA        FORCE TO MAX IF FULL OR TOO BIG              
SELECT6X STC   R2,TSFDATAL                                                      
*                                                                               
         TM    TSOPTS,TSOPTGBQ     TEST IF USER REQUESTED =GOBACK               
         BO    GOBACK              YES, GO STRAIGHT TO GOBACK EXIT              
         L     R1,SBASRCH          GET A(SEARCH DIRECTORY ENTRY)                
         TM    SDSRIND2-SDSRCHD(R1),SDSRIGBQ TEST IF ALWAYS =GOBACK             
         BO    GOBACK              YES, GO STRAIGHT TO GOBACK EXIT              
         DROP  R3                                                               
*                                                                               
         ICM   RF,1,SBEXHDNA       DISPLACEMENT TO NAME ITEM ENTRY              
         BZ    SELECT7             SKIP IF NOT GIVEN                            
         LA    R1,SBEXHEAD(RF)     R1=NAME ITEM ENTRY                           
         IC    RF,1(,R1)           DISPLACEMENT IN EXTAREA                      
         LA    RE,EXTAREA(RF)      RE=ADDRESS OF ITEM                           
         IC    RF,2(,R1)           RF=WIDTH OF ITEM                             
         B     SELECTX                                                          
SELECT7  MVCDD EXTAREA(8),SR#ITEM,S                                             
         LA    RF,8                RF=WIDTH OF ITEM                             
SELECT8  LA    RE,EXTAREA          RE=ADDRESS OF ITEM                           
*                                                                               
SELECTX  LA    R3,SBTXTBLK                                                      
         USING GETTXTD,R3                                                       
         MVI   GTMSYS,X'FF'                                                     
         MVI   GTMTYP,GTMINF                                                    
         MVC   GTMSGNO,=Y(129)     'SELECTED' MESSAGE                           
*                                                                               
         STCM  RE,B'0111',GTATXT   ADDRESS OF NAME FOR MESSAGE                  
         STC   RF,GTLTXT           AND LENGTH                                   
         DROP  R3                                                               
         MVI   MODE,MODESELQ       SLIP INTO SELECT MODE                        
         B     TRANSTXT            GO COMPLETE RETURN TO FACPAK                 
         EJECT                                                                  
* $SEARCH ERROR EXITS                                                           
*                                                                               
ENCT     MVI   SBRETURN,SBENCTQ    NOT CONNECTED                                
         LA    R2,SRVSRVH                                                       
         B     GETMESS1            DON'T DO PF HELP                             
EAUT     MVI   SBRETURN,SBAUTHQ    NOT AUTHORISED                               
         LA    R2,SRVSRVH                                                       
         B     GETMESS                                                          
EMIF     MVI   SBRETURN,SBEMIFQ    MISSING INPUT FIELD                          
         B     GETMESS                                                          
EIIF     MVI   SBRETURN,SBEIIFQ    INVALID INPUT FIELD                          
         B     GETMESS                                                          
EFTL     MVI   SBRETURN,SBEFTLQ    FIELD TOO LONG                               
         B     GETMESS                                                          
ECHS     MVI   SBRETURN,SBESRCQ    CAN'T HANDLE SEARCH                          
         LA    R2,SRVIDH                                                        
         LA    RE,SRVACT1H                                                      
         ST    RE,LASTFLDH                                                      
         B     GETMESS                                                          
EHLP     MVI   SBRETURN,SBEHLPQ    HELP DISPLAYED                               
         B     GETMESS                                                          
ESEL     MVI   SBRETURN,SBESELQ    CAN'T DO MULTI-SELECTS                       
         B     GETMESS                                                          
*                                                                               
ERPT     MVI   SBRETURN,SBETXTQ                                                 
         LA    R3,SBTXTBLK         REPORT PRINTED, OR FAILED                    
         USING GETTXTD,R3                                                       
         MVI   GTMSYS,X'FF'        SET GENERAL ERROR                            
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSGNO1,30         PRINT Q ERROR                                
         CLI   REPERRS,0                                                        
         BNE   ERPTX               SKIP IF PRINT ERROR FOUND                    
         MVI   GTMTYP,GTMINF                                                    
         MVI   GTMSGNO1,23         REPORT PRINTED                               
         LA    R2,EXTAREA          BUILD REPORT DESCRIPTION FOR GETTXT          
         STCM  R2,7,GTASUBST                                                    
         DROP  R3                                                               
         MVC   1(3,R2),REPSUBID                                                 
         MVI   4(R2),C','                                                       
         EDIT  (2,REPREPNO),(4,5(R2)),ALIGN=LEFT                                
         LR    RE,R0                                                            
         LA    RE,5(,RE)                                                        
         STC   RE,0(R2)            XXX,9999  - REP I.D.     PARAM &1            
         AR    R2,RE                                                            
*                                                                               
         EDIT  (2,REPPAGES),(4,1(R2)),ALIGN=LEFT                                
         LR    RE,R0                                                            
         LA    RE,1(,RE)                                                        
         STC   RE,0(R2)            9999  - PAGE COUNT       PARAM &2            
         AR    R2,RE                                                            
*                                                                               
         EDIT  (3,REPLINES),(5,1(R2)),ALIGN=LEFT                                
         LR    RE,R0                                                            
         LA    RE,1(,RE)                                                        
         STC   RE,0(R2)            99999  - LINE COUNT      PARAM &3            
         AR    R2,RE                                                            
         MVI   0(R2),0             EOT                                          
ERPTX    LA    R2,SRVFILTH         POINT CURSOR AT FILTER AREA                  
         B     GETMESS                                                          
*                                                                               
* SRCHEXEC ERRORS/WARNING EXIT                                                  
*                                                                               
SRCHERR  CLI   SBRETURN,SBERRORQ   IS IT A NASTY ERROR                          
         BNH   *+6                                                              
         DC    H'0'                DIE IF IT IS                                 
         LA    R2,SRVFILTH         POINT CURSOR AT FILTER AREA                  
         CLI   SBERRFLD,1          UNLESS ID FIELD IN ERROR                     
         BNE   *+8                                                              
         LA    R2,SRVIDH                                                        
*                                                                               
         L     RE,ATIA                                                          
         MVI   TSSEQLOK-TIAD(RE),0 INDICATE SEQUENCE LIST NO GOOD               
*                                                                               
* CONVERT INTERNAL SEARCH RETURN CODE TO GETTXT MESSAGE NUMBER                  
* AND BUILD PF HELP LINE                                                        
*                                                                               
GETMESS  MVC   SRVPFKS(HPFHELQ),HPFHELP   ALWAYS SET PF1=HELP                   
         LA    RE,SRVPFKS+HPFHELQ                                               
         L     R1,ATIA                                                          
         NC    TSNDXS-TIAD(,R1),TSNDXS-TIAD(R1) 1ST/LAST INDEXES ZERO           
         BZ    *+14                IF ALL ON ONE SCREEN                         
         MVC   0(HPFPALQ,RE),HPFPAGE      ELSE SET PF2/3 PAGING                 
         LA    RE,HPFPALQ(,RE)                                                  
         NC    ITEMCNT,ITEMCNT     IF ANY ITEMS ON SCREEN,                      
         BZ    *+14                                                             
         MVC   0(HPFPRLQ,RE),HPFPRINT     SET PF7=PRINT                         
         LA    RE,HPFPRLQ(,RE)                                                  
         CLI   MODE,MODEXCTQ       IF XCTLED                                    
         BNE   *+10                                                             
         MVC   0(HPFRELQ,RE),HPFRETRN     SET PF12=RETURN                       
*                                                                               
GETMESS1 LA    R4,TXTABEOL         POINT TO A GENERAL TABLE ENTRY               
         CLI   SBRETURN,SBETXTQ    IS IT AN APPLICATION MESSAGE                 
         BE    SAVEIT              YES LEAVE IT TO GETTXT                       
         LA    R3,SBTXTBLK                                                      
         USING GETTXTD,R3                                                       
         MVI   GTMSYS,X'FF'        SET GENERAL ERROR                            
*                                                                               
* END OF LIST RETURN HAS TO BE QUALIFIED ACCORDING TO SITUATION                 
*                                                                               
         CLI   SBRETURN,SBREOLQ    SKIP IF NOT END OF LIST                      
         BNE   GETMESS4                                                         
         NC    ITEMCNT,ITEMCNT     IF NO ITEMS ON SCREEN, USE FIRST             
         BZ    GETMESS6            ENTRY, IE NO HITS FOUND                      
         LA    R4,TXTABL(,R4)      POINT TO SECOND ENTRY                        
         L     R1,ATIA                                                          
         NC    TSNDXS-TIAD(,R1),TSNDXS-TIAD(R1) 1ST/LAST INDEXES ZERO           
         BZ    *+8                 IF ALL ON ONE SCREEN, USE 2ND ENTRY          
         LA    R4,2*TXTABL(,R4)    ELSE USE 4TH ENTRY (MORE TO COME)            
         CLI   MODE,MODEXCTQ       IF NOT STAND ALONE, CHANGE 2ND/4TH           
         BNE   *+8                 ENTRY TO 3RD/5TH ENTRY WHICH INCLUDE         
         LA    R4,TXTABL(,R4)      A PROMPT FOR SELECT                          
         TM    SRCHLFLG-SRCHLSTD+SEQLIST-TIAD(R1),SRCHLFDQ                      
         BO    *+12                IF LIST WAS DIRECTORY FILTERED, OR           
         CLI   SBNNAMES,0          IF NO NAMES USED, CHANGE 2ND-5TH             
         BNE   GETMESS2            ENTRY TO 6TH-9TH ENTRY TO EXCLUDE            
         LA    R4,4*TXTABL(,R4)    HIT COUNT WHICH IS UNCERTAIN OR              
         B     GETMESS6            TEXT IS NOT APPROPRIATE                      
*                                                                               
GETMESS2 XR    RF,RF               PREPARE HIT COUNT FOR MESSAGE                
         L     RE,ASEQLIST                                                      
         USING SRCHLSTD,RE                                                      
         ICM   RF,B'0111',SRCHLNAC                                              
         DROP  RE                                                               
         EDIT  (RF),(3,EXTAREA),ALIGN=LEFT                                      
         STC   R0,GTLTXT                                                        
         LA    RE,EXTAREA                                                       
         STCM  RE,B'0111',GTATXT                                                
         B     GETMESS6                                                         
*                                                                               
* OTHER RETURNS ARE TRANSLATED BY LOOKING THEM UP IN TXTABMES                   
*                                                                               
GETMESS4 CLI   0(R4),255                                                        
         BE    GETMESS5                                                         
         CLC   0(1,R4),SBRETURN                                                 
         BE    GETMESS6                                                         
         LA    R4,5(,R4)                                                        
         B     GETMESS4                                                         
GETMESS5 MVC   GTINDX,SBRETURN     RETURN CODE NOT IN TABLE                     
*                                                                               
GETMESS6 MVC   GTMTYP,1(R4)                                                     
         MVC   GTMSGNO,2(R4)                                                    
         DROP  R3                                                               
         EJECT                                                                  
* SAVE EVERYTHING FOR NEXT TIME IN                                              
*                                                                               
         USING TIAD,R3                                                          
SAVEIT   L     R3,ATIA                                                          
         CLI   MODE,MODEXCTQ                                                    
         BNE   SAVESA                                                           
*                                                                               
* IF XCTLED MODE, SAVE WHOLE TIA IN USER ALLOCATED TEMPSTR PAGE                 
*                                                                               
         XR    R1,R1               IF XCTLED MODE                               
         IC    R1,SVPAGENO         WRITE SPECIFIED TEMPSTR PAGE                 
         L     RF,ATIA                                                          
         BAS   RE,TEMPPUT                                                       
         B     SAVESV              THEN GO UPDATE S/R SAVE PAGE                 
*                                                                               
* IF STAND ALONE MODE, SAVE WHAT WE CAN IN SERVICE SAVE SLOT                    
*                                                                               
SAVESA   XC    SVSASAVE,SVSASAVE   IF STAND ALONE MODE SAVE WHAT WE CAN         
         MVC   SVNDXS(9),TSNDXS    FIRST/LAST/MAX INDEXES                       
         MVC   SVDDIRS,SBDDIRS     DIRECTORY ENTRY DISPLACEMENTS                
         LA    RE,SBOTHER+L'SBOTHER-1 END OF 'OTHERS' FIELD                     
         LA    RF,L'SBOTHER        LENGTH THEREOF                               
SAVESA10 CLI   0(RE),C' '          LOOK FOR LAST USED BYTE                      
         BH    SAVESA15                                                         
         BCTR  RE,0                                                             
         BCT   RF,SAVESA10                                                      
         B     SAVESA20                                                         
SAVESA15 LR    RE,RF               COPY SIGNIFICANT 'OTHER' DATA IN             
         BCTR  RE,0                                                             
         MVC   SVSASAVE+1(0),SBOTHER                                            
         EX    RE,*-6                                                           
SAVESA20 STC   RF,SVSASAVE                                                      
         LA    R1,SVSASAVE+1(RF)   NEXT FREE LOCATION                           
         LA    RE,SVSASAVE+L'SVSASAVE-1 END OF SPACE                            
         SR    RE,R1               LENGTH LEFT -1                               
*                                                                               
         IC    RF,TSFILTL          LENGTH INPUT FILTERS                         
         CR    RF,RE               TOO MUCH TO SAVE                             
         BH    TRANSTXT            YES, DON'T SAVE, SO FORCES FIRST             
SAVESA30 STC   RF,0(,R1)                                                        
         BCTR  RF,0                                                             
         MVC   1(0,R1),TSFILTER    SAVE FILTERS AS INPUT                        
         EX    RF,*-6                                                           
*                                                                               
* IN BOTH MODES, COPY SERVICE SAVE STORAGE BACK TO SERVICE PAGE                 
*                                                                               
SAVESV   LH    R1,RECLEN           SIZE OF PAGE                                 
         BAS   RE,GETWRK                                                        
         LR    R7,R0               R7=AQUIRED WORK SPACE FOR S/R PAGE           
*                                                                               
         LA    R1,SRPAGENO         TEMPSTR SERVICE REQUEST PAGE                 
         LR    RF,R7                                                            
         BAS   RE,TEMPLOCK                                                      
*                                                                               
         MVC   SVNDETL,ITEMCNT+1   SET NUMBER OF LINES ON SCREEN                
         MVC   SR$SEAR-SRSD(,R7),SVDATA COPY SAVE DATA BACK IN                  
*                                                                               
         LA    R1,SRPAGENO                                                      
         LR    RF,R7                                                            
         BAS   RE,TEMPPUT                                                       
*                                                                               
         BAS   RE,PUTWRK                                                        
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* TRANSLATE EXTRA TEXT IF NOT FOR SERVICE SYSTEM                                
* CALL GETTXT TO BUNG MESSAGE INTO SCREEN.                                      
*                                                                               
TRANSTXT LA    R3,SBTXTBLK                                                      
         USING GETTXTD,R3                                                       
         MVC   DDFSYS,GTMSYS       USE GTMSYS                                   
         TM    DDFSYS,X'FF'                                                     
         BM    *+8                 0 OR FF=SERVICE, ELSE SDOVSYS                
         MVI   DDFSYS,1            SET SERVICE                                  
         ICM   R1,15,GTLTXT                                                     
         BZ    TRANSTX1            NO &T TEXT                                   
         BAS   RE,DDFIELD                                                       
*                                                                               
TRANSTX1 XR    R5,R5                                                            
         ICM   R5,7,GTASUBST                                                    
         BZ    GETTXTGO            NO &1-&9 TEXT                                
TRANSTX2 LA    R1,1(,R5)                                                        
         ICM   R1,8,0(R5)                                                       
         BZ    GETTXTGO            END OF &1-&9 TEXT                            
         BAS   RE,DDFIELD                                                       
         SRL   R1,24                                                            
         AR    R5,R1                                                            
         B     TRANSTX2                                                         
*                                                                               
GETTXTGO LR    R1,R3                                                            
         L     RF,ACOMFACS                                                      
         L     RF,CGETTXT-COMFACSD(,RF)                                         
         GOTO1 (RF)                                                             
         DROP  R3                                                               
*                                                                               
* READ TWA 0 BACK IN IF DOING AUTO $RE TO CALLER. FILL IN SELECT                
* DETAILS IF AVAILABLE.                                                         
*                                                                               
* *** NOTE *** THIS DESTROYS ALL STORAGE IN TWA                                 
*                                                                               
         CLI   MODE,MODESELQ                                                    
         BE    RESTTWA0            MUST DO AUTO $RE IF SELECT MODE              
         CLI   MODE,MODESTAQ                                                    
         BE    SCREND              CANT DO AUTO $RE IF STAND ALONE              
         CLI   4(R4),C'R'                                                       
         BE    RESTTWA0            RESTORE IF ERROR TYPE REQUIRES IT            
         CLI   FIRST,FIRSTQ                                                     
         BNE   SCREND              OTHERWISE ONLY IF FIRST TIME IN              
         CLI   4(R4),C'N'                                                       
         BE    SCREND              UNLESS ERROR TYPE FORBIDS IT                 
*                                                                               
RESTTWA0 L     R3,ATIA                                                          
         USING TIAD,R3                                                          
         XC    EXTAREA,EXTAREA                                                  
         MVC   EXTAREA(L'SRVMSG),SRVMSG SAVE ERROR MESSAGE                      
         XR    R1,R1               TWA0                                         
         LR    RF,R8                                                            
         BAS   RE,TEMPGET          TWA IS NOW OVERLAID                          
         XC    LASTFLDH,LASTFLDH   DISABLE TAB FIELD                            
         LH    R2,TSFIELD          GET USERS FIELD DISP IN TWA                  
         AR    R2,R8               ADD TWA TO GET ADDRESS                       
         ST    R2,CURSORH          SET UP SO CURSOR GOES TO IT                  
*                                                                               
         IC    R1,SRVMSGH          LENGTH OF USERS MESSAGE FIELD                
         SHI   R1,9                LESS HEADER AND 1 FOR MVC                    
         TM    SRVMSGH+1,X'02'     EXTENDED HEADER                              
         BZ    *+8                 NO                                           
         SHI   R1,8                                                             
         MVC   SRVMSG(0),EXTAREA   COPY ERROR MESSAGE BACK IN                   
         EX    R1,*-6                                                           
*                                                                               
         OI    6(R2),X'01'         SET USERS FIELD MODIFIED                     
         NC    SELIX,SELIX                                                      
         BZ    SCREND              NOTHING SELECTED                             
         IC    R1,0(R2)            LENGTH OF USERS FIELD                        
         SHI   R1,9                LESS HEADER AND 1 FOR MVC                    
         TM    1(R2),X'02'         EXTENDED HEADER                              
         BZ    *+8                 NO                                           
         SHI   R1,8                                                             
         MVC   8(0,R2),TSFDATA     COPY SELECTED KEY IN                         
         EX    R1,*-6                                                           
         MVC   5(1,R2),TSFDATAL    AND LENGTH                                   
         DROP  R3                                                               
         EJECT                                                                  
* IF LASTFLDH NON ZERO, IT POINTS TO NEXT AVAILABLE SCREEN LINE. COPY           
* TAB FIELD OVER IT SO SCREEN IS TRUNCATED.                                     
*                                                                               
SCREND   ICM   RE,15,LASTFLDH      RE=WHERE TO PUT TAB FIELD                    
         BZ    SCREND2             ZERO MEANS DON'T                             
         LH    RF,2(,RE)           SAVE SCREEN ADDRESS                          
         MVC   0(SRVWORK-SRVTABH,RE),SRVTABH COPY SCREEN END                    
         STH   RF,2(,RE)           RESTORE SCREEN ADDRESS                       
*                                                                               
* IF CURSORH NON ZERO, IT POINTS TO THE FIELD HEADER ON OUR SCREEN,             
* OR THE ORIGINAL USERS SCREEN WHERE THE CURSOR IS TO GO.                       
*                                                                               
SCREND2  ICM   RE,15,CURSORH                                                    
         BZ    *+6                                                              
         LR    R2,RE                                                            
         OI    6(R2),X'40'         SET CURSOR                                   
*                                                                               
* IF SELECT MODE, TWA CONTAINS APPLICATIONS TWA, READ IN AT RESTTWA0.           
* THIS MUST BE REWRITTEN BEFORE EXIT.                                           
*                                                                               
         CLI   MODE,MODESELQ                                                    
         BNE   FINISH                                                           
         XR    R1,R1               TWA0                                         
         LR    RF,R8                                                            
         BAS   RE,TEMPPUT          TWA IS NOW REWRITTEN                         
         B     FINISH                                                           
         EJECT                                                                  
* SELECT EXIT USED WHEN 'GOBACK' OPTION APPLIES. INSTEAD OF WRITING             
* SCREEN WITH SELECTED ITEM PLACED IN INPUT FIELD, REQUIRING THE USER           
* TO PRESS ENTER TO CONTINUE, THIS EXIT CONSTRUCTS A BLOCK IN TBUFF             
* WHICH FAMONITOR WILL USE TO REQUEUE THE SELECTED ITEM AS IF IT WERE           
* INPUT BY THE USER, ELIMINATING THE EXTRA SCREEN WRITE/READ.                   
*                                                                               
GOBACK   L     R1,AUTL                                                          
         OI    TFLAG-UTLD(R1),TFLAGHLP   FORCE FULL SCREEN WRITE                
*                                                                               
         ICM   R2,15,TBUFF-UTLD(R1) POINT TO TBUFF                              
         L     R1,ATIA                                                          
         MVC   0(L'TSTBUFF,R2),TSTBUFF-TIAD(R1) COPY LEN/RRCC/DATA              
         XR    RE,RE                                                            
         ICM   RE,1,0(R2)          GET LENGTH OF DATA                           
         BNZ   *+6                                                              
         DC    H'0'                SHOULD NEVER BE ZERO                         
         LA    RE,3(,RE)           ADD 3 TO GIVE MSGQIN FIELD LEN               
         STC   RE,0(,R2)           STORE AS FIELD LEN                           
         AR    RE,R2               POINT TO END OF FIELD                        
         MVI   0(RE),0             ZERO TERMINATOR AFTER FIELD                  
         XC    1(3,RE),1(RE)       XC 3 AFTER TO BE COMPATIBLE                  
*                                  WITH OLD MSGQIN                              
*                                                                               
         XC    SRVMSG,SRVMSG       CLEAR MESSAGE AREA                           
         XC    SRVSRV,SRVSRV       CLEAR SERVICE AREA                           
         MVC   SRVSRV(8),=C'=GOBACK' TELL FAMONITOR TO REQUEUE US               
*                                                                               
* SWITCH BACK TO SERVICE SYSTEM                                                 
*                                                                               
FINISH   ICM   R0,7,=X'FFFFFF'                                                  
         GOTO1 VSWITCH,DMCB,(01,(R0)),0                                         
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                NO REASON TO FAIL SO DIE IF IT DOES          
*                                                                               
EXIT     XMOD1                                                                  
*                                                                               
PACHAREA DC    32S(*)                                                           
         EJECT                                                                  
* ASSORTED SUBROUTINES                                                          
*                                                                               
* TEMPSTR IO ROUTINES. R1=PAGE NUMBER, RF=IO AREA ADDRESS                       
*                                                                               
TEMPGET  LA    R0,DMREAD           READ A TEMPSTR PAGE                          
         B     TEMPIO                                                           
TEMPLOCK LA    R0,DMREAD           READ AND LOCK A TEMPSTR PAGE                 
         ICM   R0,B'1000',=X'80'                                                
         B     TEMPIO                                                           
TEMPPUT  LA    R0,DMWRT            WRITE A TEMPSTR PAGE                         
*                                                                               
TEMPIO   NTR1                                                                   
         ST    R0,DMCB             R0=COMMAND                                   
         SLL   R1,32-8             R1=PP000000 PAGE NUMBER                      
         L     RE,AUTL                                                          
         USING UTLD,RE                                                          
         ICM   R1,B'0011',TNUM     R1=PP00TTTT +TERMINAL NUMBER                 
         DROP  RE                                                               
         ST    R1,DMCB+8                                                        
         ST    RF,DMCB+12          RF=IO AREA ADDRESS                           
         XC    DMCB+16(4),DMCB+16                                               
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         L     RF,ACOMFACS                                                      
         L     RF,CDATAMGR-COMFACSD(,RF)                                        
         GOTO1 (RF),DMCB,,TEMPSTR                                               
         CLI   8(R1),0                                                          
         BE    EXIT                                                             
         DC    H'0'                TEMPSTR IO ERROR                             
*                                                                               
* EXTEND WORKING STORAGE                                                        
*        R1=LENGTH OF STORAGE REQUIRED                                          
*        R0=ADDRESS OF STORAGE REQUIRED ON EXIT                                 
*                                                                               
GETWRK   LA    R1,7(,R1)           ROUND TO DWRD                                
         SRL   R1,3                                                             
         SLL   R1,3                                                             
         L     RF,4(,RD)           GET PREVIOUS SAVE AREA                       
         LR    R0,RD               ADDRESS OF STORAGE AQUIRED                   
         AR    RD,R1               SET NEW SVAREA ADDRESS                       
         ST    RD,8(,RF)           SET NEW FORWARD POINTER                      
         ST    RF,4(,RD)           SAVE BACKWARD POINTER IN NEW SVAREA          
         CR    RD,R8               RD = END OF WS VS R8 = START OF TWA          
         BNHR  RE                                                               
         DC    H'00'               YOU'VE EXCEEDED THE WORKING STORAGE          
*                                                                               
* RELEASE ALL GETWRK'ED STORAGE                                                 
*                                                                               
PUTWRK   L     RF,4(,RD)           GET PREVIOUS SAVE AREA                       
         LA    RD,72(,RF)          END OF PREVIOUS SAVE AREA                    
         CR    RD,RC                                                            
         BNE   *+8                 SKIP IF NOT NMODDED STORAGE                  
         LA    RD,(((WRKL+7)/8)*8)(,RD) ALLOW FOR IT IF IT IS                   
         ST    RD,8(,RF)           RESET FORWARD POINTER                        
         ST    RF,4(,RD)           RESET BACKWARD POINTER IN NEW SVAREA         
         BR    RE                                                               
         EJECT                                                                  
* DDFTWA  - TWA FIELD DICTIONARY TRANSLATE                                      
*        R1=A(FIELD HEADER), DDFSYS=SYSTEM                                      
* DDFIELD - TWA FIELD DICTIONARY TRANSLATE                                      
*        R1=AL1(LENGTH),AL3(FIELD), DDFSYS=SYSTEM                               
*                                                                               
DDFTWA   NTR1  WORK=(R5,10)        80 BYTES WORK AREA                           
         LR    R3,R1               R3=FIELD HEADER                              
         XR    R4,R4                                                            
         IC    R4,0(,R3)                                                        
         SHI   R4,9                R4=MAX FLD LEN - 1                           
         TM    1(R3),X'02'         TEST EXTENDED FIELD HEADER                   
         BZ    *+8                                                              
         SHI   R4,8                                                             
         LA    R3,8(,R3)           R3=FIELD                                     
         B     DDFFT                                                            
*                                                                               
DDFIELD  NTR1  WORK=(R5,20)        160 BYTES WORK AREA                          
         LR    R3,R1               R3=FIELD                                     
         LR    R4,R1                                                            
         SRL   R4,24               R4=FIELD LENGTH                              
         BCTR  R4,0                                                             
*                                                                               
DDFFT    EX    R4,DDFSAVE                                                       
         L     R1,=A(TRTDD)                                                     
         A     R1,RELO                                                          
         EX    R4,DDFTEST          TEST IF DATA DICTIONARY REQUIRED             
         BZ    EXIT                NO, SKIP IT                                  
*                                                                               
         MVI   0(R5),C'T'          SET TRANSLATE WHOLE FIELD                    
         MVI   1(R5),C'L'          SET LOWER CASE                               
         MVC   2(1,R5),DDFSYS      SET SYSTEM                                   
         MVC   3(1,R5),SBLANG      SET LANGUAGE                                 
         CLI   DDFSYS,0                                                         
         BNE   DDFFTGO             SKIP IF SYSTEM SPECIFIED                     
         MVI   2(R5),C' '          PRESET CONNECT SYSTEM                        
         ICM   R1,15,SBASYS        USE SDOVSYS IF AVAILABLE                     
         BZ    DDFFTGO                                                          
         MVC   2(1,R5),SDOVSYS-SDSYSD(R1) SET SYSTEM                            
*                                                                               
DDFFTGO  LA    R1,24(,R5)          SAVE SYSTEM BEFORE DICTATE CALL              
         ICM   R0,7,=X'FFFFFF'                                                  
         GOTO1 VSWITCH,(R1),(0,(R0)),0                                          
         L     RE,0(,R1)                                                        
         MVC   SAVESE,TCBSYS-TCBD(RE)                                           
*                                                                               
         LA    RF,1(,R4)                                                        
         GOTO1 VDICTATE,(R5),,((RF),(R3)) DICTATE FOR TRANSLATION               
*                                                                               
         ICM   R0,7,=X'FFFFFF'                                                  
         GOTO1 VSWITCH,(R1),(0,(R0)),0                                          
         L     RE,0(,R1)                                                        
         CLC   SAVESE,TCBSYS-TCBD(RE) TEST STILL IN SAME SYSTEM                 
         BE    EXIT                YES, EXIT                                    
*                                                                               
         ICM   R0,7,=X'FFFFFF'                                                  
         GOTO1 (RF),(R1),(SAVESE,(R0))                                          
         CLI   4(R1),0             OK SWITCH TO SYSTEM                          
         BE    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
DDFSAVE  MVC   0(0,R5),0(R3)                                                    
DDFTEST  TRT   0(0,R5),0(R1)                                                    
         EJECT                                                                  
* STORAGE                                                                       
*                                                                               
         LTORG                                                                  
*                                                                               
DMREAD   DC    C'DMREAD'                                                        
DMWRT    DC    C'DMWRT '                                                        
TEMPSTR  DC    C'TEMPSTR'                                                       
*                                                                               
* TABLE TRANSLATES SEARCH RETURN CODES TO GENERAL TEXT NUMBER                   
*                                                                               
* FIRST 9 ARE SPECIAL TXTAB ENTRIES FOR NORMAL RETURN. THEY MUST                
* REMAIN IN THE SEQUENCE SHOWN. THE FIRST ENTRY IS ALSO USED IF                 
* THE MESSAGE IS AN APPLICATION SYSTEM MESSAGE.                                 
*                                                                               
* BYTE 1   = SEARCH RETURN CODE (SBRETURN)                                      
* BYTE 2   = GETMESS MESSAGE TYPE                                               
* BYTE 3/4 = GETMESS MESSAGE NUMBER                                             
* BYTE 5   = APPLICATION SCREEN RESTORE FLAG, USED ONLY IF XCTLED MODE          
*            C'N' MEANS DO NOT RESTORE                                          
*            C'R' MEANS ALWAYS RESTORE                                          
*            C' ' MEANS ONLY RESTORE IF FIRST TIME IN.                          
*                                                                               
TXTAB    EQU   *                                                                
TXTABEOL DC    AL1(SBREOLQ,GTMINF),AL2(120),C' ' NONE FOUND                     
TXTABL   EQU   *-TXTAB                                                          
*                                               'MATCHING NAMES FOUND'          
         DC    AL1(SBREOLQ,GTMINF),AL2(121),C'N' 1 PAGE                         
         DC    AL1(SBREOLQ,GTMINF),AL2(122),C'N' MULTIPLE PAGES                 
         DC    AL1(SBREOLQ,GTMINF),AL2(123),C'N' 1 PAGE, SELECT                 
         DC    AL1(SBREOLQ,GTMINF),AL2(124),C'N' MULTIPLE PAGES, SELECT         
*                                               'ITEMS DISPLAYED'               
         DC    AL1(SBREOLQ,GTMINF),AL2(125),C'N' 1 PAGE                         
         DC    AL1(SBREOLQ,GTMINF),AL2(126),C'N' MULTIPLE PAGES                 
         DC    AL1(SBREOLQ,GTMINF),AL2(127),C'N' 1 PAGE, SELECT                 
         DC    AL1(SBREOLQ,GTMINF),AL2(128),C'N' MULTIPLE PAGES, SELECT         
*                                                                               
TXTABMES DC    AL1(SBRSECQ,GTMERR),AL2(055),C'R'                                
         DC    AL1(SBENCTQ,GTMERR),AL2(108),C' '                                
         DC    AL1(SBENOPQ,GTMERR),AL2(098),C'R'                                
         DC    AL1(SBAUTHQ,GTMERR),AL2(099),C'R'                                
*                                                                               
         DC    AL1(SBEMIFQ,GTMERR),AL2(001),C' '                                
         DC    AL1(SBEIIFQ,GTMERR),AL2(002),C' '                                
         DC    AL1(SBEFTSQ,GTMERR),AL2(100),C' '                                
         DC    AL1(SBEFTLQ,GTMERR),AL2(101),C' '                                
         DC    AL1(SBETMNQ,GTMERR),AL2(102),C' '                                
         DC    AL1(SBEIKWQ,GTMERR),AL2(103),C' '                                
         DC    AL1(SBEDKWQ,GTMERR),AL2(104),C' '                                
         DC    AL1(SBEIFVQ,GTMERR),AL2(105),C' '                                
*                                                                               
         DC    AL1(SBEMNFQ,GTMERR),AL2(110),C' '                                
         DC    AL1(SBETNFQ,GTMERR),AL2(111),C' '                                
         DC    AL1(SBEDNFQ,GTMERR),AL2(112),C' '                                
         DC    AL1(SBEXNFQ,GTMERR),AL2(113),C' '                                
         DC    AL1(SBENWFQ,GTMERR),AL2(114),C' '                                
         DC    AL1(SBETMWQ,GTMERR),AL2(115),C' '                                
         DC    AL1(SBELTSQ,GTMERR),AL2(116),C' '                                
         DC    AL1(SBESELQ,GTMERR),AL2(048),C' '                                
*                                                                               
         DC    AL1(SBEHLPQ,GTMINF),AL2(001),C'N'                                
*                                                                               
         DC    AL1(SBESYSQ,GTMERR),AL2(106),C'R'                                
         DC    AL1(SBESRCQ,GTMERR),AL2(107),C'R'                                
*                                                                               
         DC    AL1(255,GTMERR),AL2(0),C' '                                      
*                                                                               
HELPID   DS    0C                  SYSTEM/ITEM</QUALIFIERS>'                    
         DCDD  SR#SYS,8,S                                                       
         DC    C'/'                                                             
         DCDD  SR#SRCH,8,S                                                      
         DC    C'</'                                                            
         DCDD  SR#QUALF,12,S                                                    
         DC    C'>'                                                             
HELPIDLQ EQU   *-HELPID                                                         
*                                                                               
HPFHELP  DS    0C                                                               
         DC    C' PF1='                                                         
         DCDD  SR#HELP,8,S                                                      
HPFHELQ  EQU   *-HPFHELP                                                        
HPFPAGE  DS    0C                                                               
         DC    C',PF2='                                                         
         DCDD  SR#PREV,8,S                                                      
         DC    C',PF3='                                                         
         DCDD  SR#NEXT,8,S                                                      
HPFPALQ  EQU   *-HPFPAGE                                                        
HPFPRINT DS    0C                                                               
         DC    C',PF7='                                                         
         DCDD  SR#PRINT,8,S                                                     
HPFPRLQ  EQU   *-HPFPRINT                                                       
HPFRETRN DS    0C                                                               
         DC    C',PF12='                                                        
         DCDD  SR#RETRN,8,S                                                     
HPFRELQ  EQU   *-HPFRETRN                                                       
*                                                                               
PRINTSPC DS    0X                                                               
         SPEC  H1,80,PAGE                                                       
         SPEC  H1,90,RUN                                                        
         SPEC  END                                                              
         SPACE 1                                                                
TRTDD    DC    256AL1(0)           TO TEST FOR DDICT ESCAPE SEQUENCES           
         ORG   TRTDD+X'20'                                                      
         DC    16AL1(*-TRTDD)      20-2F                                        
         ORG   ,                                                                
         EJECT                                                                  
*              DSECT TO COVER W/S                                               
*                                                                               
WRKD     DSECT                                                                  
K        EQU   1024                                                             
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
FLAG     DS    C                                                                
RELO     DS    F                   RELOCATION FACTOR                            
SAVERE   DS    F                                                                
*                                                                               
APARMS   DS    A                   ADDRESS OF INPUT PARM LIST                   
PARMS    DS    0A                  COPY OF INPUT PARM LIST                      
ASYSFAC  DS    A                                                                
ATIA     DS    A                                                                
AUTL     DS    A                                                                
ACOMFACS DS    A                                                                
ASELIST  DS    A                                                                
ATWA     DS    A                                                                
APHASEMP DS    A                                                                
ATIOB    DS    A                                                                
PARMSL   EQU   *-PARMS                                                          
ATCB     DS    A                                                                
*                                                                               
ASEQLIST DS    A                   A(SEQUENCE LIST IN TIA)                      
*                                                                               
VSRCHXEC DS    A                   A(SEARCH EXECUTIVE MODULE)                   
VREPORT  DS    A                   A(FAREPORT)                                  
VDICTATE DS    A                   A(DICTATE)                                   
VSWITCH  DS    A                   A(SWITCH)                                    
*                                                                               
DMCB     DS    6F                                                               
WORK     DS    CL64                                                             
TEMP     DS    CL80                                                             
*                                                                               
SPACES   DS    CL80                                                             
*                                                                               
SELIX    DS    A                   INDEX OF ITEM SELECTED                       
ITEMCNT  DS    H                   NUMBER OF ITEMS EXTRACTED THIS TIME          
HELPADDR DS    H                   ACTION FIELD SCREEN ADDRESS FOR HELP         
SELEADDR DS    H                   ACTION FIELD SCREEN ADDR FOR SELADDR         
*                                                                               
RECLEN   DS    XL2                 TEMPSTR RECORD SIZE                          
CHKDSP   DS    XL2                 TEMPSTR DISPLACEMENT TO CHKPNT               
*                                                                               
* FLAGS AND INDICATORS ETC.                                                     
*                                                                               
MODE     DS    XL1                 INVOCATION MODE                              
MODEXCTQ EQU   0                   TRANSFER FROM APPLICATION                    
MODESTAQ EQU   1                   STAND ALONE ($SEARCH ENTERED)                
MODESELQ EQU   2                   EXIT MODE, ITEM SELECTED                     
*                                                                               
FIRST    DS    XL1                 INDICATES FIRST TIME                         
FIRSTQ   EQU   1                   THIS IS FIRST TIME IN                        
*                                                                               
DIRFLAG  DS    XL1                 DIRECTORY FLAG                               
FILTERQ  EQU   1                   DIRECTORY FILTERING REQUIRED                 
*                                                                               
PFKEY    DS    XL1                 PF KEY (1-12), 13-24 ADJUSTED BY -12         
PFHELP   EQU   1                                                                
PFPREV   EQU   2                                                                
PFPRINT  EQU   7                                                                
PFRETURN EQU   12                                                               
*                                                                               
COMMA    DS    C                                                                
EQUALS   DS    C                                                                
DDFSYS   DS    X                   SYSTEM FOR DDFIELD/DDFTWA                    
SAVESE   DS    X                   SAVED SENUM FOR DDFIELD/DDFTWA               
*                                                                               
PRINTID  DS    XL3                 PRTQ ID IF TO BE PRINTED, ELSE NULL          
DATEB    DS    XL3                 COPY OF SSBDATEB                             
*                                                                               
* STORAGE USED IN BUILDING SEARCH LIST ON SCREEN                                
*                                                                               
CURSORH  DS    A                   A(FIELD HEADER FOR CURSOR)                   
LASTFLDH DS    A                   A(END OF USED SCREEN, TAB LOCATION)          
NEXTLWRK DS    A                   A(NEXT FREE LINEWRK ENTRY)                   
LASTLWRK DS    A                   A(LAST AVAILABLE LINEWRK ENTRY)              
NEXTLWIX DS    X                   INDEX OF NEXT LINEWRK ENTRY (REL 1)          
LINESRQD DS    X                   MAX NUMBER OF LINES REQUIRED FOR ITM         
SLNSFREE DS    X                   NUMBER OF LINES LEFT ON SCREEN               
NEXTPREV DS    X                   SEARCH DIRECTION                             
NEXTQ    EQU   0                   NEXT (FORWARD)                               
PREVQ    EQU   1                   PREVIOUS (BACKWARD)                          
SCRMAP   DS    16XL4,X             DEFINES HOW EXTAREA MAPS TO SCREEN           
*        ONE ENTRY PER COLUMN, END=X'FF', BUILT FROM EXTRACT HEADER             
*        USED TO FORMAT RECORD EXTRACT INTO ONE OR MORE SCREEN LINES            
*        BYTE 0=LINE NUMBER (0=1ST LINE) FOR THIS COLUMN                        
*        BYTE 1=RELATIVE OFFSET IN LINE (0=1ST POSITION) FOR COLUMN             
*        BYTE 2=WIDTH OF COLUMN (PRE-ADJUSTED BY -1 FOR MVC)                    
*        BYTE 3=RELATIVE OFFSET OF COLUMN DATA IN EXTAREA                       
*                                                                               
         DS    0F                                                               
FACAREA  DS    CL256               APPLICATION FACILITIES AREA                  
*                                                                               
EXTAREA  DS    0CL256              SEARCH BUILDS RECORD EXTRACT HERE            
SWAREA   DS    CL320               TCB SWITCH WORK AREA                         
#SELECTS DS    XL1                                                              
*                                                                               
* SPACE FOR SEARCH CONTROL BLOCK                                                
*                                                                               
         DS    0D                                                               
SRCHBLK  DS    CL(SEARCHDX)                                                     
*                                                                               
*        DATA DICTIONARY LISTS                                                  
*                                                                               
SRCH     CSECT                                                                  
DDLCTBL  DS    0C                  LOWER CASE ITEMS                             
         DCDDL SR#1ST,10                                                        
         DCDDL SR#PREV,10                                                       
         DCDDL SR#LAST,10                                                       
         DCDDL SR#SLECT,10                                                      
         DCDDL SR#RETRN,10                                                      
         DCDDL SR#LSTOF,8                                                       
         DCDDL SR#FLTRS,8                                                       
         DC    X'00'                                                            
WRKD     DSECT                                                                  
DDLCLST  DS    0C                                                               
         DSDDL PRINT=YES                                                        
*                                                                               
SRCH     CSECT                                                                  
DDUCTBL  DS    0C                  UPPER CASE ITEMS                             
         DCDDL SR#PRINT,10                                                      
         DC    X'00'                                                            
WRKD     DSECT                                                                  
DDUCLST  DS    0C                                                               
         DSDDL PRINT=YES                                                        
WRKL     EQU   *-WRKD                                                           
         SPACE                                                                  
* SCREEN LINE DSECT, COVERS ONE LINE IN TWA                                     
*                                                                               
LINED    DSECT                                                                  
LINE     DS    CL(SRVACT2H-SRVACT1H)                                            
         ORG   LINE                                                             
ACTH     DS    CL8                                                              
ACT      DS    CL(L'SRVACT1)                                                    
         ORG   LINE+(SRVTXT1H-SRVACT1H)                                         
TXTH     DS    CL8                                                              
TXT      DS    CL(L'SRVTXT1)                                                    
         ORG   ,                                                                
LINEL    EQU   *-LINE                                                           
         ORG   LINE                                                             
PLINE    DS    CL132                                                            
         EJECT                                                                  
* TIA STORAGE DSECT (SAVED ON TEMPSTR IF NOT STAND ALONE MODE)                  
*                                                                               
TIAD     DSECT                                                                  
       ++INCLUDE GESRCHTMPD                                                     
TSSAVED  EQU   *-TIAD                                                           
TSSAVE   DS    0C                                                               
TSTBUFF  DS    0XL81               TBUFF DATA RETURNED FOR GOBACK               
TSFDATAL DS    XL1                 RETURNED SEARCH FIELD DATA LENGTH            
TSFRRCC  DS    XL2                 ROW/COL FROM SEARCH FIELD                    
TSFDATA  DS    XL78                ORIGINAL/RETURNED SEARCH FIELD DATA          
TSFILTL  DS    X                   INPUT LENGTH OF DATA IN TSFILTER             
TSFILTER DS    CL(L'SRVFILT)       COPY OF INPUT FILTER FIELD                   
TSNAMES  DS    CL(L'SBNAMES)       COPY OF PREVIOUS PARSED NAMES                
TSASAVE  DS    CL(L'SBASAVE)       COPY OF PREVIOUS SBASAVE                     
TSSCRNDX DS    21XL5,X             LIST OF ITEMS ON SCREEN, END=XFF             
*                                  XL3=SEQLIST INDEX OF ITEM                    
*                                  XL2=SCREEN ADDRESS OF ACTION FIELD           
TSNDXS   DS    0XL6                                                             
TSPRVNDX DS    XL3                 SRCHLNXT INIT VALUE FOR BACKWARD             
TSNXTNDX DS    XL3                 SRCHLNXT INIT VALUE FOR FORWARD              
TSMAXNDX DS    XL3                 SRCHLNAC COPY (MAX VALUE)                    
TSSEQLOK DS    X                   NON-ZERO IF SEQLIST VALID                    
         DS    0D                                                               
SEQLIST  DS    0C                  SEQUENCE LIST                                
SEQLISTL EQU   TIAD+6144-*         AVAILABLE SPACE FOR SEQLIST                  
TSSAVEL  EQU   *-TSSAVE+SRCHLLNQ   CLEAR UP TO END OF SEQLIST HEADER            
         EJECT                                                                  
TWAD     DSECT                     SAME NAME AS FATWA, SAVES REGISTER           
         DS    CL64                (SEE FATWA)                                  
       ++INCLUDE SRSRCFFD                                                       
*                                                                               
* NUMBER OF LINES AVAILABLE FOR SEARCH LIST DISPLAY                             
*                                                                               
NLINES   EQU   ((SRVACTXH-SRVACT1H)/(SRVACT2H-SRVACT1H))+1                      
*                                                                               
* SPACE FOR REPORT CONTROL BLOCK                                                
*                                                                               
         DS    0D                                                               
REPTBLK  DS    CL(REPBLKL)                                                      
*                                                                               
* COPY OF $SEARCH SAVED STORAGE FROM SERVICE REQUEST SAVE PAGE                  
*                                                                               
SVDATA   DS    0CL(L'SR$SEAR)                                                   
SVTTRCNT DS    XL2                 PREVIOUS TERMINAL TRANSACTION COUNT          
SVPAGENO DS    X                   PREVIOUS SAVE STORAGE PAGE                   
SVNDETL  DS    X                   NUMBER OF USED DETAIL LINES ON SCRN          
*        REMAINDER USED ONLY IF RUNNING STAND ALONE                             
SVNDXS   DS    XL6                 SAVED TSNDXS                                 
SVMAX    DS    XL3                 SAVED TSMAXNDX                               
SVDDIRS  DS    0CL4                                                             
SVDSYS   DS    XL2                 SAVED SBDSYS                                 
SVDSRCH  DS    XL2                 SAVED SBDSRCH                                
SVSASAVE DS    XL(L'SR$SEAR-(*-SVDATA))  SAVED SEARCH STATUS                    
*                                                                               
* WORK AREA IN WHICH SCREEN DISPLAY IS INITIALLY CONSTRUCTED BEFORE             
* BEING COPIED TO SCREEN.                                                       
* ITEMS ARE BUILT STARTING AT LINEWRK AND CONTINUING TOWARDS LINEWRKX,          
* REGARDLESS OF DIRECTION ITEMS ARE RETURNED IN (FORWARD OR BACK).              
* LINES ARE THEN COPIED TO SCREEN IN SAME ORDER IF FORWARD, OR IN               
* REVERSE ORDER IF BACKWARD, THUS THE SCREEN ALWAYS LISTS IN FORWARD            
* DIRECTION EVEN IF READING BACKWARDS.                                          
* EACH ITEM MAY FILL MORE THAN ONE LINE, THE NUMBER OF LINES AVAILABLE          
* DEPENDS ON THE NUMBER USED FOR HEADINGS.                                      
* LINESQIX CONTAINS THE 1-BASED INDEX OF THE ITEM'S ENTRY IN THE SEARCH         
* SEQUENCE LIST. IT IS ZERO ON AN ITEM'S CONTINUATION LINES, IF ANY.            
* LINE1IX IS ZERO IF THIS IS 1ST LINE FOR AN ITEM, ELSE IT HOLDS THE            
* 1-BASED INDEX OF THE 1ST LINE FOR WHICH THIS IS A CONTINUATION.               
*                                                                               
LINEWRK  DS    0C                  ONE PER LIST LINE ON SCREEN                  
LINE1IX  DS    X                   ZERO IF FIRST LINE FOR AN ITEM,              
*                                  ELSE INDEX OF 1ST LINE FOR ITEM              
LINESQIX DS    XL3                 INDEX OF ITEM ON THIS LINE                   
LINETXT  DS    CL(L'SRVTXT1)       TEXT TO APPEAR ON THE SCREEN                 
LINEWRKL EQU   *-LINEWRK                                                        
         DS    (NLINES-1)CL(LINEWRKL)                                           
LINEWRKX DS    0C                                                               
         ORG   REPTBLK             RESET TO REDEFINE REPORT BLOCK               
         EJECT                                                                  
       ++INCLUDE FAREPBLK                                                       
*                                                                               
         ORG   TWAD                RESET FOR LATER FATWA DSECT                  
         EJECT                                                                  
       ++INCLUDE GESRCHBLKD                                                     
         EJECT                                                                  
       ++INCLUDE GESRCHDIRD                                                     
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
         PRINT OFF                                                              
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
* FACHKPT                                                                       
       ++INCLUDE FACHKPT                                                        
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
* SRDDEQUS                                                                      
       ++INCLUDE SRDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029SRSRC00   07/07/09'                                      
         END                                                                    
