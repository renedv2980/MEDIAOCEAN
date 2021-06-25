*          DATA SET PPMAT02    AT LEVEL 131 AS OF 07/08/03                      
*PHASE T40202C                                                                  
*INCLUDE NUMED                                                                  
*                                                                               
*    CHANGE LOG                                                                 
*                                                                               
*   SMYE 07/02    FIX LOCK TESTING FOR SUB-CLIENTS                              
*                                                                               
*   SMYE 12/14/01 FIX "DISCREPANT ENTRY" RATE CALCULATION IN VR42               
*                                                                               
*   SMYE 11/26/01 IN VPREM DO NOT GROSS UP NET ENTRY IF BUY NOT                 
*                 FOUND - "RUN-NOT-ORDERED" PROBLEM                             
*                                                                               
*   SMYE 11/01   ADD PREMIUM ENTRY AT NET IN VPREM                              
*                                                                               
*   SMYE 06/01   ADD LOCK TESTING FOR UPDATIVE SOON CONDITIONS                  
*                                                                               
*   SMYE 03/01   OC SPACES FOR PBINVPRD IN PBINVELM IN BUYREC -                 
*                2-CHAR PRD CODE HAS BINARY ZERO IN 3RD POSITION                
*                WHICH SHOULD BE A SPACE (X'40').                               
*                                                                               
*   KWAN 06/00   NO OP INSTRUCTION WITH GLV1GOTO EQUATE                         
*                                                                               
*   BPLA  3/99   DON'T SET QPRD IN RDPROD, IT DESTROYS QPRD WHEN                
*                IT WAS "***"                                                   
*                CAUSED DUMP IN PPMAT06                                         
*                                                                               
*   BPLA  3/98   PREVENT DUMPS IN PAYPRGM WHEN THEY                             
*                TRY TO GO THERE BY MISTAKE                                     
*                                                                               
*   BPLA 2/98    PREVENT GETINS DUMPS WHEN UPDATING                             
*                                                                               
*   BPLA 2/98    TO PREVENT DUMPS CHECK MINERR IN MINIORD                       
*                IF RECORD (ELEM) NOT FOUND DON'T DIE                           
*                JUST SET CC AND CHECK WHEN I GET BACK.                         
*                ALL PLACES NOW GO TO INVLSEL ERROR                             
*                (INVALID SELECTION) NOW IF CC NE                               
*                                                                               
*   BPLA 2/98    CHECK PIMCOST AND PIMUNITS COMBO                               
*                TO PREVENT DUMPS IN GETINS WHEN THEY                           
*                FORGET TO ENTER THE "T" FOR TOTAL COSTS                        
*                                                                               
*   BPLA 2/98    FIX OUTDOOR SPACE EDITING                                      
*                                                                               
*   BPLA 2/5/98  CODE REMOVED FROM VSIZE AND VPREM THAT WAS                     
*                STUPIDLY TRYING CHECK THE SPACE AND PREMIUM                    
*                VS DATA IN PJOBREC WHICH THE PROGRAM NEVER READ                
*                (AND DIDN'T NEED TO READ OR CHECK)                             
*                                                                               
***********************************************************************         
*                                                                               
*  TITLE: T40202 - CHECKING OF PRINT INVOICES                                   
*                                                                               
*  CALLED FROM: PRINT INVOICE CONTROLLER (T40200), WHICH CALLS                  
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  SCREENS:     PPMATFB (T402FB) -- CHECK/UPDATE SCREEN BEFORE LIST             
*               PPMATEE (T402EE) -- SUPER CHECK SCREEN FOR NEWSPAPERS           
*               PPMATED (T402ED) -- SUPER CHECK SCREEN FOR MAGAZINES            
*               PPMATEB (T402EB) -- CHECK SCREEN FOR NEWSPAPERS                 
*               PPMATEC (T402EC) -- UPDATE SCREEN FOR NEWSPAPERS                
*               PPMATDB (T402DB) -- CHECK SCREEN FOR MAGAZINES                  
*               PPMATDC (T402DC) -- UPDATE SCREEN FOR MAGAZINES                 
*               PPMATCB (T402CB) -- SCREEN FOR COMMENTS OF DETAILS              
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - WORK (SCREEN FIELD HEADER)                                      
*          R3 - WORK                                                            
*          R4 - OVERLAY SAVED STORAGE    (MYAREAD)                              
*          R5 - MINIO CONTROL BLOCK      (MINBLKD)                              
*          R6 - WORK (MINELEM, A(REC))                                          
*          R7 - SECOND BASE                                                     
*          R8 - SPOOLD                                                          
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM/WORK                                                     
*          RF - SYSTEM/WORK                                                     
*                                                                               
*****NOTE*****                                                                  
* IF YOU ARE MAKING CHANGES IN THIS PROGRAM THAT USE THE FIELD 'QPRD'           
* OR THAT REQUIRE ADDITIONAL CALLS TO EXISTING PRINT MATCH SUBROUTINES          
* BE AWARE THAT WITH PRODUCT VARIOUS, QPRD WILL BE *** AND THE SPECIFIC         
* PRODUCT WILL ACTUALLY BE IN THE MINIO DETAIL ELEMENT (PIMSPRD IS THE          
* FIELD).  THIS MAY AFFECT YOUR CHANGES SO YOU SHOULD LOOK AT THE               
* SUBROUTINE AND AT ALREADY EXISTING CALLS TO IT. THANKS, ABBEY                 
*                                                                               
***********************************************************************         
T40202   TITLE 'PPMAT02 - PRINT INVOICE CHECKING OVERLAY'                       
T40202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T40202*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R4,SYSSPARE         OVERLAY SAVED STORAGE                        
         USING MYAREAD,R4                                                       
         LA    R5,MINBLOCK         MINIO CONTROL BLOCK                          
         USING MINBLKD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
MAIN     BAS   RE,SETPFTBL         SET THE PFKEY TABLE                          
*                                                                               
CKMODES  CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SET THE APPROPRIATE PFKEY TABLE                                               
***********************************************************************         
SETPFTBL NTR1                                                                   
         SR    R2,R2               NO TABLE NEEDED YET                          
*                                                                               
         TM    OVLYFLAG,X'80'      IN COMMENTS SCREEN?                          
         BZ    SETPF10                                                          
         CLI   PFKEY,12            YES, RETURN TO THE CHECK SCREEN?             
         BE    SETPFX                                                           
SETPF05  MVI   PFKEY,0             CLEAR THE PFKEY                              
         L     R1,ATIOB                                                         
         MVI   TIOBAID-TIOBD(R1),0                                              
         B     SETPFX                                                           
*                                                                               
SETPF10  TM    OVLYFLAG,X'08'      IN TOTALS SCREEN?                            
         BZ    *+12                YES                                          
         NI    OVLYFLAG,X'FF'-X'08'                                             
         B     SETPF05                                                          
*                                                                               
         CLI   PFKEY,2             ONE OF THE PROGRAMMED PFKEYS?                
         BL    SETPF20                                                          
         CLI   PFKEY,12                                                         
         BL    SETPFX              YES                                          
*                                                                               
SETPF20  LA    R2,PFTABLE          USE THIS TABLE                               
*                                                                               
SETPFX   GOTO1 INITIAL,DMCB,(R2)                                                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   GLOBBER,CGLOBBER                                                 
         DROP  R1                                                               
*                                                                               
         GOTO1 CALLOV,DMCB,(X'22',0),(0,0)                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC),(RA),(R9),(R8),(R5),(R4)                          
*                                                                               
         CLC   QPRD2,=C'CDL'                                                    
         BNE   XIT                                                              
         XC    QPRD2,QPRD2                                                      
         GOTO1 CALLOV,DMCB,(X'04',0),(0,0) USE CHECK DISPLAY LOGIC              
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC),(RA),(R9),(R8),(R5),(R4)                          
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD                                                           
***********************************************************************         
VR       DS    0H                                                               
*                                                                               
         CLI   PFKEY,11            CHECK REPORT?                                
         BNE   VREC01                                                           
*                                                                               
         CLI   CHKINITH+5,0        NEED USER'S INITIALS                         
         BNE   *+12                                                             
         LA    R2,CHKINITH                                                      
         B     MISSFLD                                                          
*                                                                               
         GOTO1 CALLOV,DMCB,(X'06',0),(0,0) CALL CHECK REPORT OVERLAY            
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC),(RA),(R9),(R8),(R5),(R4)                          
*                                                                               
         MVC   QPRD2,=C'REP'                                                    
         GOTO1 CALLOV,DMCB,(X'22',0),(0,0) CALL VKEY OVERLAY AGAIN              
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC),(RA),(R9),(R8),(R5),(R4)                          
*                                                                               
VREC01   CLI   PAYFLAG,C'Y'                                                     
         BNE   VREC05                                                           
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'GETD',BLOCK,80,GLVPRRTN                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'DELE',,,GLVPRRTN  DELETE RETURN VARIABLE         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =A(MARKPAID),DMCB,(RC),RR=RELO    MARK DETAILS PAID              
*                                                                               
         MVI   GERROR1,BLNKMESS    YES, INVOICE HAS BEEN PAID                   
         MVC   BLOCK(256),BLOCK+1  COPY LENGTH & MESSAGE TO BEG(BLOCK)          
         ZIC   R1,BLOCK                                                         
         LA    R1,BLOCK(R1)                                                     
         MVI   0(R1),0             TERMINATING 0                                
*                                                                               
         LA    R2,CHKOPTNH                                                      
         OI    GENSTAT2,USGETTXT                                                
         XC    GETTXTCB,GETTXTCB                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMSYS,24                                                        
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
         B     INFEXIT                                                          
*                                                                               
VREC05   DS    0H                                                               
*                                                                               
         CLI   PFKEY,10            TOTAL SCREEN?                                
         BNE   *+12                                                             
         OI    OVLYFLAG,X'08'      YES                                          
         B     VR200               THEN GO THERE                                
*                                                                               
         TM    OVLYFLAG,X'80'      IF COMMENTS                                  
         BNZ   VR200               THEN GO THERE                                
*                                                                               
         CLI   PFKEY,2             SWITCH TO NEXT SCREEN?                       
         BNE   VR01                                                             
         MVI   WINDOW,0                                                         
         OI    BITFLAG,X'80'+X'40' MAKE BELIEVE FIRST TIME IN                   
*                                  AND SWITCHED TO ANOTHER SCREEN               
         OI    CONACTH+6,X'80'                                                  
*                                                                               
         CLI   ACTNUM,ACTCHECK     GO TO UPDATE SCREEN IF                       
         BE    *+12                    WE'RE IN CHECK OR SUPERCHECK             
         CLI   ACTNUM,ACTSUPCK                                                  
         BNE   VR00A                                                            
         MVI   ACTNUM,ACTUPDTE                                                  
         MVC   CONACT,=CL8'UPDATE'                                              
         B     VR00B                                                            
*                                                                               
VR00A    MVI   ACTNUM,ACTSUPCK                                                  
         MVC   CONACT,=CL8'SUPERCK'                                             
*                                                                               
VR00B    MVI   PFKEY,0             WIPE OUT PFKEY                               
         NI    CHKMEDH+4,X'FF'-X'20'   UNVALIDATE THE MEDIA                     
         B     VK                  GO BACK AND VALIDATE MEDIA                   
*                                                                               
VR01     CLI   PFKEY,3             GO TO COMMENTS SCREEN?                       
         BNE   VR02                                                             
         GOTO1 =A(GOOVLYS),DMCB,(RC),RR=RELO                                    
         B     LR                                                               
*                                                                               
VR02     CLI   PFKEY,4             MATCH?                                       
         BNE   VR03                                                             
         GOTO1 MATCHUNS            MATCH UNMATCHED INVOICE DETAILS              
         MVI   WINDOW,0                                                         
         OI    BITFLAG,X'80'                                                    
         CLI   ACTNUM,ACTCHECK                                                  
         BE    LR                  REDISPLAY AFTER MATCHING                     
*                                                                               
         OI    BITFLAG,X'40'       AFTER MATCH                                  
         MVC   CONACT,=CL8'CHECK'                                               
         OI    CONACTH+6,X'80'                                                  
         MVI   ACTNUM,ACTCHECK                                                  
         MVI   PFKEY,0                                                          
         NI    CHKMEDH+4,X'FF'-X'20'   UNVALIDATE THE MEDIA                     
         B     VK                                                               
*                                                                               
VR03     CLI   PFKEY,8             MBC?                                         
         BNE   VR04                                                             
         OI    BITFLAG,X'01'       SET MBC BIT ON                               
         GOTO1 =A(SWTCHPRG),DMCB,(RC),RR=RELO                                   
         NI    BITFLAG,X'FF'-X'01'  SET MBC BIT OFF                             
         B     XIT                                                              
*                                                                               
VR04     CLI   PFKEY,9             PAY?                                         
         BNE   VR05                                                             
         GOTO1 =A(PAYPRGM),DMCB,(RC),RR=RELO                                    
         GOTO1 =A(SWTCHPRG),DMCB,(RC),RR=RELO                                   
         B     XIT                                                              
*                                                                               
VR05     TM    BITFLAG,X'80'       FIRST TIME IN CHECK                          
         BNZ   LR                  YES, NOTHING TO VALIDATE YET                 
*                                                                               
         MVC   LSTMINSQ,=X'FFFFFFFF'   HIGHEST POSSIBLE MINIO KEY               
*                                                                               
         CLI   ACTNUM,ACTUPDTE                                                  
         BE    VR100                                                            
*                                                                               
         LA    R2,CK1SEL1H         POINT TO FIRST LINE IN UPPER PORTION         
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,CK2SEL1H                                                      
*                                                                               
         CLI   ACTNUM,ACTCHECK                                                  
         BE    VR05A                                                            
*                                                                               
         LA    R2,SC1SEL1H         POINT TO FIRST LINE IN UPPER PORTION         
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,SC2SEL1H                                                      
*                                                                               
VR05A    LA    R1,UPPERTBL         A(CHANGED ENTRY)                             
         ST    R1,ACHGNTRY                                                      
*                                                                               
VR00LP   LA    R3,CK1SEL9H             AND DON'T GO BEYOND THIS LINE            
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R3,CK2SEL9H                                                      
*                                                                               
         CLI   ACTNUM,ACTCHECK                                                  
         BE    VR08                                                             
*                                                                               
         LA    R3,SC1LSTLH             AND DON'T GO BEYOND THIS LINE            
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R3,SC2LSTLH                                                      
*                                                                               
VR08     GOTO1 FINDLINE,DMCB,(R2),(R3),ACHGNTRY,L'PBUYKEY                       
         BNE   VR100                                                            
*                                                                               
         MVC   AOFLINE,DMCB                                                     
         MVC   ACHGNTRY,DMCB+8                                                  
*                                                                               
         XC    VALSPACE,VALSPACE                                                
         MVI   VALUIND,0                                                        
         ZAP   VALUNITS,=P'0'                                                   
         ZAP   VALCLMS,=P'0'                                                    
         MVI   VALCOSTY,0          ZERO-OUT SO FIELDS WILL BE SHOWN             
         MVI   VALCOSIN,C' '                                                    
         ZAP   VALCOST,=P'0'                                                    
         MVI   VALCL,0                                                          
         ZAP   VALPRCOS,=P'0'                                                   
*                                                                               
         OI    UBITFLAG,X'40'      IF THERE IS A CHANGE, REDISPLAY PAGE         
*                                                                               
         L     R2,AOFLINE          R2 = A(CHANGED LINE)                         
*                                                                               
         OC    DMCB+4(4),DMCB+4    LINE HAS NO DATA                             
         BZ    VR00NX              IGNORE IT, DON'T DELETE IT                   
*                                                                               
         USING SCRLIN1D,R2                                                      
VR10     LA    R3,SLN1SELH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2SELH                                                      
         DROP  R2                                                               
*                                                                               
         L     R1,ACHGNTRY         IS THIS A BUY KEY?                           
         CLI   L'PINVMINI(R1),0                                                 
         BE    VR20                NO, IT'S A MINELEM KEY                       
*                                      CLIENT CAN'T START WITH A 0              
         CLI   5(R3),0                                                          
         BE    VR00NX                                                           
*                                                                               
         CLI   8(R3),C'*'          A MATCHED INSERTION?                         
         BE    VR00NX              YES, NOTHING TO DO HERE                      
*                                                                               
         CLI   8(R3),C' '          CLEARED OUT?                                 
         BE    VR00NX              YES, NOTHING TO DO HERE                      
*                                                                               
         CLI   8(R3),C'M'          NO, MATCH THIS INVOICE                       
         BNE   VR10P                                                            
         GOTO1 =A(MIDETAIL),DMCB,(RC),(R3),RR=RELO                              
         MVC   8(2,R3),=C'*M'                                                   
         OI    6(R3),X'08'                                                      
         B     VR10NX1                                                          
*                                                                               
VR10P    CLI   8(R3),C'P'          PAY THIS INVOICE?                            
         BNE   VR10T                                                            
         GOTO1 =A(PIDETAIL),DMCB,(RC),(R3),RR=RELO                              
         MVI   5(R3),1                                                          
         OI    6(R3),X'80'                                                      
         MVC   8(2,R3),=C'*P'                                                   
         MVC   LOWERTBL,NEWLOWTB                                                
         CLI   SCRTYPE,C'N'                                                     
         BNE   VR10P10                                                          
         USING SCRLIN1D,R3                                                      
         OI    SLN1SELH+6,X'08'                                                 
         OI    SLN1IDTH+6,X'08'                                                 
         OI    SLN1SIZH+6,X'08'                                                 
         OI    SLN1RTEH+6,X'08'                                                 
         OI    SLN1PRMH+6,X'08'                                                 
         OI    SLN1CTPH+6,X'08'                                                 
         OI    SLN1GRSH+6,X'08'                                                 
         OI    SLN1NETH+6,X'08'                                                 
         OI    SLN1ESTH+6,X'08'                                                 
         B     XIT                                                              
         USING SCRLIN2D,R3                                                      
VR10P10  OI    SLN2SELH+6,X'08'                                                 
         OI    SLN2IDTH+6,X'08'                                                 
         OI    SLN2SPCH+6,X'08'                                                 
         OI    SLN2CTPH+6,X'08'                                                 
         OI    SLN2GRSH+6,X'08'                                                 
         OI    SLN2NETH+6,X'08'                                                 
         OI    SLN2ESTH+6,X'08'                                                 
         B     XIT                 EXIT TO GIVE CONTROL TO MONITOR              
         DROP  R3                                                               
*                                                                               
VR10T    CLI   8(R3),C'T'          TEARSHEET INTO A MATCHED BUY?                
         BNE   VR10U               NO                                           
         GOTO1 =A(TIDETAIL),DMCB,(RC),(R3),RR=RELO                              
         MVC   8(2,R3),=C'*T'                                                   
         OI    6(R3),X'08'                                                      
         B     VR10NX1                                                          
*                                                                               
VR10U    CLI   8(R3),C'U'          UNMATCH INSERTION?                           
         BNE   VRSELERR                                                         
         GOTO1 =A(UIDETAIL),DMCB,(RC),(R3),RR=RELO                              
         MVC   8(2,R3),=C'*U'                                                   
         OI    6(R3),X'08'                                                      
         B     VR10NX1                                                          
*                                                                               
VR10NX   XC    8(L'SLN1SEL,R3),8(R3)       CLEAR THE SELECT FIELD               
VR10NX1  OI    6(R3),X'80'                                                      
         MVI   5(R3),0                                                          
         B     VR00NX                                                           
*                                                                               
VRSELERR LR    R2,R3               SELECTION ERROR                              
         B     INVLSEL                                                          
*                                                                               
VR20     CLI   5(R3),0           ANYTHING IN SELECT FIELD?                      
         BE    VR30                                                             
*                                                                               
         CLI   8(R3),C'D'        DELETE INVOICE?                                
         BNE   VRSELERR                                                         
         OC    0(L'PINVMINI,R1),0(R1)    IGNORE IF NO MINELEM SEQ               
         BZ    VR00NX                                                           
         MVC   MINEKEY(L'PINVMINI),0(R1)                                        
         GOTO1 =A(DIDETAIL),DMCB,(RC),RR=RELO                                   
*                                                                               
         XC    8(L'SLN1SEL,R3),8(R3)       CLEAR THE SELECT FIELD               
         OI    6(R3),X'80'                                                      
         MVI   5(R3),0                                                          
         B     VR00NX                                                           
*                                                                               
VR30     SH    R1,=Y(L'PBUYKEY)                                                 
         OC    0(L'PBUYKEY,R1),0(R1)    SHOULD HAVE A BUY KEY BEFORE IT         
         BZ    VR00NX                       IF ITS A CORRECTION                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'PBUYKEY),0(R1)                                             
         GOTO1 HIGH                                                             
         CLC   KEY(L'PBUYKEY),KEYSAVE                                           
         BE    VR30A0                                                           
*                                  BUY NOT FOUND (OR BAD KEY)                   
         B     INVLFLD             INVALID FIELD - USED TO DIE                  
*                                  ON DC H'0'                                   
VR30A0   DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         TM    PBDSTAT,X'40'       MATCHED?                                     
         BNZ   CANTCORM            YES, CAN'T CORRECTED MATCHED BUY             
*                                                                               
         MVI   ELCODE,X'25'        SEE IF BUY IS PAID                           
         BAS   RE,GETEL                                                         
VR30A1A  BNE   VR30A1                                                           
         USING PPAYELEM,R6                                                      
         OC    PPDDATE,PPDDATE                                                  
         BNZ   INVLPAID            CAN'T MANIPULATE A PAID INSERTION            
         BAS   RE,NEXTEL                                                        
         B     VR30A1A                                                          
*                                                                               
VR30A1   L     R1,ACHGNTRY                                                      
*                                                                               
         L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
*                                                                               
VR30AA   XC    0(L'MELEM,R6),0(R6)                                              
         MVI   PIMDTLEL,PIMDTLEQ                                                
         MVI   PIMDTLLN,PIMDTLLQ                                                
         MVC   PIMDTLS1,LSTHDRSQ   HEADER SEQUENCE NUMBER                       
*                                                                               
         XC    VALSPACE,VALSPACE                                                
         MVI   VALUIND,0                                                        
         ZAP   VALUNITS,=P'0'                                                   
         MVI   VALCOSTY,0                                                       
         MVI   VALCOSIN,0                                                       
         ZAP   VALCOST,=P'0'                                                    
         MVI   VALCL,0                                                          
         ZAP   VALPRCOS,=P'0'                                                   
*                                                                               
         USING SCRLIN1D,R2         ANY CORRECTION ON THE DATE?                  
         LA    R3,SLN1IDTH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2IDTH                                                      
         DROP  R2                                                               
*                                                                               
         CLI   5(R3),0             NO                                           
         BE    VR40                                                             
*                                                                               
         TM    4(R3),X'08'         VALID NUMERIC?                               
         BZ    VR30A                                                            
         ZIC   R1,5(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R3)                                                      
         CVB   R1,DUB                                                           
         MVC   DATEFLD(L'INVSTDT),INVSTDT                                       
         STC   R1,DATEFLD+2                                                     
         GOTO1 DATCON,DMCB,(3,DATEFLD),(12,8(R3))                               
         MVI   5(R3),5                                                          
*                                                                               
VR30A    GOTO1 DATVAL,DMCB,(0,8(R3)),DATEFLD  VALIDATE FOR M/D/Y                
         OC    DMCB,DMCB                                                        
         BNZ   VR32                                                             
*                                                                               
         TM    4(R3),X'04'         VALID ALPHA?                                 
         BZ    VR31                                                             
*                                                                               
         CLI   5(R3),3             A MONTH?                                     
         BNE   VR31                                                             
*                                                                               
         LA    R1,MONTHLST         CHECK AMONG LIST OF MONTHS                   
VR30B    CLI   0(R1),0                                                          
         BE    VR31                                                             
         CLC   8(3,R3),0(R1)                                                    
         BE    VR30C                                                            
         LA    R1,3(R1)                                                         
         B     VR30B                                                            
*                                                                               
VR30C    MVC   11(2,R3),=C'01'     VALID 3 LETTER MONTH, ASSUME 1ST DAY         
         NI    4(R3),X'FF'-X'04'   NOT VALID ALPHA ANYMORE                      
         MVI   5(R3),5                                                          
         OI    6(R3),X'80'                                                      
*                                                                               
VR31     GOTO1 DATVAL,DMCB,(1,8(R3)),DATEFLD  VALIDATE FOR M/D                  
         OC    DMCB,DMCB                                                        
         BNZ   *+10                                                             
         LR    R2,R3                                                            
         B     INVLDATE                                                         
*                                                                               
VR32     GOTO1 DATCON,DMCB,(0,DATEFLD),(3,PIMIDATE)  GET 3-BYTE DATE            
*                                                                               
         CLC   =C'00',DATEFLD      CHECK DATE RETURNED FOR A YEAR               
         BNE   VR35                THERE IS ONE                                 
         CLC   INVSTDT(1),INVENDDT   PERIOD IN SAME YEAR?                       
         BNE   *+14                                                             
VR32A    MVC   PIMIDATE(1),INVSTDT   YES, INVOICE DATE SHOULD BE ALSO           
         B     VR35                                                             
*                                                                               
         CLC   PIMIDATE+1(2),INVSTDT+1   NO, STDT YEAR < ENDDT YEAR             
         BH    VR32A                 INVOICE IN START DATE'S YEAR               
         MVC   PIMIDATE(1),INVENDDT                                             
*                                                                               
VR35     CLC   PIMIDATE,INVSTDT    INVOICE DATE SHOULD BE WITHIN PERIOD         
         BNL   *+10                                                             
         LR    R2,R3                                                            
         B     DATEPER                                                          
         CLC   PIMIDATE,INVENDDT                                                
         BNH   *+10                                                             
         LR    R2,R3                                                            
         B     DATEPER                                                          
*                                                                               
         CLI   PIMIDATE+2,1                                                     
         BNL   VR40                                                             
         CLI   MAGFREQ,C'M'        IF MAGAZINE IS MONTHLY                       
         BNE   *+8                                                              
         MVI   PIMIDATE+2,1        THEN DATE IS SET TO 1                        
*                                                                               
         USING SCRLIN1D,R2                                                      
VR40     LA    R3,SLN1SIZH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2SPCH                                                      
         DROP  R2                                                               
*                                                                               
         CLI   5(R3),0                                                          
         BE    VR41                                                             
         GOTO1 =A(VSIZE),DMCB,(RC),(R2),RR=RELO                                 
*                                                                               
         MVC   PIMSPACE,VALSPACE   SPACE                                        
         MVC   PIMUIND,VALUIND     UNIT INDICATOR                               
         ZAP   PIMUNITS,VALUNITS   NUMBER OF UNITS                              
         ZAP   PIMCLMS,VALCLMS     NUMBER OF COLUMNS                            
*                                                                               
VR41     CLI   SCRTYPE,C'N'        IF MAGAZINE                                  
         BE    VR42                                                             
         GOTO1 =A(SHWREGIL),DMCB,(RC),(R3),RR=RELO                              
         MVC   PIMSPACE,VALSPACE                                                
         B     VR44                                                             
*                                                                               
         USING SCRLIN1D,R2                                                      
VR42     CLI   SLN1RTEH+5,0        COPY INSERTION'S RATE?                       
         BE    VR43                                                             
         GOTO1 =A(VRATE),DMCB,(RC),(R2),RR=RELO    NO                           
*                                                                               
         CLI   MYGRSNET,C'N'       NET DOLLARS?                                 
         BE    *+12                                                             
         CLI   VALCTYP,C'N'        FLAG IF NET DOLLARS                          
         BNE   VR42A                                                            
         CLI   VALCOSIN,C'S'                                                    
         BE    VR42A                                                            
         OI    PIMDSTAT,X'20'                                                   
         ZAP   NETAMNT,VALCOST     PUT IN THE NET RATE                          
         ZAP   PERCENTG,MYPUBAC        AND THE AGENCY COMMISSION                
         L     RF,AIO                                                           
         CP    PBDACP-PBUYREC(3,RF),PERCENTG   IS BUY COMM. "SAME" ?            
         BE    *+10                            YES                              
         ZAP   PERCENTG,PBDACP-PBUYREC(3,RF)   NO - USE IT, NOT MYPUBAC         
         BAS   RE,NETTOGRS         CHECK NET COST TO GROSS COST                 
         ZAP   VALCOST,GROSSAMT    COPY THE GROSS RATE                          
*                                                                               
VR42A    ZAP   PIMCOST,VALCOST     COPY RATE                                    
         MVC   PIMCSIND,VALCOSIN        COST INDICATOR                          
*                                                                               
         CLI   VALCOSTY,C'U'       FLAG IF UNIT COST                            
         BNE   VR43                                                             
         OI    PIMDSTAT,X'80'                                                   
*                                                                               
         ZAP   WORK(16),VALUNITS                                                
         CP    VALUNITS,=P'0'          SEE IF UNITS ENTERED                     
         BH    VR42D                                                            
         L     RF,AIO                  TRY AND USE PBUYREC'S UNITS              
         CLC   PBUYKMED-PBUYREC(2,RF),=X'D520'   NEWSPAPER BUY?                 
         BNE   VR43                                                             
         ZAP   WORK(16),PBDUNITS-PBUYREC(L'PBDUNITS,RF)                         
*                                                                               
VR42D    MP    WORK(16),PIMCOST       PIMCOST HAS 5 DECIMALS                    
         DP    WORK(16),=P'100'                                                 
         ZAP   WORK(16),WORK(14)                                                
         DP    WORK(16),=P'10'                                                  
         CP    WORK(14),=P'-2100000000'  MAX FOR FULL WORD                      
         BL    VR42ERR                                                          
*                                                                               
         CP    WORK(14),=P'2100000000'   MAX FOR FULL WORD                      
         BL    VR43                                                             
VR42ERR  DS    0H                                                               
         LA    R2,SLN1RTEH                                                      
         B     INVLRATE     RATE INVALID - PROBABLY FORGOT "T"                  
*                           FOR TOTAL RATE                                      
*                           THIS EDIT SHOULD PREVENT DUMPS                      
*                           IN GETINS                                           
*                                                                               
VR43     CLI   SLN1PRMH+5,0                                                     
         BE    VR44                                                             
         GOTO1 =A(VPREM),DMCB,(RC),(R2),RR=RELO                                 
*                                                                               
         CP    VALPRCOS,=P'0'      DO WE HAVE A PREMIUM COST?                   
         BNE   *+12                                                             
         LA    R2,SLN1PRMH                                                      
         B     NOPREM              NO, THEN ERROR                               
*                                                                               
         ZAP   PIMPREM,VALPRCOS    PREMIUM CHARGE                               
         MVC   PIMCLRS,VALCL       NUMBER OF COLORS                             
*                                                                               
         USING SCRLIN2D,R2                                                      
VR44     CLI   SCRTYPE,C'N'                                                     
         BE    VR50                                                             
         LA    R3,SLN2GRSH                                                      
         DROP  R2                                                               
*                                                                               
         CLI   5(R3),0                                                          
         BE    VR50                                                             
         GOTO1 =A(VALGROSS),DMCB,(RC),(R2),RR=RELO                              
         ZAP   PIMCOST,VALCOST     NEW COST IF NO RATE OR MAGAZINE              
         MVC   PIMCSIND,VALCOSIN                                                
*                                                                               
VR50     L     R1,ACHGNTRY                                                      
         SH    R1,=Y(L'PBUYKEY)                                                 
         USING PBUYKEY,R1                                                       
         MVC   PIMBZONE(PIMBLINE-PIMBZONE),PBUYKZON                             
         MVC   PIMBLINE,PBUYKLIN   STICK IN BUYLINE                             
*                                                                               
         OC    PIMIDATE,PIMIDATE   IF DATE WASN'T CHANGED                       
         BNZ   *+10                                                             
         MVC   PIMIDATE,PIMBDATE   USE THE BUY DATE FOR INVOICE DATE            
         MVC   PIMIEST,PBUYKEST    USE BUY ESTIMATE                             
         DROP  R1                                                               
*                                                                               
         XC    KEY,KEY             GET BUY RECORD                               
         MVC   KEY(L'PBUYKEY),0(R1)                                             
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         USING PBUYREC,R3                                                       
*                                                                               
         OC    PIMSPACE,PIMSPACE   COPY WHAT WASN'T CHANGED OVER                
         BNZ   VR52                    TO INVOICE DETAIL                        
         OC    PIMUNITS,PIMUNITS                                                
         BNZ   VR52                                                             
         MVC   PIMSPACE,PBDSPACE                                                
         CLI   QMED,C'O'           OUTDOORS?                                    
         BNE   VR52                                                             
         CP    PBDSHOW,=P'0'                                                    
         BNE   VR52                                                             
         CP    PBDREG,=P'0'                                                     
         BNE   VR52                                                             
         CP    PBDILLUM,=P'0'                                                   
         BNE   VR52                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'66'                                                     
         BAS   RE,GETEL                                                         
         BE    VR51B                                                            
*                                                                               
VR51A    L     R6,MINELEM                                                       
         B     VR51C                                                            
*                                                                               
         USING PCOMELEM,R6                                                      
VR51B    ZIC   R1,PCOMELEM+1                                                    
         SH    R1,=H'2'                                                         
         CH    R1,=Y(L'VALSPACE)                                                
         BNH   *+14                                                             
         MVC   VALSPACE,PCOMELEM+2    SEE PPBUY05, LABEL FMTCOM                 
         B     VR51A                                                            
*                                                                               
         CH    R1,=H'2'                                                         
         BNH   VR51A                                                            
         XC    VALSPACE,VALSPACE                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     VR51A                                                            
         MVC   VALSPACE(0),PCOMELEM+2                                           
*                                                                               
         USING PIMDTLEL,R6                                                      
VR51C    MVC   PIMSPACE,VALSPACE                                                
*                                                                               
VR52     OC    PIMCOST,PIMCOST     ELEMENT WAS NULLED OUT WITH   XC             
         BNZ   VR55                                                             
         MVC   PIMCSIND,PBDCOSIN                                                
         ZAP   PIMCOST,PBDCOS                                                   
         CLI   PBDCOSTY,C'U'       UNIT COST?                                   
         BNE   VR55                                                             
         OI    PIMDSTAT,X'80'      YES, SET THAT BIT ON                         
*                                                                               
VR55     OC    PIMUNITS,PIMUNITS                                                
         BNZ   VR55A0                                                           
         ZAP   PIMUNITS,PBDUNITS                                                
         MVC   PIMUIND,PBDUIND                                                  
         ZAP   PIMCLMS,PBDCLMS                                                  
*                                                                               
VR55A0   OC    PIMPREM,PIMPREM                                                  
         BNZ   *+10                                                             
         ZAP   PIMPREM,PBDPRCOS                                                 
*                                                                               
         OC    PIMCLRS,PIMCLRS                                                  
         BNZ   *+10                                                             
         MVC   PIMCLRS,PBDCL                                                    
*                                                                               
         USING SCRLIN1D,R2                                                      
         LA    R1,SLN1CTPH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R1,SLN2CTPH                                                      
         DROP  R2                                                               
*                                                                               
         CLC   =C'***',QPRD        FOR PRD VARIOUS                              
         BNE   VR55A1                                                           
*                                                                               
VR5A     ST    R3,FULL2            TEMP STORAGE OF R3                           
         LR    R3,R1               R3 NEEDS TO PT TO FLD IN RDPROD              
         BAS   RE,RDPROD                                                        
         L     R3,FULL2            RESTORE R3                                   
         CLC   =C'CT',CHKOPTN                                                   
         BE    VR55AA1                                                          
*                                                                               
         CP    PBDCD,=P'0'         ANY CASH DISCOUNT IN BUY?                    
         BNE   *+8                                                              
         OI    PIMDSTAT,X'40'      NO, SET NO CASH DISCOUNT BIT                 
         B     VR59                                                             
*                                                                               
VR55AA1  DS    0H                                                               
         USING SCRLIN1D,R2         RD IN CTPBI FROM NET COLUMN                  
         LA    R1,SLN1NETH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R1,SLN2NETH                                                      
         DROP  R2                                                               
*                                                                               
VR55A1   CLI   5(R1),0             ANYTHING IN THIS FIELD?                      
         BNE   VR55B                                                            
*                                                                               
VR55A    MVI   8(R1),C'Y'          DEFAULT IS C'Y'                              
         CP    PBDCD,=P'0'         ANY CASH DISCOUNT IN BUY?                    
         BNE   *+8                                                              
         MVI   8(R1),C'N'          NO, NONE                                     
*                                                                               
VR55B    CLI   8(R1),C'Y'          CASH DISCOUNT?                               
         BE    VR56                                                             
         CLI   8(R1),C'N'                                                       
         BNE   VR55A               NOT Y OR N, SET TO BUY'S CASH DISC           
         OI    PIMDSTAT,X'40'      NO, SET NO CASH DISCOUNT BIT                 
*                                                                               
VR56     CLI   5(R1),0                                                          
         BNE   VR56B                                                            
*                                                                               
VR56A    MVI   9(R1),C'T'                                                       
         TM    PBDSTAT,X'10'                                                    
         BNZ   *+8                                                              
         MVI   9(R1),0                                                          
*                                                                               
VR56B    CLI   9(R1),C'T'          TEAR SHEET PROOF?                            
         BNE   VR58                                                             
         OI    PIMDSTAT,X'08'      YES                                          
         DROP  R3                                                               
*                                                                               
VR58     OC    10(3,R1),10(R1)     PBI USED FOR DISPLAY ONLY                    
         BZ    *+10                                                             
         LR    R2,R1                                                            
         B     INVLPBI                                                          
*                                                                               
VR59     L     R1,ACHGNTRY         SEE IF WE NEED TO ADD OR CHANGE              
         OC    0(L'PINVMINI,R1),0(R1)                                           
         BZ    VR60                ADD A MINIO ELEMENT                          
*                                                                               
         MVC   MELEM2,MELEM        CHANGE AN ELEMENT                            
         MVC   MINEKEY(L'PINVMINI),0(R1)                                        
         BAS   RE,MINIORD                                                       
         BNE   INVLSEL        ELEMENT NOT FOUND                                 
*                                                                               
         TM    PIMDSTAT,X'04'      COMMENT FOR THIS CORRECTED DETAIL?           
         BZ    *+8                                                              
         OI    MELEM2+PIMDSTAT-PIMDTLEL,X'04'   YES                             
*                                                                               
         MVC   MELEM,MELEM2                                                     
         MVC   PIMDTLS1(L'SEQUENCE),MINEKEY+1 CHANGE ON THIS SEQUENCE           
*                                                                               
         BAS   RE,MINIOWRT                                                      
         B     VR00NX                                                           
*                                                                               
VR60     GOTO1 NXDTLSEQ                                                         
         BE    VR65                                                             
         GOTO1 RESEQNCE,DMCB,LOWERTBL,UPPERTBL                                  
         B     VR60                                                             
VR65     MVC   PIMDTLS2,HALF       NEXT DETAIL SEQUENCE NUMBER                  
         BAS   RE,MINIOADD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR00NX   CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         AH    R2,=Y(SLN1LEN)                                                   
         B     *+8                                                              
         AH    R2,=Y(SLN2LEN)                                                   
*                                                                               
         L     R1,ACHGNTRY                                                      
         LA    R1,L'PBUYKEY(R1)                                                 
         ST    R1,ACHGNTRY                                                      
         LA    R0,CK1ITEMH         CHECK LAST LINE ALREADY?                     
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R0,CK2ITEMH         POINT TO FIRST LINE IN LOWER PORTION         
         CR    R2,R0                                                            
         BNE   VR00LP              NO, CONTINUE FROM THIS LINE ON               
         EJECT                                                                  
*                                                                               
* LOWER PORTION OF SCREEN                                                       
*                                                                               
VR100    CLI   ACTNUM,ACTSUPCK                                                  
         BE    VRX                                                              
*                                                                               
         CLI   ACTNUM,ACTCHECK                                                  
         BNE   VR100A                                                           
         LA    R2,CK1SEL0H         POINT TO FIRST LINE IN LOWER PORTION         
         CLI   SCRTYPE,C'N'                                                     
         BE    VR100B                                                           
         LA    R2,CK2SEL0H                                                      
         B     VR100B                                                           
*                                                                               
VR100A   LA    R2,UP1SEL1H         POINT TO FIRST LINE IN LOWER PORTION         
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R2,UP2SEL1H                                                      
*                                                                               
VR100B   MVC   NEWLOWTB,LOWERTBL   COPY TABLE OF WHAT'S REALLY THERE            
         LA    R1,NEWLOWTB         A(CHANGED ENTRY)                             
         LA    R1,0(R1)            CLEAR HOB                                    
         ST    R1,ACHGNTRY                                                      
*                                                                               
VR100LP  CLI   ACTNUM,ACTCHECK                                                  
         BNE   VR105                                                            
         LA    R3,CK1LSTLH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    VR105A                                                           
         LA    R3,CK2LSTLH                                                      
         B     VR105A                                                           
*                                                                               
VR105    LA    R3,UP1LSTLH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R3,UP2LSTLH                                                      
*                                                                               
VR105A   GOTO1 FINDLINE,DMCB,(R2),(R3),ACHGNTRY,L'PINVMINI                      
         BNE   VRX                                                              
*                                                                               
         MVC   AOFLINE,DMCB                                                     
         MVC   ACHGNTRY,DMCB+8                                                  
*                                                                               
         XC    VALSPACE,VALSPACE                                                
         MVI   VALUIND,0                                                        
         ZAP   VALUNITS,=P'0'                                                   
         ZAP   VALCLMS,=P'0'                                                    
         MVI   VALCOSTY,0          ZERO-OUT SO FIELDS WILL BE SHOWN             
         MVI   VALCOSIN,C' '                                                    
         ZAP   VALCOST,=P'0'                                                    
         MVI   VALCL,0                                                          
         ZAP   VALPRCOS,=P'0'                                                   
*                                                                               
         L     R2,AOFLINE          R2 = A(CHANGED LINE)                         
*                                                                               
         OC    DMCB+4(4),DMCB+4    LINE HAS NO DATA                             
         BZ    VR100NX             IGNORE IT (NOT A DELETION)                   
*                                                                               
         OI    LBITFLAG,X'40'      THERE IS A CHANGE, REDISPLAY NEWEST          
*                                                                               
         CLI   SCRTYPE,C'N'        TRANSMIT ALL FIELDS ON THE LINE TO           
         BNE   VR110                   AVOID ANY PROBLEMS CAUSED BY             
         USING SCRLIN1D,R2             UNPROTECTED FIELDS FOLLOWING             
         OI    SLN1SELH+6,X'80'        EACH OTHER SEPARATED BY ONE BYTE         
         OI    SLN1IDTH+6,X'80'                                                 
         OI    SLN1SIZH+6,X'80'                                                 
         OI    SLN1RTEH+6,X'80'                                                 
         OI    SLN1PRMH+6,X'80'                                                 
         OI    SLN1CTPH+6,X'80'                                                 
         OI    SLN1GRSH+6,X'80'                                                 
         OI    SLN1NETH+6,X'80'    IN CASE OF ZONE/ED INPUT                     
         OI    SLN1ESTH+6,X'80'                                                 
         B     VR110A                                                           
         USING SCRLIN2D,R2                                                      
VR110    OI    SLN2SELH+6,X'80'                                                 
         OI    SLN2IDTH+6,X'80'                                                 
         OI    SLN2SPCH+6,X'80'                                                 
         OI    SLN2CTPH+6,X'80'                                                 
         OI    SLN2GRSH+6,X'80'                                                 
         OI    SLN2NETH+6,X'80'    IN CASE OF ZONE/ED INPUT                     
         OI    SLN2ESTH+6,X'80'                                                 
         DROP  R2                                                               
*                                                                               
         USING SCRLIN1D,R2                                                      
VR110A   LA    R3,SLN1SELH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2SELH                                                      
         DROP  R2                                                               
*                                                                               
         CLI   5(R3),0             SELECTION CHANGED?                           
         BE    VR120                                                            
*                                                                               
         CLI   8(R3),C'D'          DELETE INVOICE ITEM?                         
         BNE   VR115                                                            
         L     R1,ACHGNTRY                                                      
*                                                                               
         OC    0(L'PINVMINI,R1),0(R1)   YES, SEE IF ANYHTHING TO DELETE         
         BZ    VR115               NOTHING TO DELETE                            
*                                                                               
         XC    MINEKEY,MINEKEY     GOT SOMETHING TO DELETE                      
         MVC   MINEKEY(L'PINVMINI),0(R1)                                        
         GOTO1 =A(DIDETAIL),DMCB,(RC),RR=RELO                                   
*                                                                               
         L     R1,ACHGNTRY                                                      
*                                                                               
         CLC   =X'FFFFFFFF',LSTMINSQ  ONLY DELETION FOR CHANGE SO FAR?          
         BNE   VR112                  NO                                        
*                                                                               
*                                  SEE IF THERE ARE ANY DETAILS AFTER           
         LA    R1,L'PINVMINI(R1)       THE DELETED ONE                          
         OC    0(L'PINVMINI,R1),0(R1)                                           
         BNZ   VR111                                                            
*                                                                               
         MVI   LSTMINSQ,PIMDTLEQ     THERE AREN'T ANY AFTER DELETED ONE         
         MVC   LSTMINSQ+1(L'LSTHDRSQ),LSTHDRSQ                                  
         XC    LSTMINSQ+2(L'PIMDTLS2),LSTMINSQ+2   SHOW FROM 1ST DETAIL         
         B     VR100NX                                                          
*                                                                               
VR111    MVI   LSTMINSQ,PIMDTLEQ    THERE IS A DETAIL AFTER DELETED ONE         
         MVC   LSTMINSQ+1(L'LSTHDRSQ),LSTHDRSQ                                  
         MVC   LSTMINSQ+2(L'PIMDTLS2),2(R1)   SHOW FROM THAT DETAIL             
         B     VR100NX                                                          
*                                                                               
VR112    CLC   LSTMINSQ+2(2),2(R1)  SEE IF WE SHOW FROM DETAIL AFTER            
         BL    VR100NX             NO, FROM BEFORE IT                           
         LR    RE,R1                                                            
         LA    RE,L'PINVMINI(RE)                                                
*                                                                               
         OC    0(L'PINVMINI,RE),0(RE)  ANY DETAILS AFTER DELETED ONE?           
         BNZ   *+14                                                             
         XC    LSTMINSQ+2(L'PIMDTLS2),LSTMINSQ+2  NO, SHOW FROM 1ST             
         B     VR100NX                                                          
*                                                                               
         MVC   LSTMINSQ+2(L'PIMDTLS2),2(RE)  YES, SHOW FROM THAT DETAIL         
         B     VR100NX                                                          
*                                                                               
VR115    LR    R2,R3               INVALID SELECTION                            
         B     INVLSEL                                                          
*                                                                               
VR120    L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
         XC    0(L'MELEM,R6),0(R6)                                              
         MVI   PIMDTLEL,PIMDTLEQ                                                
         MVI   PIMDTLLN,PIMDTLLQ                                                
         MVC   PIMDTLS1,LSTHDRSQ   HEADER SEQUENCE NUMBER                       
*                                                                               
         XC    VALSPACE,VALSPACE                                                
         MVI   VALUIND,0                                                        
         ZAP   VALUNITS,=P'0'                                                   
         ZAP   VALCLMS,=P'0'                                                    
         MVI   VALCOSTY,0                                                       
         MVI   VALCOSIN,0                                                       
         ZAP   VALCOST,=P'0'                                                    
         MVI   VALCL,0                                                          
         ZAP   VALPRCOS,=P'0'                                                   
*                                                                               
* VALIDATE THE INVOICE DATE                                                     
*                                                                               
         USING SCRLIN1D,R2                                                      
         LA    R3,SLN1IDTH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2IDTH                                                      
         DROP  R2                                                               
*                                                                               
         CLI   5(R3),0             WE NEED AN INVOICE DATE                      
         BNE   *+10                                                             
         LR    R2,R3                                                            
         B     MISSFLD                                                          
*                                                                               
         TM    4(R3),X'08'         VALID NUMERIC?                               
         BZ    VR122                                                            
         ZIC   R1,5(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R3)                                                      
         CVB   R1,DUB                                                           
         XC    DATEFLD,DATEFLD                                                  
         MVC   DATEFLD(L'INVSTDT),INVSTDT                                       
         STC   R1,DATEFLD+2                                                     
         GOTO1 DATCON,DMCB,(3,DATEFLD),(12,8(R3))                               
         MVI   5(R3),5                                                          
*                                                                               
VR122    GOTO1 DATVAL,DMCB,(0,8(R3)),DATEFLD  VALIDATE FOR M/D/Y                
         OC    DMCB,DMCB                                                        
         BNZ   VR124                                                            
*                                                                               
         TM    4(R3),X'04'         VALID ALPHA?                                 
         BZ    VR123                                                            
*                                                                               
         CLI   5(R3),3             A MONTH?                                     
         BNE   VR123                                                            
*                                                                               
         LA    R1,MONTHLST         CHECK AMONG LIST OF MONTHS                   
VR122A   CLI   0(R1),0                                                          
         BE    VR123                                                            
         CLC   8(3,R3),0(R1)                                                    
         BE    VR122B                                                           
         LA    R1,3(R1)                                                         
         B     VR122A                                                           
*                                                                               
VR122B   MVC   11(2,R3),=C'01'     VALID 3 LETTER MONTH, ASSUME 1ST DAY         
         NI    4(R3),X'FF'-X'04'   NOT VALID ALPHA ANYMORE                      
         MVI   5(R3),5                                                          
         OI    6(R3),X'80'                                                      
*                                                                               
VR123    GOTO1 DATVAL,DMCB,(1,8(R3)),DATEFLD  VALIDATE FOR M/D                  
         OC    DMCB,DMCB                                                        
         BNZ   *+10                                                             
         LR    R2,R3                                                            
         B     INVLDATE                                                         
*                                                                               
VR124    GOTO1 DATCON,DMCB,(0,DATEFLD),(3,PIMIDATE)  GET 3-BYTE DATE            
*                                                                               
         CLC   =C'00',DATEFLD      CHECK DATE RETURNED FOR A YEAR               
         BNE   VR128               THERE IS ONE                                 
         CLC   INVSTDT(1),INVENDDT   PERIOD IN SAME YEAR?                       
         BNE   *+14                                                             
VR126    MVC   PIMIDATE(1),INVSTDT   YES, INVOICE DATE SHOULD BE ALSO           
         B     VR128                                                            
*                                                                               
         CLC   PIMIDATE+1(2),INVSTDT+1   NO, STDT YEAR < ENDDT YEAR             
         BH    VR126                 INVOICE IN START DATE'S YEAR               
         MVC   PIMIDATE(1),INVENDDT                                             
*                                                                               
VR128    CLC   PIMIDATE,INVSTDT    INVOICE DATE SHOULD BE WITHIN PERIOD         
         BNL   *+10                                                             
         LR    R2,R3                                                            
         B     DATEPER                                                          
         CLC   PIMIDATE,INVENDDT                                                
         BNH   *+10                                                             
         LR    R2,R3                                                            
         B     DATEPER                                                          
*                                                                               
         CLI   PIMIDATE+2,1        IF DD IS NOT 0                               
         BNL   VR130               THEN DON'T TEST IF MONTHLY                   
*                                                                               
         CLI   MAGFREQ,C'M'        IF MAGAZINE IS MONTHLY                       
         BNE   *+8                                                              
         MVI   PIMIDATE+2,1        THEN DATE IS SET TO 1                        
*                                                                               
* VALIDATE THE SIZE OR SPACE                                                    
*                                                                               
         USING SCRLIN1D,R2                                                      
VR130    LA    R3,SLN1SIZH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2SPCH                                                      
         DROP  R2                                                               
*                                                                               
         CLI   5(R3),0             ANYTHING IN THIS FIELD?                      
         BNE   VR135               YES                                          
*                                                                               
         CLI   ACTNUM,ACTUPDTE     UPDATE SCREEN?                               
         BE    VR132               YES, CHECK FOR COPY DOWN                     
VR100MIS LR    R2,R3                                                            
         B     MISSFLD                                                          
*                                                                               
VR132    LA    R1,NEWLOWTB         NOTHING TO COPY DOWN IF NONE BEFORE          
         LA    R1,0(R1)                                                         
         C     R1,ACHGNTRY                                                      
         BNL   VR135                                                            
*                                                                               
         L     R1,ACHGNTRY                                                      
*                                                                               
         OC    0(L'PINVMINI,R1),0(R1)  DETAIL CHANGE?                           
         BNZ   VR100MIS            YES, WE NEED DATA IN HERE                    
*                                                                               
         SH    R1,=Y(L'PINVMINI)                                                
         OC    0(L'PINVMINI,R1),0(R1)                                           
         BZ    VR135               NONE, VALIDATE IT AS IS                      
*                                                                               
         MVC   MELEM2,MELEM                                                     
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(L'PINVMINI),0(R1)                                        
         BAS   RE,MINIORD                                                       
         BNE   INVLSEL        ELEMENT NOT FOUND                                 
         MVC   VALSPACE,PIMSPACE                                                
         MVC   VALUIND,PIMUIND                                                  
         ZAP   VALUNITS,PIMUNITS                                                
*                                                                               
         OC    PIMCLMS,PIMCLMS                                                  
         BZ    *+14                                                             
         ZAP   VALCLMS,PIMCLMS                                                  
         B     *+10                                                             
         ZAP   VALCLMS,=P'0'                                                    
*                                                                               
         MVC   MELEM,MELEM2                                                     
         B     VR140                                                            
*                                                                               
VR135    GOTO1 =A(VSIZE),DMCB,(RC),(R2),RR=RELO                                 
*                                                                               
VR140    CLI   SCRTYPE,C'N'        IF NOT NEWSPAPER                             
         BE    VR150                                                            
         GOTO1 =A(SHWREGIL),DMCB,(RC),(R3),RR=RELO                              
         B     VR158               SKIP RATE AND PREMIUM VALIDATION             
*                                                                               
         USING SCRLIN1D,R2                                                      
VR150    LA    R3,SLN1RTEH                                                      
         DROP  R2                                                               
*                                                                               
         CLI   ACTNUM,ACTUPDTE     UPDATE SCREEN?                               
         BNE   VR154               YES, NO NEED TO CHECK FOR COPY DOWN          
*                                                                               
         CLI   5(R3),1             IF JUST A PERIOD                             
         BNE   VR152                                                            
         CLI   8(R3),C'.'                                                       
         BE    VR156                                                            
*                                                                               
VR152    CLI   5(R3),0             COPY DOWN THIS FIELD?                        
         BNE   VR154               NO                                           
*                                                                               
         LA    R1,NEWLOWTB         NOTHING TO COPY DOWN IF NONE BEFORE          
         LA    R1,0(R1)                                                         
         C     R1,ACHGNTRY                                                      
         BNL   VR154                                                            
*                                                                               
         L     R1,ACHGNTRY                                                      
*                                                                               
         OC    0(L'PINVMINI,R1),0(R1)  DETAIL CHANGE?                           
         BNZ   VR100MIS            YES, WE NEED DATA HERE                       
*                                                                               
         SH    R1,=Y(L'PINVMINI)                                                
         OC    0(L'PINVMINI,R1),0(R1)                                           
         BZ    VR154               NONE, VALIDATE IT AS IS                      
*                                                                               
         MVC   MELEM2,MELEM                                                     
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(L'PINVMINI),0(R1)                                        
         BAS   RE,MINIORD                                                       
         BNE   INVLSEL        ELEMENT NOT FOUND                                 
         ZAP   VALCOST,PIMCOST                                                  
         MVC   VALCOSIN,PIMCSIND                                                
*                                                                               
         MVI   VALCOSTY,C'U'                                                    
         TM    PIMDSTAT,X'80'                                                   
         BNZ   *+8                                                              
         MVI   VALCOSTY,C'T'                                                    
*                                                                               
         MVI   VALCTYP,0                                                        
         TM    PIMDSTAT,X'20'                                                   
         BZ    *+8                                                              
         MVI   VALCTYP,C'N'                                                     
*                                                                               
         MVC   MELEM,MELEM2                                                     
         B     VR156                                                            
*                                                                               
VR154    CLI   5(R3),0                                                          
         BNE   *+10                                                             
         LR    R2,R3                                                            
         B     MISSFLD                                                          
*                                                                               
         GOTO1 =A(VRATE),DMCB,(RC),(R2),RR=RELO                                 
*                                                                               
         CLI   VALCOSTY,C'U'       FLAG IF UNIT COST                            
         BNE   VR154X                                                           
         OI    PIMDSTAT,X'80'                                                   
*                                                                               
         ZAP   WORK(16),VALUNITS                                                
         CP    VALUNITS,=P'0'          SEE IF UNITS ENTERED                     
         BH    VR154D                                                           
         L     RF,AIO                  TRY AND USE PBUYREC'S UNITS              
         CLC   PBUYKMED-PBUYREC(2,RF),=X'D520'   NEWSPAPER BUY?                 
         BNE   VR154X                                                           
         ZAP   WORK(16),PBDUNITS-PBUYREC(L'PBDUNITS,RF)                         
*                                                                               
VR154D   MP    WORK(16),VALCOST    VALCOST SHOULD HAVE 5 DECIMALS               
         DP    WORK(16),=P'100'                                                 
         ZAP   WORK(16),WORK(14)                                                
         DP    WORK(16),=P'10'                                                  
         CP    WORK(14),=P'-2100000000'  MAX FOR FULL WORD                      
         BL    VR154ERR                                                         
*                                                                               
         CP    WORK(14),=P'2100000000'   MAX FOR FULL WORD                      
         BL    VR154X                                                           
VR154ERR DS    0H                                                               
*                                                                               
         USING SCRLIN1D,R2                                                      
         LA    R2,SLN1RTEH                                                      
         B     INVLRATE     RATE INVALID - PROBABLY FORGOT "T"                  
         DROP  R2                                                               
*                           FOR TOTAL RATE                                      
*                           THIS EDIT SHOULD PREVENT DUMPS                      
*                           IN GETINS                                           
*                                                                               
*                                                                               
VR154X   DS    0H                                                               
         CLI   MYGRSNET,C'N'       NET DOLLARS?                                 
         BE    *+12                                                             
         CLI   VALCTYP,C'N'        FLAG IF NET DOLLARS                          
         BNE   VR156                                                            
         CLI   VALCOSIN,C'S'                                                    
         BE    VR156                                                            
         OI    PIMDSTAT,X'20'                                                   
         ZAP   NETAMNT,VALCOST     PUT IN THE NET RATE                          
         ZAP   PERCENTG,MYPUBAC        AND THE AGENCY COMMISSION                
         BAS   RE,NETTOGRS         CHECK NET COST TO GROSS COST                 
         ZAP   VALCOST,GROSSAMT    COPY THE GROSS RATE                          
*                                                                               
         USING SCRLIN1D,R2                                                      
VR156    LA    R3,SLN1PRMH                                                      
         DROP  R2                                                               
*                                                                               
         CLI   ACTNUM,ACTCHECK     CHECK SCREEN?                                
         BNE   *+16                YES, NO NEED TO CHECK FOR COPY DOWN          
         CLI   5(R3),0                                                          
         BE    VR158               NOTHING TO VALIDATE                          
         B     VR156B                                                           
*                                  CHECK FOR COPY DOWN                          
         CLI   5(R3),1             IF JUST A PERIOD                             
         BNE   VR156A                                                           
         CLI   8(R3),C'.'                                                       
         BE    VR158               THEN NOTHING TO VALIDATE                     
*                                                                               
VR156A   CLI   5(R3),0             COPY DOWN THIS FIELD?                        
         BNE   VR156B              NO                                           
*                                                                               
         LA    R1,NEWLOWTB         NOTHING TO COPY DOWN IF NONE BEFORE          
         LA    R1,0(R1)                                                         
         C     R1,ACHGNTRY                                                      
         BNL   VR158                                                            
*                                                                               
         L     R1,ACHGNTRY         SEE IF THERE WAS A DETAIL BEFORE             
*                                                                               
         OC    0(L'PINVMINI,R1),0(R1)  DETAIL CHANGE?                           
         BNZ   VR158               YES, PREMIUM IS OPTIONAL                     
*                                                                               
         SH    R1,=Y(L'PINVMINI)                                                
         OC    0(L'PINVMINI,R1),0(R1)                                           
         BZ    VR158               NONE, VALIDATE IT AS IS                      
*                                                                               
         MVC   MELEM2,MELEM                                                     
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(L'PINVMINI),0(R1)                                        
         BAS   RE,MINIORD                                                       
         BNE   INVLSEL        ELEMENT NOT FOUND                                 
         MVC   VALPRCOS,PIMPREM                                                 
         MVC   VALCL,PIMCLRS                                                    
*                                                                               
         MVC   MELEM,MELEM2                                                     
         B     VR158                                                            
*                                                                               
VR156B   GOTO1 =A(VPREM),DMCB,(RC),(R2),RR=RELO                                 
*                                                                               
         CP    VALPRCOS,=P'0'      DO WE HAVE A PREMIUM COST?                   
         BNE   *+10                                                             
         LR    R2,R3                                                            
         B     NOPREM              NO, THEN ERROR                               
*                                                                               
         USING SCRLIN1D,R2                                                      
VR158    LA    R3,SLN1CTPH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2CTPH                                                      
         DROP  R2                                                               
*                                                                               
         CLC   =C'***',QPRD                                                     
         BNE   VR158A1                                                          
         BAS   RE,RDPROD                                                        
         CLC   =C'CT',CHKOPTN                                                   
         BNE   VR160                                                            
*                                                                               
VR158A0  DS    0H                                                               
         USING SCRLIN1D,R2                                                      
         LA    R3,SLN1NETH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2NETH                                                      
         DROP  R2                                                               
*                                                                               
VR158A1  CLI   ACTNUM,ACTCHECK     ACTION CHECK?                                
         BNE   VR158C              NO                                           
*                                                                               
         CLI   5(R3),0             IF NOTHING ENTERED                           
         BNE   VR159                                                            
*                                                                               
VR158A   MVI   5(R3),2             THEN SET DEFAULTS                            
         MVI   9(R3),0                                                          
VR158B   MVI   8(R3),C'Y'                                                       
         CLI   CASHDISC,C'Y'           CASH DISCOUNT FROM INV HEADER            
         BE    *+8                                                              
         MVI   8(R3),C'N'                                                       
         B     VR159               VALIDATE CTPBI NOW                           
*                                                                               
VR158C   CLI   5(R3),1                                                          
         BNE   *+12                                                             
         CLI   8(R3),C'.'                                                       
         BE    VR158A                                                           
*                                                                               
         CLI   5(R3),0                                                          
         BNE   VR159                                                            
*                                                                               
         LA    R1,NEWLOWTB         NOTHING TO COPY DOWN IF NONE BEFORE          
         LA    R1,0(R1)                                                         
         C     R1,ACHGNTRY                                                      
         BNL   VR158A              SET DEFAULTS                                 
*                                                                               
         L     R1,ACHGNTRY         SEE IF THERE WAS A DETAIL BEFORE             
*                                                                               
         OC    0(L'PINVMINI,R1),0(R1)  DETAIL CHANGE?                           
         BNZ   VR158A              YES, SET DEFAULTS                            
*                                                                               
         SH    R1,=Y(L'PINVMINI)                                                
         OC    0(L'PINVMINI,R1),0(R1)                                           
         BZ    VR158A              NONE, SET DEFAULTS                           
*                                                                               
         MVC   MELEM2,MELEM                                                     
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(L'PINVMINI),0(R1)                                        
         BAS   RE,MINIORD                                                       
         BNE   INVLSEL        ELEMENT NOT FOUND                                 
*                                                                               
         MVI   8(R3),C'Y'                                                       
         TM    PIMDSTAT,X'40'      IF PREVIOUS HAS NO CASH DISCOUNT             
         BZ    *+12                                                             
         OI    MELEM2+PIMDSTAT-PIMDTLEL,X'40'   THEN NONE FOR THIS ONE          
         MVI   8(R3),C'N'                                                       
*                                                                               
         TM    PIMDSTAT,X'08'      IF PREVIOUS HAS TEAR SHEET PROOF             
         BZ    *+12                                                             
         OI    MELEM2+PIMDSTAT-PIMDTLEL,X'08'   THEN THIS ONE DOES ALSO         
         MVI   9(R3),C'T'                                                       
*                                                                               
         MVC   MELEM,MELEM2                                                     
         B     VR160                                                            
*                                                                               
VR159    CLI   8(R3),C'Y'          NO CASH DISCOUNT?                            
         BE    VR159A              THERE IS                                     
         CLI   8(R3),C'N'                                                       
         BNE   VR158B              NOT Y OR N, USE DEFAULT FROM HEADER          
         OI    PIMDSTAT,X'40'      NONE                                         
*                                                                               
VR159A   CLI   9(R3),C'T'          TEAR SHEET PROOF?                            
         BNE   *+8                                                              
         OI    PIMDSTAT,X'08'      YES                                          
*                                                                               
         OC    10(3,R3),10(R3)     PBI USED FOR DISPLAY ONLY                    
         BZ    *+10                                                             
         LR    R2,R3                                                            
         B     INVLPBI                                                          
*                                                                               
VR160    CLI   SCRTYPE,C'N'                                                     
         BE    VR165                                                            
*                                                                               
         GOTO1 =A(VALGROSS),DMCB,(RC),(R2),RR=RELO                              
*                                                                               
VR165    CLI   CHKOPTN,C'Z'                                                     
         BNE   VR170                                                            
         USING SCRLIN1D,R2                                                      
         LA    R3,SLN1NETH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2NETH                                                      
*                                                                               
         XC    PIMBZONE(2),PIMBZONE                                             
         BAS   RE,ZONED                                                         
         MVC   PIMBZONE,NEWNUM+4                                                
         MVC   PIMBEDTN,NEWNUM+5                                                
         DROP  R2                                                               
*                                                                               
         USING SCRLIN1D,R2                                                      
VR170    LA    R3,SLN1ESTH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2ESTH                                                      
         DROP  R2                                                               
*                                                                               
         CLI   ACTNUM,ACTCHECK                                                  
         BNE   VR171                                                            
         CLI   5(R3),0             IF NO ESTIMATE                               
         BNE   VR175                                                            
         B     VR172                                                            
*                                                                               
VR171    CLI   5(R3),1                                                          
         BNE   *+12                                                             
         CLI   8(R3),C'.'                                                       
         BE    VR172                                                            
*                                                                               
         CLI   5(R3),0                                                          
         BNE   VR175                                                            
*                                                                               
         LA    R1,NEWLOWTB         NOTHING TO COPY DOWN IF NONE BEFORE          
         LA    R1,0(R1)                                                         
         C     R1,ACHGNTRY                                                      
         BNL   VR172                                                            
*                                                                               
         L     R1,ACHGNTRY         SEE IF THERE WAS A DETAIL BEFORE             
*                                                                               
         OC    0(L'PINVMINI,R1),0(R1)  DETAIL CHANGE?                           
         BNZ   VR172               YES, SEE IF WE NEED AN ESTIMATE              
*                                                                               
         SH    R1,=Y(L'PINVMINI)                                                
         OC    0(L'PINVMINI,R1),0(R1)                                           
         BZ    VR172               NONE, VALIDATE IT AS IS                      
*                                                                               
         MVC   MELEM2,MELEM                                                     
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(L'PINVMINI),0(R1)                                        
         BAS   RE,MINIORD                                                       
         BNE   INVLSEL        ELEMENT NOT FOUND                                 
*                                                                               
         MVC   MELEM2+PIMIEST-PIMDTLEL(L'PIMIEST),PIMIEST                       
         MVC   MELEM,MELEM2                                                     
         B     VR180                                                            
*                                                                               
VR172    CLI   SVPROF+3,C'Y'       MULTI-ESTIMATE?                              
         BNE   *+14                                                             
VR172A   MVC   PIMIEST,MYBEST      THEN USE HEADER ESTIMATE                     
         B     VR180                                                            
*                                                                               
         OC    MYBEST,MYBEST                                                    
         BNZ   VR172A                                                           
         LR    R2,R3                                                            
         B     MISSFLD                                                          
*                                                                               
VR175    LR    R0,R2               VALIDATE THE ESTIMATE                        
         LR    R2,R3                                                            
         CLC   CHKPRD,=C'***'                                                   
         BNE   *+10                                                             
         MVC   QPRD,PIMSPRD                                                     
*        L     R1,ATIOB                                                         
*        USING TIOBD,R1                                                         
*        OI    TIOBINDS,TIOBSETC                                                
*        LR    R0,R2                                                            
*        SR    R0,RA                                                            
*        STCM  R0,3,TIOBCURD                                                    
*        MVI   TIOBCURI,0                                                       
*        DROP  R1                                                               
         GOTO1 VALIEST                                                          
*        L     R1,ATIOB                                                         
*        USING TIOBD,R1                                                         
*        NI    TIOBINDS,X'FF'-TIOBSETC                                          
*        DROP  R1                                                               
         MVC   QPRD,CHKPRD                                                      
         LR    R3,R2                                                            
         LR    R2,R0                                                            
         GOTO1 DATCON,DMCB,(3,PIMIDATE),(0,DUB)                                 
         CLC   ESTSTDT,DUB         DATE MUST BE IN EST'S PERIOD                 
         BH    DATEPER                                                          
         CLC   ESTNDDT,DUB                                                      
         BL    DATEPER                                                          
*                                                                               
         MVC   PIMIEST,BEST        SAVE THE ESTIMATE                            
         MVC   BEST,MYBEST         RESTORE HEADER ESTIMATE                      
*                                                                               
VR180    MVC   PIMSPACE,VALSPACE   SPACE                                        
         MVC   PIMUIND,VALUIND     UNIT INDICATOR                               
         ZAP   PIMUNITS,VALUNITS   NUMBER OF UNITS                              
         ZAP   PIMCLMS,VALCLMS     NUMBER OF COLUMNS                            
*                                                                               
VR181    ZAP   PIMCOST,VALCOST     RATE                                         
         MVC   PIMCSIND,VALCOSIN   COST INDICATOR                               
         MVC   PIMCLRS,VALCL       NUMBER OF COLORS                             
         ZAP   PIMPREM,VALPRCOS    PREMIUM CHARGE                               
         CLI   VALCOSTY,C'U'                                                    
         BNE   *+8                                                              
         OI    PIMDSTAT,X'80'                                                   
         CLI   VALCTYP,C'N'        FLAG IF NET DOLLARS                          
         BNE   *+8                                                              
         OI    PIMDSTAT,X'20'                                                   
*                                                                               
VR182    L     R1,ACHGNTRY         SEE IF DETAIL EXISTS BEFORE                  
         OC    0(L'PINVMINI,R1),0(R1)                                           
         BZ    VR186               NO, ADD DETAIL                               
*                                                                               
         MVC   MELEM2,MELEM        YES, GET THE DETAIL                          
         MVC   MINEKEY(L'PINVMINI),0(R1)                                        
         BAS   RE,MINIORD                                                       
         BNE   INVLSEL            MINIO ELEMENT NOT FOUND                       
         MVC   QPRD2(2),PIMBZONE                                                
         MVC   QPRD2+2(1),PIMDSTAT                                              
         TM    PIMDSTAT,X'04'      COMMENT EXISTS FOR THIS DETAIL?              
         BZ    *+8                                                              
         OI    MELEM2+PIMDSTAT-PIMDTLEL,X'04'  YES                              
         MVC   MELEM,MELEM2                                                     
*                                                                               
         USING SCRLIN1D,R2                                                      
         LA    R3,SLN1IDTH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2IDTH                                                      
         DROP  R2                                                               
*                                                                               
         TM    4(R3),X'20'         DATE CHANGED?                                
         BNZ   VR184                                                            
         GOTO1 MINIO,DMCB,('MINDEL',(R5))   DELETE IT BECAUSE THE DATES         
         CLI   MINERR,0                     HAVE TO BE IN ORDER                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MELEM,MELEM2                                                     
         TM    PIMDSTAT,X'04'      ANY COMMENTS?                                
         BZ    VR186                                                            
         MVI   MINEKEY,PIMCOMEQ                                                 
         BAS   RE,MINIORD                                                       
         BNE   INVLSEL            MINIO ELEMENT NOT FOUND                       
         MVC   BLOCK(L'MELEM),MELEM   MAKE A COPY OF THE COMMENT                
         GOTO1 MINIO,DMCB,('MINDEL',(R5))   DELETE IT BECAUSE THE DATES         
         CLI   MINERR,0                     HAVE TO BE IN ORDER                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MELEM,MELEM2                                                     
         B     VR186               ADD DETAIL WITH NEW SEQUENCE NUMBER          
*                                                                               
VR184    MVC   PIMDTLS1(L'SEQUENCE),MINEKEY+1 CHANGE ON THIS SEQUENCE           
*                                                                               
VR184A   MVC   MELEM2,MELEM                                                     
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
         MVC   MINEKEY+2(L'PIMDTLS2),PIMDTLS2                                   
         BAS   RE,MINIORD                                                       
         BNE   INVLSEL            MINIO ELEMENT NOT FOUND                       
         MVC   QPRD2(2),PIMBZONE                                                
         MVC   QPRD2+2(1),PIMDSTAT                                              
         MVC   MELEM,MELEM2                                                     
*                                                                               
         CLC   =C'***',CHKPRD      PRODUCT VARIOUS?                             
         BNE   VR186A                                                           
         CLC   =C'CT',CHKOPTN                                                   
         BE    VR186A                                                           
         MVC   PIMDSTAT,QPRD2+2                                                 
         NI    PIMDSTAT,X'FF'-X'80'                                             
         CLI   VALCOSTY,C'U'                                                    
         BNE   *+8                                                              
         OI    PIMDSTAT,X'80'                                                   
*                                                                               
VR186A   TM    GLOBFLG1,X'80'      PUB,ALL?                                     
         BZ    VR187                                                            
         CLI   CHKOPTN,C'Z'                                                     
         BE    VR187                                                            
         MVC   PIMBZONE(2),QPRD2                                                
*                                                                               
VR187    BAS   RE,MINIOWRT                                                      
         L     R1,ACHGNTRY                                                      
         MVC   0(1,R1),PIMDTLEL                                                 
         MVC   1(L'SEQUENCE,R1),PIMDTLS1                                        
         CLC   LSTMINSQ,0(R1)                                                   
         BL    *+10                                                             
         MVC   LSTMINSQ,0(R1)                                                   
         B     VR100NX                                                          
*                                                                               
VR186    GOTO1 NXDTLSEQ                                                         
         BE    VR188                                                            
         GOTO1 RESEQNCE,DMCB,NEWLOWTB,0                                         
         B     VR186                                                            
*                                                                               
VR188    MVC   PIMDTLS2,HALF                                                    
*                                                                               
         CLC   =C'***',CHKPRD      PRODUCT VARIOUS?                             
         BNE   VR188A                                                           
         CLC   =C'CT',CHKOPTN                                                   
         BE    VR188A                                                           
         MVC   PIMDSTAT,QPRD2+2                                                 
         NI    PIMDSTAT,X'FF'-X'80'                                             
         CLI   VALCOSTY,C'U'                                                    
         BNE   *+8                                                              
         OI    PIMDSTAT,X'80'                                                   
*                                                                               
VR188A   TM    GLOBFLG1,X'80'      PUB,ALL?                                     
         BZ    VR189                                                            
         CLI   CHKOPTN,C'Z'                                                     
         BE    VR189                                                            
         MVC   PIMBZONE(2),QPRD2                                                
*                                                                               
VR189    BAS   RE,MINIOADD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,ACHGNTRY                                                      
         MVC   0(1,R1),PIMDTLEL                                                 
         MVC   1(L'SEQUENCE,R1),PIMDTLS1                                        
         CLC   LSTMINSQ,0(R1)                                                   
         BL    *+10                                                             
         MVC   LSTMINSQ,0(R1)                                                   
*                                                                               
         TM    PIMDSTAT,X'04'      ANY COMMENT ELEMENT FOR THIS DETAIL?         
         BZ    VR100NX                                                          
*                                                                               
         MVC   MELEM2,BLOCK        YES                                          
         MVC   MELEM2+PIMDTLS2-PIMDTLEL(L'PIMDTLS2),PIMDTLS2                    
         MVC   MELEM,MELEM2                                                     
         BAS   RE,MINIOADD                                                      
         BE    VR100NX                                                          
         DC    H'0'                                                             
*                                                                               
VR100NX  CLI   ACTNUM,ACTCHECK                                                  
         BNE   VR190                                                            
         LA    R0,CK1LSTLH         CHECK LAST LINE ALREADY?                     
         CLI   SCRTYPE,C'N'                                                     
         BE    VR195                                                            
         LA    R0,CK2LSTLH                                                      
         B     VR195                                                            
*                                                                               
VR190    LA    R0,UP1LSTLH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         LA    R0,UP2LSTLH                                                      
*                                                                               
VR195    CR    R2,R0                                                            
         BE    VRX                 YES, DONE VALIDATING LINES                   
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         AH    R2,=Y(SLN1LEN)      NO, POINT TO NEXT LINE                       
         B     *+8                                                              
         AH    R2,=Y(SLN2LEN)                                                   
*                                                                               
         L     R1,ACHGNTRY         AND NEXT ENTRY IN TABLE                      
         LA    R1,L'PINVMINI(R1)                                                
         ST    R1,ACHGNTRY                                                      
         B     VR100LP                                                          
         EJECT                                                                  
VR200    GOTO1 =A(GOOVLYS),DMCB,(RC),RR=RELO                                    
*                                                                               
         TM    OVLYFLAG,X'08'      IF FROM THE TOTAL SCREEN                     
         BZ    LR                                                               
*        NI    OVLYFLAG,X'FF'-X'08'   NOT GOING THERE AGAIN                     
         NI    CHKMEDH+4,X'FF'-X'20'  SHOW OUR SCREEN NOW                       
         B     XIT                                                              
*                                                                               
VRX      MVC   LOWERTBL,NEWLOWTB                                                
*                                                                               
LR       GOTO1 CALLOV,DMCB,(X'04',0),(0,0)    USE CHECK DISPLAY LOGIC           
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC),(RA),(R9),(R8),(R5),(R4)                          
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FOR PRODUCT ***, RD PRODUCT COLUMN FROM SCREEN INTO INVOICE RECORD            
*        R3 ==> SCREEN FIELD FOR PRODUCT                                        
***********************************************************************         
*                                                                               
RDPROD   NTR1                                                                   
*                                                                               
         XC    PIMSPRD,PIMSPRD     INIT PRODUCT STORE AREA                      
*                                                                               
         MVC   WORK(L'KEY),KEY     SAVE CURRENT KEY                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY              ESTABLISH AS PRODUCT KEY                     
         USING PPRDKEY,R2                                                       
*                                                                               
         MVC   PPRDKAGY,AGENCY     AGENCY                                       
         MVC   PPRDKMED,QMED       MEDIA                                        
         MVI   PPRDKRCD,X'06'      RECORD ID                                    
         MVC   PPRDKCLT,QCLT       CLIENT                                       
*                                                                               
         MVC   PPRDKPRD,=CL3' '    PRESET PRODUCT TO SPACES                     
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R3)          INPUT LENGTH                                 
         BZ    VR100MIS            PRODUCT REQUIRED                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PPRDKPRD(0),8(R3)   ADD PRODUCT TO KEY                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                READ PRODUCT RECORD KEY                      
*                                                                               
         CLC   KEY(25),KEYSAVE     MUST FIND POINTER                            
         BNE   RDPRODNV            MUST FIND POINTER                            
*                                                                               
         MVC   PIMSPRD,PPRDKPRD    SAVE PRODUCT ID                              
*******                                                                         
*******  STATEMENT BELOW CAUSES DUMPS IN PPMAT06                                
*******  (IT CAN'T FIND HEADER WHEN QPRD WAS "***")                             
*******  I HAVE NO IDEA WHY THIS STATEMENT WAS HERE.                            
*******                                                                         
*******  MVC   QPRD,PIMSPRD                                                     
*******                                                                         
*******  NOW I ONLY SET QPRD IF IT WAS NOT "***"                                
*                                                                               
         CLC   QPRD,=C'***'         SEE IF *** - VARIOUS PRDS                   
         BE    *+10                DON'T DESTROY                                
         MVC   QPRD,PIMSPRD                                                     
*                                                                               
         MVC   KEY,WORK            RESTORE CURRENT FILE POINTERS                
         GOTO1 HIGH                                                             
*                                                                               
RDPRODX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
RDPRODNV MVI   GERROR1,INVPRD      PRODUCT INVALID                              
         LR    R2,R3               R2 MUST POINT TO SCREEN FIELD                
         B     ERREXIT                                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* FOR PRODUCT ***, RD PRODUCT COLUMN FROM SCREEN INTO INVOICE RECORD            
***********************************************************************         
*                                                                               
*DPROD   NTR1                                                                   
*                                                                               
*        XC    PIMSPRD,PIMSPRD                                                  
*        CLI   8(R3),C' '                                                       
*        BE    VR100MIS                                                         
*        LR    R2,R3               R2 PTS TO FIELD FOR VALIPRD                  
*        GOTO1 VALIPRD                                                          
*        MVC   PIMSPRD,QPRD                                                     
*        MVC   PRDNM,=C'VARIOUS                 '                               
*        XC    EDITN,EDITN                                                      
*                                                                               
*        CLI   SVPROF+4,C'Y'                                                    
*        BNE   RDPRODX                                                          
*        LA    R2,CHKESTMH                                                      
*        MVC   QPRD,PIMSPRD                                                     
*        L     R1,ATIOB                                                         
*        USING TIOBD,R1                                                         
*        OI    TIOBINDS,TIOBSETC                                                
*        LR    R0,R2                                                            
*        SR    R0,RA                                                            
*        STCM  R0,3,TIOBCURD                                                    
*        MVI   TIOBCURI,0                                                       
*        DROP  R1                                                               
*        GOTO1 VALIEST                                                          
*        L     R1,ATIOB                                                         
*        USING TIOBD,R1                                                         
*        NI    TIOBINDS,X'FF'-TIOBSETC                                          
*        DROP  R1                                                               
*        MVC   QPRD,CHKPRD                                                      
*        GOTO1 DATCON,DMCB,(3,PIMIDATE),(0,DUB)                                 
*                                                                               
*        L     R2,AOFLINE                                                       
*        LA    R2,19(R2)                                                        
*        CLC   ESTSTDT,DUB         DATE MUST BE IN EST'S PERIOD                 
*        BNH   *+8                                                              
*        B     DATEPER                                                          
*        CLC   ESTNDDT,DUB                                                      
*        BNL   *+8                                                              
*        B     DATEPER                                                          
*                                                                               
*DPRODX  B     XIT                                                              
*                                                                               
***********************************************************************         
* MOVE ZONE/EDITION FROM SCREEN INTO INVOICE RECORD                             
***********************************************************************         
ZONED    NTR1                                                                   
*                                                                               
         LR    R2,R3                                                            
         XC    NEWNUM,NEWNUM                                                    
         XC    PERVALST,PERVALST                                                
         XC    ZONE,ZONE                                                        
         XC    EDITN,EDITN                                                      
         OC    8(2,R3),8(R3)                                                    
         BZ    ZONEDX                                                           
         XC    BLOCK(50),BLOCK                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK)                                      
         CLI   DMCB+4,0                                                         
         BE    MISSFLD                                                          
*                                                                               
         LA    R3,BLOCK                                                         
         ZIC   RE,DMCB+4           NUMBER OF LINES                              
         LA    RF,PERVALST         JUST USE AS TEMP STORAGE                     
*                                                                               
ZONED10  ZIC   R1,0(R3)            LENGTH OF FIELD                              
         LTR   R1,R1                                                            
         BZ    ZONED20                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),12(R3)                                                   
         TM    2(R3),X'40'         ALPHA                                        
         BO    *+14                                                             
         MVC   ZONE,0(RF)                                                       
         B     *+10                                                             
         MVC   EDITN,0(RF)                                                      
         LA    R1,1(R1)                                                         
         LA    RF,0(R1,RF)                                                      
         MVI   0(RF),C'.'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
ZONED20  LA    R3,32(R3)                                                        
         BCT   RE,ZONED10                                                       
         BCTR  RF,0                                                             
         MVI   0(RF),C' '                                                       
*                                                                               
         XC    WORK(15),WORK                                                    
         LA    R6,8                                                             
         MVI   PACKED+4,X'0F'                                                   
         MVO   PACKED,BPUB(4)                                                   
         UNPK  WORK(8),PACKED                                                   
         OC    ZONE,ZONE                                                        
         BZ    ZONED25                                                          
         MVI   WORK+8,C','                                                      
         MVC   WORK+9(2),ZONE                                                   
         LA    R6,3(R6)                                                         
         OC    EDITN,EDITN                                                      
         BZ    ZONED30                                                          
         MVI   WORK+11,C','                                                     
         LA    R6,1(R6)                                                         
         MVC   WORK+12(3),EDITN                                                 
ZONED21  SR    R2,R2                                                            
         CLI   EDITN+1,X'00'                                                    
         BNE   *+12                                                             
         LA    R2,1                                                             
         B     ZONED23                                                          
         CLI   EDITN+2,X'00'                                                    
         BNE   *+12                                                             
         LA    R2,2                                                             
         B     ZONED23                                                          
         LA    R2,3                                                             
ZONED23  LA    R6,0(R2,R6)                                                      
         B     ZONED30                                                          
*                                                                               
ZONED25  OC    EDITN,EDITN                                                      
         BZ    ZONEDX                                                           
         MVI   WORK+8,C','                                                      
         LA    R6,1(R6)                                                         
         MVC   WORK+9(3),EDITN                                                  
         B     ZONED21                                                          
*                                                                               
ZONED30  GOTO1 VPUBVAL,DMCB,((R6),WORK),NEWNUM                                  
         MVI   GERROR1,INVPUB                                                   
         CLI   0(R1),X'FF'                                                      
         BE    ERREXIT                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),QMED         PUBREC                                       
         MVC   KEY+1(6),NEWNUM                                                  
         MVC   KEY+7(2),AGENCY                                                  
         MVI   KEY+9,X'81'                                                      
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=C'PUBDIR'                                              
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   GERROR1,INVPUB                                                   
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERREXIT                                                          
*                                                                               
ZONEDX   B     XIT                                                              
***********************************************************************         
* THIS ROUTINE CALCULATES NET AMOUNTS TO GROSS AMOUNTS BASED ON A               
* PERCENTAGE.                                                                   
*                                                                               
* ON ENTRY:    NETAMNT     P       NET AMOUNT                                   
*              PERCENTG    P       PERCENTAGE OF GROSS THAT IS NET              
*                                                                               
* ON EXIT:     GROSSAMT    P       GROSS AMOUNT                                 
*                                                                               
* WARNING:     ALL 3 SPECIFIED VARIABLES WILL GET CLOBBERED                     
***********************************************************************         
NETTOGRS NTR1                                                                   
         CP    PERCENTG,=P'0'      IF NO PERCENTAGE                             
         BNE   NTOG10                                                           
         ZAP   GROSSAMT,NETAMNT    THEN THE GROSS IS SAME AS THE NET            
         B     NTOGX                                                            
*                                                                               
NTOG10   ZAP   GROSSAMT,=P'100000'  GROSS = NET/(1-%AGE)                        
         SP    GROSSAMT,PERCENTG                                                
         ZAP   PERCENTG,GROSSAMT                                                
         MP    NETAMNT,=P'100000'   KEEP ALL DECIMALS PLACES INTACT             
         DP    NETAMNT,PERCENTG                                                 
         ZAP   GROSSAMT,NETAMNT(L'NETAMNT-L'PERCENTG)                           
* DO WE ROUND UP ONE?                                                           
         CP    NETAMNT+L'NETAMNT-L'PERCENTG(L'PERCENTG),=P'50000'               
         BL    *+10                                                             
         AP    GROSSAMT,=P'1'      YES                                          
*                                                                               
NTOGX    B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE CALCULATES GROSS AMOUNTS TO NET AMOUNTS BASED ON A               
* PERCENTAGE.                                                                   
*                                                                               
* ON ENTRY:    GROSSAMT    P       GROSS AMOUNT                                 
*              PERCENTG    P       PERCENTAGE OF GROSS THAT IS NET              
*                                                                               
* ON EXIT:     NETAMNT     P       NET AMOUNT                                   
*                                                                               
* WARNING:     ALL 3 SPECIFIED VARIABLES WILL GET CLOBBERED                     
***********************************************************************         
GRSTONET NTR1                                                                   
         CP    PERCENTG,=P'0'      IF NO PERCENTAGE                             
         BE    GTON10              THEN NET = GROSS                             
*                                                                               
         ZAP   NETAMNT,=P'100000'       NET = GROSS * (1-%AGE)                  
         SP    NETAMNT,PERCENTG                                                 
         ZAP   PERCENTG,NETAMNT                                                 
         MP    GROSSAMT,PERCENTG                                                
         SRP   GROSSAMT,64-5,5          DIVIDE BY 100000 AND ROUND              
GTON10   ZAP   NETAMNT,GROSSAMT                                                 
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS A MINIO ELEMENT.  MINEKEY MUST BE SET BY CALLER            
***********************************************************************         
MINIORD  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,MINERNF      SEE IF NOT FOUND                             
         BE    NO                                                               
         CLI   MINERR,0                                                         
         BE    YES                                                              
         DC    H'0'                DIE ON ANY OTHER ERRORS                      
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ HIGH A MINIO ELEMENT.  MINEKEY MUST BE SET BY               
* THE CALLER.                                                                   
***********************************************************************         
MINIOHI  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    NO                                                               
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ SEQUENTIAL FOR A MINIO ELEMENT.                             
***********************************************************************         
MINIOSEQ NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    NO                                                               
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE WRITES OUT A MINIO ELEMENT.  MINELEM MUST BE SET                 
***********************************************************************         
MINIOWRT NTR1                                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE ADDS A MINIO ELEMENT.  MINELEM MUST BE SET BY THE CALLER         
***********************************************************************         
MINIOADD NTR1                                                                   
         OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 MINIO,DMCB,('MINADD',(R5))                                       
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         CLI   MINERR,MINEDUP      DUPLICATE KEY?                               
         BE    NO                  YES, RETURN A NO                             
         DC    H'0'                DIE ON ANY ERROR                             
         EJECT                                                                  
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
RELO     DS    A                                                                
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
INVLNUM  MVI   GERROR1,NOTNUM                                                   
         B     ERREXIT                                                          
*                                                                               
BIGSIZE  MVI   GERROR1,SIZEBIG                                                  
         B     ERREXIT                                                          
*                                                                               
INVLRATE MVI   GERROR1,INVRATE                                                  
         B     ERREXIT                                                          
*                                                                               
INVLAMT  MVI   GERROR1,INVAMT                                                   
         B     ERREXIT                                                          
*                                                                               
INVLPREM MVI   GERROR1,INVPREM                                                  
         B     ERREXIT                                                          
*                                                                               
NOPREM   MVI   GERROR1,MISSPREM                                                 
         B     ERREXIT                                                          
*                                                                               
DATEPER  MVI   GERROR1,DTNOTPER                                                 
         B     ERREXIT                                                          
*                                                                               
INVLDATE MVI   GERROR1,INVDATE                                                  
         B     ERREXIT                                                          
*                                                                               
INVLSEL  MVI   GERROR1,INVSEL                                                   
         B     ERREXIT                                                          
*                                                                               
INVLPBI  MVI   GERROR1,PBIFORBY    PBI ONLY FOR BUYS                            
         B     ERREXIT                                                          
*                                                                               
INVLPAID MVI   GERROR1,MPAIDBUY    CAN'T MANIPULATE A PAID INSERTION            
         B     ERREXIT                                                          
*                                                                               
ALRDYMTC MVI   GERROR1,ISMATCHD    INSERTION IS ALREADY MATCHED                 
         B     ERREXIT                                                          
*                                                                               
NTMATCHD MVI   GERROR1,NOTMTCHD    INSERTION IS NOT MATCHED                     
         B     ERREXIT                                                          
*                                                                               
TORNINSR MVI   GERROR1,TORNALRD    INSERTION HAS TEARSHEET ALREADY              
         B     ERREXIT                                                          
*                                                                               
CANTMCOR MVI   GERROR1,MTCHCORR    CAN'T MATCH A CORRECTED BUY                  
         B     ERREXIT                                                          
*                                                                               
CANTCORM MVI   GERROR1,CORRMTCH    CAN'T CORRECTED A MATCHED BUY                
         B     ERREXIT                                                          
*                                                                               
CANTPCOR MVI   GERROR1,PAYCORR     CAN'T PAY A CORRECTED BUY                    
         B     ERREXIT                                                          
*                                                                               
PAYAPAID MVI   GERROR1,PAIDALRD    INSERTION HAS BEEN PAID ALREADY              
         B     ERREXIT                                                          
*                                                                               
MISSPROF MVI   GERROR1,NEEDPROF    NO A0A PROFILE                               
         B     ERREXIT                                                          
*                                                                               
NOMTCHBY MVI   GERROR1,MATCHEDQ    NO MATCHED BUYS                              
         B     PAYPERR                                                          
*                                                                               
NOMTCHTS MVI   GERROR1,MTCHTSEQ    NO MATCHED BUYS WITH TEARSHEET               
         B     PAYPERR                                                          
*                                                                               
NTALMTCH MVI   GERROR1,ALLMTCHQ    NOT ALL MATCHED BUYS                         
         B     PAYPERR                                                          
*                                                                               
NTALMTTS MVI   GERROR1,AMTCHTSQ    NOT ALL MATCHED BUYS WITH TEARSHEET          
         B     PAYPERR                                                          
*                                                                               
RECLOCKD MVI   GERROR1,DATALOK     BUYS LOCKED FOR OFFLINE PROCESSING           
         B     RECLOKS                                                          
*                                                                               
PAYPERR  LA    R2,CHKOPTNH                                                      
         B     ERREXIT                                                          
*                                                                               
RECLOKS  LA    R2,CHKMEDH                                                       
         B     ERREXIT                                                          
*                                                                               
INFEXIT  MVI   GMSGTYPE,C'I'                                                    
         B     MYERRXIT                                                         
*                                                                               
ERREXIT  MVI   GMSGTYPE,C'E'                                                    
         NI    MNIOFLAG,X'FF'-X'80'  DON'T SAVE ANY CHANGES ON ERRORS           
MYERRXIT GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PFKEY TABLE DEFINITIONS FOR INVOICE CHECK                                     
***********************************************************************         
PFTABLE  DS    0C                                                               
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,12,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
*                                                                               
MONTHLST DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC',X'00'                    
***********************************************************************         
* OPTIONS TABLE                                                                 
***********************************************************************         
OPTNTABL DS    0C                                                               
         DC    C'A.',CL11'--ADCODE---'                                          
OPTNNTRY DC    C'B.',CL11'-BILLDATE--'                                          
         DC    C'C.',CL11'----CD-----'                                          
         DC    C'CT',CL11'CTPBI------'                                          
         DC    C'G.',CL11'---GROSS---'                                          
         DC    C'GL',CL11'---GLCD----'                                          
         DC    C'GS',CL11'----GST----'                                          
         DC    C'L.',CL11'--LASTIO---'                                          
         DC    C'N.',CL11'----NET----'                                          
         DC    C'NL',CL11'---NLCD----'                                          
         DC    C'P.',CL11'--PAYDATE--'                                          
         DC    C'T.',CL11'----TAX----'                                          
         DC    C'Z.',CL11'-ZONE,EDTN-'                                          
*                                                                               
         DC    X'FF'                                                            
*                                                                               
OPTNLEN  EQU   OPTNNTRY-OPTNTABL                                                
         EJECT                                                                  
***********************************************************************         
* THIS VALIDATES THE SHOW/REG/ILLUM FIELD                                       
***********************************************************************         
SHWREGIL DS    0H                                                               
         NMOD1 0,**SRI***                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         CLI   QMED,C'O'           OUTDOORS                                     
         BNE   SRIX                                                             
         CLC   =C'SRI=',VALSPACE                                                
         BNE   SRIX                                                             
         MVC   WORK(17),VALSPACE                                                
         MVI   VALSPACE,X'FF'                                                   
         ZAP   VALSHOW,=P'99999'                                                
         ZAP   VALREG,=P'0'                                                     
         ZAP   VALILLUM,=P'0'                                                   
*                                                                               
         LA    R5,WORK+7                                                        
         CLC   WORK+4(3),=C'SPC'                                                
         BE    CO4                                                              
         ZAP   VALSHOW,=P'0'                                                    
         GOTO1 =V(NUMED),DMCB,WORK+4,DUB,RR=RELO                                
*                                                                               
         CP    DUB,=P'999'          SHOWING CAN'T EXCEED 100                    
         BH    INVLFLD              ALLOW UP TO 999 TO                          
*                                   HANDLE GRP'S                                
         ZAP   VALSHOW,DUB                                                      
         L     R5,DMCB                                                          
         CLI   0(R5),C' '                                                       
         BE    INVLFLD                                                          
CO4      DS    0H                                                               
         GOTO1 =V(NUMED),DMCB,1(R5),DUB,RR=RELO                                 
*                                                                               
         ZAP   VALREG,DUB                                                       
         L     R5,DMCB                                                          
         CLI   0(R5),C' '                                                       
         BE    INVLFLD                                                          
         GOTO1 =V(NUMED),DMCB,1(R5),DUB,RR=RELO                                 
*                                                                               
         ZAP   VALILLUM,DUB                                                     
         L     R5,DMCB                                                          
         CLI   0(R5),C' '                                                       
         BH    INVLFLD                                                          
COX      DS    0H                                                               
*                                                                               
SRIX     B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CALLS EITHER THE COMMENT OR TOTAL OVERLAY                        
***********************************************************************         
GOOVLYS  DS    0H                                                               
         NMOD1 0,**GOOV**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R8,ASPOOLD                                                       
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
         LA    R4,SYSSPARE                                                      
         LA    R5,MINBLOCK                                                      
*                                                                               
         XC    DMCB(24),DMCB                                                    
*                                                                               
         MVI   DMCB,X'03'          USE COMMENT OVERLAY                          
         TM    OVLYFLAG,X'08'                                                   
         BZ    *+8                                                              
         MVI   DMCB,X'05'          USE TOTAL OVERLAY                            
*                                                                               
         GOTO1 CALLOV,DMCB            CALL THE OVERLAY                          
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC),(RA),(R9),(R8),(R5),(R4)                          
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF THE USER CAN SWITCH TO THE PAY PROGRAM.         
***********************************************************************         
PAYPRGM  DS    0H                                                               
         NMOD1 0,**PAYP**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
         LA    R4,SYSSPARE                                                      
         LA    R5,MINBLOCK                                                      
*                                                                               
         XC    WORK,WORK           GET THE A0A PROFILE                          
         XC    SVPROF,SVPROF                                                    
         MVC   WORK(4),=C'PA0A'                                                 
         NI    WORK,X'BF'                                                       
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         CLI   CLTOFICE,C' '                                                    
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CLTOFICE                                              
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
         CLI   SVPROF,0                                                         
         BNE   PAYP01                                                           
         B     MISSPROF            NO A0A PROFILE FOUND                         
*                                                                               
PAYP01   DS    0H                                                               
*                                                                               
         OC    INVDTLAG,INVDTLAG   SEE IF PACKED FIELD SET (GROSS)              
         BZ    NOMTCHBY            SEND ERROR INSTEAD OF DUMPING                
         OC    INVDTLAN,INVDTLAN   SEE IF PACKED FIELD SET (NET)                
         BZ    NOMTCHBY            SEND ERROR INSTEAD OF DUMPING                
***                                                                             
***      I BELIEVE THIS OCCURS IF THEY HIT PAY PFKEY                            
***      FROM INV CHECK SCREEN WITH NO BUYS                                     
***                                                                             
         GOTO1 =A(A0AOPT),RR=RELO  SET NEW PAY PROFILE OPTIONS                  
*                                                                               
         ZAP   GROSSAMT,INVDTLAG   DEFAULT IS DETAILS TOTALS                    
         ZAP   NETAMNT,INVDTLAN                                                 
*                                                                               
         CLI   SVAGNATL,C'C'                                                    
         BNE   *+16                                                             
         AP    GROSSAMT,INVDTLGS                                                
         AP    NETAMNT,INVDTLGS                                                 
*                                                                               
         CLI   SVPROF+08,C'N'      SKIP IF NO TEARSHEET RESTRICTIONS            
         BNE   *+8                                                              
         CLI   SVPROF+10,C'N'      AND  IF NO MATCHED   RESTRICTIONS            
         BE    PAYPX                                                            
*                                                                               
         NI    BITFLAG,X'FF'-X'02'   NEED SOMETHING FOR PAY PROGRAM             
         ZAP   GROSSAMT,=P'0'      WE CARE, CALCULATE TOTALS BASED ON           
         ZAP   NETAMNT,=P'0'           THE PROFILE CRITERIA                     
*                                                                               
         XC    MINEKEY,MINEKEY     LOOK AT THE INVOICE DETAILS                  
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
*                                                                               
         BAS   RE,MINIOHI                                                       
*                                                                               
PAYPLP   L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
*                                                                               
         CLI   PIMDTLEL,PIMDTLEQ   IF NO MORE DETAILS                           
         BNE   *+14                                                             
         CLC   PIMDTLS1,LSTHDRSQ                                                
         BE    PAYP10                                                           
*                                                                               
PAYP05   TM    BITFLAG,X'02'          FOUND ANY FOR PAY PROGRAM?                
         BNZ   PAYP40                 YES, DONE                                 
*                                                                               
*        NO DETAILS FOUND MATCHING ALL CRITERIA                                 
*                                                                               
         CLI   SVPROF+8,C'Y'             NEED TEARSHEET AND MATCHED?            
         BE    NOMTCHTS                                                         
         B     NOMTCHBY                  NO, "NO MATCHED BUYS"                  
*                                                                               
*        INVOICE DETAIL FOUND                                                   
*                                                                               
PAYP10   TM    PIMDSTAT,X'02'      PAID ALREADY?                                
         BO    PAYPSEQ             YES - SKIP                                   
*                                                                               
         TM    PIMDSTAT,X'10'      MATCHED?                                     
         BO    PAYP20              YES                                          
*                                                                               
*        UNMATCHED                                                              
*                                                                               
         CLI   SVPROF+8,C'Y'       PAY ONLY IF   ALL   MATCHED W/ TS?           
         BE    NTALMTTS            "NOT ALL MATCHED WITH TEARSHEET"             
*                                                                               
         CLI   SVPROF+10,C'Y'      PAY ONLY IF   ALL   MATCHED?                 
         BE    NTALMTCH            "NOT ALL MATCHED"                            
*                                                                               
         B     PAYPSEQ             IGNORE INVOICE DETAIL                        
*                                                                               
*        BUY IS MATCHED                                                         
*                                                                               
PAYP20   TM    PIMDSTAT,X'08'      TEARSHEET PROOF?                             
         BO    PAYP30              YES                                          
*                                                                               
*        MATCHED WITH NO TEARSHEET                                              
*                                                                               
         CLI   SVPROF+8,C'Y'       PAY ONLY IF   ALL   MATCHED W/ TS?           
         BE    NTALMTTS            "NOT ALL MATCHED WITH TEARSHEET"             
*                                                                               
PAYP30   OI    BITFLAG,X'02'       FOUND SOMETHING FOR PAY PROGRAM              
         GOTO1 CALCDTLG,DMCB,GETINSA                                            
*                                                                               
         LA    R3,GETINSA                                                       
         USING PVALUES,R3                                                       
*                                                                               
         L     R1,GROSS                                                         
         A     R1,GSTTAX                                                        
         CVD   R1,DUB                                                           
         AP    GROSSAMT,DUB                                                     
         L     R0,AGYCOM                                                        
         SR    R1,R0                                                            
         CVD   R1,DUB                                                           
         AP    NETAMNT,DUB                                                      
         DROP  R3                                                               
*                                                                               
PAYPSEQ  BAS   RE,MINIOSEQ                                                      
         BE    PAYPLP                                                           
         B     PAYP05                                                           
*                                                                               
PAYP40   CLI   SVPROF+8,C'Y'       PAY ONLY IF  ALL  W/ TS?                     
         BE    *+12                                                             
         CLI   SVPROF+10,C'Y'      PAY ONLY IF  ALL  MATCHED?                   
         BNE   PAYPX                                                            
*                                                                               
         XC    KEY,KEY             SET UP THE BUY KEY TO SAME YEAR AND          
         LA    R3,KEY                  MONTH AS THE INVOICE                     
         USING PBUYKEY,R3                                                       
         MVC   PBUYKAGY,AGENCY                                                  
         MVC   PBUYKMED,QMED                                                    
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,QCLT                                                    
         CLC   QPRD,=C'***'        PRODUCT VARIOUS                              
         BNE   *+14                                                             
         MVC   PBUYKPRD,PIMSPRD                                                 
         B     *+10                                                             
         MVC   PBUYKPRD,QPRD                                                    
         MVC   PBUYKPUB(L'BPUB),BPUB                                            
*                                                                               
         TM    GLOBFLG1,X'80'      ALL ZONES AND EDITIONS?                      
         BZ    *+10                                                             
         XC    PBUYKZON(2),PBUYKZON                                             
*                                                                               
         MVC   PBUYKDAT,INVSTDT    USE INVOICE PERIOD START DATE                
         DROP  R3                                                               
*                                                                               
PAYPHIGH GOTO1 HIGH                                                             
*                                                                               
PAYPLOOP LA    R3,KEY                                                           
         USING PBUYKEY,R3                                                       
*                                                                               
         TM    GLOBFLG1,X'80'      ALL ZONES AND EDITIONS?                      
         BZ    PAYP50              NO                                           
*                                                                               
         CLC   KEY(PBUYKZON-PBUYKEY),KEYSAVE    SAME UPTO ZONE?                 
         BNE   PAYPX                            NO                              
*                                                                               
         CLC   PBUYKZON(2),KEYSAVE+PBUYKZON-PBUYKEY    SAME ZONE/EDTN?          
         BE    PAYP60                                  YES                      
*                                                                               
PAYPSTDT MVC   PBUYKDAT,INVSTDT    NO, SET DATE FOR THIS ZONE/EDTN              
         B     PAYPHIGH                                                         
*                                                                               
PAYP50   CLC   KEY(PBUYKDAT-PBUYKEY),KEYSAVE    SAME BASIC INFORMATION?         
         BNE   PAYPX                                                            
*                                                                               
PAYP60   CLC   PBUYKDAT,INVSTDT    BUY DATE BELOW START DATE?                   
         BNL   PAYP65                                                           
         TM    GLOBFLG1,X'80'      YES, IF PUB,ALL                              
         BZ    PAYPX                                                            
         B     PAYPSTDT            THEN READ FROM THE START DATE                
*                                                                               
PAYP65   CLC   PBUYKDAT,INVENDDT   BUY DATE AFTER INVOICE PERIOD?               
         BNH   PAYP68                                                           
         TM    GLOBFLG1,X'80'      YES, IF WE'RE DOING PUB,ALL                  
         BZ    PAYPX                                                            
         MVI   PBUYKDAT,X'FF'                                                   
         B     PAYPHIGH            THEN FORCE KEY TO READ NXT ZONE/EDTN         
*                                                                               
PAYP68   OC    MYBEST,MYBEST       NO ESTIMATE GIVEN?                           
         BZ    PAYP70              NONE                                         
*                                                                               
         CLC   PBUYKEST,MYBEST     DOES GIVEN EST MATCH EST IN KEY?             
         BNE   PAYPNEXT            NO, GET NEXT BUY RECORD                      
*                                                                               
PAYP70   OC    PBUYKACT,PBUYKACT   IF SOMETHING HERE   (ACTIVE PRODUCT)         
         BNZ   PAYPNEXT            THEN SKIP THIS RECORD  (ASK MEL)             
         DROP  R3                                                               
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
         CLI   PBDBFD,C'T'         IGNORE TEST BUYS                             
         BE    PAYPNEXT                                                         
*                                                                               
         MVI   ELCODE,X'80'        SEE IF THERE IS A SPECIAL REP ELEM           
         BAS   RE,GETEL                                                         
         BE    PAYP80                                                           
*                                                                               
         L     R6,AIO                                                           
         OC    SPCLREP,SPCLREP     DO WE NEED SPECIAL REP?                      
         BZ    PAYP90              NO                                           
         B     PAYPNEXT            YES, GET NEXT BUY                            
*                                                                               
         USING PBSREPEL,R6                                                      
PAYP80   CLC   PBSREP,SPCLREP      SAME SPECIAL REP?                            
         BNE   PAYPNEXT                                                         
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
PAYP90   TM    PBDSTAT,X'40'       MATCHED?                                     
         BO    PAYP100                                                          
*                                                                               
         CLI   SVPROF+8,C'Y'       PAY ONLY IF   ALL   MATCHED W/ TS?           
         BE    NTALMTTS            "NOT ALL MATCHED WITH TEARSHEET"             
         B     NTALMTCH            "NOT ALL MATCHED"                            
*                                                                               
PAYP100  TM    PBDSTAT,X'10'       MATCHED WITH TEARSHEET?                      
         BO    PAYPNEXT                                                         
*                                                                               
         CLI   SVPROF+8,C'Y'       PAY ONLY IF   ALL   MATCHED W/ TS?           
         BE    NTALMTTS            "NOT ALL MATCHED WITH TEARSHEET"             
*                                                                               
PAYPNEXT GOTO1 SEQ                 CHECK NEXT BUY                               
         B     PAYPLOOP                                                         
*                                                                               
PAYPX    MVC   SVPROF,MYPROF                                                    
         B     XIT                                                              
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SWITCHES TO THE PAY PROGRAM OR TO THE MBC PROGRAM.               
*                                                                               
* ON ENTRY:    BITFLAG:X'01'       IF THIS BIT IS ON, SWITCH TO MBC             
*                                                OFF, SWITCH TO PAY             
***********************************************************************         
SWTCHPRG DS    0H                                                               
         NMOD1 0,**SWTC**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
         LA    R4,SYSSPARE                                                      
         LA    R5,MINBLOCK                                                      
*                                                                               
         XC    BLOCK(14),BLOCK                                                  
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'PRI'    FROM THE PRINT SYSTEM                        
         MVC   GLVXFRPR,=C'MAT'    MATCH PROGRAM                                
         MVC   GLVXTOSY,=C'PRI'    TO THE PRINT SYSTEM                          
         MVC   GLVXTOPR,=C'PAY'    PAY PROGRAM                                  
         TM    BITFLAG,X'01'                                                    
         BZ    *+10                                                             
         MVC   GLVXTOPR,=C'MBC'    MBC PROGRAM                                  
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
*                                                                               
******** OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,14,GLVXCTL                           
         CLI   DMCB+8,0                                                         
         BNE   SPRGNOT                                                          
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'PUTF',CHKMEDH,,GLVPRMD                           
         CLI   DMCB+8,0                                                         
         BNE   SPRGNOT                                                          
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'PUTF',CHKCLTH,,GLVPRCLT                          
         CLI   DMCB+8,0                                                         
         BNE   SPRGNOT                                                          
*                                                                               
         TM    BITFLAG,X'01'       MBC?                                         
         BNZ   SPRG00              YES, DON'T PUT PRODUCT/ESTIAMTE              
*                                                                               
         CLI   CHKESTMH+5,0                                                     
         BE    SPRG00                                                           
         XC    BLOCK(256),BLOCK                                                 
         MVC   BLOCK(L'CHKPRD),CHKPRD                                           
         LA    R2,BLOCK+2                                                       
         CLI   0(R2),C' '                                                       
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
         MVI   0(R2),C'/'                                                       
         MVC   1(L'CHKESTM,R2),CHKESTM                                          
         LA    R2,L'CHKESTM+1(R2)                                               
         LA    R1,BLOCK                                                         
         SR    R2,R1                                                            
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,(R2),GLVPRPRD                        
         CLI   DMCB+8,0                                                         
         BNE   SPRGNOT                                                          
         B     SPRG05                                                           
*                                                                               
SPRG00   DS    0H                                                               
         CLC   =C'***',CHKPRD                                                   
         BNE   *+16                                                             
         MVC   CHKPRD,=C'ALL'                                                   
         MVC   QPRD,=C'***'                                                     
         GOTO1 GLOBBER,DMCB,=C'PUTF',CHKPRDH,,GLVPRPRD                          
         MVC   CHKPRD,QPRD                                                      
         CLI   DMCB+8,0                                                         
         BNE   SPRGNOT                                                          
*                                                                               
SPRG05   DS    0H                                                               
         GOTO1 GLOBBER,DMCB,=C'PUTF',CHKPUBH,,GLVPRPUB                          
         CLI   DMCB+8,0                                                         
         BNE   SPRGNOT                                                          
*                                                                               
         TM    BITFLAG,X'01'       MBC?                                         
         BNZ   SPRG20              YES, DO THE ESTIMATE                         
*                                                                               
         CLI   CHKREPX,C'*'        IF NOT A SPECIAL REP                         
         BE    SPRG10                                                           
         MVI   BLOCK,C' '                                                       
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,1,GLVPRPAY  PUT A SPACE              
         CLI   DMCB+8,0                                                         
         BNE   SPRGNOT                                                          
         B     SPRG20                                                           
*                                                                               
SPRG10   MVI   BLOCK,C'S'          SPECIAL REP                                  
         MVC   BLOCK+1(4),CHKREPN                                               
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,5,GLVPRPAY                           
         CLI   DMCB+8,0                                                         
         BNE   SPRGNOT                                                          
*                                                                               
SPRG20   CLI   CHKESTMH+5,0        IF MULTI-ESTIMATE                            
         BNE   SPRG30                                                           
         XC    BLOCK(256),BLOCK                                                 
         TM    BITFLAG,X'01'                                                    
         BZ    *+10                                                             
         MVC   BLOCK(3),=CL3'ALL'                                               
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,3,GLVPREST  PUT OUT NULLS            
         CLI   DMCB+8,0                                                         
         BNE   SPRGNOT                                                          
         B     SPRG40                                                           
*                                                                               
SPRG30   GOTO1 GLOBBER,DMCB,=C'PUTF',CHKESTMH,,GLVPREST                         
         CLI   DMCB+8,0                                                         
         BNE   SPRGNOT                                                          
*                                                                               
SPRG40   GOTO1 DATCON,DMCB,(3,INVSTDT),(0,BLOCK)                                
         GOTO1 DATCON,DMCB,(3,INVENDDT),(0,BLOCK+6)                             
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,12,GLVPRPER                          
         CLI   DMCB+8,0                                                         
         BNE   SPRGNOT                                                          
*                                                                               
SPRG50   TM    BITFLAG,X'01'       MBC?                                         
         BZ    SPRG60              NO                                           
*                                                                               
         MVC   BLOCK(11),=CL11'CD,STATUS=L'   PUT OUT MBC DATA                  
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,11,GLVPRDTA                          
         CLI   DMCB+8,0                                                         
         BNE   SPRGNOT                                                          
*                                                                               
         B     SPRG100             THAT'S ALL FOR MBC                           
*                                                                               
SPRG60   GOTO1 GLOBBER,DMCB,=C'PUTF',CHKINITH,,GLVPRREQ                         
         CLI   DMCB+8,0                                                         
         BNE   SPRGNOT                                                          
*                                                                               
         MVC   KEY,MINMKEY                                                      
         LA    R1,KEY                                                           
         USING PINVKEY,R1                                                       
         MVI   PINVMIEL,PIMHDREQ   PUT HEADER INFORMATION IN MASTER KEY         
         MVC   PINVMIS1,LSTHDRSQ                                                
         XC    PINVMIS2,PINVMIS2                                                
         DROP  R1                                                               
         GOTO1 GLOBBER,DMCB,=C'PUTD',KEY,L'PINVKEY,GLVPRMAT                     
         CLI   DMCB+8,0                                                         
         BNE   SPRGNOT                                                          
*                                                                               
         XC    BLOCK(64),BLOCK                                                  
         EDIT  (P11,GROSSAMT),(12,BLOCK),2                                      
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,12,GLVPRGRS                          
         CLI   DMCB+8,0                                                         
         BNE   SPRGNOT                                                          
*                                                                               
         XC    BLOCK(12),BLOCK                                                  
         EDIT  (P11,NETAMNT),(12,BLOCK),2                                       
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,12,GLVPRNET                          
         CLI   DMCB+8,0                                                         
         BNE   SPRGNOT                                                          
*                                                                               
SPRG100  L     R1,ATIOB                                                         
         MVC   DUB(2),TIOBCURD-TIOBD(R1)                                        
         LA    R2,CONSERVH                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         LR    R1,RA                                                            
         AH    R1,DUB                                                           
         OI    6(R1),X'C0'                                                      
         SR    R0,R0               CLEAR CC                                     
*                                                                               
SPRGX    B     XIT                                                              
*                                                                               
SPRGNOT  MVI   GERROR1,PAYSWTCH    CAN'T SWITCH TO THE PAY PROGRAM              
         TM    BITFLAG,X'01'                                                    
         BZ    *+8                                                              
         MVI   GERROR1,MBCSWTCH    CAN'T SWITCH TO THE MBC PROGRAM              
         LA    R2,CHKOPTNH                                                      
         B     ERREXIT                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DELETES AN INVOICE DETAIL.  MINEKEY MUST BE SET BY               
* THE CALLER.                                                                   
***********************************************************************         
DIDETAIL DS    0H                                                               
         NMOD1 0,**DDTL**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
         LA    R5,MINBLOCK                                                      
*                                                                               
         BAS   RE,MINIORD                                                       
         BNE   INVLSEL            MINIO ELEMENT NOT FOUND                       
*                                                                               
         MVC   MELEM2,MELEM                                                     
*                                                                               
         GOTO1 MINIO,DMCB,('MINDEL',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,MELEM2                                                        
         USING PIMDTLEL,R6                                                      
         TM    PIMDSTAT,X'04'                                                   
         BZ    DIDTLX                                                           
*                                                                               
         MVI   MINEKEY,PIMCOMEQ                                                 
         BAS   RE,MINIORD                                                       
         BNE   INVLSEL                                                          
         GOTO1 MINIO,DMCB,('MINDEL',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DIDTLX   OI    MNIOFLAG,X'80'      REMEMBER TO CLOSE THE FILE                   
*                                                                               
         L     R1,ACHGNTRY                                                      
         XC    0(L'PINVMINI,R1),0(R1)  CLEAR THIS ENTRY                         
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE MATCHES AN INSERTION WITH AN INVOICE DETAIL.                     
***********************************************************************         
MIDETAIL DS    0H                                                               
         NMOD1 0,**MDTL**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)            R2 = A(FLDHDR OF SEL FIELD)                  
*                                                                               
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
         LA    R5,MINBLOCK                                                      
*                                                                               
         L     R3,MINELEM                                                       
         USING PIMDTLEL,R3                                                      
         XC    0(L'MELEM,R3),0(R3)                                              
*                                                                               
         L     R1,ACHGNTRY         IF INVOICE IS CORRECTED                      
         AH    R1,=Y(L'PBUYKEY)                                                 
         OC    0(L'PINVMINI,R1),0(R1)                                           
         BZ    MIDTL10                                                          
         CLI   L'PINVMINI(R1),0    CORRECTED?                                   
         BE    CANTMCOR            CAN'T MATCH CORRECTED BUY                    
*                                                                               
MIDTL10  SH    R1,=Y(L'PBUYKEY)    GET THE BUY RECORD                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'PBUYKEY),0(R1)                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
*                                                                               
         BRAS  RE,TSTLOCK          RECORDS (BUY) LOCKED ?                       
         BNE   RECLOCKD            YES - CAN'T UPDATE                           
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'25'        SEE IF INSERTION IS PAID                     
         BAS   RE,GETEL                                                         
MIDTL12  BNE   MIDTL15                                                          
         USING PPAYELEM,R6                                                      
         OC    PPDDATE,PPDDATE                                                  
         BNZ   INVLPAID            CAN'T MANIPULATE A PAID INSERTION            
         BAS   RE,NEXTEL                                                        
         B     MIDTL12                                                          
*                                                                               
MIDTL15  L     R6,AIO                                                           
         USING PBUYREC,R6          SAVE ZONE, EDITION, DATE, ESTIMATE,          
*                                                                               
         TM    PBDSTAT,X'40'       ALREADY MATCHED?                             
         BNZ   ALRDYMTC            YES                                          
*                                                                               
*                                  SAVE ZONE, EDITION, DATE, ESTIMATE,          
         MVC   PIMBZONE(PIMBLINE-PIMBZONE),PBUYKZON   AND LINE                  
         MVC   PIMBLINE,PBUYKLIN                                                
         MVC   PIMIDATE,PBUYKDAT                                                
         MVC   PIMIEST,PBUYKEST                                                 
         MVC   PIMSPRD,PBUYKPRD                                                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'95'        IF BUY HAS A TEARSHEET ELEMENT               
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         OI    PIMDSTAT,X'08'      THEN TURN ON TEARSHEET BIT IN DETAIL         
         L     R6,AIO                                                           
*                                                                               
         MVI   PIMDTLEL,PIMDTLEQ   MATCHED BELONGS ON TOP ALSO                  
         MVI   PIMDTLLN,PIMDTLLQ                                                
         MVC   PIMSPACE,PBDSPACE                                                
         MVC   PIMUIND,PBDUIND                                                  
         MVC   PIMUNITS,PBDUNITS                                                
         ZAP   PIMCLMS,PBDCLMS                                                  
         CLI   PBDCOSTY,C'U'                                                    
         BNE   *+8                                                              
         OI    PIMDSTAT,X'80'                                                   
         MVC   PIMCSIND,PBDCOSIN                                                
         ZAP   PIMCOST,PBDCOS                                                   
         ZAP   PIMPREM,PBDPRCOS                                                 
         MVC   PIMCLRS,PBDCL                                                    
         OI    PIMDSTAT,X'10'      MATCHED BIT                                  
*                                                                               
         CP    PBDCD,=P'0'         NO CASH DISCOUNT?                            
         BNE   *+8                                                              
         OI    PIMDSTAT,X'40'      NONE                                         
*                                                                               
         CLI   9(R2),C'T'          MATCH WITH TEARSHEET?                        
         BNE   *+8                                                              
         OI    PIMDSTAT,X'08'      YES                                          
*                                                                               
         CLI   PIMDTLS1,0          IF NO HEADER SEQUENCE                        
         BNE   MIDTL40                                                          
MIDTL20  GOTO1 NXDTLSEQ                                                         
         BE    MIDTL30                                                          
         GOTO1 RESEQNCE,DMCB,LOWERTBL,UPPERTBL                                  
         B     MIDTL20                                                          
MIDTL30  MVC   PIMDTLS2,HALF       NEXT DETAIL SEQUENCE NUMBER                  
         MVC   PIMDTLS1,LSTHDRSQ   HEADER SEQUENCE                              
         BAS   RE,MINIOADD                                                      
         BE    MIDTL50                                                          
         DC    H'0'                                                             
*                                                                               
MIDTL40  BAS   RE,MINIOWRT                                                      
*                                                                               
MIDTL50  OI    MNIOFLAG,X'10'      MUST CLOSE MINIO BUFFER                      
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         OI    PBDSTAT,X'40'       THIS BUY HAS BEEN MATCHED                    
         CLI   9(R2),C'T'          MATCH WITH TEARSHEET?                        
         BNE   *+8                                                              
         OI    PBDSTAT,X'10'       YES                                          
*                                                                               
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
MIDTL60  XC    ELEM,ELEM           PUT THE INV MATCH ELEMENT ON BUYREC          
         LA    R2,ELEM                                                          
         USING PBINVELM,R2                                                      
*                                                                               
         MVI   PBINVELM,PBINVELQ                                                
         MVI   PBINVLEN,PBINVELL                                                
*                                                                               
         MVC   PBINVPRD,CHKPRD                                                  
         OC    PBINVPRD,=CL3' '    MAKE SURE NO NULLS                           
         GOTO1 HEXIN,DMCB,CHKPUB,PBINVPUB,6                                     
         MVC   PBINVYR,CHKYEAR                                                  
*                                                                               
         MVC   PBINVNUM,CHKINVN                                                 
         MVC   PBINVEST,CHKESTM                                                 
         GOTO1 DATCON,DMCB,(5,0),(3,PBINVMDT)                                   
         MVC   PBINVPID,SVMATPID                                                
         DROP  R2                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
         GOTO1 PUTREC              NOW WE CAN WRITE OUT BUY RECORD              
*                                                                               
MIDTLX   B     XIT                                                              
         DROP  R3,R6                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PAYS AN INSERTION                                                
***********************************************************************         
PIDETAIL DS    0H                                                               
         NMOD1 0,**PDTL**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)            R2 = A(FLDHDR OF SEL FIELD)                  
*                                                                               
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
         LA    R4,SYSSPARE                                                      
         LA    R5,MINBLOCK                                                      
*                                                                               
         XC    WORK,WORK                                                        
         XC    SVPROF,SVPROF                                                    
         MVC   WORK(4),=C'PA0A'                                                 
         NI    WORK,X'BF'                                                       
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         CLI   CLTOFICE,C' '                                                    
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CLTOFICE                                              
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         CLI   SVPROF,0                                                         
         BE    MISSPROF            NO A0A PROFILE FOUND                         
*                                                                               
         GOTO1 =A(A0AOPT),RR=RELO  SET NEW PAY OPTIONS                          
*                                                                               
         L     R1,ACHGNTRY         IF INVOICE IS CORRECTED                      
         AH    R1,=Y(L'PBUYKEY)                                                 
         OC    0(L'PINVMINI,R1),0(R1)                                           
         BZ    PIDTL10                                                          
         CLI   L'PINVMINI(R1),0    CORRECTED?                                   
         BE    CANTPCOR            CAN'T PAY CORRECTED BUY                      
*                                                                               
PIDTL10  L     R1,ACHGNTRY         GET THE BUY RECORD                           
         XC    KEY,KEY                                                          
         MVC   KEY(L'PBUYKEY),0(R1)                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'25'        SEE IF INSERTION IS PAID                     
         BAS   RE,GETEL                                                         
PIDTL15  BNE   PIDTL20                                                          
         USING PPAYELEM,R6                                                      
         OC    PPDDATE,PPDDATE                                                  
         BNZ   PAYAPAID            CAN'T MATCH A PAID INSERTION                 
         BAS   RE,NEXTEL                                                        
         B     PIDTL15                                                          
*                                                                               
PIDTL20  L     R6,AIO                                                           
         USING PBUYREC,R6          SAVE ZONE, EDITION, DATE, ESTIMATE,          
*                                                                               
PIDTL21  GOTO1 GETINS,DMCB,AIO,GETINSA,PBUYKPRD,INVSTDT,=C'GST'                 
*                                                                               
         LA    R3,GETINSA                                                       
         USING PVALUES,R3                                                       
         L     R1,DMCB+16                                                       
         MVC   GVALUES(GSTTAXBL+L'GSTTAXBL-GVALUES),0(R1)                       
*                                                                               
         L     R1,GROSS                                                         
         CVD   R1,DUB                                                           
         ZAP   GROSSAMT,DUB                                                     
         L     R0,AGYCOM                                                        
         SR    R1,R0                                                            
         CVD   R1,DUB                                                           
         ZAP   NETAMNT,DUB                                                      
*                                                                               
         CLI   SVAGNATL,C'C'       CANADIAN?                                    
         BNE   PIDTL30                                                          
         L     R1,GSTTAX           YES, THEN ADD ANY GST                        
         CVD   R1,DUB                                                           
         AP    GROSSAMT,DUB                                                     
         AP    NETAMNT,DUB                                                      
         DROP  R3                                                               
*                                                                               
PIDTL30  CLI   SVPROF+10,C'N'      DON'T CARE IF MATCHED                        
         BE    PIDTL40                                                          
*                                                                               
         TM    PBDSTAT,X'40'       MATCHED?                                     
         BNO   NTMATCHD            NO, SHOULD BE                                
*                                                                               
         CLI   SVPROF+8,C'T'       TEARSHEET REQUIRED?                          
         BNE   *+12                                                             
         TM    PBDSTAT,X'10'       YES, SEE IF INSERTION HAS TEARSHEET          
         BZ    NOTTORN             DOESN'T HAVE TEARSHEET                       
*                                                                               
         CLI   SVPROF+10,C'Y'      ALL INSERTIONS HAVE TO BE MATCHED            
         BNE   PIDTL40                 WITH NO RUN NOT ORDERED DETAILS?         
*                                                                               
         CLI   CKALREDY,C'Y'       DID WE CHECK TO MAKE SURE ALREADY?           
         BE    PIDTL40             YES                                          
*                                                                               
         BAS   RE,CKEVERY          NO                                           
         MVI   CKALREDY,C'Y'       CHECKED ALREADY                              
*                                                                               
         L     R1,ACHGNTRY         BETTER GET THE BUY RECORD AGAIN              
         XC    KEY,KEY                 BUT AMOUNTS NETAMNT AND GROSSAMT         
         MVC   KEY(L'PBUYKEY),0(R1)    SHOULD STILL BE INTACT                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
PIDTL40  XC    BLOCK(14),BLOCK                                                  
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'PRI'    FROM THE PRINT SYSTEM                        
         MVC   GLVXFRPR,=C'MAT'    MATCH PROGRAM                                
         MVC   GLVXTOSY,=C'PRI'    TO THE PRINT SYSTEM                          
         MVC   GLVXTOPR,=C'PAY'    PAY PROGRAM                                  
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
*                                                                               
******** OI    GLVXFLG1,GLV1GOTO+GLV1SEPS  CALL BASE ON TRANSFER                
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,14,GLVXCTL                           
         CLI   DMCB+8,0                                                         
         BNE   PIDTLNOT                                                         
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'PUTF',CHKMEDH,,GLVPRMD                           
         CLI   DMCB+8,0                                                         
         BNE   PIDTLNOT                                                         
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'PUTF',CHKCLTH,,GLVPRCLT                          
         CLI   DMCB+8,0                                                         
         BNE   PIDTLNOT                                                         
*                                                                               
         XC    BLOCK(256),BLOCK                                                 
         MVC   BLOCK(L'PBUYKPRD),PBUYKPRD                                       
***      MVC   BLOCK(L'CHKPRD),CHKPRD                                           
         LA    R3,BLOCK+2                                                       
         CLI   0(R3),C' '                                                       
         BNH   *+8                                                              
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'/'                                                       
         EDIT  (B2,PBUYKEST),(3,1(R3)),FILL=0                                   
         LA    R3,4(R3)                                                         
         LA    R1,BLOCK                                                         
         SR    R3,R1                                                            
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,(R3),GLVPRPRD                        
         CLI   DMCB+8,0                                                         
         BNE   PIDTLNOT                                                         
*                                                                               
         TM    GLOBFLG1,X'80'      ALL ZONES AND EDITIONS?                      
         BNZ   PIDTL45                                                          
         GOTO1 GLOBBER,DMCB,=C'PUTF',CHKPUBH,,GLVPRPUB                          
         CLI   DMCB+8,0                                                         
         BNE   PIDTLNOT                                                         
         B     PIDTL47                                                          
*                                                                               
         USING PBUYREC,R6                                                       
PIDTL45  XC    BLOCK(20),BLOCK                                                  
         GOTO1 VPUBEDIT,DMCB,(8,PBUYKPUB),(C'S',BLOCK)                          
*                                                                               
         LA    R0,15               DECREASE LENGTH IF TRAILING NULLS            
         LA    RE,BLOCK+14                                                      
PIDTL45A CLI   0(RE),0                                                          
         BNE   PIDTL45B                                                         
         SH    R0,=H'1'                                                         
         BCTR  RE,0                                                             
         B     PIDTL45A                                                         
*                                                                               
PIDTL45B GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,(R0),GLVPRPUB                        
         CLI   DMCB+8,0                                                         
         BNE   PIDTLNOT                                                         
*                                                                               
PIDTL47  CLI   CHKREPX,C'*'        IF NOT A SPECIAL REP                         
         BE    PIDTL50                                                          
         MVI   BLOCK,C' '                                                       
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,1,GLVPRPAY  PUT A SPACE              
         CLI   DMCB+8,0                                                         
         BNE   PIDTLNOT                                                         
         B     PIDTL60                                                          
*                                                                               
PIDTL50  MVI   BLOCK,C'S'          SPECIAL REP                                  
         MVC   BLOCK+1(4),CHKREPN                                               
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,5,GLVPRPAY                           
         CLI   DMCB+8,0                                                         
         BNE   PIDTLNOT                                                         
*                                                                               
PIDTL60  CLI   CHKESTMH+5,0        IF MULTI-ESTIMATE                            
         BNE   PIDTL70                                                          
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,3,GLVPREST  PUT OUT NULLS            
         CLI   DMCB+8,0                                                         
         BNE   PIDTLNOT                                                         
         B     PIDTL80                                                          
*                                                                               
PIDTL70  GOTO1 GLOBBER,DMCB,=C'PUTF',CHKESTMH,,GLVPREST                         
         CLI   DMCB+8,0                                                         
         BNE   PIDTLNOT                                                         
*                                                                               
PIDTL80  GOTO1 DATCON,DMCB,(3,PBUYKDAT),(0,BLOCK)                               
         MVI   BLOCK+6,C'-'        PUT OUT THE BUYLINE                          
         EDIT  (B1,PBUYKLIN),(3,BLOCK+7),FILL=0                                 
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,10,GLVPRPER                          
         CLI   DMCB+8,0                                                         
         BNE   PIDTLNOT                                                         
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'PUTF',CHKINITH,,GLVPRREQ                         
         CLI   DMCB+8,0                                                         
         BNE   PIDTLNOT                                                         
*                                                                               
         MVC   KEY,MINMKEY                                                      
         LA    R1,KEY                                                           
         USING PINVKEY,R1                                                       
         MVI   PINVMIEL,PIMHDREQ   PUT HEADER INFORMATION IN MASTER KEY         
         MVC   PINVMIS1,LSTHDRSQ                                                
         XC    PINVMIS2,PINVMIS2                                                
         DROP  R1                                                               
         GOTO1 GLOBBER,DMCB,=C'PUTD',KEY,L'PINVKEY,GLVPRMAT                     
         CLI   DMCB+8,0                                                         
         BNE   PIDTLNOT                                                         
*                                                                               
         XC    BLOCK(64),BLOCK                                                  
         EDIT  (P11,GROSSAMT),(12,BLOCK),2                                      
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,12,GLVPRGRS                          
         CLI   DMCB+8,0                                                         
         BNE   PIDTLNOT                                                         
*                                                                               
         XC    BLOCK(12),BLOCK                                                  
         EDIT  (P11,NETAMNT),(12,BLOCK),2                                       
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,12,GLVPRNET                          
         CLI   DMCB+8,0                                                         
         BNE   PIDTLNOT                                                         
*                                                                               
         L     R1,ATIOB                                                         
         MVC   DUB(2),TIOBCURD-TIOBD(R1)                                        
         LA    R2,CONSERVH                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         LR    R1,RA                                                            
         AH    R1,DUB                                                           
         OI    6(R1),X'C0'                                                      
         SR    R0,R0               CLEAR CC                                     
*                                                                               
PIDTLX   MVC   SVPROF,MYPROF                                                    
         B     XIT                                                              
*                                                                               
PIDTLNOT MVI   GERROR1,PAYSWTCH    CAN'T SWITCH TO THE PAY PROGRAM              
         LA    R2,CHKOPTNH                                                      
         B     ERREXIT                                                          
*                                                                               
NOTTORN  MVI   GERROR1,NOTEARSH                                                 
         B     ERREXIT                                                          
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS TO MAKE SURE ALL THE INSERTIONS ARE MATCHED AND           
* NO RUN NOT ORDERED DETAILS.                                                   
***********************************************************************         
CKEVERY  NTR1                                                                   
         XC    MINEKEY,MINEKEY     LOOK AT THE INVOICE DETAILS                  
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
*                                                                               
         BAS   RE,MINIOHI                                                       
*                                                                               
CEVERYLP L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
*                                                                               
         CLI   PIMDTLEL,PIMDTLEQ   IF NO MORE DETAILS                           
         BNE   *+14                                                             
         CLC   PIMDTLS1,LSTHDRSQ                                                
         BE    CEVERY10                                                         
*                                                                               
CEVERY05 TM    BITFLAG,X'02'       FOUND ANY FOR PAY PROGRAM?                   
         BNZ   CEVERY40            YES, DONE                                    
*                                                                               
         CLI   SVPROF+8,C'Y'       NEED TEARSHEET?                              
         BE    NOMTCHTS            "NO MATCHED BUYS WITH TEARSHEET"             
         B     NOMTCHBY            NO, "NO MATCHED BUYS"                        
*                                                                               
CEVERY10 TM    PIMDSTAT,X'02'      PAID ALREADY?                                
         BNZ   CEVERYSQ            YES                                          
*                                                                               
         TM    PIMDSTAT,X'10'      MATCHED?                                     
         BNZ   CEVERY20            YES                                          
*                                                                               
         CLI   SVPROF+8,C'Y'       PAY ONLY IF   ALL   MATCHED W/ TS?           
         BE    NTALMTTS            "NOT ALL MATCHED WITH TEARSHEET"             
*                                                                               
         CLI   SVPROF+10,C'Y'      PAY ONLY IF   ALL   MATCHED?                 
         BE    NTALMTCH            "NOT ALL MATCHED"                            
         B     CEVERYSQ                                                         
*                                                                               
CEVERY20 TM    PIMDSTAT,X'08'      TEARSHEET PROOF?                             
         BNZ   CEVERY30            YES                                          
*                                                                               
         CLI   SVPROF+8,C'Y'       PAY ONLY IF   ALL   MATCHED W/ TS?           
         BE    NTALMTTS            "NOT ALL MATCHED WITH TEARSHEET"             
*                                                                               
CEVERY30 OI    BITFLAG,X'02'       FOUND SOMETHING FOR PAY PROGRAM              
*                                                                               
CEVERYSQ BAS   RE,MINIOSEQ                                                      
         BE    CEVERYLP                                                         
         B     CEVERY05                                                         
*                                                                               
CEVERY40 CLI   SVPROF+8,C'T'       PAY ONLY IF  ALL  MATCHED W/ TS?             
         BE    *+12                                                             
         CLI   SVPROF+10,C'Y'      PAY ONLY IF  ALL  MATCHED?                   
         BNE   CEVERYX                                                          
*                                                                               
         XC    KEY,KEY             SET UP THE BUY KEY TO SAME YEAR AND          
         LA    R3,KEY                  MONTH AS THE INVOICE                     
         USING PBUYKEY,R3                                                       
         MVC   PBUYKAGY,AGENCY                                                  
         MVC   PBUYKMED,QMED                                                    
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,QCLT                                                    
         MVC   PBUYKPRD,QPRD                                                    
         CLC   =C'***',CHKPRD      PRODUCT VARIOUS                              
         BNE   *+10                                                             
         MVC   PBUYKPRD,PIMSPRD                                                 
         MVC   PBUYKPUB(L'BPUB),BPUB                                            
*                                                                               
         TM    GLOBFLG1,X'80'      ALL ZONES AND EDITIONS?                      
         BZ    *+10                                                             
         XC    PBUYKZON(2),PBUYKZON                                             
*                                                                               
         MVC   PBUYKDAT,INVSTDT    USE INVOICE PERIOD START DATE                
         DROP  R3                                                               
*                                                                               
CEVERYHI GOTO1 HIGH                                                             
*                                                                               
CEVERYLO LA    R3,KEY                                                           
         USING PBUYKEY,R3                                                       
*                                                                               
         TM    GLOBFLG1,X'80'      ALL ZONES AND EDITIONS?                      
         BZ    CEVERY50            NO                                           
*                                                                               
         CLC   KEY(PBUYKZON-PBUYKEY),KEYSAVE    SAME UPTO ZONE?                 
         BNE   CEVERYX                          NO                              
*                                                                               
         CLC   PBUYKZON(2),KEYSAVE+PBUYKZON-PBUYKEY    SAME ZONE/EDTN?          
         BE    CEVERY60                                YES                      
*                                                                               
CEVERYST MVC   PBUYKDAT,INVSTDT    NO, SET DATE FOR THIS ZONE/EDTN              
         B     CEVERYHI                                                         
*                                                                               
CEVERY50 CLC   KEY(PBUYKDAT-PBUYKEY),KEYSAVE    SAME BASIC INFORMATION?         
         BNE   CEVERYX                                                          
*                                                                               
CEVERY60 CLC   PBUYKDAT,INVSTDT    BUY DATE BELOW START DATE?                   
         BNL   CEVERY65                                                         
         TM    GLOBFLG1,X'80'      YES, IF PUB,ALL                              
         BZ    CEVERYX                                                          
         B     CEVERYST            THEN READ FROM THE START DATE                
*                                                                               
CEVERY65 CLC   PBUYKDAT,INVENDDT   BUY DATE AFTER INVOICE PERIOD?               
         BNH   CEVERY68                                                         
         TM    GLOBFLG1,X'80'      YES, IF WE'RE DOING PUB,ALL                  
         BZ    CEVERYX                                                          
         MVI   PBUYKDAT,X'FF'                                                   
         B     CEVERYHI            THEN FORCE KEY TO READ NXT ZONE/EDTN         
*                                                                               
CEVERY68 OC    MYBEST,MYBEST       NO ESTIMATE GIVEN?                           
         BZ    CEVERY70            NONE                                         
*                                                                               
         CLC   PBUYKEST,MYBEST     DOES GIVEN EST MATCH EST IN KEY?             
         BNE   CEVERYNX            NO, GET NEXT BUY RECORD                      
*                                                                               
CEVERY70 OC    PBUYKACT,PBUYKACT   IF SOMETHING HERE   (ACTIVE PRODUCT)         
         BNZ   CEVERYNX            THEN SKIP THIS RECORD  (ASK MEL)             
         DROP  R3                                                               
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
         CLI   PBDBFD,C'T'         IGNORE TEST BUYS                             
         BE    CEVERYNX                                                         
*                                                                               
         MVI   ELCODE,X'80'        SEE IF THERE IS A SPECIAL REP ELEM           
         BAS   RE,GETEL                                                         
         BE    CEVERY80                                                         
*                                                                               
         L     R6,AIO                                                           
         OC    SPCLREP,SPCLREP     DO WE NEED SPECIAL REP?                      
         BZ    CEVERY90            NO                                           
         B     CEVERYNX            YES, GET NEXT BUY                            
*                                                                               
         USING PBSREPEL,R6                                                      
CEVERY80 CLC   PBSREP,SPCLREP      SAME SPECIAL REP?                            
         BNE   CEVERYNX                                                         
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
CEVERY90 TM    PBDSTAT,X'40'       MATCHED?                                     
         BNZ   CEVERY95                                                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'25'        SEE IF INSERTION PAID THOUGH                 
         BAS   RE,GETEL                                                         
CEVERY91 BNE   CEVERY92                                                         
         USING PPAYELEM,R6                                                      
         OC    PPDDATE,PPDDATE     IT IS                                        
         BNZ   CEVERYNX            SKIP THIS ONE                                
         BAS   RE,NEXTEL                                                        
         B     CEVERY91                                                         
*                                                                               
CEVERY92 CLI   SVPROF+8,C'Y'       PAY ONLY IF   ALL   MATCHED W/ TS?           
         BE    NTALMTTS            "NOT ALL MATCHED WITH TEARSHEET"             
         B     NTALMTCH            "NOT ALL MATCHED"                            
*                                                                               
         USING PBUYREC,R6                                                       
CEVERY95 TM    PBDSTAT,X'10'       MATCHED WITH TEARSHEET?                      
         BNZ   CEVERYNX                                                         
*                                                                               
         CLI   SVPROF+8,C'Y'       PAY ONLY IF   ALL   MATCHED W/ TS?           
         BE    NTALMTTS            "NOT ALL MATCHED WITH TEARSHEET"             
*                                                                               
CEVERYNX GOTO1 SEQ               CHECK NEXT BUY                                 
         B     CEVERYLO                                                         
*                                                                               
CEVERYX  B     XIT                                                              
         LTORG                                                                  
         TITLE 'PPMAT02 - DETERMINE A0A PAY OPTIONS - A0AOPT'                   
*===============================================================*               
*                                                               *               
*        DETERMINE PAY & TEARSHEET OPTIONS +8 THROUGH +10       *               
*        IF +8-+10 ARE ALL NULLS USE OLD OPTIONS IN +4 TO SET   *               
*           THEM                                                *               
*        IF ANY IS  'P' THEN SET FROM OPTIONS IN PUB RECORD     *               
*                                                               *               
*EXIT                                                           *               
*                                                               *               
*===============================================================*               
         SPACE 2                                                                
A0AOPT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    SVPROF+8(3),SVPROF+8 IF ALL NEW OPTIONS ARE NULL                 
         BNZ   A0AOPT10                                                         
*                                  THEN USE SVPROF+4 TO SET THEM                
*                                                                               
         MVC   SVPROF+8(3),=C'NNN' SET NEW OPTIONS TO DEFAULT                   
*                                                                               
         CLI   SVPROF+4,C'T'       IF ALL BUYS MUST HAVE TEARSHEET              
         BE    *+8                                                              
         CLI   SVPROF+4,C'S'                                                    
         BNE   *+8                                                              
         MVI   SVPROF+8,C'Y'          SET  TEARSHEET OPTION                     
*                                                                               
         CLI   SVPROF+4,C'Y'       IF ALL BUYS MUST BE MATCHED                  
         BE    *+8                                                              
         CLI   SVPROF+4,C'S'                                                    
         BNE   *+12                                                             
         MVI   SVPROF+9,C'Y'          SET ALL MATCHED OPT NORMAL PAY            
         MVI   SVPROF+10,C'Y'         SET ALL MATCHED OPT FOR $MAT              
*                                                                               
         CLI   SVPROF+4,C'O'       IF ONLY MATCHED BUYS                         
         BNE   *+12                                                             
         MVI   SVPROF+9,C'O'          SET ONLY MATCHED OPT NORMAL PAY           
         MVI   SVPROF+10,C'O'         SET ALL  MATCHED OPT FOR $MAT             
*                                                                               
         CLI   SVPROF+4,C'I'       IF ONLY BUYS MATCHED TO INVOICE              
         BNE   *+12                                                             
         MVI   SVPROF+9,C'O'          SET ONLY MATCHED OPT NORMAL PAY           
         MVI   SVPROF+10,C'I'         SET ONLY INVOICE OPT FOR $MAT             
*                                                                               
         B     A0AOPTX                                                          
*                                                                               
A0AOPT10 DS    0H                                                               
*                                                                               
         LA    R3,SVREPELM         POINT TO SAVED REPELM                        
         USING PUBREPD,R3          ESTABLISH AS REP ELEMENT                     
*                                                                               
         CLI   PUBREPEL,0          DONE IF NO ELEMENT EXISTS                    
         BE    A0AOPTX                                                          
*                                                                               
         CLI   PUBREPEL+1,53       DONE IF OLD ELEMENT FORMAT                   
         BL    A0AOPTX                                                          
*                                                                               
         CLI   SVPROF+8,C'P'       IF OPTION DEPENDS ON PUB                     
         BNE   *+10                                                             
         MVC   SVPROF+8,PUBPCTL1      REPLACE WITH OPTION IN PUB                
*                                                                               
         CLI   SVPROF+9,C'P'       IF OPTION DEPENDS ON PUB                     
         BNE   *+10                                                             
         MVC   SVPROF+9,PUBPCTL2      REPLACE WITH OPTION IN PUB                
*                                                                               
         CLI   SVPROF+10,C'P'      IF OPTION DEPENDS ON PUB                     
         BNE   *+10                                                             
         MVC   SVPROF+10,PUBPCTL3     REPLACE WITH OPTION IN PUB                
*                                                                               
A0AOPTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3                                                               
*                                                                               
         TITLE 'PPMAT02 - PRINT INVOICE CHECKING OVERLAY'                       
***********************************************************************         
* THIS ROUTINE TEARSHEETS A MATCHED INSERTION.                                  
***********************************************************************         
TIDETAIL DS    0H                                                               
         NMOD1 0,**TDTL**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
         LA    R5,MINBLOCK                                                      
*                                                                               
         L     R1,ACHGNTRY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(L'PBUYKEY),0(R1)                                             
         GOTO1 READ                                                             
*                                                                               
         BRAS  RE,TSTLOCK          RECORDS (BUY) LOCKED ?                       
         BNE   RECLOCKD            YES - CAN'T UPDATE                           
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
         TM    PBDSTAT,X'40'       MATCHED?                                     
         BZ    NTMATCHD            NO                                           
*                                                                               
         TM    PBDSTAT,X'10'       HAS TEARSHEET ALREADY?                       
         BNZ   TORNINSR            YES                                          
*                                                                               
         OI    PBDSTAT,X'10'       SET TEARSHEET BIT ON                         
*                                                                               
         MVI   ELCODE,X'25'        LOOK FOR PAID ELEMENT                        
         BAS   RE,GETEL                                                         
TIDTL0A  BNE   TIDTL00                                                          
         USING PPAYELEM,R6                                                      
         OC    PPDDATE,PPDDATE                                                  
         BNZ   INVLPAID            CAN'T MANIPULATE A PAID INSERTION            
         BAS   RE,NEXTEL                                                        
         B     TIDTL0A                                                          
*                                                                               
TIDTL00  L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         XC    MINEKEY,MINEKEY     READ THE INVOICE DETAIL                      
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
         BAS   RE,MINIOHI                                                       
         BNE   TIDTLNO             DIE IF NO CORRESPONDING DETAIL               
*                                                                               
         L     R3,MINELEM                                                       
         USING PIMDTLEL,R3                                                      
*                                                                               
TIDTLLP  CLC   PIMBZONE(PIMBLINE-PIMBZONE),PBUYKZON                             
         BNE   TIDTLNXT                                                         
         CLC   PIMBLINE,PBUYKLIN                                                
         BE    TIDTLYES                                                         
TIDTLNXT BAS   RE,MINIOSEQ                                                      
         BE    TIDTLLP                                                          
*                                                                               
TIDTLNO  B     INVLSEL                                                          
*NOP*TIDTLNO  DC    H'0'                                                        
*                                                                               
TIDTLYES L     R3,MINELEM                                                       
         USING PIMDTLEL,R3                                                      
         OI    PIMDSTAT,X'08'      SET TEARSHEET ON                             
         BAS   RE,MINIOWRT         WRITE OUT INVOICE ITEM AS TORN               
*                                                                               
         GOTO1 PUTREC              NOW WE CAN CHANGE BUY                        
*                                                                               
TIDTLX   B     XIT                                                              
         DROP  R3,R6                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE UNMATCHES AN INSERTION WITH AN INVOICE DETAIL.                   
***********************************************************************         
UIDETAIL DS    0H                                                               
         NMOD1 0,**UDTL**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
         LA    R5,MINBLOCK                                                      
*                                                                               
         L     R1,ACHGNTRY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(L'PBUYKEY),0(R1)                                             
         GOTO1 READ                                                             
*                                                                               
         BRAS  RE,TSTLOCK          RECORDS (BUY) LOCKED ?                       
         BNE   RECLOCKD            YES - CAN'T UPDATE                           
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         TM    PBDSTAT,X'40'       MATCHED?                                     
         BZ    NTMATCHD            NO                                           
         NI    PBDSTAT,X'FF'-X'40'         UNMATCHED                            
*                                                                               
         MVI   ELCODE,X'25'        LOOK FOR PAID ELEMENT                        
         BAS   RE,GETEL                                                         
UIDTL0A  BNE   UIDTL00                                                          
         USING PPAYELEM,R6                                                      
         OC    PPDDATE,PPDDATE                                                  
         BNZ   INVLPAID            CAN'T MANIPULATE A PAID INSERTION            
         BAS   RE,NEXTEL                                                        
         B     UIDTL0A                                                          
*                                                                               
UIDTL00  L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         XC    MINEKEY,MINEKEY     READ THE INVOICE DETAIL                      
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
         BAS   RE,MINIOHI                                                       
         BNE   UIDTLNO                                                          
*                                                                               
         L     R3,MINELEM                                                       
         USING PIMDTLEL,R3                                                      
*                                                                               
UIDTLLP  CLC   PIMBZONE(PIMBLINE-PIMBZONE),PBUYKZON                             
         BNE   UIDTLNXT                                                         
         CLC   PIMBLINE,PBUYKLIN                                                
         BE    UIDTLYES                                                         
UIDTLNXT BAS   RE,MINIOSEQ                                                      
         BE    UIDTLLP                                                          
*                                                                               
UIDTLNO  B     INVLSEL                                                          
*                                                                               
UIDTLYES L     R3,MINELEM                                                       
         USING PIMDTLEL,R3                                                      
         MVC   PIMIEST,BEST        SET EST TO THE GLOBAL ONE                    
         CLI   SVPROF+3,C'Y'                                                    
         BE    *+10                                                             
         MVC   PIMIEST,PIMBEST                                                  
***      XC    PIMBZONE(PIMSPACE-PIMBZONE),PIMBZONE                             
         XC    PIMBLINE,PIMBLINE                                                
         NI    PIMDSTAT,X'FF'-X'10'                                             
         BAS   RE,MINIOWRT         WRITE OUT INVOICE ITEM AS UNMATCHED          
*                                                                               
         OI    MNIOFLAG,X'10'      MUST CLOSE MINIO BUFFER                      
*                                                                               
         OI    LBITFLAG,X'40'                                                   
         CLI   LSTMINSQ,PIMDTLEQ                                                
         BE    UIDTL10                                                          
         MVI   LSTMINSQ,PIMDTLEQ                                                
         MVC   LSTMINSQ+1(L'SEQUENCE),PIMDTLS1                                  
         B     UIDTLPUT                                                         
*                                                                               
UIDTL10  CLC   LSTMINSQ+1(L'SEQUENCE),PIMDTLS1                                  
         BL    *+10                                                             
         MVC   LSTMINSQ+1(L'SEQUENCE),PIMDTLS1                                  
*                                                                               
UIDTLPUT DS    0H                                                               
         DROP  R3                                                               
         SR    R3,R3                                                            
         LR    R3,R6                                                            
         MVI   ELCODE,X'50'        PBINVELM                                     
         BAS   RE,GETEL                                                         
         BNE   UIDTLPT1            RECUP DEL THE MATCH ELEM FROM BUY            
         GOTO1 VRECUP,DMCB,(1,(R3)),(R6),(R6)                                   
*                                                                               
UIDTLPT1 GOTO1 PUTREC              NOW WE CAN CHANGE BUY                        
*                                      NO DUPLICATE INVOICE DETAILS             
UIDTLX   B     XIT                                                              
         DROP  R6                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE COST                                               
***********************************************************************         
VALGROSS DS    0H                                                               
         NMOD1 0,**VGRS**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
         LA    R5,MINBLOCK                                                      
*                                                                               
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2GRSH                                                      
         DROP  R2                                                               
*                                                                               
         CLI   5(R3),0             ANY GROSS AMOUNT?                            
         BNE   VALGS10                                                          
*                                                                               
         CLI   ACTNUM,ACTCHECK     NO, ACTION CHECK?                            
         BNE   *+10                                                             
VALGSERR LR    R2,R3               YES, MISSING IF AMOUNT                       
         B     MISSFLD                                                          
*                                                                               
*                                  ACTION UPDATE CAN COPY DOWN                  
         LA    R1,NEWLOWTB         NOTHING TO COPY DOWN IF NONE BEFORE          
         LA    R1,0(R1)                                                         
         C     R1,ACHGNTRY                                                      
         BNL   VALGSERR            MISSING AMOUNT                               
*                                                                               
         L     R1,ACHGNTRY         SEE IF THERE WAS A DETAIL BEFORE             
*                                                                               
         OC    0(L'PINVMINI,R1),0(R1)  DETAIL CHANGE?                           
         BNZ   VALGSERR            YES, NEED AMOUNT                             
*                                                                               
         SH    R1,=Y(L'PINVMINI)                                                
         OC    0(L'PINVMINI,R1),0(R1)                                           
         BZ    VALGSERR            NONE, MISSING AMOUNT                         
*                                                                               
         MVC   MELEM2,MELEM                                                     
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(L'PINVMINI),0(R1)                                        
         BAS   RE,MINIORD                                                       
         BNE   INVLSEL                                                          
         L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
         ZAP   VALCOST,PIMCOST                                                  
         MVC   VALCOSIN,PIMCSIND                                                
         MVI   VALCOSTY,C'T'       TOTAL COST                                   
         MVC   MELEM,MELEM2                                                     
         B     VALGSX                                                           
*                                                                               
VALGS10  MVI   VALCOSTY,0                                                       
*                                                                               
         ZIC   R0,5(R3)                                                         
         LA    R3,8(R3)                                                         
*                                                                               
         USING SCRLIN2D,R2                                                      
         TM    SLN2GRSH+4,X'04'    VALID ALPHA?                                 
         BNZ   VALGS20                                                          
         DROP  R2                                                               
*                                                                               
         TM    0(R3),X'F0'         SEE IF INPUT IS NUMERIC FIRST                
         BO    VALGS20             IT IS                                        
*                                                                               
         CLI   0(R3),C'-'          NEGATIVE AMOUNT?                             
         BE    VALGS20                                                          
*                                                                               
         MVI   VALCOSIN,C' '                                                    
         CLI   0(R3),C'S'          NET AND GROSS SAME?                          
         BNE   *+12                                                             
         MVI   VALCOSIN,C'S'                                                    
         B     *+10                DON'T COPY THE LETTER TO COST TYPE           
         MVC   VALCOSTY,0(R3)      IT'S NOT, COPY THE COST TYPE                 
         SH    R0,=H'1'            DECREMENT LENGTH OF INPUT                    
         LA    R3,1(R3)                                                         
*                                                                               
VALGS20  GOTO1 CASHVAL,DMCB,(X'82',(R3)),(R0)                                   
*                                                                               
         USING SCRLIN2D,R2                                                      
         LA    R3,SLN2GRSH                                                      
         DROP  R2                                                               
*                                                                               
         CLI   DMCB,X'FF'                                                       
         BNE   *+10                                                             
         LR    R2,R3                                                            
         B     INVLAMT                                                          
*                                                                               
         ZAP   VALCOST,DMCB+4(8)                                                
         CLI   VALCOSTY,0                                                       
         BNE   VALGSX                                                           
         MVI   VALCOSTY,C'T'       TOTAL COST                                   
*                                                                               
         CLI   MYGRSNET,C'N'       NET DOLLARS?                                 
         BE    *+12                                                             
         CLI   VALCTYP,C'N'        FLAG IF NET DOLLARS                          
         BNE   VALGSX                                                           
         CLI   VALCOSIN,C'S'                                                    
         BE    VALGSX                                                           
         OI    PIMDSTAT,X'20'                                                   
         ZAP   NETAMNT,VALCOST     PUT IN THE NET RATE                          
         ZAP   PERCENTG,MYPUBAC        AND THE AGENCY COMMISSION                
         BAS   RE,NETTOGRS         CHECK NET COST TO GROSS COST                 
         ZAP   VALCOST,GROSSAMT    COPY THE GROSS RATE                          
*                                                                               
VALGSX   B     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE MARKS PAID THOSE DETAILS WHOSE CORRESPONDING MATCHED             
* ARE PAID                                                                      
***********************************************************************         
MARKPAID DS    0H                                                               
         NMOD1 0,**MRKP**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         LA    R5,MINBLOCK                                                      
         LA    R4,SYSSPARE                                                      
*                                                                               
         MVC   MELEM2,MELEM        SAVE INVOICE HEADER                          
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
*                                                                               
         BAS   RE,MINIOHI                                                       
         BNE   MRKPX                                                            
*                                                                               
MRKPLOOP L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
*                                                                               
         CLI   PIMDTLEL,PIMDTLEQ                                                
         BNE   MRKPX                                                            
         CLC   PIMDTLS1,LSTHDRSQ                                                
         BNE   MRKPX                                                            
*                                                                               
         TM    PIMDSTAT,X'02'      PAID ALREADY?                                
         BNZ   MRKPNXT                                                          
         TM    PIMDSTAT,X'10'      NO, IS DETAIL MATCHED?                       
         BZ    MRKPNXT                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING PBUYKEY,R3                                                       
         MVC   PBUYKAGY,AGENCY                                                  
         MVC   PBUYKMED,QMED                                                    
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,QCLT                                                    
         MVC   PBUYKPRD,QPRD                                                    
         CLC   =C'***',CHKPRD                                                   
         BNE   *+10                                                             
         MVC   PBUYKPRD,PIMSPRD                                                 
         MVC   PBUYKPUB(L'BPUB),BPUB                                            
         MVC   PBUYKZON(2),PIMBZONE                                             
         MVC   PBUYKDAT,PIMBDATE                                                
         MVC   PBUYKEST,PIMBEST                                                 
         MVC   PBUYKLIN,PIMBLINE                                                
         DROP  R3,R6                                                            
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'PBUYKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         CLI   PBDBFD,C'T'         IGNORE TEST BUYS                             
         BE    MRKPNXT                                                          
*                                                                               
         MVI   ELCODE,X'25'        SEE IF THIS INSERTION'S PAID                 
         BAS   RE,GETEL                                                         
MRKP00   BNE   MRKPNXT                                                          
         USING PPAYELEM,R6                                                      
         OC    PPDDATE,PPDDATE                                                  
         BNZ   MRKP05                                                           
         BAS   RE,NEXTEL                                                        
         B     MRKP00                                                           
***      BZ    MRKPNXT                                                          
         DROP  R6                                                               
*                                                                               
MRKP05   L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
         OI    PIMDSTAT,X'02'                                                   
         BAS   RE,MINIOWRT                                                      
         DROP  R6                                                               
*                                                                               
         LA    R1,UPPERTBL                                                      
         LA    R2,1                LINE NUMBER COUNTER                          
         L     R6,AIO              WE SHOULD MARK THE SCREEN ALSO               
         USING PBUYREC,R6                                                       
MRKP10   CLC   PBUYKEY,0(R1)       THIS LINE'S BUY KEY THE SAME?                
         BNE   MRKP50              NO, CHECK NEXT LINE'S BUY KEY                
*                                                                               
         BCTR  R2,0                R2 = LINE DISP TO FIRST LINE                 
         CLI   SCRTYPE,C'N'                                                     
         BNE   *+12                                                             
         MH    R2,=Y(SLN1LEN)                                                   
         B     *+8                                                              
         MH    R2,=Y(SLN2LEN)      R2 = # OF BYTES FROM FIRST LINE              
*                                                                               
         CLI   ACTNUM,ACTCHECK                                                  
         BNE   MRKP30                                                           
*                                                                               
         CLI   SCRTYPE,C'N'                                                     
         BNE   MRKP20                                                           
         LA    R2,CK1SEL1H(R2)                                                  
         USING SCRLIN1D,R2                                                      
MRKP15   CLC   =C'***',QPRD                                                     
         BNE   *+16                                                             
         MVI   SLN1NET+2,C'P'                                                   
         OI    SLN1NETH+6,X'80'                                                 
         B     *+12                                                             
         MVI   SLN1CTP+2,C'P'      SHOW THE C'P'                                
         OI    SLN1CTPH+6,X'80'                                                 
         B     MRKPNXT                                                          
*                                                                               
MRKP20   LA    R2,CK2SEL1H(R2)                                                  
         USING SCRLIN2D,R2                                                      
MRKP25   CLC   =C'***',QPRD                                                     
         BNE   *+16                                                             
         MVI   SLN2NET+2,C'P'                                                   
         OI    SLN2NETH+6,X'80'                                                 
         B     *+12                                                             
         MVI   SLN2CTP+2,C'P'      SHOW THE C'P'                                
         OI    SLN2CTPH+6,X'80'                                                 
         B     MRKPNXT                                                          
         DROP  R2                                                               
*                                                                               
MRKP30   CLI   SCRTYPE,C'N'                                                     
         BNE   MRKP40                                                           
         LA    R2,SC1SEL1H(R2)                                                  
         B     MRKP15                                                           
*                                                                               
MRKP40   LA    R2,SC2SEL1H(R2)                                                  
         B     MRKP25                                                           
*                                                                               
MRKP50   LA    R1,L'PBUYKEY(R1)                                                 
         LA    R2,1(R2)                                                         
         CH    R2,=H'12'           NO MORE LINES?                               
         BNH   MRKP10              MORE LINES STILL                             
         DROP  R6                                                               
*                                                                               
MRKPNXT  BAS   RE,MINIOSEQ                                                      
         BE    MRKPLOOP                                                         
*                                                                               
MRKPX    MVC   MELEM,MELEM2                                                     
         B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         DROP  R5                  DON'T NEED MINIO BLOCK FROM HERE ON          
*                                                                               
***********************************************************************         
* THIS ROUTINE VALIDATES THE SIZE OR SPACE  (FROM PPBUY12 AND MODIFIED)         
*                                                                               
* ON ENTRY:    (R2)                A(CHANGED LINE)                              
*                                                                               
* ON EXIT:     *                                                                
***********************************************************************         
VSIZE    DS    0H                                                               
         NMOD1 0,**VSIZ**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         DROP  RA                                                               
*                                                                               
         MVI   RATIND,0                                                         
         MVI   VALSPCE,C'N'                                                     
*                                                                               
         USING SCRLIN1D,R2                                                      
         LA    R5,SLN1SIZH                                                      
         CLI   SCRTYPE,C'N'                                                     
         BE    *+8                                                              
         USING SCRLIN2D,R2                                                      
         LA    R5,SLN2SPCH                                                      
         DROP  R2                                                               
*                                                                               
         CLI   5(R5),0             WE NEED THE SIZE                             
         BNE   *+10                                                             
         LR    R2,R5                                                            
         B     MISSFLD                                                          
*                                                                               
         CLI   SCRTYPE,C'N'        IF MAGAZINE                                  
         BE    *+14                                                             
         MVC   VALSPACE,8(R5)      THEN COPY SPACE DESCRIPTION                  
         B     VSIZX               AND EXIT                                     
*                                                                               
         LA    R3,8(R5)            R3 = A(DATA)                                 
         ZIC   R6,5(R5)            R6 = L(DATA)                                 
*                                                                               
         CLI   0(R3),C'*'          TEST NO INSERTION IND                        
         BNE   VSIZ10                                                           
         MVI   VALSPACE,C'*'       SET NO INSERTION IND                         
         LA    R3,1(R3)            BUMP POINTER                                 
         BCTR  R6,0                AND LENGTH                                   
         LTR   R6,R6                                                            
         BZ    VSIZ80                                                           
*                                                                               
VSIZ10   ST    R3,DUB              SAVE PTR/LEN                                 
         ST    R6,DUB+4                                                         
*                                                                               
VSIZ20   CLI   0(R3),C'0'          VALIDATE NUMERIC                             
         BL    VSIZ30                                                           
         CLI   0(R3),C'9'                                                       
         BNH   *+10                                                             
         LR    R2,R5                                                            
         B     INVLNUM                                                          
         LA    R3,1(R3)            GET AS MUCH OF THE NUMBER AS WE CAN          
         BCT   R6,VSIZ20                                                        
*                                                                               
VSIZ30   L     RE,DUB              RE = A(START OF NUMERIC DATA)                
         SR    R3,RE               R3 = LENGTH BEFORE NON-NUMERIC CHAR          
         BNP   VSIZ90                                                           
         BCTR  R3,0                                                             
         EX    R3,PACKLN                                                        
         CP    WORK(5),=P'99999'   MAX LINES OR INCHES                          
         BNH   *+10                THAT CAN BE CARRIED IN VALUNITS              
         LR    R2,R5                                                            
         B     BIGSIZE                                                          
         ZAP   VALUNITS,WORK(5)                                                 
*                                                                               
         MVI   VALUIND,C'L'        ASSUME LINES FIRST                           
*                                                                               
         LTR   R6,R6               NO MORE INPUT                                
         BZ    VSIZ80                                                           
*                                                                               
         LA    R3,1(RE,R3)         R3 = A(BYTE PAST NUMBER)                     
*                                                                               
         CLI   0(R3),C'L'          LINES                                        
         BE    VSIZ40                                                           
         CLI   0(R3),C'/'                                                       
         BE    VSIZ50                                                           
*                                                                               
         MVI   VALUIND,C'I'        ASSUME INCHES IF NOT LINES                   
         CLI   0(R3),C'I'                                                       
         BE    VSIZ40                                                           
*                                                                               
         CLI   0(R3),C'X'          NEW SAU NNXNN.NN?                            
         BNE   VSIZ36                  COLUMNS X INCHES (2 DECIMALS)            
*                                                                               
         CP    VALUNITS,=P'13'     YES, MAX COLS =13                            
         BH    VSIZ90              TREAT AS SPACE                               
         CP    VALUNITS,=P'0'                                                   
         BNH   VSIZ90                                                           
         LA    R3,1(R3)                                                         
         BCT   R6,*+8              NO MORE INPUT                                
         B     VSIZ90              NO MUST BE SPACE                             
*                                                                               
         CLC   0(2,R3),=C'FD'      FULL DEPTH?                                  
         BNE   VSIZ34                                                           
         CLI   2(R3),C' '                                                       
         BNL   VSIZ90              INPUT AFTER FD                               
*                                                                               
         L     R6,AIO                                                           
         GOTO1 VALIPUB                                                          
*         L     R6,APUBIO                                                       
*         CLI   0(R6),0             SEE IF PUB THERE                            
*         BNE   VSIZ33                                                          
*         XC    KEY,KEY                                                         
*         MVC   KEY+27(4),SVPUBDA                                               
**                                  MUST REREAD PUB                             
*         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PUBFILE',        X         
*               KEY+27,APUBIO,(TERMNAL,DMWORK)                                  
*         MVC   BYTE,DMCB+8                                                     
*         NC    BYTE,DMOUTBTS                                                   
*         BZ    *+6                                                             
*         DC    H'0'                MUST FIND PUB                               
*                                                                               
VSIZ33   MVC   DATADISP,=H'33'     DISPLACEMENT FOR PUB ALSO                    
         MVI   ELCODE,X'20'        LOOK FOR GENERAL PUB INFO ELEMENT            
         BAS   RE,GETEL                                                         
         BNE   VSIZ90              CAN'T FIND IT                                
*                                                                               
         USING PUBGENEL,R6                                                      
         OC    PUBFD,PUBFD         SEE IF I HAVE FULL DEPTH                     
         BZ    VSIZ90              NO - TREAT AS SPACE                          
         ZAP   VALCLMS,VALUNITS                                                 
         ZAP   DUB,VALUNITS                                                     
         MP    DUB,PUBFD                                                        
         CP    DUB,=P'99999'       MAX IS 999.99 COL INCHES                     
         BNH   *+10                                                             
         LR    R2,R5                                                            
         B     BIGSIZE                                                          
         ZAP   VALUNITS,DUB                                                     
         NI    VALUIND,X'BF'       MAKE I LOWER CASE                            
         B     VSIZ95                                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
VSIZ34   LR    R1,R3                                                            
         AR    R1,R6                                                            
         BCTR  R1,0                                                             
         CLI   0(R1),C'L'          WILL BE L FOR NNXNNNL FORMAT                 
         BNE   VSIZ35                                                           
         CH    R6,=H'2'            MUST BE AT LEAST NL                          
         BL    VSIZ90                                                           
         BCTR  R6,0                                                             
         GOTO1 CASHVAL,DMCB,(2,0(R3)),(R6)                                      
         CLI   DMCB,X'FF'                                                       
         BE    VSIZ90                                                           
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    VSIZ90                                                           
         ZAP   VALCLMS,VALUNITS                                                 
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'0'           CAN'T BE NEGATIVE                            
         BNH   VSIZ90                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'      MUST GET REMAINDER ZERO                      
         BNE   VSIZ90                                                           
         ZAP   DUB,DUB(6)                                                       
         MP    DUB,VALCLMS                                                      
         CP    DUB,=P'99999'       MAX LINES                                    
         BH    VSIZ90                                                           
         MVI   VALUIND,C'L'        RESET TO LINES - L                           
         ZAP   VALUNITS,DUB                                                     
         MVI   RATIND,1            RATE INDICATOR?                              
         B     VSIZ95              SAVES SPACE                                  
*                                                                               
VSIZ35   GOTO1 CASHVAL,DMCB,(2,0(R3)),(R6)                                      
         CLI   DMCB,X'FF'                                                       
         BE    VSIZ90                                                           
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    VSIZ90                                                           
         ZAP   VALCLMS,VALUNITS                                                 
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'0'           CAN'T BE NEGATIVE                            
         BNH   VSIZ90                                                           
         MP    DUB,VALCLMS                                                      
         CP    DUB,=P'99999'       MAX COLUMN INCHES                            
         BH    VSIZ90                                                           
         NI    VALUIND,X'BF'       MAKE LOWER CASE I                            
         ZAP   VALUNITS,DUB                                                     
         B     VSIZ95              SAVES SPACE                                  
*                                                                               
VSIZ36   DS    0H                  CHECK FOR DECIMAL POINT                      
         CLI   0(R3),C'.'                                                       
         BNE   VSIZ90              TREAT AS SPACE                               
         BCT   R6,*+8                                                           
         B     VSIZ90              NO MORE INPUT                                
         LA    R3,1(R3)                                                         
         CLI   0(R3),C'I'                                                       
         BNE   VSIZ37                                                           
         B     VSIZ40              CHK FOR NN.I/XX                              
*                                                                               
VSIZ37   CLI   0(R3),C'I'                                                       
         BE    VSIZ38                                                           
         CLI   0(R3),C'0'                                                       
         BL    VSIZ90                                                           
         CLI   0(R3),C'9'                                                       
         BH    VSIZ90                                                           
         LA    R3,1(R3)                                                         
         BCT   R6,VSIZ37                                                        
         B     VSIZ90              IF DOESN'T END WITH I ASSUME SPACE           
*                                                                               
VSIZ38   DS    0H                  MUST DECREMENT R6                            
         LR    RA,R3                                                            
         L     RE,DUB              START OF WHOLE NUMBER                        
         SR    RA,RE               LENGTH OF WHOLE NUMBER                       
         XC    WORK(13),WORK       BUILD LINE FOR CASHVAL                       
*                                                                               
         BCTR  RA,0                SO I WON'T MOVE THE I                        
         EX    RA,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)                                                    
         LA    RA,1(RA)            RESTORE FROM THE EX                          
         GOTO1 CASHVAL,DMCB,(2,WORK),(RA)                                       
         CLI   DMCB,X'FF'                                                       
         BE    VSIZ90              IF ERROR TREAT AS SPACE                      
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'99999'                                                    
         BNH   *+10                CAN'T FIT IN VALUNITS                        
         LR    R2,R5                                                            
         B     BIGSIZE                                                          
         ZAP   VALUNITS,DUB                                                     
         NI    VALUIND,X'BF'       MAKE I LOWER CASE 'I'                        
         BCT   R6,VSIZ45                                                        
         MVC   VALSPACE(8),8(R5)     NO MORE INPUT AFTER I                      
         MVI   VALSPCE,C'Y'                                                     
         B     VSIZ80             NO MORE INPUT                                 
*                                  INCHES TO 2 DECIMALS                         
VSIZ40   DS    0H                                                               
         BCT   R6,*+8              NO MORE INPUT AFTER L OR I                   
         B     VSIZ80                                                           
VSIZ45   CLI   1(R3),C'/'          '/'  MUST BE NEXT                            
         BNE   VSIZ90                                                           
         LA    R3,1(R3)                                                         
*                                                                               
VSIZ50   DS    0H                                                               
         LA    R3,1(R3)                                                         
*                                                                               
VSIZ60   DS    0H                                                               
         BCT   R6,*+8              NO MORE INPUT                                
         B     VSIZ90                                                           
         ST    R3,DUB              START OF NUMBER                              
VSIZ70   CLI   0(R3),C'0'                                                       
         BL    VSIZ90                                                           
         CLI   0(R3),C'9'                                                       
         BH    VSIZ90                                                           
         LA    R3,1(R3)                                                         
         BCT   R6,VSIZ70                                                        
         L     RE,DUB              START OF NUMBER                              
         SR    R3,RE               R3 = LENGTH                                  
         BCTR  R3,R0                                                            
         EX    R3,PACKLN                                                        
         CP    WORK(5),=P'999'     MAX COLUMNS IS 999                           
         BNH   *+10                CAN'T FIT IN VALCLMS                         
         LR    R2,R5                                                            
         B     BIGSIZE                                                          
         ZAP   VALCLMS,WORK(5)                                                  
         BNZ   *+10                                                             
         LR    R2,R5                                                            
         B     BIGSIZE                                                          
         ZAP   DUB,VALUNITS                                                     
         BNZ   *+10                                                             
         LR    R2,R5                                                            
         B     BIGSIZE                                                          
         MVC   VALSPACE(8),8(R5)                                                
         MVI   VALSPCE,C'Y'                                                     
         CLI   VALUIND,C'I'       INCHES DOESN'T HAVE TO DIVIDE                 
         BE    VSIZ80             EVENLY                                        
*                                                                               
         DP    DUB,VALCLMS                                                      
         CP    DUB+8-L'VALCLMS(L'VALCLMS),=P'0'                                 
         BE    *+10                                                             
         LR    R2,R5                                                            
         B     INVLFLD                                                          
*                                                                               
VSIZ80   DS    0H                                                               
         B     VSIZ88                                                           
*                                                                               
VSIZ88   CLI   VALSPCE,C'Y'        SEE IF SPACE ONLY TEMPORARY                  
         BNE   VSIZX                                                            
         XC    VALSPACE(8),VALSPACE                                             
         B     VSIZX                                                            
         SPACE 3                                                                
VSIZ90   DS    0H                                                               
*                                  TREAT AS SPACE DESC                          
         CLI   0(R5),20            ERROR IF EDITING CLE                         
         BNH   *+10                                                             
         LR    R2,R5                                                            
         B     BIGSIZE                                                          
         CLI   5(R5),8             MAX LEN                                      
         BNH   *+10                                                             
         LR    R2,R5                                                            
         B     BIGSIZE                                                          
         MVI   VALUIND,0           SPACES SHOULDN'T HAVE                        
         ZAP   VALUNITS,=P'0'      THESE FIELDS SET                             
         ZAP   VALCLMS,=P'0'                                                    
*                                                                               
VSIZ95   DS    0H                                                               
         OI    4(R5),X'20'                                                      
         MVC   VALSPACE(8),8(R5)                                                
         B     VSIZ80                                                           
*                                                                               
PACKLN   PACK  WORK(5),0(0,RE)                                                  
*                                                                               
VSIZX    B     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES NEWSPAPER RATES (FROM PPBUY12 AND MODIFIED)            
*                                                                               
* ON ENTRY:    (R2)                A(CHANGED LINE)                              
*                                                                               
* ON EXIT:     *                                                                
***********************************************************************         
VRATE    DS    0H                                                               
         NMOD1 0,**VRTE**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         USING SCRLIN1D,R2                                                      
*                                                                               
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         XC    VALRCODE,VALRCODE   CLEAR RATE CODE                              
         MVI   BYTE,5              PRESET FOR 5 DEC.                            
         MVI   VALCOSIN,C' '                                                    
         MVI   VALCOSTY,C'U'       PRESET RATE TYPE                             
         CLI   VALUIND,X'89'       LOWER CASE I                                 
         BE    VRTE20              SKIP SPACE CHECK                             
*                                                                               
         CLI   RATIND,1            MAY ALSO HAVE SPACE                          
         BE    VRTE20                                                           
*                                                                               
         CLC   VALSPACE(2),=C'* '  UNLESS SPACE BUY                             
         BNH   VRTE20                                                           
         MVI   VALCOSTY,C'T'                                                    
         MVI   BYTE,2                                                           
VRTE20   MVI   VALCOSIN,C' '                                                    
         MVI   VALCTYP,0                                                        
*                                                                               
         CLI   SLN1RTEH+5,0                                                     
         BE    VRTEX                                                            
*                                                                               
         ZIC   R3,SLN1RTEH+5                                                    
         LA    R6,SLN1RTE                                                       
         TM    SLN1RTEH+4,X'04'    TEST VALID ALPHA                             
         BNZ   VRTE70              (FREE, ETC)                                  
*                                                                               
VRTE30   DS    0H                                                               
         CLI   0(R6),C'S'          GROSS=NET                                    
         BNE   VRTE35                                                           
         ZAP   VALACP,=P'1'         TO PREVENT AC LOOK-UP                       
*                                    IS RESET TO P'0'                           
         MVI   VALCOSIN,C'S'                                                    
         LA    R6,1(R6)                                                         
         BCTR  R3,R0                                                            
**NEW 6/1/88                                                                    
VRTE35   DS    0H                                                               
**COM                                                                           
         CLC   =C'SJ',AGENCY                                                    
         BNE   VRTE40                                                           
**COM                         SKIP COMMISSION RATE LOGIC FOR NOW                
**COM                                                                           
         CLI   0(R6),C'C'          COMMISSION RATE                              
         BNE   VRTE40                                                           
         CLI   VALCOSIN,C' '                                                    
         BE    *+12                                                             
         LA    R2,SLN1RTEH                                                      
         B     INVLFLD             CAN'T ALREADY HAVE                           
         MVI   VALCOSIN,C'C'       GETINS SETS GROSS TO AGYCOM                  
         LA    R6,1(R6)                                                         
         BCTR  R3,R0                                                            
*                                                                               
VRTE40   DS    0H                                                               
         CLI   0(R6),C'N'          NET TO BE GROSSED UP                         
         BNE   VRTE50                                                           
         MVI   VALCTYP,C'N'                                                     
         LA    R6,1(R6)                                                         
         BCTR  R3,R0                                                            
         B     VRTE30                                                           
*                                                                               
VRTE50   DS    0H                                                               
         CLI   0(R6),C'T'          TOTAL RATE                                   
         BNE   VRTE60                                                           
         MVI   VALCOSTY,C'T'                                                    
         MVI   BYTE,2              DECIMALS                                     
         LA    R6,1(R6)                                                         
         BCTR  R3,R0                                                            
         B     VRTE30                                                           
*                                                                               
VRTE60   DS    0H                                                               
         CLI   0(R6),C'*'          FROZEN RATE                                  
         BNE   VRTE65                                                           
         OI    VALRLIND,X'08'                                                   
         LA    R6,1(R6)                                                         
         BCTR  R3,R0                                                            
         B     VRTE30                                                           
*                                                                               
VRTE65   CLC   =C'R=',0(R6)             RATE CODE                               
         BNE   VRTE70                                                           
         LA    R6,2(R6)                                                         
         BCTR  R3,R0                                                            
         BCTR  R3,R0                                                            
         CLI   0(R6),C' '                                                       
         BH    *+12                                                             
         LA    R2,SLN1RTEH                                                      
         B     INVLFLD                                                          
         CH    R3,=H'3'                 MAX 3 CHARS                             
         BNH   *+12                                                             
         LA    R2,SLN1RTEH                                                      
         B     INVLFLD                                                          
         MVC   VALRCODE,0(R6)           SAVE CODE IN PBUYREC                    
         OC    VALRCODE,=3C' '                                                  
*                                       NEW RATE LOOK WILL FIND RATE            
         B     VRTEX                      RETURN                                
*                                                                               
VRTE70   DS    0H                                                               
**NEW 3/30/89                                                                   
         CLI   SVESPROF+28,C'C'          SEE IF 'C' RATE EST                    
         BNE   VRTE80                                                           
         CLI   VALCOSIN,C'C'              'C' INPUT                             
         BE    VRTE90                                                           
         CLI   VALCOSIN,C' '            NOTHING INPUT  - SET TO 'C'             
         BE    *+12                                                             
         LA    R2,SLN1RTEH                                                      
         B     INVLFLD                                                          
         MVI   VALCOSIN,C'C'                                                    
         B     VRTE90                                                           
*                                                                               
VRTE80   CLI   VALCOSIN,C'C'         'C' RATE ON NON 'C' RATE EST               
         BNE   *+12                                                             
         LA    R2,SLN1RTEH                                                      
         B     INVLFLD                                                          
*                                                                               
VRTE90   DS    0H                                                               
         GOTO1 CASHVAL,DMCB,(BYTE,(R6)),(R3)                                    
         CLI   0(R1),X'FF'                                                      
         BNE   *+12                                                             
         LA    R2,SLN1RTEH                                                      
         B     INVLFLD                                                          
         L     R0,4(R1)                                                         
         CVD   R0,DUB                                                           
         CP    DUB,=P'999999999'   MAX FOR PDBCOS                               
         BNH   *+12                                                             
         LA    R2,SLN1RTEH                                                      
         B     INVLRATE                                                         
         ZAP   VALCOST,DUB                                                      
*        LTR   R0,R0               IF RATE IS FREE, SET COST TO                 
*        BNZ   *+10                  .00001 AND RESET AFTER                     
*        ZAP   VALCOST,=P'1'         RATE LOOK-UP                               
*                                                                               
VRTEX    B     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE PREMIUM DESC                                       
***********************************************************************         
VPREM    DS    0H                                                               
         NMOD1 0,**VPRM**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)                                                         
         USING SCRLIN1D,R2                                                      
*                                                                               
         L     R9,ASYSD                                                         
         LA    R4,SYSSPARE                                                      
*                                                                               
         MVI   VALPCTYP,C' '       CLEAR                                        
         CLI   SLN1PRMH+5,0                                                     
         BE    VPRMX                                                            
         CLC   =C'NONE',SLN1PRM                                                 
         BE    VPRM99                                                           
         LA    R6,SLN1PRM                                                       
         ZIC   R3,SLN1PRMH+5                                                    
         CLI   SLN1PRM+1,C'C'                                                   
         BNE   VPRM20                                                           
         CLI   SLN1PRM,C'1'                                                     
         BNL   *+12                                                             
         LA    R2,SLN1PRMH                                                      
         B     INVLPREM                                                         
         CLI   SLN1PRM,C'4'                                                     
         BNH   *+12                                                             
         LA    R2,SLN1PRMH                                                      
         B     INVLPREM                                                         
         MVC   VALCL,SLN1PRM                                                    
         MVC   VALPRIN,VALCOSIN    SET DEFAULT PREMIUM RATE IND                 
         CLI   SLN1PRMH+5,2        TEST INPUT LENGTH                            
         BE    VPRM99                                                           
         CLI   SLN1PRM+2,C'/'                                                   
         BE    *+12                                                             
         LA    R2,SLN1PRMH                                                      
         B     INVLPREM                                                         
         LA    R6,SLN1PRM+3                                                     
         SH    R3,=H'3'                                                         
         BNZ   *+12                                                             
         LA    R2,SLN1PRMH                                                      
         B     INVLPREM                                                         
*                                                                               
VPRM20   DS    0H                                                               
         CLI   0(R6),C'S'          TEST NON-COMM OVERRIDE                       
         BNE   VPRM22                                                           
         LA    R6,1(R6)            ADJUST POINTER                               
         BCTR  R3,0                 AND LEN                                     
         MVI   VALPRIN,C'S'        SET IND IN VALELEM                           
         CLI   VALCOSIN,C'S'       MUST BE THE SAME                             
         BE    VPRM24                                                           
         LA    R2,SLN1PRMH                                                      
         B     INVLPREM                                                         
*                                                                               
VPRM22   DS    0H                                                               
**COM                                                                           
         CLC   =C'SJ',AGENCY       ONLY OR SJ                                   
         BNE   VPRM24                                                           
         CLI   0(R6),C'C'          TEST NON-COMM OVERRIDE                       
         BNE   VPRM24                                                           
         LA    R6,1(R6)            ADJUST POINTER                               
         BCTR  R3,0                 AND LEN                                     
         MVI   VALPRIN,C'C'        SET IND IN VALELEM                           
         CLI   VALCOSIN,C'C'       MUST BE THE SAME                             
         BE    *+12                                                             
         LA    R2,SLN1PRMH                                                      
         B     INVLFLD                                                          
*                                                                               
VPRM24   DS    0H                                                               
**NEW 3/30/89                                                                   
         CLI   SVESPROF+28,C'C'    SEE IF 'C' RATE EST                          
         BNE   VPRM26                                                           
         CLI   VALPRIN,C'C'        SEE IF 'C' INPUT                             
         BE    VPRM30                                                           
         CLI   VALPRIN,C' '                                                     
         BE    *+12                                                             
         LA    R2,SLN1PRMH                                                      
         B     INVLFLD                                                          
         MVI   VALPRIN,C'C'         SET TO 'C'                                  
         B     VPRM30                                                           
*                                                                               
VPRM26   CLI   VALPRIN,C'C'     NO 'C' PREMIUM FOR NON 'C' RATE EST             
         BNE   *+12                                                             
         LA    R2,SLN1PRMH                                                      
         B     INVLFLD                                                          
*                                                                               
VPRM30   DS    0H                                                               
         CLI   0(R6),C'N'          NET TO BE GROSSED UP                         
         BNE   VPRM90                                                           
         CLI   VALPRIN,C' '   CAN'T BE ENTERED IF VALPRIN PRESENT               
         BNH   *+12                                                             
         LA    R2,SLN1PRMH                                                      
         B     INVLFLD                                                          
         MVI   VALPCTYP,C'N'                                                    
         LA    R6,1(R6)                                                         
         BCTR  R3,R0                                                            
         B     VPRM20                                                           
*                                                                               
VPRM90   DS    0H                                                               
**NEW 3/30/89                                                                   
         GOTO1 CASHVAL,DMCB,(R6),(R3)                                           
         CLI   0(R1),X'FF'                                                      
         BNE   *+12                                                             
         LA    R2,SLN1PRMH                                                      
         B     INVLFLD                                                          
         ZAP   VALPRCOS,=P'1'      SET TO .01 IF FREE                           
         L     R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    VPRM99                                                           
         CVD   R0,DUB                                                           
         ZAP   VALPRCOS,DUB                                                     
*                                                                               
         CLI   VALPCTYP,C'N'        TEST NET PREMIUM CHARGE INPUT               
         BNE   VPRM99              DONE                                         
*                                                                               
         L     R6,AIO                                                           
         USING PBDELEM,R6                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
*                                  IF NO BUY DETAIL ELEMENT FOUND               
*                                  (PROBABLY "RUN-NOT-ORDERED" ENTRY),          
         BNE   VPRM99              CAN'T GROSS UP, SO LEAVE COST ALONE          
         MVC   VALACP,PBDACP                                                    
         DROP  R6                                                               
*                                  GROSS UP COST                                
         CP    VALACP,=P'0'        UNLESS THERE IS NO AC                        
         BE    VPRM99              THEN LEAVE IT ALONE                          
         CP    VALACP,=P'1'        OR AC WAS OVERRIDDEN TO 0                    
         BE    VPRM99              THEN LEAVE IT ALONE                          
*                                                                               
         ZAP   DUB,VALPRCOS                                                     
         CVB   R1,DUB                                                           
         M     R0,=F'100000'                                                    
         ZAP   DUB,VALACP                                                       
         CVB   RF,DUB                                                           
         S     RF,=F'100000'       =NET PCT                                     
         LCR   RF,RF                                                            
         BNP   VPRM99                                                           
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         CVD   R1,DUB                                                           
         ZAP   VALPRCOS,DUB                                                     
*                                                                               
VPRM99   DS    0H                                                               
         B     VPRMX                                                            
*                                                                               
VPRMX    B     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*=================================================================*             
* TEST DATA LOCKED BY OFFLINE APPLICATION                         *             
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS *             
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                  *             
*=================================================================*             
         SPACE 1                                                                
         DS    0D                                                               
TSTLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,KEY                                                           
         USING PBUYKEY,RE                                                       
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,PBUYKAGY                                               
         MVC   L.LOCKRTY,=C'BC'    CLIENT LOCK                                  
         MVC   L.LOCKMED,PBUYKMED                                               
         MVC   L.LOCKCLT,PBUYKCLT                                               
         DROP  RE                                                               
*                                                                               
TSTLOK20 L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLOK20                                                         
*                                  SUB-CLIENT CHECKING                          
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT ?                                 
         BNE   TSTLOK28            NO                                           
*                                                                               
         MVC   L.LOCKCLT,SVCLPROF+6   USE MASTER CLIENT NOW                     
         DROP  L                                                                
*                                                                               
TSTLOK24 L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLOK24                                                         
*                                  CHECK CLIENT/PUB LOCK                        
TSTLOK28 LA    RE,KEY                                                           
         USING PBUYKEY,RE                                                       
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,PBUYKAGY                                               
         MVC   L.LOCKRTY,=C'BP'    CLIENT/PUB LOCK                              
         MVC   L.LOCKMED,PBUYKMED                                               
         MVC   L.LOCKCLT,PBUYKCLT                                               
         MVC   L.LOCKPUB,PBUYKPUB                                               
         XC    L.LOCKPUB,=4X'FF'   TO ELIMINATE X'00' FIELDS                    
         DROP  RE                                                               
*                                                                               
TSTLOK30 L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLOK30                                                         
*                                  SUB-CLIENT CHECKING                          
         CLI   SVCLPROF+5,C'2'     SUB-CLIENT ?                                 
         BNE   TSTLOK40            NO                                           
*                                                                               
         MVC   L.LOCKCLT,SVCLPROF+6   USE MASTER CLIENT NOW                     
         DROP  L                                                                
*                                                                               
TSTLOK34 L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),ACOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ            RECORD LOCKED MESSAGE                        
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTLOK34                                                         
*                                                                               
TSTLOK40 DS    0H                                                               
*                                                                               
TSTLKEQ  CR    RB,RB                                                            
         B     *+6                                                              
TSTLKNEQ LTR   RB,RB               REC LOCKED - SEND MESSAGE                    
         XIT1                                                                   
*                                                                               
LKUPKEY  DS    XL16                DATA LOCKING KEY                             
*                                                                               
         LTORG                                                                  
         SPACE 3                                                                
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    XL1                                                              
LOCKCLT  DS    XL3                                                              
LOCKPUB  DS    XL4                 BASE PUB ONLY                                
         DS    XL2                                                              
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
       ++INCLUDE PPMATFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PPMATFBD          (OUR CHECK SCREEN)                           
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATEED          (OUR SUPERCK SCREEN FOR NEWSPAPER)           
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATEDD          (OUR SUPERCK SCREEN FOR MAGAZINE)            
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATEBD          (OUR CHECK SCREEN FOR NEWSPAPER)             
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATECD          (OUR UPDATE SCREEN FOR NEWSPAPER)            
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATDBD          (OUR CHECK SCREEN FOR MAGAZINE)              
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATDCD          (OUR UPDATE SCREEN FOR MAGAZINE)             
         EJECT                                                                  
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATCBD          (OUR COMMENTS SCREEN)                        
         EJECT                                                                  
* DDGENTWA                                                                      
* DDGLOBEQUS                                                                    
* DDGLVXCTLD                                                                    
* DDPERVALD                                                                     
* DDMINBLK                                                                      
* PUBGENEL                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
       ++INCLUDE DDGLVXCTLD                                                     
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE DDMINBLK                                                       
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE PUBGENEL                                                       
         PRINT ON                                                               
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE PPMATWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
PREPRECD DSECT                                                                  
       ++INCLUDE PREPREC                                                        
PPRDRECD DSECT                                                                  
       ++INCLUDE PPRDREC                                                        
PUBREPD  DSECT                                                                  
       ++INCLUDE PUBREPEL                                                       
         EJECT                                                                  
PBUYRECD DSECT                                                                  
       ++INCLUDE BUYDSECTS                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
* MY STORAGE AREA                                                               
       ++INCLUDE PPMATWK02D        (SYSTEM AREAS)                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'131PPMAT02   07/08/03'                                      
         END                                                                    
