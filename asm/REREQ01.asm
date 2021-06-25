*          DATA SET REREQ01    AT LEVEL 078 AS OF 10/10/11                      
*PHASE T80701A,*                                                                
*INCLUDE HEXOUT                    DUMMY INCLUDE.  SEE BELOW.                   
REQ1     TITLE 'T80701 - REREQ01 - NEW REP REQUEST SCREEN BUILDER'              
*                                                                               
**********************************************************************          
*                                                                    *          
*    REREQ01 (T80701) --- REQUEST PROGRAM - BUILDS SCN               *          
*                                                                    *          
*   ** NOTE **  'HEXOUT' INCLUDED SO THE LINK-EDITOR WILL TELL US    *          
*               THE END OF THIS CSECT (AFTER THE 'LEVEL' LITERAL).   *          
*               THE REREQDEF OVERLAY WILL BE LOADED AT THE ADDRESS.  *          
*                                                                    *          
*                                                                    *          
*--------------------------------------------------------------------*          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
*  12/12/89  PJS  TAKEN FROM RENEW01 AND MODIFIED FOR NEW RQST.      *          
*                                                                    *          
*  AUG13/90 (MRR) --- CHANGED REQUEST TABLE AND LABELS DUE TO SIZE   *          
*                                                                    *          
*  OCT05/90 (BU ) --- CHANGE REQUEST TABLE OVERLAY MECHANISM         *          
*                                                                    *          
*  NOV01/90 (BU ) --- CONSTRUCT SCREEN TO ADD EXTENDED HEADER, WHERE *          
*                     APPLICABLE.                                    *          
*                                                                    *          
*  MAR13/92 (MRR) --- REP REC READ IN BASE, DON'T READ IT HERE       *          
*                                                                    *          
*  JAN24/94 (BU ) --- ADD ANOTHER OVERLAY TABLE, DUE TO SPACE LIMITS *          
*                                                                    *          
*  DEC16/94 (BU ) --- MODULE DATE CHANGE: NO OTHER CHANGES           *          
*                                                                    *          
*  MAR27/95 (BU ) --- STEREO DATA IN PROTECTED FIELD                 *          
*                                                                    *          
*  APR03/95 (BU ) --- FIX 'STEREO' BUGS.                             *          
*                                                                    *          
*  MAY02/95 (BU ) --- FIX 'STEREO' OBSCURE BUG.                      *          
*                                                                    *          
*  JUL31/96 (BU ) --- ADD ANOTHER OVERLAY TABLE                      *          
*                                                                    *          
*  SEP28/11 (BOB) --- BEST $ OPTION                                  *          
*                                                                    *          
*                                                                    *          
*                     ***  END TOMBSTONE  ***                        *          
**********************************************************************          
         PRINT NOGEN                                                            
T80701   CSECT                                                                  
         NMOD1 0,T80701,RR=R2                                                   
*                                                                               
         L     R9,0(R1)            A(WORK AREA)                                 
         USING REQWRK,R9                                                        
*                                                                               
         ST    R2,SUBRELO          SUB OVERLAY RELOCATION FACTOR                
*                                                                               
         L     RA,ASAVE                                                         
         USING TWAD,RA             TWA/SCREEN AREA                              
*                                                                               
         MVC   GOPROC,SELECTSW     SET 0/^0                                     
*                                                                               
*                                                                               
*- SEE IF SCREEN IS LOADABLE FROM DISK OR IF IT NEEDS TO BE BUILT.              
*                                                                               
         LA    R1,LOADTBL                                                       
MAIN100  CLI   0(R1),0                                                          
         BE    MAIN200             NOT LOADABLE                                 
*                                                                               
         CLC   ACTION,0(R1)                                                     
         BE    MAIN120                                                          
*                                                                               
         LA    R1,2(R1)            NEXT ENTRY                                   
         B     MAIN100                                                          
*                                                                               
*- LOAD SCREEN AND EXIT                                                         
MAIN120  EQU   *                                                                
         MVI   GOPROC,1            GO DIRECTLY TO PROCESS OVLY                  
*                                                                               
         CLC   SCRNBILT(1),1(R1)   SCREEN ALREADY UP?                           
         BE    OKEXIT5             YES -                                        
*                                                                               
         MVC   SCRNBILT(1),1(R1)   SAVE SCREEN NUMBER WE ARE LOADING            
*                                                                               
         LA    R2,RQSLAST          LOAD POINT                                   
         ZIC   R3,1(R1)            SCREEN NUMBER                                
         GOTO1 CALLOV,P1,((R3),(R2)),0                                          
         CLI   P2,X'FF'                                                         
         BNE   *+6                                                              
         DC    H'0'                ERROR LOADING SCREEN FROM DISK               
*                                                                               
         B     OKEXIT5                                                          
         SPACE 2                                                                
*                                                                               
*- ACTIONS WITH LOADABLE SCREENS.                                               
LOADTBL  DS    0XL2                X'ACTION EQU',X'SCREEN NUMBER'               
         DC    AL1(EQLIST),AL1(SCLIST)                                          
         DC    X'00'               EOT                                          
         DS    0H                  ALIGNMENT                                    
         SPACE 2                                                                
*                                                                               
*- SCREEN NEEDS TO BE BUILT ON THE FLY FROM REQTBL ENTRY.                       
*  LOAD IN REREQ10 AND 11 - TABLE DEFINITION PHASE.  CALL PHASE TO              
*  SET UP AREQTBL (RELOCATED TABLE).  THEN CALL 'FINDREQ'                       
*  IN BASE TO FIND REQUEST WE NEED TO BUILD.                                    
*                                                                               
*  ONCE TABLE ENTRY IS FOUND, MOVE ENTRY TO WORK AREA.                          
*  DEFAULTS ARE ALSO SAVED (IN THE TWA)                                         
*                                                                               
*   REQUEST OVERLAY TABLE.  IF MORE OVERLAYS ARE NEEDED, ADD THE                
*   EQUATE (IE, OVTBL1) HERE                                                    
*                                                                               
OVTBL    DS    0XL2                X'OVERLAY NUMBER'                            
         DC    AL1(OVTBL1)         REREQ10                                      
         DC    AL1(OVTBL5)         REREQ14                                      
         DC    AL1(OVTBL2)         REREQ11                                      
         DC    AL1(OVTBL6)         REREQ15                                      
         DC    AL1(OVTBL3)         REREQ12                                      
         DC    AL1(OVTBL4)         REREQ13                                      
         DC    X'00'               DELIMITER                                    
         DS    0H                  ALIGNMENT                                    
*                                                                               
MAIN200  EQU   *                                                                
         LA    R3,OVTBL            A(TABLE OF OVLAYS TO LOAD/CHECK)             
MAIN201  EQU   *                                                                
         CLI   0(R3),0             END OF TABLE?                                
         BNE   MAIN202             NO                                           
         DC    H'0'                REQUEST NOT FOUND: DUMP                      
MAIN202  EQU   *                                                                
         ZIC   R4,0(R3)            LOAD OVERLAY NUMBER                          
         L     R2,=V(HEXOUT)       LOAD AND CALL TBL DEF OVLY                   
         A     R2,SUBRELO                                                       
         PRINT GEN                                                              
         GOTO1 AGETOVLY,P1,((R4),(R2))                                          
*                                                                               
         GOTO1 AFINDREQ,P1,0       FIND REQTBL ENTRY                            
         PRINT NOGEN                                                            
         BZ    MAIN204             REQUEST FOUND                                
         LA    R3,1(R3)            NEXT TABLE ENTRY                             
         B     MAIN201                                                          
MAIN204  EQU   *                                                                
*                                                                               
         LR    RC,RA               MOVE REQTBL ENTRY TO WRK AREA                
         AH    RC,=H'4096'                                                      
         USING TWAD+4096,RC        FOR ADDRESSABILITY                           
*                                                                               
         L     RE,P1               A(REQTBL ENTRY)                              
         SR    R1,R1                                                            
         ICM   R1,3,RQLNTRY(RE)    LENGTH TO MOVE                               
*                                                                               
         LA    RF,REQDEFX-2        DON'T EXCEED WRK AREA                        
         LA    R0,REQDEF                                                        
         AR    R0,R1                                                            
         CR    R0,RF                                                            
         BNH   *+6                                                              
         DC    H'0'                REQTBL ENTRY WON'T FIT IN WRK AREA.          
*                                                                               
         MOVE  (REQDEF,(R1)),(RE)                                               
*                                                                               
***      LA    R2,RQSOPTH          CLEAR THE OPTION FIELD                       
***      MVI   8(R2),C' '          INSERT SPACES                                
***      MVC   9(L'RQSOPT-1,R2),8(R2)                                           
***      FOUT  (R2)                                                             
*                                                                               
         XC    DEFAULTS,DEFAULTS   SAVE DEFAULTS IN TWA                         
         L     RF,P1               A(REQTBL ENTRY IN OVLY)                      
         SR    RE,RE                                                            
         ICM   RE,7,RQDFLT(RF)     A(DEFAULT LIST, RELATIVE TO REQTBL)          
         A     RE,AREQTBLC         RELOCATE                                     
*                                                                               
         LR    R1,RE                                                            
MAIN205  CLC   =XL2'00',0(R1)                                                   
         BE    MAIN210                                                          
         ZIC   RF,2(R1)            DATA LEN                                     
         LA    R1,3(RF,R1)         + ENTRY OVERHEAD = A(NEXT ENTRY)             
         B     MAIN205                                                          
*                                                                               
MAIN210  LA    R1,2(R1)            INCLUDE THE XL2'00' IN MOVE                  
         SR    R1,RE                                                            
         MOVE  (DEFAULTS,(R1)),(RE)                                             
         LA    RE,REQDEF           POINT TO RQST DEF IN WRK AREA                
         ST    RE,AREQNTRY                                                      
         ST    RE,AREQTBLC                                                      
*                                                                               
*  IF 'MASTER REP' FLAG IS SET TO 'N', THE REP RECORD MUST BE                   
*      RETRIEVED TO DETERMINE WHETHER THE REQUESTING REP IS A                   
*      'MASTER.'  IF SO, THE REQUEST IS TO BE BLOCKED.                          
*                                                                               
         MVI   NOMASTER,0          TURN OFF ERROR IN BASE FLAG                  
         TM    RQCNTL2(RE),RQ2MASTR PERMIT/PROHIBIT MASTER REP?                 
         BO    MAIN215                                                          
*                                                                               
         CLI   IAMAMAST,C'Y'                                                    
         BNE   MAIN215                                                          
         MVI   NOMASTER,1          YES - SET FLAG FOR ERROR IN BASE             
         B     ERROR                                                            
MAIN215  EQU   *                                                                
*                                                                               
         L     RE,AREQNTRY                                                      
         SR    R1,R1               MOVE OUT END OF ENTRY MARKER                 
         ICM   R1,3,RQLNTRY(RE)                                                 
         AR    RE,R1                                                            
         MVC   0(2,RE),=XL2'00'                                                 
*                                                                               
         DROP  RC                                                               
*                                                                               
         CLC   SCRNBILT,RQSNUM     SAVE RPT ID FROM SCREEN                      
         BE    OKEXIT5                                                          
*                                                                               
*- START BUILDING SCREEN                                                        
         L     RE,AREQNTRY         REQTBL ENTRY TO BUILD SCREEN FROM            
         LA    RE,RQFIELD(RE)      POINT TO FIELD DEFINITIONS                   
         ST    RE,WKFLD                                                         
*                                                                               
         LA    RF,LSTLAST-RQSLAST  L(SCREEN WORK AREA)                          
         LA    RE,RQSLAST                                                       
         PRINT GEN                                                              
         XCEF                                                                   
         PRINT NOGEN                                                            
*                                                                               
         LA    RE,RQSLAST          START BUILDING HERE                          
         ST    RE,WKTWA                                                         
*                                                                               
         LA    RE,REQMAP           BUILD MAP HERE                               
         ST    RE,WKMAP                                                         
         MVC   0(2,RE),=XL2'00'    END OF MAP INDICATOR                         
*                                                                               
         MVI   HAVEREQD,0          ASSUME NO REQUIRED FLDS                      
*                                                                               
MAIN220  EQU   *                                                                
         L     RE,WKFLD            END OF FIELDS?                               
         CLC   =XL2'00',0(RE)                                                   
         BE    MAIN300                                                          
*                                                                               
         BAS   RE,FINDFLD          FIND FIELD IN FIELD TBL                      
         BNZ   ERROR                                                            
*                                                                               
         BAS   RE,MOVEFLD          MOVE FLD TO TWA                              
         BNZ   ERROR                                                            
*                                                                               
         BAS   RE,MAPIT            MAP SCREEN TO VAL RTN.                       
         BNZ   ERROR                                                            
*                                                                               
         BAS   RE,NEXTDEF          POINT TO NEXT FLD DEF                        
         BNZ   ERROR                                                            
*                                                                               
         BAS   RE,NEXTTWA          POINT TO NEXT FLD TWA                        
         BNZ   ERROR                                                            
         B     MAIN220                                                          
*                                                                               
*- SCREEN IS UP.                                                                
*  SET TRANSMIT BITS FOR HEADER FIELDS (CLEARS RESIDUAL SCREEN FLDS)            
*  IF NO REQUIRED FIELDS IN THIS REQUEST, TURN ON 'MODIFIED' BIT                
*     IN ACTION TO PREVENT 'NO INPUT' MESSAGE                                   
*  PUT OUT LAST FIELD INDICATOR AND EXIT                                        
MAIN300  EQU   *                                                                
         LA    RE,RQSMSGH          1ST LINE OF BASE SCREEN                      
         L     RF,WKTWA            A(END OF SCREEN)                             
*                                                                               
MAIN320  OI    6(RE),OI1T          TRANSMIT                                     
         ZIC   R0,0(RE)                                                         
         AR    RE,R0                                                            
         CR    RE,RF               AT END OF BASE SCREEN?                       
         BL    MAIN320             NO. LOOP BACK FOR NEXT FIELD                 
*                                                                               
         CLI   HAVEREQD,0                                                       
         BNE   MAIN330             REQ'D FIELDS ON SCREEN                       
*                                                                               
         OI    RQSACTH+6,X'01'     TREAT AS MODIFIED                            
*                                                                               
MAIN330  EQU   *                                                                
         L     RE,WKTWA                                                         
         MVC   0(3,RE),=X'000100'     LAST FIELD                                
*                                                                               
*- IF SCREEN HAS NO FIELDS, GO DIRECTLY TO PROCESS OVLY                         
         CLC   =XL2'00',REQMAP                                                  
         BNE   MAIN340                                                          
         MVI   GOPROC,1                                                         
*                                                                               
MAIN340  EQU   *                                                                
         B     OKEXIT                                                           
         SPACE                                                                  
OKEXIT   EQU   *                                                                
         MVC   SCRNBILT,RQSNUM     SAVE RPT ID FROM SCREEN                      
OKEXIT5  SR    R0,R0               GOOD CC                                      
         B     EXIT                                                             
*                                                                               
ERROR    EQU   *                                                                
         XC    SCRNBILT,SCRNBILT   FORCE NEW SCREEN                             
         LTR   RD,RD               BAD CC                                       
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*- FINDFLD -- FIND FIELD NUMBER FROM DEFINITION ENTRY IN FIELD TBL              
*                                                                               
*  INPUT:  WKFLD -- A(FIELD DEFINITION ENTRY)                                   
*  RETURN:  AFLD -- A(FIELD TBL ENTRY)                                          
*                                                                               
FINDFLD  NTR1                                                                   
         L     RE,WKFLD                                                         
         LA    RF,FLDTBL                                                        
*                                                                               
FINDF20  EQU   *                                                                
         CLI   0(RF),0             END OF FIELD TBL                             
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   1(2,RF),FDNUM(RE)   FLDTBL -VS- FLD DEF ENTRY                    
         BE    FINDF40                                                          
*                                                                               
         ZIC   R0,0(RF)            LOOK AT NEXT FLDTBL ENTRY                    
         AR    RF,R0                                                            
         B     FINDF20                                                          
*                                                                               
FINDF40  EQU   *                                                                
         ST    RF,AFLD             PASS BACK A(FLDTBL ENTRY)                    
SUBEXIT  EQU   *                                                                
         SR    R0,R0               GOOD CC                                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*- MOVEFLD -- MOVE FIELD FROM FIELD TABLE TO TWA                                
*                                                                               
*  INPUT:  AFLD  - A(FLDTBL ENTRY)                                              
*          WKFLD - A(FIELD DEFINITION ENTRY)                                    
*          WKTWA - A(SCREEN OUTPUT AREA)                                        
*                                                                               
MOVEFLD  NTR1                                                                   
*                                                                               
         L     R2,WKTWA            A(OUTPUT AREA)                               
         L     R3,AFLD             A(FLDTBL ENTRY)                              
         L     R4,WKFLD            A(FLD DEF ENTRY)                             
*                                                                               
*- BUILD FIELD IN TWA AREA.                                                     
*                                                                               
         SPACE                                                                  
*                                                                               
*- MAKE SPACE FOR HEADER                                                        
         XC    0(8,R2),0(R2)                                                    
*                                                                               
*- DETERMINE DATA LENGTH AND FIELD SIZE (DATA LEN + 8 BYTE HEADER)              
         ZIC   RF,0(R3)            FLDTBL ENTRY LEN                             
         LA    R0,FLDLHEAD                                                      
         SR    RF,R0               - FLDTBL ENTRY HEADER                        
         STC   RF,7(R2)            = DATA LENGTH (SAVE IN FIELD HEADER)         
*                                                                               
         LA    R1,8(RF)            LENGTH OF TWA FIELD (DATA+HEADER)            
         ZIC   RE,FDEXTHDR(R4)     EXTENDED FIELD HEADER NEEDED?                
         LTR   RE,RE               ANY VALUE?                                   
         BZ    MFLD080             NO                                           
         LA    R1,8(R1)            YES - ADD L(EXTENDED)                        
         OI    1(R2),X'02'         TURN ON 'EXTENDED' BIT                       
         LTR   RE,RE               ** DUMMY FOR CONDITION DUMP **               
MFLD080  EQU   *                                                                
         STC   R1,0(R2)                                                         
*                                                                               
*- MOVE FIELD DATA FROM TBL TO TWA                                              
         BCTR  RF,0                                                             
         LR    R7,RF               SAVE VALUE OF RF                             
         BAS   RE,CHKSTREO         CHECK FOR STEREO                             
         BNZ   MFLD100             NOT STEREO REQUEST                           
         BAS   RE,STEREO           YES - LOAD CONTROL INFO                      
         BNZ   MFLD110             CONTROL INFO DISPLAYED: NO TWA               
MFLD100  EQU   *                                                                
         LR    RF,R7               RESET VALUE OF RF                            
         EX    RF,FLD2TWA          MOVE FROM FIELD TO TWA                       
MFLD110  EQU   *                                                                
*                                                                               
*- SET SCREEN ADDRESS IN HEADER  (ROW-1)*80+(COL-1)                             
*                                                                               
         ZIC   RE,FDROW(R4)        ROW NUMBER                                   
         BCTR  RE,0                -1                                           
         MH    RE,=H'80'           * 80 (BYTE PER ROW)                          
*                                                                               
         ZIC   RF,FDCOL(R4)        COLUMN NUMBER                                
         BCTR  RF,0                -1                                           
*                                                                               
         AR    RE,RF               (ROW-1)*80 + (COL-1)                         
         STCM  RE,3,2(R2)          SCREEN ADDRESS IN FLD HEADER                 
*                                                                               
*- ALWAYS TRANSMIT THE FIELD                                                    
         MVI   6(R2),X'80'         TRANSMIT THIS FIELD                          
*                                                                               
*- IS FIELD PROTECTED?                                                          
         TM    FDCNTL(R4),FCCMT                                                 
         BZ    MFLD120                                                          
         OI    1(R2),X'20'         TURN ON PROTECT BIT                          
*                                                                               
*- SET HI-INTENSITY IF REQUIRED                                                 
MFLD120  EQU   *                                                                
         CLI   WHEN,EQSOON         RUNNING SOON?                                
         BNE   MFLD140                                                          
         TM    FDCNTL(R4),FCRQSOON FIELD REQ. FOR SOON?                         
         BZ    MFLD140                                                          
         OI    1(R2),X'08'         SET HI-INTENSITY                             
*                                                                               
MFLD140  EQU   *                                                                
         CLI   WHEN,EQOVRN         RUNNING OVERNIGHT?                           
         BNE   MFLD160                                                          
         TM    FDCNTL(R4),FCRQOVRN FIELD REQ. FOR OVERNIGHT                     
         BZ    MFLD160                                                          
         OI    1(R2),X'08'         SET HI-INTENSITY                             
*                                                                               
*- NUMERIC?                                                                     
MFLD160  EQU   *                                                                
         TM    FDCNTL(R4),FCNUM                                                 
         BZ    MFLD180                                                          
         OI    1(R2),X'10'         TURN ON NUMERIC BIT                          
*                                                                               
MFLD180  EQU   *                                                                
*                                                                               
*   SET UP EXTENDED HEADER FIELD, IF NEEDED                                     
*                                                                               
         ZIC   RE,FDEXTHDR(R4)     CHECK INDICATOR AGAIN                        
         LTR   RE,RE               ANY VALUE?                                   
         BZ    MFLD190             NO                                           
         ZIC   R1,0(R2)            GET L(FLD) W/L(EXT HDR)                      
         LA    RF,8                SUBTRACT L(EXT HDR)                          
         SR    R1,RF                                                            
         LA    R2,0(R1,R2)         A(START OF EXTENDED HEADER)                  
         XC    0(8,R2),0(R2)       MAKE SPACE FOR EXT HDR                       
         STC   RE,0(R2)            STORE BYTE WITH FIELD ID #                   
         LTR   RE,RE               ** DUMMY FOR CONDITION TEST **               
MFLD190  EQU   *                                                                
         B     SUBEXIT                                                          
*                                                                               
FLD2TWA  MVC   8(0,R2),FLDLHEAD(R3)    MOVE FIELD DATA TO TWA                   
         EJECT                                                                  
*                                                                               
*   CHKSTREO:  CHECK FOR A STEREO REQUEST.  IF STEREO, PASS BACK                
*        CC = ZERO.                                                             
*                                                                               
CHKSTREO NTR1                                                                   
         L     RF,ACOMFACS         A(COMFACS)                                   
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
**>>>    TM    FATSTAT6,X'80'      STEREO REQUEST?                              
**>>>    BNO   CSTR0100            NO                                           
         TM    FATSTAT6,TST6STRO+TST6STFU                                       
*                                  FULL STEREO REQUEST?                         
         BNO   CSTR0100            NO                                           
         SR    R0,R0               YES - SET CC = ZERO                          
         B     CSTR0200                                                         
CSTR0100 EQU   *                                                                
         LTR   RB,RB               SET CC = NOT ZERO                            
CSTR0200 EQU   *                                                                
         XIT1                                                                   
                                                                                
*                                                                               
         DROP  R1,RF                                                            
         EJECT                                                                  
*                                                                               
*  STEREO:  ONLY CALLED WHEN REQUEST IS FOR 'STEREO'.  THIS REQUIRES            
*           LOADING 'STEREO' CONTROL INFORMATION INTO THE SCREEN'S              
*           DESCRIPTOR FIELDS, WHICH PRECEDE UNPROTECTED FIELDS.                
*           R4  -->  SCREEN FIELD DEFINITION                                    
*                                                                               
STEREO   NTR1                                                                   
         OC    FDVAL(2,R4),FDVAL(R4)                                            
*                                  ANY VALUE IN FIELD VALIDATION?               
         BNZ   STER0200            YES - THIS IS NOT PROTECTED FLD!             
*                                     EXIT THE ROUTINE                          
         OC    FDVAL+FDLENGTH(2,R4),FDVAL+FDLENGTH(R4)                          
*                                  NO  - THIS IS A PROTECTED FLD!               
*                                     IS NEXT FIELD UNPROTECTED?                
         BZ    STER0240            NO  - DON'T DO ANYTHING!                     
         MVC   8(4,R2),=C'FFFF'    YES - INSERT DEFAULT FOR FLD                 
         MVC   WORK(1),FDNUM+1(R4) MOVE FIELD TO BE HEXOUT'ED                   
*                                     FROM CURRENT FLD DEF                      
         GOTO1 LHEXOUT,DMCB,WORK,WORK+1,1,=C'TOG',0                             
         MVC   8(2,R2),WORK+1                                                   
*                                  INSERT NECESSARY INFO..                      
         MVC   12(2,R2),=C'00'     SET EXPANSION FLAG TO 'NO'                   
         TM    FDCNTL+FDLENGTH(R4),FCXPAN                                       
*                                  EXPANSION TO FOLLOW?                         
         BNO   STER0100            NO                                           
         MVC   12(2,R2),=C'81'     YES - SET EXPANSION FLAG TO 'YES'            
         B     STER0200            DON'T CHECK FURTHER                          
STER0100 EQU   *                                                                
         SR    RF,RF               INITIALIZE COUNTER                           
         LR    R5,R4               SET A(FIELD DEFINITION)                      
         LA    R5,FDLENGTH(R5)     BUMP TO UNPROTECTED FIELD DEF                
STER0120 EQU   *                                                                
         LA    R5,FDLENGTH(R5)     BUMP TO NEXT FIELD DEFINITION                
         CLC   =X'0000',0(R5)      END OF FD LIST?                              
         BE    STER0170            YES - FINISHED - NO FOLLOWING                
*                                     FIELD, SO DON'T ADJUST COUNT              
         OC    FDVAL(2,R5),FDVAL(R5)                                            
*                                  ANY VALUE IN FIELD VALIDATION?               
         BNZ   STER0160            YES - THIS IS NOT PROTECTED FLD!             
*                                     THIS SCAN IS FINISHED                     
         CLC   FDNUM(2,R5),=X'0084'                                             
*                                  NO  - 'REQUEST CONTROLS' COMMENT?            
         BE    STER0120            YES - DON'T COUNT IT.                        
*                                                                               
         LA    RF,1(RF)            NO  - THIS IS PROTECTED (COMMENT)            
*                                     SO COUNT IT                               
         B     STER0120            GO BACK AND CHECK NEXT FLD DEF               
STER0160 EQU   *                                                                
         BCTR  RF,0                SUBTRACT 1 FROM COUNT: ASSUME                
*                                     FIELD BEFORE UNPROTECTED ONE              
*                                     IS ITS DESCRIPTOR                         
STER0170 EQU   *                                                                
         LTR   RF,RF                                                            
         BZ    STER0200            NOTHING IN FIELD                             
STER0180 EQU   *                                                                
         LA    R6,13(R2)           SET A(COMMENT COUNTER)                       
         EDIT (RF),(1,(R6))                                                     
STER0200 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO:                             
*                                     STEREO INFO LOADED                        
STER0240 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        HEXADECIMAL CHARACTERS AS A DESTINATION STRING.THE DESTINATION         
*        STRING HAS ONE OF TWO POSSIBLE FORMATS                                 
*        MIXED    - EACH ZONE PRECEDES EACH DIGIT ZDZD..ZDZDZDZD..ZDZD          
*        SEPARATE - ALL ZONES PRECEDE ALL DIGITS  ZZZZ..ZZZZDDDD..DDDD          
*        PARAMS VIA R1 ARE WORDS DEFINED AS FOLLOWS                             
*        1ST   A(SOURCE STRING)                                    (R2)         
*        2ND   A(DESTN  STRING)                                    (R3)         
*        3RD   LENGTH OF SOURCE STRING                             (R4)         
*        4TH   A(FORMAT OPTION) IF C'SEP' SEPERATE ELSE MIXED      (R5)         
*        5TH   VALUE RETURNED ZERO    ERROR IN LENGTH PARAM        (R6)         
*                             NONZERO LENGTH OF DESTN STRING                    
*                                                                               
LHEXOUT  NTR1                                                                   
*                                                                               
         LM    R2,R5,0(R1)                                                      
         LTR   R4,R4                                                            
         BP    HEXO1                                                            
         SR    R6,R6                         RETURN ZERO LENGTH LE ZERO         
         B     HEXOX                                                            
*                                                                               
*        CONVERT EACH SOURCE CHR X'ZD' TO TWO DESTN CHRS X'0Z' & X'0D'          
*                                                                               
HEXO1    CLC   0(3,R5),=C'SEP'                                                  
         BE    HEXO2                                                            
         LA    R5,2                          DEFAULT TO MIXED OPTION            
         LA    R6,1(R3)                                                         
         B     HEXO3                                                            
HEXO2    LA    R5,1                          SET TO SEPERATE OPTION             
         LA    R6,0(R3,R4)                                                      
HEXO3    UNPK  0(1,R3),0(1,R2)                                                  
         NI    0(R3),X'0F'                                                      
         MVN   0(1,R6),0(R2)                                                    
         NI    0(R6),X'0F'                                                      
         LA    R2,1(R2)                      UP SOURCE PTR                      
         AR    R3,R5                         UP DESTN  ZONE PTR                 
         AR    R6,R5                         UP DESTN  DIGT PTR                 
         BCT   R4,HEXO3                                                         
*                                                                               
         LM    R3,R4,4(R1)                   R3=A(DESTN)                        
         SLA   R4,1                          R4=L'DESTN                         
         LR    R6,R4                                                            
HEXO4    CH    R4,=H'256'                    TRANSLATE DESTN                    
         BL    HEXO6                                                            
         TR    0(256,R3),CHRHTAB                                                
         SH    R4,=H'256'                                                       
         LA    R3,256(R3)                                                       
         B     HEXO4                                                            
HEXO5    TR    0(0,R3),CHRHTAB                                                  
HEXO6    LTR   R4,R4                                                            
         BZ    HEXOX                                                            
         BCTR  R4,R0                                                            
         EX    R4,HEXO5                                                         
*                                                                               
HEXOX    ST    R6,16(R1)                                                        
         XIT1                                                                   
*                                                                               
CHRHTAB  DC    C'0123456789ABCDEF'                                              
*                                                                               
*                                                                               
*- MAPIT -- ESTABLISH LINKAGE BETWEEN SCREEN FIELDS AND VALIDATION              
*           ROUTINES.  (ESSENTIALLY, A LIST OF UNPROTECTED FIELDS)              
*                                                                               
*  INPUT:  WKTWA - A(SCREEN FIELD WE JUST BUILT)                                
*          WKFLD - A(FIELD DEFINITION ENTRY IN BASE)                            
*          WKMAP - A(NEXT MAP AREA)  * UPDATED ON RETURN *                      
*                                                                               
MAPIT    NTR1                                                                   
         L     R2,WKTWA                                                         
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    MAPIT90             YES.  OUT.                                   
*                                                                               
         L     R3,WKFLD                                                         
         L     R4,WKMAP                                                         
         MVC   MAPVAL(2,R4),FDVAL(R3) VAL ROUTINE                               
         MVC   MAPFMT(1,R4),FDFMT(R3) FORMAT BITS                               
         MVC   MAPCNTL(1,R4),FDCNTL(R3) CONTROL BITS                            
         SR    R2,RA               TWA FLD ADDRESS - TWA START                  
         ST    R2,MAPFLD(R4)       FIELD HEADER DISP                            
*                                                                               
         LA    RE,REQMAPX                                                       
         CR    R4,RE                                                            
         BL    *+6                                                              
         DC    H'0'                REQMAP EXCEEDED.                             
*                                                                               
         LA    R4,MAPLNTRY(R4)                                                  
         ST    R4,WKMAP                                                         
         MVC   0(2,R4),=XL2'00'    NEW END OF LIST                              
*                                                                               
MAPIT90  B     SUBEXIT                                                          
         EJECT                                                                  
*                                                                               
*- NEXTDEF -- POINT TO NEXT FIELD DEFINITION ENTRY                              
NEXTDEF  NTR1                                                                   
         L     RE,WKFLD                                                         
         LA    RE,FDLNTRY(RE)                                                   
         ST    RE,WKFLD                                                         
         B     SUBEXIT                                                          
         SPACE 2                                                                
*                                                                               
*- NEXTTWA -- POINT TO NEXT FIELD TWA OUTPUT AREA                               
*             CHECK FOR SCREEN MAX EXCEEDED.                                    
NEXTTWA  NTR1                                                                   
         L     RE,WKTWA                                                         
         ZIC   RF,0(RE)            FIELD LEN (HEADER + DATA)                    
         AR    RE,RF                                                            
*                                                                               
         LA    RF,SCREENX-1        DON'T EXCEED SCREEN AREA                     
         CR    RE,RF                 (THE -1 IS FOR LAST FIELD BYTE)            
         BNH   *+6                                                              
         DC    H'0'                SCREEN EXCEEDS MAX SPACE                     
         ST    RE,WKTWA                                                         
         B     SUBEXIT                                                          
         SPACE 2                                                                
         LTORG                                                                  
***  FLAG INCLUDE REREQFLD                                                      
       ++INCLUDE REREQFLD                                                       
*                                                                               
*- REQUEST TWA AND WORK AREA DSECTS                                             
*        PRINT OFF                                                              
***  FLAG INCLUDE REREQTWA                                                      
       ++INCLUDE REREQTWA                                                       
***  FLAG INCLUDE REREQWRK                                                      
       ++INCLUDE REREQWRK                                                       
*        PRINT OFF                                                              
         SPACE                                                                  
         ORG   USERWORK                                                         
SUBRELO  DS    F                   SUB OVERLAY RELOCATION FACTOR                
WKFLD    DS    A                   A(REQTBL FLD DEFINITION FOR BUILD)           
WKTWA    DS    A                   A(SCREEN OUTPUT AREA)                        
AFLD     DS    A                   A(FLDTBL ENTRY)                              
WKMAP    DS    A                   A(REQMAP ENTRY)                              
*                                                                               
HAVEREQD DS    X                   ^0 = REQUIRED FLD ON SCREEN                  
*                                                                               
         SPACE 2                                                                
AMTLOCAL EQU   USERWRKX-*          AVAILABLE USERWORK LEN                       
         DS    (AMTLOCAL)X                                                      
***  FLAG INCLUDE DDCOMFACS                                                     
       ++INCLUDE DDCOMFACS                                                      
***  FLAG INCLUDE FAFACTS                                                       
       ++INCLUDE FAFACTS                                                        
***  FLAG INCLUDE FAUTL                                                         
       ++INCLUDE FAUTL                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'078REREQ01   10/10/11'                                      
         END                                                                    
