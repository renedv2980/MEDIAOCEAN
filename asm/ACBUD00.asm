*          DATA SET ACBUD00    AT LEVEL 016 AS OF 08/05/08                      
*                                                                               
*PHASE T61000A,*                                                                
*INCLUDE ACSPLIT                                                                
         TITLE 'ACBUD00 - BUDGET PROGRAM - ROOT'                                
ACBUD00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 GWSX-GWS,**BUD0**,RA,RR=R5,CLEAR=YES                             
         LR    R9,RC                                                            
         USING GWS,R9              R9=A(GLOBAL W/S)                             
*                                                                               
         ST    R5,RELO                                                          
         L     R8,4(,R1)                                                        
         USING TWAD,R8             R8=A(TWA)                                    
*                                                                               
         ST    RB,ABASE1                                                        
         ST    RA,ABASE2                                                        
         ST    RD,AWORK                                                         
         ST    R8,ATWA                                                          
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
         MVC   COMPANY,0(R1)                                                    
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
*                                                                               
         MVC   VCALLOV,CCALLOV     BUILD EXTERNAL DIRECTORY                     
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VDATCON,CDATCON                                                  
         MVC   VADDAY,CADDAY                                                    
         MVC   VGETMSG,CGETMSG                                                  
         MVC   VGETPROF,CGETPROF                                                
         MVC   VHELLO,CHELLO                                                    
         MVC   VSCANNER,CSCANNER                                                
         MVC   VGETFACT,CGETFACT                                                
         MVC   VUNSCAN,CUNSCAN                                                  
         MVC   VCASHVAL,CCASHVAL                                                
         GOTO1 VCALLOV,DMCB,0,X'D9000A62',0                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VOFFAL,0(R1)                                                     
         DROP  RE                                                               
*                                                                               
         MVI   IOAREAS,0                                                        
         LA    RE,IOAREAS          SET A(4 I/O AREAS)                           
         ST    RE,ABUDREC          1ST ONE FOR BUDGET RECORDS                   
         LA    RE,2048(,RE)                                                     
         ST    RE,AIOAREA1                                                      
         LA    RE,2048(,RE)                                                     
         ST    RE,AIOAREA2                                                      
         LA    RE,2048(,RE)                                                     
         ST    RE,AIOAREA3                                                      
         LA    RE,2048(,RE)                                                     
         ST    RE,AACCUMS                                                       
         LA    RE,14*40(,RE)                                                    
         ST    RE,ASAVE                                                         
         LH    RE,=Y(OFFBLK-GWS)                                                
         LA    RE,GWS(RE)                                                       
         ST    RE,AOFFBLK                                                       
         LA    RE,ACRECORD-ACKEYD                                               
         STH   RE,DATADISP         SET DISP TO FIRST ELEMENT                    
         EJECT ,                                                                
*                                  BUILD INTERNAL/EXTERNAL DIRECTORY            
         SPACE 1                                                                
         LA    R1,AROUTINE                                                      
         LA    RF,ROUTTAB                                                       
*                                                                               
INIT2    ICM   RE,7,0(RF)                                                       
         LA    RE,0(,RE)           RELOCATE A/V TYPE                            
         A     RE,RELO                                                          
         LA    RF,3(,RF)                                                        
*                                                                               
INIT4    ICM   RE,8,0(RF)                                                       
         ST    RE,0(,R1)                                                        
         LA    R1,4(,R1)                                                        
         LA    RF,1(,RF)                                                        
         CLI   0(RF),X'FF'         END OF SUB-LIST                              
         BNE   INIT4                                                            
         LA    RF,1(,RF)                                                        
         CLI   0(RF),X'FF'         END OF LIST                                  
         BNE   INIT2                                                            
*                                  SET OTHER FIELDS                             
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVC   MSG,SPACES                                                       
         MVC   XTRAMESS,SPACES                                                  
         LA    RE,ACCNTRL          ADDRESSES OF PRIMARY & SECONDARY             
         LA    RF,CACNTRL          IO CONTROL BLOCKS                            
         CLI   BUDFORM,C'A'        CONTRA IS PRIMARY IF FORMAT IS ACC           
         BNE   *+10                                                             
         LR    RE,RF                                                            
         LA    RF,ACCNTRL                                                       
         STM   RE,RF,APRIMIOC                                                   
         GOTO1 VGETFACT,DMCB,0                                                  
*                                                                               
         L     RE,0(,R1)                                                        
         USING FACTSD,RE                                                        
         MVC   TODAYB,FADATEB                                                   
         DROP  RE                                                               
*                                                                               
         LA    RE,VIRRECH                                                       
         ST    RE,FADR                                                          
         EJECT ,                                                                
* FIRST TIME INITIALIZATION CODE                                                
         SPACE 1                                                                
INIT6    CLI   NEXTMODE,RUNFIRST   TEST FIRST TRANSACTION                       
         BNE   INIT10                                                           
         MVC   KEY,SPACES          GET COMPANY RECORD FOR START OF              
         MVC   KEY(1),COMPANY      FINANCIAL YEAR                               
         GOTO1 AREAD,AIOAREA2                                                   
         BNE   ERROR                                                            
         L     RE,AIOAREA                                                       
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
INIT7    CLI   0(RE),0             LOCATE COMPANY ELEMENT                       
         BE    INIT8                                                            
         CLI   0(RE),ACMPELQ                                                    
         BE    *+14                                                             
         IC    RF,1(,RE)                                                        
         AR    RE,RF                                                            
         B     INIT7                                                            
*                                                                               
         USING ACCOMPD,RE                                                       
*                                                                               
         MVC   COMPSTA1,ACMPSTAT                                                
         MVC   COMPSTA2,ACMPSTA2                                                
         MVC   COMPSTA3,ACMPSTA3                                                
         MVC   COMPSTA4,ACMPSTA4                                                
         CLI   ACMPSTM,0                                                        
         BNE   *+8                                                              
         MVI   ACMPSTM,C'1'                                                     
         MVC   COSTARTM,ACMPSTM    SAVE COMPANY START MONTH IN BINARY           
         NI    COSTARTM,X'0F'                                                   
         TM    ACMPSTM,X'F0'                                                    
         BO    INIT8                                                            
         LLC   R1,COSTARTM                                                      
         LA    R1,9(,R1)                                                        
         STC   R1,COSTARTM                                                      
*                                                                               
         DROP  RE                                                               
*                                                                               
INIT8    XC    WORK,WORK           SAVE AGENCY-LEVEL PROGRAM PROFILE            
         MVC   WORK(4),=C'ABUD'                                                 
         NI    WORK,X'BF'                                                       
         MVC   WORK+4(1),COMPANY                                                
         MVC   WORK+12(2),TWAAGY                                                
         GOTO1 VGETPROF,DMCB,WORK,WORK1,VDATAMGR                                
         MVC   PROGPROF,WORK1                                                   
         EJECT ,                                                                
* INITIALIZE OFFAL BLOCK                                                        
         SPACE 1                                                                
         USING OFFALD,R1                                                        
         SPACE 1                                                                
INIT10   L     R1,AOFFBLK                                                       
         MVC   OFFACOMF,ACOMFACS                                                
         MVC   OFFAALPH,TWAAGY                                                  
         MVC   OFFACPY,COMPANY                                                  
         MVC   OFFACST1(OFFAOPOS-OFFACST1),COMPSTA1                             
         MVC   OFFALIMA,TWAACCS                                                 
         MVC   OFFAAUTH,TWAAUTH                                                 
         OI    OFFAINDS,OFFAIOFF   CHECK OFFICE SECURITY                        
         MVI   OFFAACT,OFFAINI     INITIALIZE FOR FIRST TRANSACTION             
         CLI   NEXTMODE,RUNFIRST   TEST FIRST TRANSACTION                       
         BE    *+14                YES                                          
         MVI   OFFAACT,OFFARES                                                  
         MVC   OFFASAV(OFFASAVL),SAVEOFFA                                       
         GOTO1 VOFFAL                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SAVEOFFA,OFFASAV                                                 
*                                                                               
         DROP  R1                                                               
         EJECT ,                                                                
* CHOOSE NEXT MODE                                                              
         SPACE 1                                                                
WHCHMODE CLI   NEXTMODE,RUNFIRST                                                
         BE    ACTF0                                                            
         CLI   NEXTMODE,ACTFIRST                                                
         BE    ACTF0                                                            
         GOTO1 CHKINPUT,DMCB,VIRRECH,VIRACTH,1                                  
         BNZ   ACTF0               CHANGE TO RTYPE/ACTION                       
         CLI   NEXTMODE,TYPEMODE   BUDGET TYPE CONTINUED                        
         BE    WHCH12              GO INIT PHASE FOR OVLY 2                     
         CLI   NEXTMODE,FRMFIRST                                                
         BE    WHCH11              GO INIT PHASE FOR OVLY 1                     
*                                                                               
WHCH02   GOTO1 CHKINPUT,DMCB,BUDACCH,BUDTYP4H,1                                 
         BNZ   WHCH11              CHANGE TO 2ND STAGE BUDGET FLDS              
         CLI   NEXTMODE,INPFIRST                                                
         BE    WHCH13              GO INIT PHASE FOR OVLY 3                     
         CLI   NEXTMODE,DISPMODE                                                
         BE    WHCH13                                                           
         LH    R2,DFSTFLDH         PREPARE TO CHECK 3RD STAGE FIELDS            
         AR    R2,R8                                                            
         LH    R3,DLSTFLDH                                                      
         AR    R3,R8                                                            
         BCTR  R3,0                                                             
         LLC   R0,FPERLINE                                                      
         LA    RF,CHKINPUT                                                      
         GOTO1 ,DMCB,(R2),(R3),(R0)                                             
         TM    SAVSTAT,INPUTISY    IF INPUT=Y CHECK FOR CHANGE TO INPUT         
         BNO   *+10                ACCOUNTS                                     
         BASR  RE,RF                                                            
         BNZ   WHCH04              INDICATE DISPMODE-OVLY 3                     
         CLI   NEXTMODE,CHCKMODE                                                
         BE    WHCH13              OVLY 3                                       
         MVI   DMCB+11,1           CHECK FOR CHANGE TO INPUT VALUES             
         BASR  RE,RF                                                            
         BNZ   WHCH06              INDICATE CHCKMODE-OVLY 3                     
         CLI   NEXTMODE,UPDTMODE                                                
         B     WHCH13              OVLY 3                                       
         DC    H'0'                UNRECOGNISED MODE                            
*                                                                               
WHCH04   MVI   NEXTMODE,DISPMODE                                                
         B     WHCH13                                                           
*                                                                               
WHCH06   MVI   NEXTMODE,CHCKMODE                                                
         B     WHCH13                                                           
*                                                                               
WHCH11   MVI   PHASE,1                                                          
         B     ACTF40             TO OVLY CALL LOGIC                            
*                                                                               
WHCH12   MVI   PHASE,2                                                          
         B     ACTF40                                                           
*                                                                               
WHCH13   MVI   PHASE,3                                                          
         B     ACTF40                                                           
         EJECT ,                                                                
* FIRST FDR RECORD TYPE/ACTION                                                  
         SPACE 1                                                                
ACTF0    NI    SAVSTAT,X'FF'-CONTINUE                                           
         TM    VIRRECH+4,VALPREV   CHECK RECORD TYPE IF DIFFERENT               
         BO    ACTF03              BE SURE IT'S NOT STILL HELP                  
         MVI   NEXTMODE,ACTFIRST                                                
         MVI   ACTION,0                                                         
         MVI   SAVSTAT,0                                                        
         GOTO1 AFVAL,VIRRECH                                                    
         BZ    ERROR                                                            
         LLC   RE,FLDH+5                                                        
         BCTR  RE,0                                                             
         LA    RF,RTYPTAB                                                       
         MVI   FERN,INVALID                                                     
*                                                                               
ACTF02   CLI   0(RF),X'FF'                                                      
         BE    ERROR                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),0(RF)                                                     
         BE    *+12                                                             
         LA    RF,L'RTYPTAB(,RF)                                                
         B     ACTF02                                                           
         CLC   FLD(L'RTYPTAB),0(RF)                                             
         BE    *+14                                                             
         MVC   VIRREC,0(RF)                                                     
         OI    VIRRECH+6,TRANSMIT                                               
*                                                                               
ACTF03   CLI   VIRREC,C'?'                                                      
         BE    ACTF03A                                                          
         CLI   VIRREC,C'H'                                                      
         BNE   ACTF04B                                                          
*                                                                               
ACTF03A  CLI   SCREEN,X'FB'        HANDLE BDGT HELP                             
         BE    EXIT                                                             
         MVI   SCREEN,X'FB'                                                     
         B     ACTF04A                                                          
*                                                                               
ACTF04B  CLI   VIRACT,C'?'                                                      
         BE    ACTF04                                                           
         CLI   VIRACT,C'H'                                                      
         BNE   ACTF10                                                           
*                                                                               
ACTF04   CLI   SCREEN,X'FA'        HANDLE RTYPE HELP                            
         BE    EXIT                                                             
         MVI   SCREEN,X'FA'                                                     
*                                                                               
ACTF04A  BAS   RE,GETSCRN                                                       
         BNE   ERROR                                                            
         MVC   MSG(14),=C'HELP DISPLAYED'                                       
         B     OKEND                                                            
*                                                                               
ACTF10   OI    VIRRECH+4,VALPREV   CHECK ACTION & SUBACTION                     
         NI    VIRACTH+4,ALL-VALPREV                                            
         NI    SAVSTAT,ALL-TOTAL                                                
         GOTO1 AFVAL,VIRACTH                                                    
         BZ    ERROR                                                            
         GOTO1 VSCANNER,DMCB,FLDH,(2,TEMP)                                      
         MVI   FERN,INVALID                                                     
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         CLI   TEMP+1,0                                                         
         BNE   ERROR                                                            
         LLC   RE,TEMP                                                          
         SH    RE,=H'1'                                                         
         BM    ERROR                                                            
         LA    RF,ACTNTAB                                                       
         USING ACTD,RF                                                          
         CLI   4(R1),1                                                          
         BE    *+8                                                              
         MVI   FNDX,1                                                           
*                                                                               
ACTF15   CLI   0(RF),X'FF'         CHECK ACTION V TABLE                         
         BE    ERROR                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   ACTDNAME(0),TEMP+12 KEYWORD                                      
         BNE   ACTF16                                                           
         CLC   ACTDTYPE,VIRREC     RECTYPE                                      
         BNE   ACTF16                                                           
         TM    ACTDINDS,DDSONLY    DDS-ONLY                                     
         BZ    ACTF17                                                           
         CLI   TWAOFFC,C'*'                                                     
         BE    ACTF17                                                           
*                                                                               
ACTF16   LA    RF,ACTDLEN(,RF)     BUMP TO NEXT TAB ENTRY                       
         B     ACTF15                                                           
*                                                                               
ACTF17   MVI   FLAG,0              SUB-ACTION TOTAL                             
         CLI   4(R1),2                                                          
         BNE   ACTF20                                                           
         MVI   FNDX,2                                                           
         CLI   TEMP+33,0                                                        
         BNE   ERROR                                                            
         IC    RE,TEMP+32                                                       
         SH    RE,=H'1'                                                         
         BM    ERROR                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   TEMP+32+12(0),=C'TOTALS'                                         
         BNE   ERROR                                                            
         TM    ACTDINDS,TOTAL      IS IT ALLOWED                                
         BNO   ERROR                                                            
         OI    FLAG,TOTAL                                                       
*                                                                               
ACTF20   OI    VIRACTH+4,VALPREV   ACTION/SUBACTION VALID                       
         MVC   ACTINDS,ACTDINDS                                                 
         MVC   PHASE,ACTDOVER                                                   
         MVC   VIRACT,SPACES       EXPAND ACTION NAME                           
         MVC   VIRACT(L'ACTDNAME),ACTDNAME                                      
         OI    VIRACTH+6,X'80'                                                  
         TM    FLAG,TOTAL                                                       
         BNO   ACTF22                                                           
         LA    RE,VIRACT-1+L'ACTDNAME                                           
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVC   1(4,RE),=C',TOT'                                                 
*                                                                               
ACTF22   CLC   ACTDTYPE(4),ACTCHA  CHECK FOR ACTION CHANGE THAT DOESNT          
         BNE   ACTF30              BREAK SEQUENCE IE DIS-CHA                    
         CLI   ACTION,DIS                                                       
         BNE   ACTF30                                                           
         CLI   NEXTMODE,DISPMODE                                                
         BL    ACTF30                                                           
         CLC   FLAG,SAVSTAT        SUBACTION CHANGED                            
         BNE   ACTF30                                                           
         MVC   ACTION,ACTDNUM                                                   
         MVI   NEXTMODE,CHCKMODE                                                
         B     WHCH02                                                           
*                                                                               
ACTF30   MVI   NEXTMODE,TYPEMODE   SET NEXT MODE                                
         MVC   ACTION,ACTDNUM                                                   
         CLI   ACTDTYPE,C'T'       ALWAYS TYPEMODE FOR TYPE                     
         BE    ACTF35                                                           
         MVC   SAVSTAT,FLAG                                                     
         MVI   NEXTMODE,FRMFIRST                                                
*                                                                               
ACTF35   CLC   ACTDSCRN,SCREEN     LOAD SCREEN IF REQUIRED                      
         BE    ACTF40                                                           
         MVC   SCREEN,ACTDSCRN                                                  
         BAS   RE,GETSCRN                                                       
         BNE   ERROR                                                            
         TM    ACTINDS,GOTOVER     UNLESS MORE DATA NOT REQUIRED EXIT           
         BO    ACTF40              FOR IT                                       
         MVC   MSG(10),=C'ENTER DATA'                                           
         LA    R1,VIRTABH                                                       
         SR    R0,R0                                                            
         TM    1(R1),PROTECT                                                    
         BZ    *+14                                                             
         IC    R0,0(,R1)                                                        
         AR    R1,R0                                                            
         B     *-14                                                             
         ST    R1,FADR                                                          
         B     OKEND                                                            
*                                                                               
ACTF40   CLI   PHASE,0             GO TO NEXT MODE - VIA OVERLAY IF             
         BE    ACTF50              REQUIRED                                     
*                                                                               
ACTF41   LLC   R0,PHASE                                                         
         MVI   PHASE,0   CLEAR PHASE FOR CHECK UPON RETURN FROM OV CALL         
*                                                                               
ACTF42   GOTO1 VCALLOV,DMCB,((R0),0),0,0                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APHASE,0(R1)                                                     
         LA    R1,VIRACTH                                                       
         ST    R1,FADR                                                          
         MVI   FERN,OK                                                          
         GOTO1 APHASE                                                           
         CLI   FERN,OK                                                          
         BNE   ERROR                                                            
         CLI   PHASE,0             HAS PHASE RETURNED WITH A OVLY VALUE         
         BNE   ACTF41              YES, GO DO IT                                
         B     OKEND                                                            
*                                                                               
         DROP  RF                                                               
         EJECT ,                                                                
* FIRST FOR FORMAT (ACCOUNT/CONTRA)                                             
         SPACE 1                                                                
ACTF50   CLI   ACTION,HLP          HELP                                         
         BNE   OKEND                                                            
         MVC   MSG(14),=C'HELP DISPLAYED'                                       
         B     OKEND                                                            
*                                                                               
         EJECT ,                                                                
* FORMAT OUTPUT MESSAGE/EXTRA MESSAGE INTO MSG & EXIT.                          
         SPACE 1                                                                
ERROR    CLI   FERN,SPECIAL                                                     
         BE    OMSG                                                             
         GOTO1 VGETMSG,DMCB1,(FERN,MSG),(FNDX,DMCB),0                           
         CLC   XTRAMESS,SPACES                                                  
         BE    OMSG                                                             
         LA    R1,XTRAMESS+L'XTRAMESS-1                                         
         LA    RE,XTRAMESS-1                                                    
         LLC   RF,DMCB1                                                         
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         SR    R1,RE               R1=L'XTRAMESS                                
         LA    R1,3(RF,R1)         R1=TOTAL MESSAGE LENGTH                      
         LA    RE,L'MSG                                                         
         CR    R1,RE               CHECK MESSAGE FITS                           
         BH    OMSG                                                             
         LA    RF,MSG+1(RF)        AND IF SO TACK ON EXTRA MESSAGE              
         MVI   0(RF),C'-'                                                       
         MVC   2(L'XTRAMESS,RF),XTRAMESS                                        
         B     OMSG                                                             
*                                                                               
OKEND    DS    0H                                                               
*                                                                               
OMSG     MVC   VIRMSG,MSG                                                       
         OI    VIRMSGH+6,TRANSMIT                                               
         L     R1,FADR                                                          
         OI    6(R1),X'40'         SET CURSOR TO FIELD                          
         B     EXIT                                                             
*                                                                               
OKXIT    SR    RB,RB                                                            
*                                                                               
ERRXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT ,                                                                
* CHECK FOR INPUT WITHIN A RANGE OF FIELDS IN THE TWA                           
*                                                                               
* P1 = A(1ST FLD HDR)                                                           
* P2 = A(LAST FLD HDR)                                                          
* P3 = N - IE CHECK EVERY NTH FLD STARTING WITH 1ST (N=2 = EVERY OTHER)         
*                                                                               
* RETURN WITH CC=EQU IF NO CHANGE                                               
         SPACE 1                                                                
         USING FLDHDRD,R3                                                       
         SPACE 1                                                                
CHKINPUT NTR1                                                                   
         L     R3,0(,R1)                                                        
         LM    R5,R6,4(R1)                                                      
         SR    R4,R4                                                            
*                                                                               
CHKI2    TM    FLDATB,PROTECT      CHECK FOR VALIDATED PREVIOUSLY ON            
         BO    *+12                UNPROT FLDS AS THIS IS CLEARED BY            
         TM    FLDIIND,VALPREV     ANY CHANGE                                   
         BNO   ERRXIT                                                           
         LR    R0,R6               COUNT TO SKIP CHECK                          
*                                                                               
CHKI4    IC    R4,0(,R3)                                                        
         BXLE  R3,R4,*+8                                                        
         B     OKXIT               END OF FLD RANGE                             
         BCT   R0,CHKI4                                                         
         B     CHKI2                                                            
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
* GET AN OVERLAY SCREEN - SCREEN = OVERLAY NUMBER                               
*                                                                               
* RETURN WITH CC=NEQ IF UNSUCCESSFUL                                            
         SPACE 1                                                                
GETSCRN  NTR1                                                                   
         GOTO1 VCALLOV,DMCB,(SCREEN,VIRTABH),0,0                                
         CLI   4(R1),X'FF'                                                      
         BNE   *+16                                                             
         MVI   FERN,NOTAVAIL                                                    
         MVI   SCREEN,0                                                         
         B     ERRXIT                                                           
         LA    R3,VIRMSGH          RETRANSMIT                                   
         LA    R5,VIRTABH                                                       
         BCTR  R5,0                                                             
         SR    R4,R4                                                            
         OI    6(R3),TRANSMIT                                                   
         IC    R4,0(,R3)                                                        
         BXLE  R3,R4,*-8                                                        
         B     OKXIT                                                            
         EJECT ,                                                                
* CHECK MONTH QUALIFIER IN BUDGET TYPE FIELD AND CONVERT TO BINARY              
*                                                                               
* P1 = A(MONTH QUALIFIER EXPRESSION IN PARENTHESES)                             
*      ON RETURN CONTAINS A(NEXT BYTE AFTER CLOSE PARENTHESIS)                  
* BUDTAB ENTRY IS ADDRESSED BY R7 (FOR BUDTSTAT)                                
* RETURNS 3 BINARY VALUES IN WORK(3), 1ST MNTH NUMBER, 2ND MNTH NUMBER          
* AND # MONTHS                                                                  
* RETURNS CC=NEQ IF ERROR AND FERN SET                                          
         SPACE 1                                                                
CHKMON   NTR1                                                                   
         LR    R2,R1               R2 = ADDR OF PARM LIST                       
         L     R3,0(,R2)           R3 = ADDR OF PERIOD                          
         LR    RF,R3               CHECK FOR CLOSE PARENTHESIS                  
         LA    R0,8                                                             
*                                                                               
CHKMON1  LA    RF,1(,RF)                                                        
         CLI   0(RF),C')'                                                       
         BE    CHKMON2                                                          
         BCT   R0,CHKMON1                                                       
*                                                                               
         MVI   FERN,NOPARNTH                                                    
         B     ERRXIT                                                           
*                                                                               
CHKMON2  LA    RF,1(,RF)           RETURN A(NEXT BYTE AFTER)                    
         ST    RF,0(,R2)                                                        
         LA    R3,1(,R3)                                                        
*                                                                               
CHKMON4  TM    0(R3),X'F0'         HANDLE ALPHA MONTHS                          
         BO    CHKMON10                                                         
         MVI   FERN,INVNUM                                                      
         TM    SAVSTAT,THIRTEEN    IF 13 PERIOD TYPE MUST BE NUMERIC            
         BO    ERRXIT                                                           
         MVI   FERN,INVALID                                                     
         BAS   RE,MTOBIN           CONVERT 1ST MONTH TO BINARY                  
         BNE   ERRXIT              NG,  INVALID INPUT                           
         STC   R1,WORK                                                          
         LR    R5,R1               KEEP 1ST VALUE IN R5                         
         CLI   3(R3),C')'                                                       
         BE    CHKMON19            PROCESS ONE VALID MONTH                      
         CLI   3(R3),C'-'                                                       
         BNE   ERRXIT                                                           
         LA    R3,4(,R3)           CONVERT 2ND MONTH TO BINARY                  
         LA    RE,CHKMON6                                                       
*                                                                               
MTOBIN   LA    RF,MONTHS         CONVERT MMM AT R3 TO BINARY IN R1              
         LA    R1,1                                                             
*                                                                               
MTOBIN2  CLC   0(3,R3),0(RF)                                                    
         BER   RE                                                               
         LA    RF,3(,RF)                                                        
         LA    R1,1(,R1)                                                        
         CH    R1,=H'12'                                                        
         BNH   MTOBIN2             NOT  IN TABLE, INVALID INPUT                 
         BR    RE                                                               
*                                                                               
CHKMON6  BNE   ERRXIT              NG,  INVALID INPUT                           
         B     CHKMON20            PROCESS TWO VALID MONTHS                     
*                                                                               
CHKMON10 MVI   FERN,INVALID                                                     
         BAS   RE,NTOBIN           HANDLE NUMERIC MONTHS/PERIODS                
         STC   R1,WORK                                                          
         LR    R5,R1               KEEP 1ST VALUE IN R5                         
         CLI   0(R3),C')'                                                       
         BE    CHKMON19            PROCESS ONE VALID MONTH                      
         CLI   0(R3),C'-'                                                       
         BNE   ERRXIT                                                           
         LA    R3,1(,R3)                                                        
         MVI   FERN,INVNUM                                                      
         TM    0(R3),X'F0'                                                      
         BNO   ERRXIT                                                           
         LA    RE,CHKMON20         CONVERT 2ND MONTH TO BINARY                  
         MVI   FERN,INVALID                                                     
*                                                                               
NTOBIN   SR    RF,RF               CONVERT NN AT R3 TO BINARY IN R1             
         TM    1(R3),X'F0'         2ND  CHARACTER NUMERIC ?                     
         BNO   NTOBIN10            NO,  SKIP                                    
         LA    RF,1                YES, PACK TWO CHARACTERS                     
         TM    2(R3),X'F0'         3RD  CHARACTER ALSO NUMERIC ?                
         BO    ERRXIT              YES, INVALID INPUT                           
*                                                                               
NTOBIN10 LA    R0,1(RF,R3)                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R3)                                                      
         CVB   R1,DUB                                                           
         LR    R3,R0               RETURN BUMPED ADDR IN R3                     
         BR    RE                                                               
*                                                                               
CHKMON19 LR    R1,R5               2ND VALUE = FIRST                            
*                                                                               
CHKMON20 STC   R1,WORK+1           RETURN 2ND VALUE                             
         SR    R1,R5               CALCULATE # MONTHS                           
         MVI   FERN,STGTREND                                                    
         BNM   CHKMON22                                                         
         TM    SAVSTAT,THIRTEEN    END MONTH LESS THAN START ONLY VALID         
         BO    ERRXIT              IF 12 MONTH FINANCIAL YEAR                   
*                                                                               
         USING BUDTD,R7                                                         
*                                                                               
         TM    BUDTSTAT,FINANCEY   FINANCIAL YEAR OPTION ?                      
*                                                                               
         DROP  R7                                                               
*                                                                               
         BZ    ERRXIT              NO,  START MONTH < END MONTH INVALID         
         CLC   COSTARTM,WORK                                                    
         BH    ERRXIT                                                           
         AH    R1,=H'12'                                                        
*                                                                               
CHKMON22 LA    R1,1(,R1)                                                        
         STC   R1,WORK+2           RETURN # MONTHS                              
         MVI   FERN,INVALID        CHECK RANGE                                  
         CLI   WORK,0                                                           
         BE    ERRXIT                                                           
         CLI   WORK+1,0                                                         
         BE    ERRXIT                                                           
         MVI   FERN,TOOBIG                                                      
         CLI   WORK,13             START MONTH >= 13 ?                          
         BH    ERRXIT              HIGH, INVALID                                
         BL    CHKMON24            LOW,  CONTINUE                               
         TM    SAVSTAT,THIRTEEN    SAME, CHECK FOR 13                           
         BNO   ERRXIT                                                           
*                                                                               
CHKMON24 CLI   WORK+1,13           END   MONTH >=13 ?                           
         BH    ERRXIT              HIGH, INVALID                                
         BL    OKXIT               LOW,  OKAY                                   
         TM    SAVSTAT,THIRTEEN    SAME, CHECK FOR 13                           
         BNO   ERRXIT                                                           
         B     OKXIT                                                            
         EJECT ,                                                                
* CHECK YEAR IN BUDGET TYPE FIELD AND CONVERT TO BINARY                         
*                                                                               
* P1 = A(YEAR IN 2 EBCDIC DIGITS)                                               
* RETURNS BINARY YEAR IN WORK(1)                                                
* RETURNS CC=NEQ IF ERROR AND FERN SET                                          
         SPACE 1                                                                
CHKYR    NTR1                                                                   
         L     R2,0(,R1)                                                        
         TM    0(R2),C'0'                                                       
         BNO   CHKYERR                                                          
         TM    1(R2),C'0'                                                       
         BNO   CHKYERR                                                          
         TM    2(R2),C'0'                                                       
         BO    CHKYERR                                                          
         MVC   DUB(2),0(R2)        USE  YY                                      
         MVC   DUB+2(4),=C'0101'   SET  MMDD TO 0101                            
*                                  CONVERT   TO BINARY                          
         GOTO1 VDATCON,DMCB,(0,DUB),(3,WORK)                                    
*                                                                               
         LLC   R1,WORK             GET  YEAR                                    
         MVI   HALF,0                                                           
         MVC   HALF+1(1),TODAYB    CHECK FOR THIS YEAR +/- 5 YRS                
         LA    R1,5(,R1)                                                        
         CH    R1,HALF                                                          
         BL    CHKYERR                                                          
         SH    R1,=H'10'                                                        
         CH    R1,HALF                                                          
         BNH   OKXIT                                                            
*                                                                               
CHKYERR  MVI   FERN,INVYEAR                                                     
         B     ERRXIT                                                           
         EJECT ,                                                                
* SET LEVEL DATA IN I/O CONTROL BLOCK                                           
*                                                                               
* LEVEL NUMBER IS IN R1 AND A(IOCB) IN R7                                       
         SPACE 1                                                                
         USING IOCBD,R7                                                         
         SPACE 1                                                                
LEVSET   NTR1  BASE=ABASE1,LABEL=N                                              
         NI    IOCSTAT,X'FF'-IOCSCKOF                                           
         MVI   IOCOFLEV,0                                                       
         L     RA,ABASE2                                                        
         LR    RE,R1               SAVE REQUESTED LEVEL                         
         SR    R2,R2               DISP TO LEVEL END                            
         LTR   R1,R1                                                            
         BZ    *+8                                                              
         IC    R2,IOCLLENS-1(R1)   GET LEV LENGTH IF ANY                        
         LLC   R3,IOCDS15                                                       
         LA    R2,3(R2,R3)         PLUS 3 FOR CUL                               
         STC   R2,IOCDLEND                                                      
         SR    R2,R2               DISP TO LEVEL START IS ZERO                  
         SH    R1,=H'1'            OR DISP TO END OF HIGHER LEVEL IF            
         BM    *+16                ANY                                          
         LA    R3,3(,R3)                                                        
         BZ    *+8                                                              
         IC    R2,IOCLLENS-1(R1)                                                
         AR    R2,R3                                                            
         STC   R2,IOCDLSTA                                                      
*&&US                                                                           
         TM    IOCSTAT,IOCSCLDG    TEST SPECIAL CONTRA LEDGER                   
         BNO   EXIT                                                             
         LA    R1,1                                                             
         LA    R2,IOCLLENS                                                      
         LA    R0,4                                                             
         CLC   IOCOFPOS,0(R2)       GET LEVEL FOR OFFICE POSITION               
         BNH   LEVSET3                                                          
         LA    R2,1(R2)                                                         
         AHI   R1,1                                                             
         BCT   R0,*-18                                                          
         DC    H'0'                                                             
*                                                                               
LEVSET3  STC   R1,IOCOFLEV          SAVE LEVEL FOR OFFPOS                       
         CR    RE,R1                REQ LEVEL VS. OFF POS LEVEL                 
         BL    *+8                                                              
         OI    IOCSTAT,IOCSCKOF                                                 
*&&                                                                             
         B     EXIT                                                             
*                                                                               
         DROP  R7                                                               
         EJECT ,                                                                
* EXTRACT AND PRE-VALIDATE AN INPUT FIELD.                                      
*                                                                               
* ADDRESS OF FIELD HEADER IS PASSED IN R1. RETURN WITH:-                        
*                                                                               
*              FADR     = A(INPUT FIELD HEADER)                                 
*              FERN     = MISSING INPUT FIELD IF NO INPUT                       
*              FNDX     = ZERO                                                  
*              FLDH     = INPUT FIELD HEADER                                    
*              FLD      = EXTRACTED & SPACE FILLED INPUT FIELD                  
*                                                                               
* RETURN WITH CC=EQU IF NO INPUT IN FIELD                                       
         SPACE 1                                                                
FVAL     NTR1  BASE=ABASE1,LABEL=N                                              
         L     RA,ABASE2                                                        
         MVI   FNDX,0                                                           
         MVI   FERN,NOINPUT                                                     
         ST    R1,FADR                                                          
         MVC   FLDH,0(R1)                                                       
         MVC   FLD,SPACES                                                       
*                                                                               
FVAL2    TM    1(R1),PROTECT       IF FLD IS PROTECTED                          
         BNO   FVAL4               CONSTRUCT AN INPUT LENGTH                    
         LLC   RE,0(,R1)                                                        
         LA    RF,0(RE,R1)                                                      
         BCTR  RF,0                                                             
         SH    RE,=H'8'                                                         
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   RE,*-10                                                          
         STC   RE,FLDH+5                                                        
*                                                                               
FVAL4    LLC   RE,FLDH+5           MOVE FLD TO FLD                              
         SH    RE,=H'1'                                                         
         BM    FVALX                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),8(R1)                                                     
         MVI   FERN,OK                                                          
*                                                                               
FVALX    CLI   FERN,NOINPUT                                                     
         B     EXIT                                                             
         EJECT ,                                                                
* EXTRACT NAME FROM A RECORD INTO WORK.                                         
*                                                                               
* RECORD IS ADDRESSED BY WORD AT R1.                                            
         SPACE 1                                                                
GETNAME  NTR1  BASE=ABASE1,LABEL=N                                              
         L     RA,ABASE2                                                        
         L     R1,0(,R1)                                                        
         MVC   WORK,SPACES                                                      
         AH    R1,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
GETNAME2 CLI   0(R1),0                                                          
         BE    GETNAMEX                                                         
         LA    R0,3                                                             
         CLI   0(R1),X'20'                                                      
         BE    GETNAME3                                                         
         LA    R0,18                                                            
         CLI   0(R1),X'43'                                                      
         BE    GETNAME3                                                         
         IC    RF,1(,R1)                                                        
         AR    R1,RF                                                            
         B     GETNAME2                                                         
*                                                                               
GETNAME3 IC    RF,1(,R1)                                                        
         SR    RF,R0                                                            
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    RF,*+8                                                           
         B     GETNAMEX                                                         
         MVC   WORK(0),0(R1)                                                    
*                                                                               
GETNAMEX B     EXIT                                                             
         EJECT ,                                                                
* INCREMENT BINARY YM BY N MONTHS                                               
*                                                                               
* P1 B0 = N                                                                     
*    B1-3 = A(YM) - INPUT & RETURNED                                            
         SPACE 1                                                                
BUMPMON  NTR1                                                                   
         L     R3,0(,R1)                                                        
         LLC   R0,0(,R1)                                                        
         LLC   RF,1(,R3)                                                        
         AR    RF,R0                                                            
         CH    RF,=H'12'                                                        
         BNH   BUMPMEX                                                          
         LA    R7,12                                                            
         TM    SAVSTAT,THIRTEEN                                                 
         BZ    BUMPM10                                                          
         CH    RF,=H'13'                                                        
         BNH   BUMPMEX                                                          
         LA    R7,13                                                            
*                                                                               
BUMPM10  SR    RF,R7                                                            
         LLC   RE,0(,R3)                                                        
         LA    RE,1(,RE)                                                        
         STC   RE,0(,R3)                                                        
*                                                                               
BUMPMEX  STC   RF,1(,R3)                                                        
         B     EXIT                                                             
         EJECT ,                                                                
* ACCOUNT FILE I/O EXECUTIVE.                                                   
*                                                                               
* I/O IS EXECUTED ON KEY INTO I/O AREA ADDRESSED BY R1. COMMAND IS              
* PASSED IN THE HIGH ORDER BYTE OF RF AS FOLLOWS:-                              
*                                                                               
*              BITS 0-3 = COMMAND NUMBER (1-5 SEE IOCMNDS)                      
*                   5ON = PASS BACK DELETED RECORDS                             
*                   6ON = READ KEY WITH LOCK AND SAVE REC IN TIA                
*                   7ON = SAVE KEY IN KEYSAVE BEFORE I/O                        
*                                                                               
* RETURN WITH CC=NEQ ON I/O ERROR WITH FERN SET TO ERROR MESSAGE NUM.           
         SPACE 1                                                                
ACCIO    NTR1  BASE=ABASE1,LABEL=N                                              
         L     RA,ABASE2                                                        
         STCM  RF,8,DUB            SAVE COMMAND BYTE                            
         L     R1,0(,R1)                                                        
         ST    R1,AIOAREA          SAVE A(I/O AREA)                             
         SRL   RF,28                                                            
         SLL   RF,3                                                             
         LA    RF,IOCMNDS-8(RF)                                                 
         ST    RF,DMCB             SET A(COMMAND)                               
         TM    DUB,X'04'                                                        
         BZ    *+8                                                              
         OI    DMCB,X'08'          SET TO PASS BACK DELETES                     
         TM    DUB,X'02'                                                        
         BZ    *+8                                                              
         OI    DMCB,X'80'          SET TO READ WITH LOCK                        
         TM    DUB,X'01'                                                        
         BZ    *+10                                                             
         MVC   KEYSAVE,KEY         SAVE KEY                                     
*                                                                               
         GOTO1 VDATAMGR,DMCB,,IOFILE,KEY,AIOAREA                                
         MVI   FERN,OK             SET FIELD ERROR NUMBER                       
         CLI   DMCB+8,0                                                         
         BNE   ACCIO2                                                           
*                                                                               
ACCIO1   TM    DUB,X'02'           IF RECORD FOUND AND LOCKED, SAVE IN          
         BNO   ACCIOX              TIA                                          
         L     RE,AIOAREA                                                       
         LH    RF,ACLENGTH-ACKEYD(RE)                                           
         L     R0,ATIA                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         B     ACCIOX                                                           
*                                                                               
ACCIO2   MVI   FERN,NOTFOUND       ERRORS                                       
         TM    DMCB+8,X'10'        TEST N/F                                     
         BO    ACCIOX                                                           
         MVI   FERN,DELETED                                                     
         TM    DMCB+8,X'02'        TEST DELETED                                 
         BO    ACCIOX                                                           
         MVI   FERN,IOERROR        EOF/ERR/DUP/LOCKS                            
*                                                                               
ACCIOX   CLI   FERN,OK             EXIT WITH CC=EQ IF I/O OK                    
         B     EXIT                                                             
*                                                                               
*                                  LIST OF I/O COMMANDS/FILES                   
IOCMNDS  DS    0CL8                                                             
         DC    C'DMADD   '                                                      
         DC    C'DMRDHI  '                                                      
         DC    C'DMREAD  '                                                      
         DC    C'DMRSEQ  '                                                      
         DC    C'DMWRT   '                                                      
*                                                                               
IOFILE   DC    C'ACCOUNT '                                                      
         EJECT ,                                                                
* CHECK UNIT/LEDGER                                                             
*                                                                               
* P1 = A(UL CODE)                                                               
* P2 = A(I/O CONTROL BLOCK) - SEE IOCBD                                         
* RETURNS UL CODE AND A/C LENGTHS IN IOCLCODE AND IOCLLENS                      
* RETURNS CC=NEQ IF ERROR AND FERN SET                                          
         SPACE 1                                                                
         USING IOCBD,R3                                                         
         SPACE 1                                                                
CHECKUL  NTR1  BASE=ABASE1,LABEL=N                                              
         L     RA,ABASE2                                                        
         LM    R2,R3,0(R1)                                                      
         CLC   0(2,R2),SPACES      SPACES IN UL IS OK                           
         BNE   *+14                                                             
         XC    IOCLCODE(6),IOCLCODE                                             
         B     OKXIT                                                            
         CLC   IOCLCODE,0(R2)      SAME AS LAST                                 
         BE    OKXIT                                                            
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),0(R2)                                                   
         GOTO1 AREAD,AIOAREA2      READ FOR LEDGER                              
         MVI   FERN,LEDGNVAL                                                    
         BNE   ERRXIT                                                           
         SR    R0,R0                                                            
         XC    IOCLCODE(6),IOCLCODE                                             
         L     R5,AIOAREA                                                       
         AH    R5,DATADISP                                                      
*                                                                               
CHKUL2   CLI   0(R5),0             ELEMENT SEARCH                               
         BE    CHKUL12             MISSING STATUS - LOCK OUT                    
         CLI   0(R5),X'14'                                                      
         BE    CHKUL6                                                           
         CLI   0(R5),X'16'                                                      
         BE    CHKUL8                                                           
         CLI   0(R5),X'30'                                                      
         BE    CHKUL10                                                          
*                                                                               
CHKUL4   IC    R0,1(,R5)                                                        
         AR    R5,R0                                                            
         B     CHKUL2                                                           
*                                                                               
CHKUL6   LA    RF,ACCNTRL          SAVE OFFICE DISP. IF A/C NOT C/AC            
         CR    R3,RF                                                            
         BNE   CHKUL7                                                           
*                                                                               
         USING ACLEDGD,R5                                                       
         MVC   SAVLTOFF,ACLTOFF                                                 
         CLI   ACLTLEN,X'20'                                                    
         BL    CHKUL4              OLD ELEMENT                                  
         CLI   ACLTBUD,0                                                        
         BE    CHKUL4              NO BUDGET OVERRIDE                           
         MVC   SAVLTOFF,ACLTBUD    SPECIAL FILTER FOR BUDGETS                   
         B     CHKUL4                                                           
*                                                                               
CHKUL7   DS    0H                  CONTRA UL                                    
         NI    IOCSTAT,X'FF'-IOCSCLDG                                           
         MVI   IOCOFPOS,0                                                       
         MVI   IOCLOOFF,0                                                       
         CLC   0(2,R2),=C'13'      TEST FOR  SPECIAL CONTRAS                    
         BL    CHKUL4                                                           
         CLC   0(2,R2),=C'16'                                                   
         BH    CHKUL4                                                           
         OI    IOCSTAT,IOCSCLDG                                                 
         MVC   IOCOFPOS,ACLTOFF                                                 
         TM    IOCOFPOS,X'40'                                                   
         BNO   ERRXIT                                                           
         NI    IOCOFPOS,X'0F'                                                   
         LLC   R1,IOCOFPOS                                                      
         LA    R1,15+2+2(R1)       ACC(15),WC(2),CUL(2)                         
         STC   R1,IOCLOOFF         SET START OF OFFICE                          
         B     CHKUL4                                                           
*                                                                               
         USING ACHEIRD,R5                                                       
*                                                                               
CHKUL8   DS    0H                  SAVE HIERARCHY LENGTHS IN I/O TABLE          
         MVC   IOCLLENS(1),ACHRLEVA                                             
         MVC   IOCLLENS+1(1),ACHRLEVB                                           
         MVC   IOCLLENS+2(1),ACHRLEVC                                           
         MVC   IOCLLENS+3(1),ACHRLEVD                                           
         MVC   IOCLCODE,0(R2)                                                   
         B     CHKUL4                                                           
*                                                                               
         USING ACSTATD,R5                                                       
*                                                                               
CHKUL10  DS    0H                  CHECK SECURITY                               
         CLC   0(2,R2),=C'1R'                                                   
         BNO   *+12                                                             
         TM    TWAAUTH,X'10'                                                    
         BNO   CHKUL12                                                          
         CLC   TWAAUTH+1(1),ACSTSECY+1                                          
         BNL   OKXIT                                                            
*                                                                               
CHKUL12  MVI   FERN,LOCKOUT        SECURITY ERROR                               
         XC    IOCLCODE(6),IOCLCODE                                             
         B     ERRXIT                                                           
*                                                                               
         DROP  R3,R5               KEEP IT CLEAN                                
         EJECT ,                                                                
         SPACE 1                                                                
PROTVAL  NTR1  BASE=ABASE1,LABEL=N                                              
         L     RA,ABASE2                                                        
         LH    R3,DFSTFLDH         R3/4/5 = BXLE REGS FOR LINE CONTROL          
         AR    R3,R8                                                            
         SR    R4,R4                                                            
         LH    R5,DLSTFLDH                                                      
         AR    R5,R8                                                            
         BCTR  R5,0                                                             
         LLC   R2,FPERLINE                                                      
         BCTR  R2,0                R2 = VALUES PER LINE (INCLUDING NAM)         
*                                                                               
PROTV02  LR    R0,R2                                                            
         IC    R4,0(,R3)           BUMP PAST CONTRA FIELD                       
         AR    R3,R4                                                            
*                                                                               
PROTV03  OI    6(R3),TRANSMIT+PROTECT                                           
         IC    R4,0(,R3)                                                        
         BXLE  R3,R4,*+8                                                        
         B     EXIT                                                             
         BCT   R0,PROTV03                                                       
         B     PROTV02                                                          
         EJECT ,                                                                
* GET AMOUNTS FROM TWA INTO ACCUMS, CHECKING VALIDITY                           
         SPACE 2                                                                
* CHECK ACCOUNT SECURITY                                                        
*                                                                               
* P1 = A(ACCOUNT REC)                                                           
* P2 = A(I/O CONTROL BLOCK)                                                     
* RETURNS LEVEL NUMBER IN P1 B0                                                 
* RETURNS CC=NEQ IF ERROR AND FERN SET AND KEY SET FOR NEXT READ HIGH           
         SPACE 1                                                                
         USING IOCBD,R4                                                         
         USING ACKEYD,R3                                                        
         SPACE 1                                                                
SECHECK  NTR1  BASE=ABASE1,LABEL=N FIND ACCOUNT LEVEL                           
         L     RA,ABASE2                                                        
         LR    R2,R1                                                            
         LM    R3,R4,0(R1)                                                      
         CLC   ACKEYACC+3(12),SPACES                                            
         BE    OKXIT               LEDGER IS LEVEL 0                            
         MVC   SJOFFICE,SPACES                                                  
         LA    R1,TEMP+150                                                      
*                                                                               
         USING ACHEIRD,R1                                                       
*                                                                               
         MVC   ACHRLEVA,IOCLLENS                                                
         MVC   ACHRLEVB,IOCLLENS+1                                              
         MVC   ACHRLEVC,IOCLLENS+2                                              
         MVC   ACHRLEVD,IOCLLENS+3                                              
*                                                                               
         DROP  R1                                                               
*                                                                               
         GOTO1 VACSPLIT,DMCB,(4,(R3)),TEMP+150,CRD                              
         LA    R1,1                                                             
         LA    R5,CRD                                                           
*                                                                               
SECHK2   CLC   ACKEYACC,0(R5)                                                   
         BE    SECHK10                                                          
         LA    R5,L'ACKEYACC(,R5)                                               
         LA    R1,1(,R1)                                                        
         B     SECHK2                                                           
*                                                                               
SECHK10  MVC   FULL,AIOAREA        IF THIS ISNT LEVEL 1 AND ITS A               
         STH   R1,HALF             SINGLE ACCOUNT READ, WE NEED HIGHER          
         LR    R5,R1               LEVELS                                       
         CH    R1,=H'1'                                                         
         BE    SECHK14                                                          
         CLC   IOCLOKEY,IOCHIKEY                                                
         BNE   SECHK14                                                          
         LA    R5,1                                                             
*                                                                               
SECHK12  LR    R1,R5               LOOP TO READ HIGHER LEVEL ACCOUNTS           
         MH    R1,=H'15'                                                        
         LA    R1,CRD-15(R1)                                                    
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),0(R1)                                                    
         GOTO1 AREAD,AIOAREA2                                                   
         BE    *+12                                                             
         MVI   FERN,LEDGNVAL                                                    
         B     SECHKER1                                                         
         L     R3,AIOAREA2                                                      
*                                                                               
SECHK14  SR    R0,R0               GET RELEVANT ELS                             
         LA    R6,ACRECORD                                                      
         XC    DUB,DUB             CLEAR DOUBLE WORD FOR EL ADDS                
         XC    SAVADDR,SAVADDR                                                  
*                                                                               
SECHK16  CLI   0(R6),0                                                          
         BE    SECHK18                                                          
         CLI   0(R6),ACBLELQ                                                    
         BNE   *+12                                                             
         ST    R6,DUB              BALANCE IN DUB                               
         B     SECHK17                                                          
         CLI   0(R6),X'24'                                                      
         BNE   *+12                                                             
         ST    R6,SAVADDR          PROF                                         
         B     SECHK17                                                          
         CLI   0(R6),ACSTELQ                                                    
         BNE   *+8                                                              
         ST    R6,DUB+4            STATUS IN DUB+4                              
*                                                                               
SECHK17  IC    R0,1(,R6)                                                        
         AR    R6,R0                                                            
         B     SECHK16                                                          
*                                                                               
SECHK18  ICM   R6,15,DUB+4         SECURITY LEVEL                               
         BZ    SECHKER                                                          
*                                                                               
         USING ACSTATD,R6                                                       
*                                                                               
         CLC   TWAAUTH+1(1),ACSTSECY+1                                          
         BL    SECHKER                                                          
*                                                                               
SECHK20  LA    RF,ACCNTRL          SET UP OFFICE CODE FOR ACCOUNT               
         CR    RF,R4                                                            
         BNE   SECHK30                                                          
*                                                                               
         MVC   OFFICE,SPACES                                                    
         CLI   SAVLTOFF,C'C'       SJ CLIENT OFFICE                             
         BNE   SECHK21                                                          
         L     RE,SAVADDR                                                       
         LTR   RE,RE                                                            
         BZ    SECHK26                                                          
*                                                                               
         USING ACPROFD,RE                                                       
*                                                                               
         MVC   OFFICE,SJOFFICE     START WITH OFFICE FROM HIGHER LEVEL          
         CLC   ACPROFFC,SPACES                                                  
         BNH   SECHK26                                                          
         MVC   OFFICE,ACPROFFC                                                  
         MVC   SJOFFICE,OFFICE                                                  
         B     SECHK26                                                          
*                                                                               
SECHK21  TM    SAVLTOFF,X'F0'      TEST OFFICE IN FILTER LEDGER                 
         BNO   SECHK24             NO                                           
*                                                                               
         LA    R1,ACSTFILT                                                      
         CLI   SAVLTOFF,C'1'       F1 = FILTER 1                                
         BE    SECHK22                                                          
         LA    R1,ACSTFILT+1                                                    
         CLI   SAVLTOFF,C'2'       F2 = FILTER 2                                
         BE    SECHK22                                                          
         LA    R1,ACSTANAL                                                      
         CLI   SAVLTOFF,C'3'       F3= FILTER 3                                 
         BE    SECHK22                                                          
         LA    R1,ACSTSUB          F4 = SUBCOMPANY                              
*                                                                               
SECHK22  CLI   HALF+1,1            TEST FIRST LEVEL                             
         BE    *+12                YES                                          
         CLI   0(R1),C' '          TEST FILTER PRESENT                          
         BNH   *+10                NO                                           
         MVC   OFFICE(1),0(R1)     SET OFFICE                                   
         B     SECHK26                                                          
*                                                                               
SECHK24  CLI   SAVLTOFF,1          TEST FOR OFFICE IN KEY                       
         BL    SECHK28             NO                                           
         CLI   SAVLTOFF,12                                                      
         BH    SECHK25                                                          
         LLC   R1,SAVLTOFF                                                      
         LA    R1,ACKEYACC+2(R1)                                                
         MVC   OFFICE(1),0(R1)                                                  
         B     SECHK27                                                          
*                                                                               
SECHK25  SR    R1,R1                                                            
         IC    R1,SAVLTOFF                                                      
         SLL   R1,28               SHIFT OUT THE X'40' BIT                      
         SRL   R1,28                                                            
         CH    R1,=H'1'            MUST BE FROM 1 THRU 12                       
         BL    SECHK28                                                          
         CH    R1,=H'12'                                                        
         BH    SECHK28                                                          
         LA    R1,ACKEYACC+2(R1)                                                
         MVC   OFFICE,0(R1)                                                     
         B     SECHK27                                                          
*                                                                               
SECHK26  OC    DUB(4),DUB          TEST FOR LOWEST LEVEL                        
         BZ    SECHK30             NO-SKIP OFFICE SECURITY TEST                 
*                                                                               
SECHK27  CLC   TWAACCS,SPACES      TEST ANY OFFICE ACCESS CONTROL               
         BNH   SECHK28                                                          
*                                                                               
         USING OFFALD,R1                                                        
*                                                                               
         L     R1,AOFFBLK                                                       
         ST    R3,OFFAREC                                                       
         MVC   OFFAOPOS,SAVLTOFF                                                
         MVC   OFFAOFFC,OFFICE                                                  
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         BNE   SECHKER                                                          
*                                                                               
         DROP  R1                                                               
*                                                                               
SECHK28  TM    COMPSTA4,X'01'      TEST NEW OFFICES AGENCY                      
         BO    SECHK30             YES                                          
         CLI   TWAACCS,C'*'        NO-TEST SINGLE OFFICE CONTROL                
         BE    SECHK30             YES                                          
         CLI   OFFILT,C' '         TEST OFFICE FILTER                           
         BNH   SECHK30                                                          
         CLC   OFFILT(1),OFFICE                                                 
         BNE   SECHKER                                                          
*                                                                               
SECHK30  LA    R5,1(,R5)           BUMP LEVEL                                   
         CH    R5,HALF                                                          
         BL    SECHK12                                                          
         BE    *+14                                                             
         MVC   0(1,R2),HALF+1      RETURN LEVEL IN P1 B0                        
         B     OKXIT                                                            
         MVC   AIOAREA,FULL                                                     
         L     R3,AIOAREA                                                       
         B     SECHK14                                                          
*                                                                               
SECHKER  MVI   FERN,LOCKOUT        IF ERROR RETURN OPTIMAL KEY FOR              
*                                                                               
SECHKER1 LLC   R1,IOCLLENS-1(R5)   NEXT READ HIGH                               
         LA    R1,ACKEYACC+3(R1)                                                
         MVI   0(R1),X'FF'                                                      
         LA    R1,KEY                                                           
         CLI   IOCLOKEY,ACBTKTEQ                                                
         BNE   *+12                                                             
         MVI   KEY,ACBTKTEQ                                                     
         LA    R1,1(,R1)                                                        
         MVC   0(L'KEY,R1),ACKEYACC                                             
         B     ERRXIT                                                           
*                                                                               
         DROP  R3,R4,R6                                                         
         EJECT ,                                                                
*              LITERALS ETC.                                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
         SPACE 1                                                                
RELO     DS    F                                                                
SAVADDR  DS    A                                                                
SJOFFICE DS    CL2                                                              
*                                                                               
* TABLE OF A&V-TYPES FOR RELOCATING INTO GLOBAL W/S.                            
*                                                                               
*              BYTE 0-3 = A/V-TYPE ADDRESS                                      
*                   1-N = HIGH ORDER BYTE VALUES DELIMITED BY X'FF'             
*                                                                               
ROUTTAB  DS    0X                                                               
         DC    VL3(ACSPLIT),X'00FF'                                             
         DC    AL3(ACTNTAB),X'00FF'                                             
         DC    AL3(MONTHS),X'00FF'                                              
         DC    AL3(FVAL),X'00FF'                                                
         DC    AL3(GETNAME),X'00FF'                                             
         DC    AL3(ACCIO),X'1025273436404250FF'                                 
         DC    AL3(CHECKUL),X'00FF'                                             
         DC    AL3(SECHECK),X'00FF'                                             
         DC    AL3(LEVSET),X'00FF'                                              
         DC    AL3(PROTVAL),X'00FF'                                             
         DC    X'FF'                                                            
*                                                                               
RTYPTAB  DS    0CL6                RECORD TYPE TABLE                            
         DC    CL6'BUDGET'                                                      
         DC    CL6'TYPE'                                                        
         DC    CL6'HELP'                                                        
         DC    CL6'?'                                                           
         DC    X'FF'                                                            
*                                                                               
MONTHS   DC    2CL36'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                      
*                                                                               
ACTNTAB  DS    0CL12               ACTION TABLE COVERED BY DSECT ACTD           
         DC    CL8'BADD    ',AL1(ADD),X'4001FC'                                 
ACTDIS   DC    CL8'BDISPLAY',AL1(DIS),X'4001FC'                                 
ACTCHA   DC    CL8'BCHANGE ',AL1(CHA),X'4001FC'                                 
         DC    CL8'BHELP   ',AL1(HLP),X'2000FB'                                 
         DC    CL8'B?      ',AL1(HLP),X'2000FB'                                 
*                                                                               
         DC    CL8'TADD    ',AL1(ADD),X'8002FE'                                 
         DC    CL8'TDISPLAY',AL1(DIS),X'0002FE'                                 
         DC    CL8'TCHANGE ',AL1(CHA),X'8002FE'                                 
         DC    CL8'TLIST   ',AL1(LIS),X'2002FD'                                 
         DC    CL8'TLNUM   ',AL1(NUM),X'2002FD'                                 
         DC    CL8'TRENAME ',AL1(REN),X'8002FE'                                 
         DC    CL8'TDELETE ',AL1(DEL),X'8002FE'                                 
         DC    CL8'TRESTORE',AL1(RES),X'8002FE'                                 
         DC    CL8'THELP   ',AL1(HLP),X'A002F9'                                 
         DC    CL8'T?      ',AL1(HLP),X'A002F9'                                 
         DC    X'FF'                                                            
         EJECT ,                                                                
*              DSECT TO COVER ACTION TABLE ENTRY                                
         SPACE 1                                                                
ACTD     DSECT                                                                  
ACTDTYPE DS    CL1                 RECORD TYPE 1ST LETTER                       
ACTDNAME DS    CL7                 ACTION NAME                                  
ACTDNUM  DS    CL1                 ACTION NUMBER (EQUATED)                      
ACTDINDS DS    CL1                 INDICATORS                                   
ACTDOVER DS    CL1                 OVERLAY                                      
ACTDSCRN DS    CL1                 SCREEN NUMBER                                
ACTDLEN  EQU   *-ACTD              ENTRY LENGTH                                 
         SPACE 1                                                                
         EJECT ,                                                                
         SPACE 1                                                                
       ++INCLUDE ACBUDDSECT                                                     
         EJECT ,                                                                
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDACCFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDACCFACS                                                      
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACBUD00   08/05/08'                                      
         END                                                                    
