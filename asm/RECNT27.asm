*          DATA SET RECNT27    AT LEVEL 013 AS OF 05/01/02                      
*          DATA SET RECNT27    AT LEVEL 018 AS OF 07/23/97                      
*PHASE T80227A,+0                                                               
         TITLE 'REPPAK CONTRACT - BOP DISPLAY/EDIT T80227'                      
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT27 (T80227) --- BOP DISPLAY/EDIT                    *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 26MAR90 (EFJ) --- ADDED TOMBSTONE (HISTORY LOST)                *             
*                                                                 *             
* CNT HISTORY:                                                    *             
*                                                                 *             
* 04APR90 (EFJ) --- USE GETEL MACRO                               *             
*                                                                 *             
* 02AUG90 (EFJ) --- COMBINE WITH CNT28 (BOP EDIT)                 *             
*                                                                 *             
* 03SEP90 (SK)  --- CHANGE BOOK ENTRIES                           *             
*                                                                 *             
* 07JAN92 (SKU) --- FIX BUG THAT CLEARS OUT VREGENSC              *             
*                                                                 *             
* 07NOV94 (SKU) --- FIX BUG OF WRONG USING IN PTRS ROUTINE        *             
*                                                                 *             
* 06OCT95 (SKU) --- 2K CONTRACT SUPPORT                           *             
*                                                                 *             
* 23JUL97 (SKU) --- 4K CONTRACT SUPPORT                           *             
*                                                                 *             
* 24JUL97 (SKU) --- 8D/8E PTRS ADDED                              *             
*                                                                 *             
* 07MAR02 (RHV) --- BOOK TYPE ADDED                               *             
*                                                                 *             
*******************************************************************             
*                                                                               
T80227   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80227                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         L     R2,4(R1)                                                         
         CLC   =C'DISP',0(R2)      DISPLAY BOP?                                 
         BE    DISP                                                             
         CLC   =C'EDIT',0(R2)      EDIT BOP?                                    
         BE    EDIT                                                             
         DC    H'0'                DIE OTHERWISE                                
         EJECT                                                                  
DISP     DS    0H                                                               
         GOTO1 VLOAD,DMCB,(X'80',0)                                             
         GOTO1 VFOUTBLK,DMCB,BOPORDH,BOPLAST,0                                  
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   BOPDISX                                                          
         USING RCONBPEL,R6                                                      
         MVC   BOPORD(2),=C'NO'                                                 
         CLI   RCONBORD,C'Y'                                                    
         BNE   *+10                                                             
         MVC   BOPORD(3),=C'YES'                                                
         MVC   BOPTIME,RCONBPTM                                                 
         MVC   BOPBOBJ,RCONBPOB                                                 
         MVC   BOPBOOK(6),RCONBBKS                                              
*                                  BOOK TYPE DISPLAY                            
         CLC   BOPBOOK(6),MYSPACES                                              
         BE    DIS4                NO BOOK                                      
         CLI   RCONBBKT,0                                                       
         BE    DIS4                NO BOOK TYPE                                 
         CLI   RCONBBKT,C' '                                                    
         BE    DIS4                NO BOOK TYPE                                 
         LA    RF,BOPBOOK+6                                                     
         BCTR  RF,0                DISPLAY BOOK TYPE AFTER BOOK                 
         CLI   0(RF),C' '                                                       
         BE    *-6                                                              
         MVC   1(3,RF),=C'( )'                                                  
         MVC   2(1,RF),RCONBBKT                                                 
*                                                                               
DIS4     CLI   RCONBPDM,X'FF'      VALIDATED BY DEMOVAL?                        
         BNE   DIS5A               NO, IT'S AN OLD CON                          
*                                                                               
* CONVERT TO TEXT                                                               
         LA    R2,BOPDEMOH         DEMOS                                        
         LA    R4,RBUYREC                                                       
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'R'                                                    
         SPACE 1                                                                
         LA    R5,WORK2                                                         
         XC    WORK2(30),WORK2                                                  
         MVC   0(L'RCONBPDM-1,R5),RCONBPDM+1     DEMOS + ENDING ZERO            
         LA    R3,2                                                             
DIS5     CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
         LA    R5,3(R5)                                                         
         BCT   R3,DIS5                                                          
         SPACE 1                                                                
         LA    R5,WORK2                                                         
         SPACE 1                                                                
***********NOTE- DEMCON IS REALLY DEMOCON - SEE T80280***********               
         SPACE 1                                                                
         GOTO1 DEMCON,DMCB,(2,(R5)),(9,8(R2)),(0,DBLOCKD)                       
         MVC   5(1,R2),DMCB        COPY OUTPUT LEN TO INPUT LEN                 
         B     *+10                                                             
         DROP  R4                                                               
         EJECT                                                                  
DIS5A    MVC   BOPDEMO,RCONBPDM                                                 
         MVC   BOPMRKT(3),RCONBPMK                                              
         MVC   BOPMERC(2),=C'NO'                                                
         CLI   RCONBMER,C'Y'                                                    
         BNE   *+10                                                             
         MVC   BOPMERC(3),=C'YES'                                               
         CLI   RCONBAWK,X'40'                                                   
         BNE   *+8                                                              
         MVI   RCONBAWK,0                                                       
         EDIT  (B1,RCONBAWK),(2,BOPAIRW),ALIGN=LEFT                             
         STC   R0,BOPAIRWH+5                                                    
         SPACE 1                                                                
DIS6     LA    R6,RCONREC                                                       
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   BOPDISX                                                          
         LA    R2,BOPOTH1H                                                      
         SPACE 1                                                                
DIS6A    SR    R3,R3                                                            
         IC    R3,1(R6)                                                         
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),2(R6)       COMMENTS                                     
*                                                                               
*        L     R8,AIO4                                                          
*        GOTO1 VREGENSC,DMCB,(1,(R2)),(R8),DATAMGR,RCONREC,GETTXT               
*        BZ    *+10                                                             
*        MVC   22(24,R2),=C'***CMNT REC NOT FOUND***'                           
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   BOPDISX                                                          
         ZIC   R1,0(R2)            NEXT OUTPUT FIELD                            
         AR    R2,R1                                                            
         B     DIS6A                                                            
         SPACE 1                                                                
BOPDISX  DC    0H'0'                                                            
         GOTO1 VFOUTBLK,DMCB,BOPORDH,BOPLAST,1                                  
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
EDIT     DS    0H                                                               
*          DATA SET RECNT28    AT LEVEL 027 AS OF 07/23/90                      
*                                                                               
* BOP NOT REQ'D FOR ACCTG K'S OR IF STA REC HAS BOP=NO                          
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'           BUILD STATION KEY AND GET RECORD             
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),RCONKSTA                                               
         SPACE                                                                  
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    BOP01                                                            
         DC    H'0',C'STA REC NOT FOUND'                                        
BOP01    GOTO1 VGETREC,DMCB,IOAREA                                              
         TM    RSTASTAT,X'80'      BOP CHECK OVERRIDE                           
         BO    BOP4A               YES                                          
         CLC   =C'ACC-',CONBUY                                                  
         BE    BOP4A                                                            
         CLC   =C'GEN',CONADV                                                   
         BNE   BOP4B                                                            
         CLC   =C'ZZ',CONCAT                                                    
         BNE   BOP4B                                                            
BOP4A    L     R0,ABUYFH                                                        
         AR    R0,RA                                                            
         C     R0,LASTFLD                                                       
         BNL   EXXMOD                                                           
BOP4B    DS    0H                                                               
         CLC   CONACT,=C'ADDR'                                                  
         BE    BOP4                                                             
         CLC   CONACT,=C'ADDB'                                                  
         BE    BOP4                                                             
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                                                               
BOP4     DS    0H                                                               
         GOTO1 PTRS,DMCB,WORK2     BUILD OLD POINTER IN WORK2                   
         GOTO1 VDELELEM,DMCB,(X'10',RCONREC)      DELETE OLD BOP                
         GOTO1 VDELELEM,DMCB,(X'11',RCONREC)      AND COMMENTS                  
BOP5     MVC   WORK3(2),=X'105A'                  CODE/ LENGTH = 90             
         MVI   WORK3+2,C' '                                                     
         MVC   WORK3+3(87),WORK3+2                                              
         SPACE 1                                                                
         LA    R4,WORK3                                                         
         USING RCONBPEL,R4                                                      
         SPACE 1                                                                
EDTBOP   LA    R3,2                INVALID INPUT FIELD                          
         MVI   RCONBORD,C'N'                                                    
         LA    R2,BOPORDH          ORDER PLACED?                                
         CLI   5(R2),0                                                          
         BE    EDTB1                                                            
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,NO                                                            
         BE    EDTB1                                                            
         EX    R1,YES                                                           
         BNE   ERROR                                                            
         MVI   RCONBORD,C'Y'                                                    
         B     EDTB1                                                            
         SPACE 1                                                                
NO       CLC   8(0,R2),=C'NO '                                                  
YES      CLC   8(0,R2),=C'YES'                                                  
         SPACE 1                                                                
EDTB1    LA    R2,BOPTIMEH                                                      
         XR    R3,R3                                                            
         CLI   5(R2),0                                                          
         BE    EDTB2                                                            
         IC    R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     EDTB2                                                            
         MVC   RCONBPTM(0),8(R2)                                                
         SPACE 1                                                                
EDTB2    LA    R2,BOPBOBJH         BUYING OBJECTIVES                            
         CLI   5(R2),0                                                          
         BE    EDTBK                                                            
         IC    R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     EDTBK                                                            
         MVC   RCONBPOB(0),8(R2)                                                
         SPACE 1                                                                
EDTBK    LA    R2,BOPBOOKH         BOOK                                         
         LA    R3,1                MISSING INPUT FIELD                          
         CLI   5(R2),0                                                          
         BE    ERROR               BOOK IS MANDATORY                            
         LA    R3,2                INVALID INPUT FIELD                          
         LA    R1,BOOKTAB                                                       
         SR    R5,R5                                                            
         SPACE 1                                                                
EDTBKP   CLI   0(R1),X'FF'                                                      
         BE    ERROR               NOT IN TABLE                                 
         IC    R5,0(R1)                                                         
         EX    R5,COMPBKS                                                       
         BE    EDTBKR                                                           
         LA    R1,L'BOOKTAB(R1)                                                 
         B     EDTBKP                                                           
         SPACE 1                                                                
EDTBKR   EX    R5,MOVEBKS                                                       
         TM    1(R1),X'80'                                                      
         BZ    EDTBKT                                                           
         LA    RF,9(R5,R2)         NEEDS YEAR VALIDATION                        
         LA    RE,2                                                             
EDTBKS   CLI   0(RF),C'0'          MUST BE NUMERIC                              
         BL    ERROR                                                            
         CLI   0(RF),C'9'                                                       
         BH    ERROR                                                            
         LA    RF,1(RF)                                                         
         BCT   RE,EDTBKS                                                        
         SPACE 1                                                                
         LA    R5,2(R5)                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   RCONBBKS(0),8(R2)                                                
         SPACE                                                                  
EDTBKT   MVI   RCONBBKT,C' '                                                    
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         CR    R5,RF                                                            
         BE    EDTBKT5                                                          
         BH    ERROR                                                            
         LA    R5,3(R5)                                                         
         CR    R5,RF                                                            
         BNE   ERROR                                                            
         LA    R5,6(R5,R2)                                                      
         CLI   0(R5),C'('                                                       
         BNE   ERROR                                                            
         CLI   2(R5),C')'                                                       
         BNE   ERROR                                                            
         MVC   RCONBBKT,1(R5)                                                   
EDTBKT5  LA    R3,111              ERROR,MUST MATCH BOOK TO RTG SVC             
         CLC   2(3,R1),=C'BIR'     IF BOOK IS BIRCH                             
         BNE   EDTBKW                                                           
         CLC   CONRTG(3),=C'BIR'   RTG SVC MUST BE BIRCH                        
         BNE   ERROR                                                            
         B     EDITDEM                                                          
EDTBKW   CLC   CONRTG(3),=C'BIR'   AND VICE VERSA                               
         BE    ERROR                                                            
         B     EDITDEM                                                          
         SPACE 1                                                                
COMPBKS  CLC   8(0,R2),2(R1)                                                    
MOVEBKS  MVC   RCONBBKS(0),2(R1)                                                
         SPACE 1                                                                
* BYTE 1 = LENGTH OF BOOK - 1 (FOR EXECUTE)                                     
* BYTE 2 = X'80' IF YEAR VALIDATION NEEDED (IE F86)                             
* BYTE 3-8 = VALID BOOKS                                                        
         SPACE 1                                                                
BOOKTAB  DS    0CL8                                                             
         DC    X'0400',C'BIRCH '                                                
         DC    X'0400',C'4BOOK '                                                
         DC    X'0500',C'LATEST'                                                
         DC    X'0400',C'2BOOK '                                                
         DC    X'0400',C'3BOOK '                                                
         DC    X'0080',C'F     '                                                
         DC    X'0080',C'W     '                                                
         DC    X'0180',C'SP    '                                                
         DC    X'0180',C'SU    '                                                
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*   EDIT DEMOS - PUT X'FF' IN FIRST BYTE TO FLAG VALIDATED BY DEMOVAL           
EDITDEM  LA    R2,BOPDEMOH         EDIT DEMOS                                   
         GOTO1 VANY                                                             
         XC    RCONBPDM,RCONBPDM                                                
         XC    WORK,WORK                                                        
         LR    R5,RC                                                            
         A     R5,=AL4(RBUYREC-GENOLD)  AREA FOR DBLOCK                         
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'R'                                                    
         MVC   DBFILE,=C'INV'                                                   
         DROP  R5                                                               
         NI    1(R2),X'FF'-X'20'   UNPROTECT FOR DEMOVAL???                     
         GOTO1 VDEMOVAL,DMCB,(R2),(2,WORK),(C'Y',(R5))                          
         LA    R3,INVINP                                                        
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
*                      ELIMINATE EOL MARKER (X'FF')                             
         ZIC   R5,DMCB+4                                                        
         MH    R5,=H'3'                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   RCONBPDM+1(0),WORK                                               
         MVI   RCONBPDM,X'FF'                                                   
*                                                                               
* TEST PROFILE SET TO REQUIRE PRIMARY DEMO                                      
         TM    PROFILES+CNTPDEMB,CNTPDEMA                                       
         BZ    EDTB4                                                            
         LA    R1,RCONBPDM+1                                                    
         LA    R0,8                                                             
         SPACE 1                                                                
EDDEM10  TM    0(R1),X'40'         PRIMARY DEMO CHECK                           
         BO    EDTB4                                                            
         LA    R1,3(R1)                                                         
         BCT   R0,EDDEM10                                                       
         LA    R3,PDEMERR                                                       
         B     ERROR                                                            
         EJECT                                                                  
EDTB4    LA    R2,BOPMRKTH         MARKET                                       
         CLC   BOPBOOK(3),=C'BIR'  IF BOOK IS BIRCH                             
         BNE   EDTB4F                                                           
         LA    R3,2                INVALID INPUT FIELD                          
         CLI   5(R2),0             MARKET FIELD MUST BE LEFT BLANK              
         BNE   ERROR                                                            
         B     EDTB5                                                            
         SPACE 1                                                                
EDTB4F   LA    R3,1                MISSING INPUT FIELD                          
         CLI   5(R2),0                                                          
         BE    ERROR               OTHERWISE, MARKET IS MANDATORY               
         SR    RE,RE                                                            
         LA    R3,2                INVALID INPUT FIELD                          
         CLI   5(R2),3                                                          
         BH    ERROR               ALLOW UP TO 3 CHARACTERS                     
         SPACE 1                                                                
         LA    R1,MKTTAB                                                        
EDTB4K   CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(R1)                                                    
         BE    EDTB4P                                                           
         LA    R1,3(R1)                                                         
         B     EDTB4K                                                           
         SPACE 1                                                                
EDTB4P   MVC   RCONBPMK(3),0(R1)                                                
         B     EDTB5                                                            
         SPACE 1                                                                
MKTTAB   DS    0CL3                                                             
         DC    CL3'ADI'                                                         
         DC    CL3'MSA'                                                         
         DC    CL3'TSA'                                                         
         DC    X'FF'                                                            
         SPACE 1                                                                
EDTB5    LA    R3,2                INVALID INPUT FIELD                          
         MVI   RCONBMER,C'N'                                                    
         LA    R2,BOPMERCH         MERCHANDISING?                               
         CLI   5(R2),0                                                          
         BE    EDTB6                                                            
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,NO                                                            
         BE    EDTB6                                                            
         EX    R1,YES                                                           
         BNE   ERROR                                                            
         MVI   RCONBMER,C'Y'                                                    
         SPACE 1                                                                
EDTB6    LA    R2,BOPAIRWH         AIR WEEK                                     
         GOTO1 VANY                                                             
         GOTO1 VPACK                                                            
         LA    R3,2                INVALID INPUT FIELD                          
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         STC   R0,RCONBAWK                                                      
         CLC   RCONBPTM(60),MYSPACES                                            
         BNE   EDTB7                                                            
         CLC   RCONBPMK(10),MYSPACES                                            
         BE    EDTEND              NO BOP INPUT                                 
         SPACE 1                                                                
EDTB7    MVC   RCONBPDT,TODAY                                                   
         XC    RCONBPRF,RCONBPRF                                                
         CLC   CONACT,=C'ADDR'                                                  
         BE    EDTB7A                                                           
         CLC   CONACT,=C'ADDB'                                                  
         BE    EDTB7A                                                           
         MVC   RCONBPRF,RCONKCON                                                
EDTB7A   GOTO1 VADDELEM,DMCB,RCONREC,WORK3                                      
         SPACE 1                                                                
*                                                                               
* UPDATE SEND ELEMENT TO SHOW BOP CHANGED (UNLESS NO SEND ELEM)                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   EDTB7B                                                           
         OI    4(R6),X'08'         RCONSENF                                     
*                                                                               
* EDIT BOP COMMENT                                                              
EDTB7B   LA    R2,BOPOTH1H         COMMENTS                                     
         LA    R4,4                                                             
         SPACE 1                                                                
EDTB8    CLI   5(R2),0                                                          
         BE    EDTEND                                                           
*                                  STORED COMMENT ROUTINE PENDING               
*        BE    EDTB10                                                           
*                                                                               
*        L     R8,AIO4                                                          
*        GOTO1 VREGENSC,DMCB,(0,(R2)),(R8),DATAMGR,RCONREC,GETTXT               
*        BZ    *+12                                                             
*        L     RD,BASERD           ERROR, RETURN CONTROL TO USER                
*        B     EXXMOD                                                           
*                                                                               
         XR    R1,R1                                                            
         IC    R1,5(R2)            FIELD LENGTH                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+2(0),8(R2)     DATA TO ELEMENT                              
         AH    R1,=H'3'            ELEMENT LENGTH                               
         STC   R1,WORK+1                                                        
         MVI   WORK,X'11'                                                       
         SPACE 1                                                                
         GOTO1 VADDELEM,DMCB,RCONREC,WORK                                       
*                                                                               
*EDTB10   ZIC   R1,0(R2)                                                        
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               NEXT COMMENT LINE                            
         BCT   R4,EDTB8                                                         
         SPACE 1                                                                
EDTEND   EQU   *                                                                
         CLC   CONACT,=C'ADDR'                                                  
         BE    EXXMOD                                                           
         CLC   CONACT,=C'ADDB'                                                  
         BE    EXXMOD                                                           
         GOTO1 PTRS,DMCB,WORK3        BUILD NEW POINTERS IN WORK3               
         LR    R4,RA                                                            
         AH    R4,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,R4                                                       
         GOTO1 ADDPTRS,DMCB,WORK2,WORK3,TWAKADDR                                
         DROP  R4                                                               
         SPACE 1                                                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         SPACE 1                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
         SPACE 2                                                                
* ROUTINE TO BUILD POINTER LIST IN P1                                           
*                                                                               
PTRS     NTR1                                                                   
                                                                                
         XC    WORK,WORK           CLEAR 8D/8E WORK AREA                        
                                                                                
         L     R2,0(R1)                                                         
         XC    0(120,R2),0(R2)                                                  
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   PTRS15                                                           
         SPACE 1                                                                
         MVI   0(R2),X'DC'         BUILD BOP POINTER                            
         MVC   5(2,R2),REPALPHA                                                 
         MVC   7(4,R2),RCONKADV                                                 
         MVC   18(5,R2),RCONKSTA                                                
         MVC   23(4,R2),RCONKCON                                                
                                                                                
         USING RCONBPEL,R6                                                      
         MVC   11(3,R2),RCONBPDT                                                
         MVC   14(4,R2),RCONBPRF                                                
                                                                                
         LA    R2,40(R2)                                                        
*                                                                               
         EJECT                                                                  
***************************************************************                 
* ADD 8D/8E POINTERS                                                            
                                                                                
*   CREATE X'8D' POINTERS                                                       
                                                                                
                                                                                
* - GET DEMO FROM BOP AND SAVE IN WORK+45                                       
* - LOOK FOR DEMO MARKED AS PRIMARY (X'40' IN 1ST BYTE)                         
* - IF NO DEMO MARKED AS PRIMARY, USE 1ST DEMO AS DEFAULT                       
*                                                                               
         LA    RE,RCONBPDM+1                                                    
         LA    RF,6                                                             
         MVC   WORK+45(3),RCONBPDM+1                                            
PTRS10   TM    0(RE),X'40'              IS IT MARKED AS PRIMARY DEMO?           
         BO    PTRS12                   YES                                     
         LA    RE,3(RE)                 NO/BUMP TO NEXT DEMO                    
         MVC   WORK+45(3),0(RE)                                                 
         BCT   RF,PTRS10                                                        
         MVC   WORK+45(3),RCONBPDM+1     NO PRIMARY/USE 1ST AS DEFAULT          
PTRS12   NI    WORK+45,X'FF'-X'40'           CLEAR MAIN DEMO INDICATOR          
         DROP R6                                                                
                                                                                
* ENTER HERE IF NO BOP ELEM BUT WE NEED TO BUILD 8D/8E PTRS                     
* WITH 00 DEMOS WHICH WERE CREATED BY RECNT10                                   
PTRS15   DS    0H                                                               
* - GET FLIGHT START/END DATES AND SAVE IN WORK+40                              
         GOTO1 DATCON,DMCB,(3,RCONDATE),(2,WORK+40)    START DATE               
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(2,WORK+42)  END DATE                 
                                                                                
                                                                                
* BUILD BASIC KEY IN WORK                                                       
         LA    R4,WORK                                                          
         MVI   0(R4),X'8D'                                                      
         MVC   1(2,R4),RCONKREP                                                 
         MVC   8(2,R4),WORK+40    START DATE                                    
         MVC   10(2,R4),WORK+42    END DATE                                     
         MVC   12(4,R4),RCONKCON   CONTRACT NUMBER                              
                                                                                
                                                                                
* ID = 3 = OFFICE(SALESPERSON)/DEMO/CREATION DATE                               
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),3                                                         
         MVC   17(2,R2),RCONKOFF      SALESPERSON OFFICE                        
                                                                                
* CREATION DATE YMD -> BINARY                                                   
         GOTO1 DATCON,DMCB,(3,RCONHDRD),(2,22(R2))                              
         MVC   19(3,R2),WORK+45       DEMO                                      
                                                                                
* END X'8D' PASSIVE POINTER                                                     
                                                                                
         MVC   40(27,R2),0(R2)       SET 8D TO NEXT PTR AREA                    
         LA    R2,40(R2)             BUMP TO NEXT AREA                          
*                                                                               
                                                                                
*   CREATE X'8E' POINTER -  X'8D' ALREADY SET IN THIS AREA                      
         MVI   0(R2),X'8E'                                                      
         MVC   3(5,R2),RCONKSTA                                                 
                                                                                
                                                                                
* END X'8E' PASSIVE POINTERS                                                    
                                                                                
****************************************************************                
                                                                                
         B     EXXMOD                                                           
*                                                                               
         EJECT                                                                  
* ROUTINE TO ADD POINTERS TO FILE                                               
*              P1=A(OLD PTR LIST)                                               
*              P2=A(NEW PTR LIST)                                               
*              P3=A(DISK ADDR)                                                  
ADDPTRS  NTR1                                                                   
         LA    R5,3                3 POINTERS                                   
*                                                                               
         LM    R2,R4,0(R1)                                                      
         MVI   DMOUTBTS,0          FOR PROPER RECOVERY                          
         OI    DMINBTS,X'08'       PASS DELETES                                 
         SPACE 1                                                                
AP15     CLI   0(R2),0                                                          
         BNE   AP25                                                             
         CLI   0(R3),0                                                          
         BE    AP100               NO POINTERS                                  
         B     AP40                NO OLD  JUST NEW                             
         SPACE 1                                                                
AP25     CLC   0(27,R2),0(R3)      SAME                                         
         BE    AP40                                                             
         SPACE 1                                                                
* CHANGE                                                                        
AP30     MVC   KEY,0(R2)                                                        
         MVI   UPDATE,C'Y'                                                      
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 VHIGH                                                            
         BAS   RE,CHECK                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BNE   AP40                                                             
         MVI   KEY+27,X'FF'                                                     
         GOTO1 VWRITE                                                           
         BAS   RE,CHECK                                                         
* ADD NEW PTR                                                                   
AP40     CLI   0(R3),0                                                          
         BE    AP100               NO NEW                                       
         MVC   KEY,0(R3)                                                        
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         BAS   RE,CHECK                                                         
         CLC   KEY(27),KEYSAVE     KEY ALREADY THERE?                           
         BE    *+14                                                             
         MVC   KEY,KEYSAVE                                                      
         B     AP50                                                             
* UNDELETE OLD PTR                                                              
         MVI   KEY+27,0                                                         
         GOTO1 VWRITE                                                           
         BAS   RE,CHECK                                                         
         B     AP100                                                            
* ADD PTR                                                                       
AP50     MVI   KEY+27,0                                                         
         MVC   KEY+28(4),0(R4)     DISK ADDR                                    
         GOTO1 VADD                                                             
         BAS   RE,CHECK                                                         
         SPACE 1                                                                
AP100    EQU   *                                                                
         LA    R2,40(R2)           BUMP TO NEXT POINTERS                        
         LA    R3,40(R3)                                                        
         BCT   R5,AP15                                                          
*                                                                               
         NI    DMINBTS,X'F7'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
                                                                                
         XIT1                                                                   
CHECK    TM    DMCB+8,X'FD'        ALL BUT DELETE                               
         BCR   8,RE                                                             
         DC    H'0'                                                             
         EJECT                                                                  
DEMEDT   LA    R6,REDEMO                                                        
DEMEDT2  CLC   0(7,R5),0(R6)                                                    
         BER   R8                                                               
         SPACE 1                                                                
         LA    R6,7(R6)                                                         
         CLI   0(R6),X'FF'                                                      
         BNE   DEMEDT2                                                          
         SPACE 1                                                                
DEMERR   LA    R2,BOPDEMOH                                                      
         LA    R3,2                INVALID INPUT FIELD                          
         B     ERROR                                                            
         EJECT                                                                  
REDEMO   DS    0F                                                               
         DC    CL7'TN12-17'                                                     
         DC    CL7'FM12-17'                                                     
         DC    CL7'ML12-17'                                                     
         DC    CL7'MN18-24'                                                     
         DC    CL7'MN18-34'                                                     
         DC    CL7'MN18-49'                                                     
         DC    CL7'MN18-54'                                                     
         DC    CL7'MN18-64'                                                     
         DC    CL7'MN18+  '                                                     
         DC    CL7'MN25-44'                                                     
         DC    CL7'MN25-54'                                                     
         DC    CL7'MN25-64'                                                     
         DC    CL7'MN25+  '                                                     
         DC    CL7'MN18-44'                                                     
         DC    CL7'MN12+  '                                                     
         DC    CL7'MN12-24'                                                     
         DC    CL7'MN12-34'                                                     
         DC    CL7'MN35-44'                                                     
         DC    CL7'MN35-54'                                                     
         DC    CL7'MN35-64'                                                     
         DC    CL7'MN35+  '                                                     
         DC    CL7'MN50+  '                                                     
         DC    CL7'MN50-54'                                                     
         DC    CL7'MN50-64'                                                     
         DC    CL7'MN65+  '                                                     
         DC    CL7'MN25-34'                                                     
         DC    CL7'MN35-49'                                                     
         DC    CL7'MN25-49'                                                     
         SPACE 1                                                                
         DC    CL7'WN18-24'                                                     
         DC    CL7'WN18-34'                                                     
         DC    CL7'WN18-49'                                                     
         DC    CL7'WN18-54'                                                     
         DC    CL7'WN18-64'                                                     
         DC    CL7'WN18+  '                                                     
         DC    CL7'WN25-44'                                                     
         DC    CL7'WN25-54'                                                     
         DC    CL7'WN25-64'                                                     
         DC    CL7'WN25+  '                                                     
         DC    CL7'WN18-44'                                                     
         DC    CL7'WN12+  '                                                     
         DC    CL7'WN12-24'                                                     
         DC    CL7'WN12-34'                                                     
         DC    CL7'WN35-44'                                                     
         DC    CL7'WN35-54'                                                     
         DC    CL7'WN35-64'                                                     
         DC    CL7'WN35+  '                                                     
         DC    CL7'WN50+  '                                                     
         DC    CL7'WN50-54'                                                     
         DC    CL7'WN50-64'                                                     
         DC    CL7'WN65+  '                                                     
         DC    CL7'WN25-34'                                                     
         DC    CL7'WN35-49'                                                     
         DC    CL7'WN25-49'                                                     
         SPACE 1                                                                
         DC    CL7'AD18-24'                                                     
         DC    CL7'AD18-34'                                                     
         DC    CL7'AD18-49'                                                     
         DC    CL7'AD18-54'                                                     
         DC    CL7'AD18-64'                                                     
         DC    CL7'AD18+  '                                                     
         DC    CL7'AD25-44'                                                     
         DC    CL7'AD25-54'                                                     
         DC    CL7'AD25-64'                                                     
         DC    CL7'AD25+  '                                                     
         DC    CL7'AD18-44'                                                     
         DC    CL7'AD12+  '                                                     
         DC    CL7'AD12-24'                                                     
         DC    CL7'AD12-34'                                                     
         DC    CL7'AD35-44'                                                     
         DC    CL7'AD35-54'                                                     
         DC    CL7'AD35-64'                                                     
         DC    CL7'AD35+  '                                                     
         DC    CL7'AD50+  '                                                     
         DC    CL7'AD50-54'                                                     
         DC    CL7'AD50-64'                                                     
         DC    CL7'AD55+  '                                                     
         DC    CL7'AD65+  '                                                     
         DC    CL7'AD25-34'                                                     
         DC    CL7'AD35-49'                                                     
         DC    CL7'AD25-49'                                                     
         SPACE 1                                                                
         DC    CL7'FARM'                                                        
         DC    CL7'POLITCL'                                                     
         DC    CL7'RELIGN '                                                     
         DC    CL7'BLACK  '                                                     
         DC    CL7'HSPANIC'                                                     
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTFBD                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013RECNT27   05/01/02'                                      
         END                                                                    
