*          DATA SET RECNT4B    AT LEVEL 006 AS OF 05/01/02                      
*          DATA SET RECNT4B    AT LEVEL 012 AS OF 07/23/97                      
*PHASE T8024BA,+0                                                               
         TITLE 'REPPAK CONTRACT - BOP DISPLAY/EDIT T8024B - COMBO'              
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT27 (T8024B) --- BOP DISPLAY/EDIT - COMBO            *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 24JUL97 (PXZ) --- 8D/8E PTR SUPPORT                             *             
* 23JUL97 (SKU) --- 4K CONTRACT SUPPORT                           *             
* 09OCT95 (SKU) --- 2K CONTRACT SUPPORT                           *             
* 05AUG92 (BU ) --- ORIGINAL INPUT                                *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*                                                                 *             
*******************************************************************             
*                                                                               
T8024B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T8024B                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         L     R2,4(R1)                                                         
*                                                                               
         BAS   RE,SETCOMBO         RETRIEVE AND SET COMBO ELEMENT               
*                                                                               
         CLC   =C'DISP',0(R2)      DISPLAY BOP?                                 
         BE    DISP0000                                                         
         CLC   =C'EDIT',0(R2)      EDIT BOP?                                    
         BE    EDIT0000                                                         
         DC    H'0'                DIE OTHERWISE                                
         EJECT                                                                  
DISP0000 DS    0H                                                               
         GOTO1 VLOAD,DMCB,(X'80',0)                                             
         GOTO1 VFOUTBLK,DMCB,BOPORDH,BOPLAST,0                                  
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0050                                                         
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
         BE    DISP0005            NO BOOK                                      
         CLI   RCONBBKT,0                                                       
         BE    DISP0005            NO BOOK TYPE                                 
         CLI   RCONBBKT,C' '                                                    
         BE    DISP0005            NO BOOK TYPE                                 
         LA    RF,BOPBOOK+6                                                     
         BCTR  RF,0                DISPLAY BOOK TYPE AFTER BOOK                 
         CLI   0(RF),C' '                                                       
         BE    *-6                                                              
         MVC   1(3,RF),=C'( )'                                                  
         MVC   2(1,RF),RCONBBKT                                                 
*                                                                               
DISP0005 CLI   RCONBPDM,X'FF'      VALIDATED BY DEMOVAL?                        
         BNE   DISP0020            NO, IT'S AN OLD CON                          
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
DISP0010 CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
         LA    R5,3(R5)                                                         
         BCT   R3,DISP0010                                                      
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
DISP0020 MVC   BOPDEMO,RCONBPDM                                                 
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
DISP0030 LA    R6,RCONREC                                                       
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0050                                                         
         LA    R2,BOPOTH1H                                                      
         SPACE 1                                                                
DISP0040 SR    R3,R3                                                            
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
         BNE   DISP0050                                                         
         ZIC   R1,0(R2)            NEXT OUTPUT FIELD                            
         AR    R2,R1                                                            
         B     DISP0040                                                         
         SPACE 1                                                                
DISP0050 DC    0H'0'                                                            
         GOTO1 VFOUTBLK,DMCB,BOPORDH,BOPLAST,1                                  
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
EDIT0000 DS    0H                                                               
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
         BE    EDIT0010                                                         
         DC    H'0',C'STA REC NOT FOUND'                                        
EDIT0010 GOTO1 VGETREC,DMCB,IOAREA                                              
         TM    RSTASTAT,X'80'      BOP CHECK OVERRIDE                           
         BO    EDIT0020            YES                                          
         CLC   =C'ACC-',CONBUY                                                  
         BE    EDIT0020                                                         
         CLC   =C'GEN',CONADV                                                   
         BNE   EDIT0030                                                         
         CLC   =C'ZZ',CONCAT                                                    
         BNE   EDIT0030                                                         
EDIT0020 L     R0,ABUYFH                                                        
         AR    R0,RA                                                            
         C     R0,LASTFLD                                                       
         BNL   EXXMOD                                                           
EDIT0030 DS    0H                                                               
         MVI   STACTR,1            SET TO FIRST CONTRACT IN ORDER               
         CLC   CONACT,=C'ADDR'                                                  
         BE    EDIT0039                                                         
         CLC   CONACT,=C'ADDB'                                                  
         BE    EDIT0039                                                         
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC                                             
         B     EDIT0050                                                         
EDIT0039 EQU   *                                                                
*                                                                               
*   FOR ADDR/ADDB CONTRACT ACTIONS, ONLY THE FIRST CONTRACT IS                  
*     PROCESSED.  RECNT10 LOOPS TO GENERATE THE OTHER CONTRACTS                 
*     OF THE SET, AND CALLS THIS MODULE WITHIN THE LOOP.  BECAUSE               
*     THE 'BOP' ELEMENT IS INSERTED IN THE FIRST PASS, AND NOTHING              
*     CHANGES WITHIN IT, NOTHING HAS TO BE DONE HERE.  HENCE THE                
*     EXIT.                                                                     
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
*                                                                               
*  TEST FOR TWA POINTER = CONTRACT COUNTER IN ORDER FOR 1ST PASS                
*                                                                               
         CLC   TWACMBPT,CONCTR     1ST PASS THROUGH FOR ADDR/ADDB?              
         BNE   EXXMOD              NO  - EXIT MODULE                            
         B     EDIT0050            YES - PROCESS IT                             
         DROP  RF                                                               
EDIT0040 EQU   *                                                                
         GOTO1 NEXTCON#,DMCB,1     GET NEXT CONTRACT FOR UPDATE                 
EDIT0050 EQU   *                                                                
         GOTO1 PTRS,DMCB,WORK2     BUILD OLD POINTER IN WORK2                   
         GOTO1 VDELELEM,DMCB,(X'10',RCONREC)      DELETE OLD BOP                
         GOTO1 VDELELEM,DMCB,(X'11',RCONREC)      AND COMMENTS                  
EDIT0060 MVC   WORK3(2),=X'105A'                  CODE/ LENGTH = 90             
         MVI   WORK3+2,C' '                                                     
         MVC   WORK3+3(87),WORK3+2                                              
         SPACE 1                                                                
         LA    R4,WORK3                                                         
         USING RCONBPEL,R4                                                      
         SPACE 1                                                                
EDIT0070 LA    R3,2                INVALID INPUT FIELD                          
         MVI   RCONBORD,C'N'                                                    
         LA    R2,BOPORDH          ORDER PLACED?                                
         CLI   5(R2),0                                                          
         BE    EDIT0100                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,EDIT0080                                                      
         BE    EDIT0100                                                         
         EX    R1,EDIT0090                                                      
         BNE   ERROR                                                            
         MVI   RCONBORD,C'Y'                                                    
         B     EDIT0100                                                         
         SPACE 1                                                                
EDIT0080 CLC   8(0,R2),=C'NO '                                                  
EDIT0090 CLC   8(0,R2),=C'YES'                                                  
         SPACE 1                                                                
EDIT0100 LA    R2,BOPTIMEH                                                      
         XR    R3,R3                                                            
         CLI   5(R2),0                                                          
         BE    EDIT0110                                                         
         IC    R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     EDIT0110                                                         
         MVC   RCONBPTM(0),8(R2)                                                
         SPACE 1                                                                
EDIT0110 LA    R2,BOPBOBJH         BUYING OBJECTIVES                            
         CLI   5(R2),0                                                          
         BE    EDIT0120                                                         
         IC    R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     EDIT0120                                                         
         MVC   RCONBPOB(0),8(R2)                                                
         SPACE 1                                                                
EDIT0120 LA    R2,BOPBOOKH         BOOK                                         
         LA    R3,1                MISSING INPUT FIELD                          
         CLI   5(R2),0                                                          
         BE    ERROR               BOOK IS MANDATORY                            
         LA    R3,2                INVALID INPUT FIELD                          
         LA    R1,BOOKTAB                                                       
         SR    R5,R5                                                            
         SPACE 1                                                                
EDIT0130 CLI   0(R1),X'FF'                                                      
         BE    ERROR               NOT IN TABLE                                 
         IC    R5,0(R1)                                                         
         EX    R5,EDIT0180                                                      
         BE    EDIT0140                                                         
         LA    R1,L'BOOKTAB(R1)                                                 
         B     EDIT0130                                                         
         SPACE 1                                                                
EDIT0140 EX    R5,EDIT0190                                                      
         TM    1(R1),X'80'                                                      
         BZ    EDIT0160                                                         
         LA    RF,9(R5,R2)         NEEDS YEAR VALIDATION                        
         LA    RE,2                                                             
EDIT0150 CLI   0(RF),C'0'          MUST BE NUMERIC                              
         BL    ERROR                                                            
         CLI   0(RF),C'9'                                                       
         BH    ERROR                                                            
         LA    RF,1(RF)                                                         
         BCT   RE,EDIT0150                                                      
         SPACE 1                                                                
         LA    R5,2(R5)                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   RCONBBKS(0),8(R2)                                                
         SPACE                                                                  
EDIT0160 MVI   RCONBBKT,C' '                                                    
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         CR    R5,RF                                                            
         BE    EDIT0165                                                         
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
EDIT0165 LA    R3,111              ERROR,MUST MATCH BOOK TO RTG SVC             
         CLC   2(3,R1),=C'BIR'     IF BOOK IS BIRCH                             
         BNE   EDIT0170                                                         
         CLC   CONRTG(3),=C'BIR'   RTG SVC MUST BE BIRCH                        
         BNE   ERROR                                                            
         B     EDIT0200                                                         
EDIT0170 CLC   CONRTG(3),=C'BIR'   AND VICE VERSA                               
         BE    ERROR                                                            
         B     EDIT0200                                                         
         SPACE 1                                                                
EDIT0180 CLC   8(0,R2),2(R1)                                                    
EDIT0190 MVC   RCONBBKS(0),2(R1)                                                
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
EDIT0200 LA    R2,BOPDEMOH         EDIT DEMOS                                   
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
         BZ    EDIT0220                                                         
         LA    R1,RCONBPDM+1                                                    
         LA    R0,8                                                             
         SPACE 1                                                                
EDIT0210 TM    0(R1),X'40'         PRIMARY DEMO CHECK                           
         BO    EDIT0220                                                         
         LA    R1,3(R1)                                                         
         BCT   R0,EDIT0210                                                      
         LA    R3,PDEMERR                                                       
         B     ERROR                                                            
         EJECT                                                                  
EDIT0220 LA    R2,BOPMRKTH         MARKET                                       
         CLC   BOPBOOK(3),=C'BIR'  IF BOOK IS BIRCH                             
         BNE   EDIT0230                                                         
         LA    R3,2                INVALID INPUT FIELD                          
         CLI   5(R2),0             MARKET FIELD MUST BE LEFT BLANK              
         BNE   ERROR                                                            
         B     EDIT0260                                                         
         SPACE 1                                                                
EDIT0230 LA    R3,1                MISSING INPUT FIELD                          
         CLI   5(R2),0                                                          
         BE    ERROR               OTHERWISE, MARKET IS MANDATORY               
         SR    RE,RE                                                            
         LA    R3,2                INVALID INPUT FIELD                          
         CLI   5(R2),3                                                          
         BH    ERROR               ALLOW UP TO 3 CHARACTERS                     
         SPACE 1                                                                
         LA    R1,MKTTAB                                                        
EDIT0240 CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(R1)                                                    
         BE    EDIT0250                                                         
         LA    R1,3(R1)                                                         
         B     EDIT0240                                                         
         SPACE 1                                                                
EDIT0250 MVC   RCONBPMK(3),0(R1)                                                
         B     EDIT0260                                                         
         SPACE 1                                                                
MKTTAB   DS    0CL3                                                             
         DC    CL3'ADI'                                                         
         DC    CL3'MSA'                                                         
         DC    CL3'TSA'                                                         
         DC    X'FF'                                                            
         SPACE 1                                                                
EDIT0260 LA    R3,2                INVALID INPUT FIELD                          
         MVI   RCONBMER,C'N'                                                    
         LA    R2,BOPMERCH         MERCHANDISING?                               
         CLI   5(R2),0                                                          
         BE    EDIT0270                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,EDIT0080                                                      
         BE    EDIT0270                                                         
         EX    R1,EDIT0090                                                      
         BNE   ERROR                                                            
         MVI   RCONBMER,C'Y'                                                    
         SPACE 1                                                                
EDIT0270 LA    R2,BOPAIRWH         AIR WEEK                                     
         GOTO1 VANY                                                             
         GOTO1 VPACK                                                            
         LA    R3,2                INVALID INPUT FIELD                          
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         STC   R0,RCONBAWK                                                      
         CLC   RCONBPTM(60),MYSPACES                                            
         BNE   EDIT0280                                                         
         CLC   RCONBPMK(10),MYSPACES                                            
         BE    EDIT0320            NO BOP INPUT                                 
         SPACE 1                                                                
EDIT0280 MVC   RCONBPDT,TODAY                                                   
         XC    RCONBPRF,RCONBPRF                                                
         CLC   CONACT,=C'ADDR'                                                  
         BE    EDIT0290                                                         
         CLC   CONACT,=C'ADDB'                                                  
         BE    EDIT0290                                                         
         MVC   RCONBPRF,RCONKCON                                                
EDIT0290 GOTO1 VADDELEM,DMCB,RCONREC,WORK3                                      
         SPACE 1                                                                
*                                                                               
* UPDATE SEND ELEMENT TO SHOW BOP CHANGED (UNLESS NO SEND ELEM)                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   EDIT0300                                                         
         OI    4(R6),X'08'         RCONSENF                                     
*                                                                               
* EDIT BOP COMMENT                                                              
EDIT0300 LA    R2,BOPOTH1H         COMMENTS                                     
         LA    R4,4                                                             
         SPACE 1                                                                
EDIT0310 CLI   5(R2),0                                                          
         BE    EDIT0320                                                         
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
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               NEXT COMMENT LINE                            
         BCT   R4,EDIT0310                                                      
         SPACE 1                                                                
EDIT0320 EQU   *                                                                
         CLC   CONACT,=C'ADDR'                                                  
         BE    EXXMOD                                                           
         CLC   CONACT,=C'ADDB'                                                  
         BE    EXXMOD                                                           
         GOTO1 PTRS,DMCB,WORK3        BUILD NEW POINTERS IN WORK3               
         CLI   STACTR,1            1ST CONTRACT IN PROGRESS?                    
         BNE   EDIT0330            EDIT0090                                     
*                                                                               
* FIRST CONTRACT:  USE DISK ADDRESS STORED IN TWA                               
*                                                                               
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
         B     EDIT0340                                                         
EDIT0330 EQU   *                                                                
*                                                                               
* 2ND-NTH CONTRACT:  USE DISK ADDRESS STORED IN SAVEDADD, DELIVERED             
*    BY ROUTINE 'NEXTCON#' FOR CURRENT CONTRACT IN PROGRESS                     
*                                                                               
         GOTO1 ADDPTRS,DMCB,WORK2,WORK3,SAVEDADD                                
         MVC   KEY+28(4),SAVEDADD                                               
EDIT0340 EQU   *                                                                
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         ZIC   RF,STACTR           CONT# IN PROGRESS                            
         LA    RF,1(RF)                                                         
         STC   RF,STACTR                                                        
         CLC   STACTR,CONCTR       ALL CONTRACTS PROCESSED?                     
         BNH   EDIT0040            NO  - GO BACK FOR NEXT                       
*                                                                               
         LR    RF,RA               YES - RELOAD ORIGINAL CONTRACT               
         AH    RF,=H'4096'         4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         B     EXXMOD                                                           
         EJECT                                                                  
         SPACE 2                                                                
* ROUTINE TO BUILD POINTER LIST IN P1                                           
*                                                                               
PTRS     NTR1                                                                   
         XC    WORK,WORK           CLEAR 8D/9E WORK AREA                        
                                                                                
         L     R2,0(R1)                                                         
         XC    0(40,R2),0(R2)                                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   PTRS15                                                           
         SPACE 1                                                                
         USING RCONBPEL,R6                                                      
         MVI   0(R2),X'DC'         BUILD BOP POINTER                            
         MVC   5(2,R2),REPALPHA                                                 
         MVC   7(4,R2),RCONKADV                                                 
         MVC   11(3,R2),RCONBPDT                                                
         MVC   14(4,R2),RCONBPRF                                                
         MVC   18(5,R2),RCONKSTA                                                
         MVC   23(4,R2),RCONKCON                                                
*********************************************************                       
                                                                                
         LA    R2,40(R2)                                                        
                                                                                
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
                                                                                
         EJECT                                                                  
* ROUTINE TO ADD POINTERS TO FILE                                               
*              P1=A(OLD PTR LIST)                                               
*              P2=A(NEW PTR LIST)                                               
*              P3=A(DISK ADDR)                                                  
ADDPTRS  NTR1                                                                   
         LA    R5,3                3 POINTERS                                   
                                                                                
         LM    R2,R4,0(R1)                                                      
         MVI   DMOUTBTS,0          FOR PROPER RECOVERY                          
         OI    DMINBTS,X'08'       PASS DELETES                                 
         SPACE 1                                                                
AP15     CLI   0(R2),0                                                          
         BNE   ADDP0010                                                         
         CLI   0(R3),0                                                          
         BE    ADDP0050            NO POINTERS                                  
         B     ADDP0030            NO OLD  JUST NEW                             
         SPACE 1                                                                
ADDP0010 CLC   0(27,R2),0(R3)      SAME                                         
         BE    ADDP0030                                                         
         SPACE 1                                                                
* CHANGE                                                                        
ADDP0020 MVC   KEY,0(R2)                                                        
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         BAS   RE,ADDP0060                                                      
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ADDP0030                                                         
         MVI   KEY+27,X'FF'                                                     
         GOTO1 VWRITE                                                           
         BAS   RE,ADDP0060                                                      
* ADD NEW PTR                                                                   
ADDP0030 CLI   0(R3),0                                                          
         BE    ADDP0050            NO NEW                                       
         MVC   KEY,0(R3)                                                        
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         BAS   RE,ADDP0060                                                      
         CLC   KEY(27),KEYSAVE     KEY ALREADY THERE?                           
         BE    *+14                                                             
         MVC   KEY,KEYSAVE                                                      
         B     ADDP0040                                                         
* UNDELETE OLD PTR                                                              
         MVI   KEY+27,0                                                         
         GOTO1 VWRITE                                                           
         BAS   RE,ADDP0060                                                      
         B     ADDP0050                                                         
* ADD PTR                                                                       
ADDP0040 MVI   KEY+27,0                                                         
         MVC   KEY+28(4),0(R4)     DISK ADDR                                    
         GOTO1 VADD                                                             
         BAS   RE,ADDP0060                                                      
         SPACE 1                                                                
ADDP0050 EQU   *                                                                
         LA    R2,40(R2)                                                        
         LA    R3,40(R3)                                                        
         BCT   R5,AP15                                                          
                                                                                
         NI    DMINBTS,X'F7'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
         XIT1                                                                   
ADDP0060 TM    DMCB+8,X'FD'        ALL BUT DELETE                               
         BCR   8,RE                                                             
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*   RETRIEVE AND SAVE COMBO CONTROL ELEMENT FROM CONTRACT RECORD.               
*      ABORT IF NOT FOUND, BECAUSE WE SHOULDN'T EVEN BE IN THIS                 
*      OVERLAY.                                                                 
*                                                                               
SETCOMBO NTR1                                                                   
         MVC   CONKSTA,RCONKSTA    SAVE STATION OF ORIG ORDER                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'        COMBO CONTROL ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    *+6                 FOUND                                        
         DC    H'0'                MUST BE FOUND                                
         ZIC   R2,1(R6)            SAVE IT BY LENGTH                            
         BCTR  R2,0                DECREMENT FOR EX                             
         LA    R3,COMBOCTL         A(STORAGE AREA)                              
         EX    R2,SETC0010                                                      
         B     SETC0020                                                         
SETC0010 MVC   0(0,R3),0(R6)       SAVE IT                                      
*                                                                               
SETC0020 EQU   *                                                                
*                                                                               
*  CALCULATE # OF CONTRACTS IN COMBO BY DIVIDING THE LENGTH OF THE              
*    COMBO CONTROL ELEMENT (MINUS ELEMENT CODE AND LENGTH BYTES)                
*    BY LENGTH OF ONE COMBO CONTRACT ENTRY                                      
*                                                                               
         ZIC   R3,1(R6)            L(COMBO CONTROL ELEMENT)                     
         SR    R2,R2               EVEN REGISTER OF PAIR                        
         BCTR  R3,0                SUBTRACT L(ELEMENT CODE BYTE)                
         BCTR  R3,0                SUBTRACT L(ELEMENT LEN  BYTE)                
         LA    R1,9                L(CONTROL ELEMENT)                           
         DR    R2,R1               DIVIDED LEN BY SIZE TO CALCULATE             
         STC   R3,CONCTR              NUMBER OF ENTRIES                         
*                                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*  CALCULATE DISPLACEMENT INTO COMBO CONTROL ELEMENT, AND EXTRACT               
*    THE APPROPRIATE CONTRACT NUMBER.  RETRIEVE THE CONTRACT RECORD             
*    VIA THE 8C KEY.                                                            
*                                                                               
NEXTCON# NTR1                                                                   
*                                                                               
*  FIRST FIND POSITION IN COMBO CONTROL ELEMENT OF STATION OF                   
*    ORIGINAL ORDER, WHICH HAS BEEN PROCESSED FIRST                             
*       P1  =  RETRIEVE FOR UPDATE IF NOT ZERO                                  
*                                                                               
         L     R5,0(R1)            SET OPTION                                   
         LA    R2,COMBOCTL+2       A(1ST STA IN COMBO CTL ELT)                  
         LA    RF,4                LOOP CONTROL                                 
         LA    RE,1                SET COUNTER                                  
NEXC0010 EQU   *                                                                
         CLC   CONKSTA,0(R2)       STATION OF ORDER FOUND IN ELT?               
         BE    NEXC0020            EDIT0090                                     
         LA    R2,9(R2)            NO  - CHECK NEXT ONE                         
         LA    RE,1(RE)                  INCREMENT COUNT                        
         BCT   RF,NEXC0010         GO BACK FOR IT                               
NEXC0020 EQU   *                                                                
         SR    R2,R2                                                            
         ZIC   R3,STACTR           CONTRACT COUNTER IN PROGRESS                 
         CR    R3,RE               IN PROG VS ORIG STA POSITION                 
         BH    NEXC0030            AFTER ORIGINAL STA POSITION:                 
*                                    NO ADDITIONAL ADJUSTMENT NEEDED            
         BCTR  R3,0                BEFORE ORIGINAL STA POSITION:                
*                                    ADDITIONAL ADJUSTMENT NEEDED               
NEXC0030 EQU   *                                                                
         BCTR  R3,0                MAKE ZERO RELATIVE                           
         LA    R1,9                SIZE OF COMBO CONTROL CONTRACT ELT           
         MR    R2,R1               CALCULATE DISPLACEMENT                       
         LA    R2,COMBOCTL+2       A(COMBO CONTROL ELEMENT)                     
         AR    R2,R3               ADD DISPLACEMENT                             
         L     RE,=X'99999999'     CALC 9'S COMP OF CONTRACT #                  
         MVC   FULL,5(R2)          LOAD CONTRACT #                              
         L     RF,FULL                                                          
         SR    RE,RF               GET 9'S COMP                                 
         ST    RE,FULL             SAVE IT                                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'           INSERT KEY ID                                
         MVC   KEY+21(2),TWAAGY    INSERT REP ID                                
         MVC   KEY+23(4),FULL      INSERT CONTRACT # IN KEY                     
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     CONTRACT FOUND?                              
         BE    *+6                 EDIT0090                                     
         DC    H'0'                NO  - MUST BE PRESENT!!                      
         LTR   R5,R5               RETRIEVE FOR UPDATE?                         
         BZ    NEXC0040            NO                                           
         MVI   UPDATE,YESS         SET RETRIEVE FOR UPDATE                      
NEXC0040 EQU   *                                                                
         MVC   SAVEDADD,KEY+28     SAVE DISK ADDRESS                            
         GOTO1 VGETREC,DMCB,RCONREC                                             
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   LOCAL STORAGE                                                               
*                                                                               
COMBOCTL DS    CL60                STORAGE FOR COMBO CONTROL ELEMENT            
CONCTR   DS    XL1                 CONTRACT COUNTER                             
STACTR   DS    XL1                 STATION COUNT IN PROGRESS                    
CONKSTA  DS    CL5                 STATION OF ORIGINAL ORDER                    
SAVEDADD DS    CL4                 SAVE AREA FOR DISK ADDRESS                   
*                                                                               
*        EQUATES                                                                
YESS     EQU   C'Y'                                                             
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTFBD                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006RECNT4B   05/01/02'                                      
         END                                                                    
