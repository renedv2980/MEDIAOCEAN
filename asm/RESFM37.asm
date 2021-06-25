*          DATA SET RESFM37    AT LEVEL 084 AS OF 05/01/02                      
*PHASE T81837A                                                                  
         TITLE 'T81837 - RESFM37 -  BUSINESS ACTIVITY REPORT RECORD'            
***********************************************************************         
*                                                                     *         
*  RESFM37 (T81837) --- MAINTENANCE/LIST OF ACTIVITY REPORT           *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 11MAR/98 -- (AST) DATE OF BIRTH                                     *         
*                                                                     *         
*                                                                     *         
*HERE******************************************************************         
T81837   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T81837*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         LA    R2,CONACTH                                                       
         CLC   =C'DEL',CONACT                                                   
         BE    BADRECAC                                                         
         CLC   =C'RES',CONACT                                                   
         BE    BADRECAC                                                         
*                                                                               
         MVI   ACTELOPT,C'N'       DON'T WANT ACTIVITIES ELEMENT                
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY?                                 
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD?                              
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LR                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
NO       LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VK05                                                             
         TM    ARMNAMEH+4,X'20'                                                 
         BNZ   VKX                                                              
         MVI   PREVFLAG,C'N'                                                    
         OI    ARMNAMEH+4,X'20'                                                 
         XC    KEY,KEY                                                          
         XC    TOTCOLS,TOTCOLS                                                  
         B     VKX                                                              
*                                                                               
* SHOW SCREEN WHEN FIRST TIME THROUGH                                           
VK05     DS    0H                                                               
         OC    ARMNAM1,ARMNAM1     HERE FOR FIRST TIME?                         
         BNZ   VK10                NO, DON'T LOAD AGAIN                         
         TWAXC ARMDESCH            CLEAR UNP FIELDS FROM DESC DOWN              
         BAS   RE,CLRTAB           CLEAR TABLE                                  
         BAS   RE,SHOWSCR          DISPLAY SCREEN FROM TABLE                    
*                                                                               
VK10     LA    R2,ARMNAMEH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RACTKEY,R4                                                       
         MVI   RACTTYP,RACTTYPQ                                                 
         MVC   RACTREP,AGENCY                                                   
         MVC   RACTNAME,ARMNAME                                                 
         OC    RACTNAME,SPACES     SPACE PAD THE LABEL NAME                     
*                                                                               
VKX      DS    0H                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                                 
***********************************************************************         
VR       DS    0H                                                               
         XC    TOTCOLS,TOTCOLS     CLEAR TOTAL # COLUMNS                        
         XC    COLNMTAB(MAXINPQ),COLNMTAB  CLEAR TABL & END FLAG                
         XC    SUBTTAB(MAXINPQ),SUBTTAB   CLEAR TABL & END FLAG                 
         MVI   PGBKFND,C'N'        PAGE BREAK NOT FOUND YET                     
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR05                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        REMOVE DESCRIPTION ELEMENT                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'02'        REMOVE COMMENT ELEMENT                       
         GOTO1 REMELEM                                                          
         BAS   RE,CLRREC           CLEAR RECORD OF ALL SIMILAR ENTRIES          
*                                                                               
VR05     DS    0H                                                               
         LA    R6,ELEM                                                          
         USING RACTDESD,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   RACTDCDE,RACTDCDQ   DESCRIPTION ELEMENT CODE                     
         MVI   RACTDELN,RACTDOV    ELEMENT LENGTH                               
*                                                                               
         LA    R2,ARMDESCH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         ZIC   R1,5(R2)            MOVE IN LABEL DESCRIPTION                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RACTDESC(0),8(R2)                                                
*                                                                               
         ZIC   R0,RACTDELN         MOVE IN TOTAL LENGTH (VARIABLE)              
         ZIC   R1,5(R2)            FOR THIS ELEMENT                             
         AR    R0,R1               TOTAL LEN=OVERHEAD+LEN(DESC)                 
         STC   R0,RACTDELN                                                      
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR09     LA    R2,ARMNAM1H         FIRST OF 'SIMILAR' SCREEN FIELDS             
         LA    R3,SCRTAB           FIRST SCREEN TABLE ENTRY                     
*                                                                               
VR10     DS    0H                                                               
         LA    R6,ELEM                                                          
         USING RACTELTP,R6                                                      
         XC    ELEM,ELEM                                                        
*                                                                               
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    VR50               YES, CHECK COMMENT LINE                       
         LA    R1,ARMCOMH                                                       
         CR    R2,R1               ARE WE PAST END OF 'NORMAL' INPUT?           
         BNL   VR50               DONE, CHECK COMMENT LINE                      
*                                                                               
         MVC   RACTELCD,21(R3)     MOVE IN ELEM CODE FROM TABLE                 
         MVI   RACTELLN,RACTELLQ   MOVE LENGTH INTO ELEM (7)                    
*                                                                               
         BAS   RE,NEXTFLD          POINT TO 'MAX LGTH' FIELD                    
         BAS   RE,NEXTFLD          POINT R2 TO 'REQ LGTH'                       
*                                                                               
VR11     DS    0H                                                               
         CLI   5(R2),0             IS REQ LEN BLANK?                            
         BE    VR11A               BLANK, CHECK COL #                           
*                                                                               
         TM    4(R2),X'08'         IS NUMERIC?                                  
         BNO   INVLFLD             NO, ERROR                                    
*                                                                               
         ZIC   RE,5(R2)            GET INPUT LEN                                
         BCTR  RE,0                MINUS 1 FOR EX PACK                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
*                                                                               
         ZIC   RE,17(R3)                                                        
         CR    R0,RE               IS REQ LEN > MAX LEN?                        
         BH    INVLFLD             YES, ERROR                                   
         CLI   16(R3),C'F'         IS MAX LEN FIXED?                            
         BNE   *+10                NO DON'T CHECK FOR SAME LEN                  
         CR    R0,RE               IS REQ LEN = MAX LEN?                        
         BNE   INVLFLD             NO, ERROR                                    
         SR    RE,RE                                                            
         CR    R0,RE               IS REQ LEN = 0?                              
         BE    INVLFLD             YES, ERROR                                   
*                                                                               
         STC   R0,RACTELRL         PUT VALID REQ LEN IN ELEM                    
*                                                                               
VR11A    DS    0H                                                               
         LR    R1,R2                                                            
         BAS   RE,NEXTFLD          BUMP TO COL #                                
         CLI   5(R2),0             BLANK?                                       
         BNE   VR11B               NO, CONTINUE                                 
         CLI   5(R1),0             IS REQ LN BLANK?                             
         BE    VR14                YES, BOTH BLANK, DON'T ADD ELEM              
         B     INVLFLD             NO, COL # IS INVALID IF BLANK                
*                                                                               
VR11B    DS    0H                                                               
         CLI   5(R1),0             IS REQ LN BLANK?                             
         BNE   *+10                                                             
         MVC   RACTELRL,17(R3)     MOVE FIXED INTO REQ LEN                      
*                                                                               
         L     R0,TOTCOLS          FIND TOTAL NUMBER OF COLUMNS                 
         ZIC   R1,RACTELRL                                                      
         LA    R1,1(R1)            ADD ONE FOR A SPACE                          
         AR    R0,R1               ADD REQ LEN TO TOTAL                         
         ST    R0,TOTCOLS                                                       
*                                                                               
         LA    R1,8(R2)            POINT TO FIRST BYTE OF OUTPUT                
         CLI   0(R1),C'P'          IS PAGE BREAK SPECIFIED?                     
         BE    VR12                YES, STORE 'P' & VALID TWO BYTES             
         CLI   0(R1),C'S'          IS SUBTOTAL SPECIFIED?                       
         BE    VR12SB              YES, STORE 'S' & VALID TWO BYTES             
         TM    4(R2),X'08'         IS NUMERIC?                                  
         BO    VR1210              YES - OK                                     
         TM    24(R3),X'80'        PERIOD FIELD?                                
         BZ    INVLFLD             'M' NOT VALID                                
         CLI   0(R1),C'M'          MARKET BUDGET?                               
         BE    VR12SB              YES, STORE 'M' & VALID TWO BYTES             
         B     INVLFLD                                                          
*                                                                               
VR1210   DS    0H                                                               
         ZIC   RE,5(R2)            GET INPUT LEN                                
         BCTR  RE,0                MINUS 1 FOR EX PACK                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         B     VR13                CHECK FOR ERRORS AND ADD ELEM                
*                                                                               
VR12SB   DS    0H                                                               
         MVC   RACTELCP,0(R1)      STORE SUBTOTAL FLAG                          
         B     VR12CL              VALIDATE COLUMN #                            
*                                                                               
VR12     DS    0H                                                               
         CLI   PGBKFND,C'Y'        PAGE BREAK ALREADY EXISTS?                   
         BE    INVLFLD                                                          
         MVC   RACTELCP,0(R1)      STORE PAGE BREAK FLAG                        
         MVI   PGBKFND,C'Y'        SET PAGE BREAK FOUND FLAG                    
*                                                                               
*** VALIDATE PAGE BREAKS - ONLY OFFICE,STATION,S/P  VALID                       
*** MAY DELETE LATER TO ALLOW PAGE BREAKS FOR ALL FIELDS                        
         BAS   RE,VALPB            VALID PAGE BREAK?                            
*                                                                               
VR12CL   LA    R1,1(R1)            POINT TO AFTER THE 'P'                       
*                                                                               
*** VALIDATE BYTES                                                              
         CLI   0(R1),C'0'          CHECK FIRST NUMERIC BYTE                     
         BL    INVLFLD                                                          
         CLI   0(R1),C'9'                                                       
         BH    INVLFLD                                                          
*                                                                               
         CLI   5(R2),3             ARE THERE 3 BYTES INPUT?                     
         BL    VR12A               NO DON'T CHECK FOR SEC NUM BYTE              
*                                                                               
         CLI   1(R1),C'0'          CHECK SECOND NUMERIC BYTE                    
         BL    INVLFLD                                                          
         CLI   1(R1),C'9'                                                       
         BH    INVLFLD                                                          
*                                                                               
VR12A    ZIC   RE,5(R2)            GET INPUT LEN                                
         BCTR  RE,0                MINUS 1 FOR 'P'                              
         BCTR  RE,0                MINUS 1 FOR EX PACK                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R1)                                                      
         CVB   R0,DUB                                                           
         CLI   RACTELCP,C'S'       SUBTOTAL?                                    
         BE    VR13                YES, SKIP                                    
         CLI   RACTELCP,C'M'       MARKET BUD?                                  
         BE    VR13                YES, SKIP                                    
*                                                                               
*** PAGE BREAK CAN ONLY BE WITH FIRST OR SECOND COLUMN FIELD                    
*                                                                               
         LA    RE,1                                                             
         CR    R0,RE               PAGE BREAK FOR COLUMN 1?                     
*        BE    VR13                YES, OK                                      
*        LA    RE,1(RE)                                                         
*        CR    R0,RE               PAGE BREAK FOR COLUMN 2?                     
*        BNE   INVLFLD                                                          
         BNE   PBCOLERR            PAGE BREAK MUST BE ON COL 1                  
*                                                                               
VR13     SR    RE,RE                                                            
         CR    R0,RE               R0 HAS BINARY INPUT COL #                    
         BE    INVLFLD             CAN'T HAVE ZERO VALUE IF 'P' INPUT           
         LA    RE,MAXINPQ          CAN'T BE HIGHER THAN '27'                    
         CR    R0,RE                                                            
         BH    INVLFLD                                                          
*                                                                               
         LA    RE,COLNMTAB         PT TO START OF TABLE                         
         AR    RE,R0               ADVANCE COL # BYTES BEYOND                   
         BCTR  RE,0                DECREMENT TO GET TO RIGHT POS                
         CLI   0(RE),0             IS SPACE EMPTY?                              
         BNE   CLDPERR             CAN'T HAVE DUPLICATE ENTRY                   
*                                                                               
         STC   R0,0(RE)            PUT COL # INTO TABLE IN RIGHT POS            
         STC   R0,RACTELCN                                                      
*                                                                               
         CLI   RACTELCP,C'S'       SUBTOTAL?                                    
         BE    VR13A               YES                                          
         CLI   RACTELCP,C'P'       TOTAL?                                       
         BNE   VR13B               NO, SKIP                                     
VR13A    LR    RF,R0               COPY BINARY INPUT COL #                      
         BCTR  RF,0                DECREM ONCE FOR DISP INTO SUBTTAB            
         LA    RE,SUBTTAB                                                       
         AR    RE,RF               POINT TO CORRECT BYTE                        
         MVI   0(RE),C'T'          SUBTOTAL/TOTAL IN THAT COLUMN                
         DROP  R6                                                               
*                                                                               
VR13B    L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR14     BAS   RE,NEXTFLD          POINT TO NEXT PROT FIELD                     
         LA    R3,TABENTQ(R3)      PT TO NEXT SCREEN TABLE ENTRY                
         B     VR10                ADD NEXT ELEM                                
         EJECT                                                                  
*                                                                               
* ADD COMMENT ELEM                                                              
VR50     DS    0H                                                               
         CLI   PGBKFND,C'Y'        PAGE BREAK FOUND?                            
         BNE   PBFLDERR            NO, MUST PAGE BK ON OFF OR STA               
*                                                                               
         LA    R6,ELEM                                                          
         USING RACTCOM,R6                                                       
         XC    ELEM,ELEM                                                        
         MVI   RACTCMCD,RACTCMCQ   MOVE IN ELEM CODE                            
         MVI   RACTCMLN,RACTCMLQ   MOVE LENGTH INTO ELEM                        
*                                                                               
         LA    R2,ARMRCNH                                                       
         CLI   5(R2),0             COMMENT INPUT?                               
         BE    VR90                NO, EXIT                                     
         CLI   8(R2),C'R'                                                       
         BE    VR55                                                             
         CLI   8(R2),C'C'                                                       
         BNE   INVLFLD                                                          
         B     VR60                                                             
*                                                                               
* PROCESS FOR ROWS OF COMMENTS                                                  
*                                                                               
VR55     MVC   RACTCMRC,8(R2)                                                   
*                                                                               
         LA    R2,ARMCLNH          LENGTH #                                     
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLC   =C'ONE',8(R2)       SINGLE ROW?                                  
         BNE   *+12                                                             
         MVI   RACTCMRL,C'1'                                                    
         B     VR59                                                             
         CLC   =C'ALL',8(R2)       ALL TEXT OF PENDING BUSINESS?                
         BNE   INVLFLD                                                          
         MVI   RACTCMRL,C'A'                                                    
*                                                                               
VR59     LA    R2,ARMCCNH          NO COL # INPUT IF IN ROWS                    
         CLI   5(R2),0                                                          
         BNE   INVLFLD                                                          
         B     VR70                ADD THE ELEMENT                              
*                                                                               
* PROCESS FOR COMMENT BY THE COLUMN                                             
*                                                                               
VR60     DS    0H                                                               
         MVC   RACTCMRC,8(R2)                                                   
         LA    R2,ARMCLNH          LENGTH #                                     
         CLI   5(R2),0             REQ LEN BLANK?                               
         BNE   *+12                                                             
         LA    R0,MAXCMLNQ         PUT MAX CMNT LEN IN RECORD                   
         B     VR61                                                             
         TM    4(R2),X'08'                                                      
         BNO   INVLFLD                                                          
*                                                                               
         ZIC   RE,5(R2)            GET INPUT LEN                                
         BCTR  RE,0                MINUS 1 FOR EX PACK                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
*                                                                               
         SR    RE,RE                                                            
         CR    R0,RE                                                            
         BE    INVLFLD             CAN'T BE ZERO                                
         LA    RE,MAXCMLNQ         CAN'T BE > MAXIMUM COM LEN                   
         CR    R0,RE                                                            
         BH    INVLFLD                                                          
*                                                                               
VR61     STC   R0,RACTCMRL                                                      
*                                                                               
         L     R1,TOTCOLS          FIND TOTAL NUMBER OF COLUMNS                 
         AR    R1,R0               ADD REQ LEN TO TOTAL                         
         LA    R1,1(R1)            ADD ONE FOR A SPACE                          
         ST    R1,TOTCOLS                                                       
*                                                                               
         LA    R2,ARMCCNH          COLUMN #                                     
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         TM    4(R2),X'08'                                                      
         BNO   INVLFLD                                                          
*                                                                               
         ZIC   RE,5(R2)            GET INPUT LEN                                
         BCTR  RE,0                MINUS 1 FOR EX PACK                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
*                                                                               
         SR    RE,RE                                                            
         CR    R0,RE                                                            
         BE    INVLFLD                                                          
         LA    RE,MAXINPQ          CAN'T BE HIGHER THAN '27'                    
         CR    R0,RE                                                            
         BH    INVLFLD                                                          
*                                                                               
         LA    RE,COLNMTAB         PT TO START OF TABLE                         
         AR    RE,R0               ADVANCE COL # BYTES BEYOND                   
         BCTR  RE,0                DECREMENT TO GET TO RIGHT POS                
         CLI   0(RE),0             IS SPACE EMPTY?                              
         BNE   CLDPERR             CAN'T HAVE DUPLICATE ENTRY                   
*                                                                               
         STC   R0,0(RE)            PUT COL # INTO TABLE IN RIGHT POS            
         STC   R0,RACTCMCN                                                      
         DROP  R6                                                               
*                                                                               
VR70     L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* CHECK FOR # COLUMNS ON LINE                                                   
VR90     DS    0H                                                               
         BAS   RE,COLNMCHK         CHECK NUMBER OF COL'S ON PRINT LINE          
         BAS   RE,COLSQCHK         CHECK IF COL #'S IN SEQUENCE                 
         BAS   RE,SUBSQCHK         CHECK IF SUBTOTALS IN SEQ.                   
*                                                                               
VRX      DS    0H                                                               
         B     DR                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE KEY                                                               
***********************************************************************         
DK       DS    0H                                                               
         MVI   PREVFLAG,C'Y'                                                    
         MVC   PREVKEY,KEY         RETURN KEY AFTER LIST SELECT                 
*                                                                               
DK10     DS    0H                                                               
         L     R6,AIO                                                           
         USING RACTREC,R6                                                       
         MVC   ARMNAME,RACTNAME    DISPLAY LABEL NAME                           
         OI    ARMNAMEH+6,X'80'    XMIT FIELD                                   
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DR       DS    0H                                                               
         TWAXC ARMDESCH            CLEAR UNPROT FIELDS FROM DESC DOWN           
         XC    TOTCOLS,TOTCOLS                                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        GET DESCRIPTION ELEMENT                      
         BAS   RE,GETEL                                                         
         USING RACTDESD,R6                                                      
*                                                                               
         CLI   RACTDELN,RACTDOV    ANY DESCRIPTION?                             
         BNH   DR10                                                             
         ZIC   R1,RACTDELN         YES, MOVE IT IN                              
         LA    R0,RACTDOV                                                       
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
                                                                                
         LA    RF,L'ARMDESC        GET LENGTH OF DISPLAY FIELD                  
         CR    R1,RF               IS DESCRIPTION TOO BIG?                      
         BL    DR5                 NO, PRINT NORMALLY                           
         LA    R1,L'ARMDESC        YES, TOO BIG, TRUNCATE                       
         BCTR  R1,0                                                             
*                                                                               
DR5      EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ARMDESC(0),RACTDESC                                              
         OI    ARMDESCH+6,X'80'                                                 
*                                                                               
DR10     DS    0H                                                               
         BAS   RE,CLRTAB           CLEAR PREVIOUS ENTRIES                       
         BAS   RE,LOADTAB          LOAD TABLE WITH RECORD VALUES                
         BAS   RE,SHOWSCR          DISPLAY SCREEN FROM TABLE                    
*                                                                               
*                                                                               
* PRINT COMMENT LINE                                                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        GET COMMENT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         USING RACTCOM,R6                                                       
*                                                                               
* SIMILAR TO PRINTING THE 'SIMILAR' ENTRIES!!                                   
         MVC   ARMRCN,RACTCMRC                                                  
         OI    ARMRCNH+6,X'80'                                                  
         CLI   RACTCMRC,C'R'                                                    
         BNE   DR50                PRINT COMMENTS BY COLUMN                     
*                                                                               
         CLI   RACTCMRL,C'1'                                                    
         BNE   *+14                                                             
         MVC   ARMCLN,=C'ONE'                                                   
         B     *+10                                                             
         MVC   ARMCLN,=C'ALL'                                                   
         OI    ARMCLNH+6,X'80'                                                  
         B     DRX                                                              
*                                                                               
DR50     DS    0H                                                               
         EDIT  RACTCMRL,ARMCLN,ALIGN=LEFT,ZERO=BLANK                            
         OI    ARMCLNH+6,X'80'                                                  
*                                                                               
* ACCUMULATE TOTAL                                                              
*                                                                               
         L     R1,TOTCOLS          ACCUMULATE COLUMN TOTAL FOR DISPLAY          
         ZIC   R0,RACTCMRL         GET REQ LEN                                  
         AR    R1,R0                                                            
         LA    R1,1(R1)            INCREM. FOR SPACE                            
         ST    R1,TOTCOLS                                                       
*                                                                               
         EDIT  RACTCMCN,ARMCCN,ALIGN=LEFT,ZERO=BLANK                            
         OI    ARMCCNH+6,X'80'                                                  
*                                                                               
DRX      DS    0H                                                               
         L     R1,TOTCOLS                                                       
         BCTR  R1,0                DECREM ONCE FOR LAST SPACE                   
         ST    R1,TOTCOLS                                                       
         EDIT  TOTCOLS,ARMCOLW,ZERO=NOBLANK                                     
         OI    ARMCOLWH+6,X'80'                                                 
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS ON SCREEN                                                        
***********************************************************************         
LR       DS    0H                                                               
         CLI   PREVFLAG,C'N'       PREVIOUS KEY EXAMINED?                       
         BE    LR05                                                             
         MVC   KEY,PREVKEY         USE SELECTED KEY                             
         MVI   PREVFLAG,C'N'                                                    
         B     LR10                                                             
*                                                                               
LR05     OC    KEY(L'RACTKEY),KEY  FIRST TIME THRU?                             
         BNZ   LR10                                                             
*                                                                               
         LA    R6,KEY                                                           
         USING RACTKEY,R6                                                       
         MVI   RACTTYP,RACTTYPQ                                                 
         MVC   RACTREP,AGENCY                                                   
         MVC   RACTNAME,ARMNAME                                                 
         OC    RACTNAME,SPACES     SPACE PADDED                                 
         DROP  R6                                                               
*                                                                               
LR10     DS    0H                                                               
         GOTO1 HIGH                                                             
*                                                                               
LR20     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RACTKEY,R6                                                       
         CLI   RACTTYP,RACTTYPQ    MATCHING REC TYPE AND REP CODE               
         BNE   LRX                                                              
         CLC   RACTREP,AGENCY                                                   
         BNE   LRX                                                              
         DROP  R6                                                               
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
* CONSTRUCT ONE LIST LINE                                                       
*                                                                               
         L     R6,AIO                                                           
         USING RACTREC,R6                                                       
         MVC   LISTAR,SPACES                                                    
         MVC   LARNAME,RACTNAME    LABEL NAME                                   
*                                                                               
         CLI   RACTDELN,RACTDOV    ANY DESCRIPTION?                             
         BNH   LR40                                                             
         ZIC   R1,RACTDELN         YES, MOVE IT IN                              
         LA    R0,RACTDOV                                                       
         SR    R1,R0                                                            
         CH    R1,=H'60'           HAS ROOM FOR THIS MUCH                       
         BH    LR30                                                             
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LARDESC(0),RACTDESC                                              
         B     LR40                                                             
*                                                                               
LR30     MVC   LARDESC,RACTDESC                                                 
LR40     GOTO1 LISTMON             SEND LINE TO SCREEN                          
*                                                                               
LRSEQ    GOTO1 SEQ                 GET NEXT RECORD                              
         B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
*                                                                               
CLHIERR  DS    0H                                                               
         LA    R2,ARMRLN1H                                                      
         MVC   RERROR,=AL2(786)    report width must be < 164 char's            
         B     ERREND                                                           
*                                                                               
CLSQERR  DS    0H                                                               
         LA    R2,ARMCOL1H                                                      
         MVC   RERROR,=AL2(787)    COLUMN NUMBERS OUT OF SEQUENCE               
         B     ERREND                                                           
*                                                                               
SUBSQERR DS    0H                                                               
         LA    R2,ARMCOL1H                                                      
         MVC   RERROR,=AL2(812)    SUBTOTALS MUST BE IN CONSEQ. COL'S           
         B     ERREND                                                           
*                                                                               
CLDPERR  MVC   RERROR,=AL2(788)    DUPLICATE COLUMN # ENTRY                     
         B     ERREND                                                           
*                                                                               
NOREPORT DS    0H                                                               
         LA    R2,ARMRLN1H                                                      
         MVC   RERROR,=AL2(789)    NO REPORTING INFO GIVEN                      
         B     ERREND                                                           
*                                                                               
PBFLDERR DS    0H                                                               
         LA    R2,ARMCOL1H                                                      
         MVC   RERROR,=AL2(790)    MUST PAGE BREAK ON OFF OR STA                
         B     ERREND                                                           
*                                                                               
PBCOLERR DS    0H                                                               
         LA    R2,ARMCOL1H                                                      
         MVC   RERROR,=AL2(813)    MUST PAGE BREAK ON 1ST COL                   
         B     ERREND                                                           
*                                                                               
BADRECAC DS    0H                                                               
         MVC   RERROR,=AL2(INVRCACT)                                            
         B     ERREND                                                           
*                                                                               
ERREND   GOTO1 MYERROR             DO A GETTXT CALL                             
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
DISHEAD  DC    CL56'Max   ^Req  ^Col                        Max   ^Req +        
                ^Col'                                                           
DISHEAD2 DC    CL56'Lgth  ^Lgth ^#                          Lgth  ^Lgth+        
                ^#'                                                             
*                                                                               
*********************************************************************           
*  THE INTERNAL TABLE, BABY !!!                                                 
*********************************************************************           
* DISP   ENTRY #  DESCRIPTION                                                   
*********************************************************************           
*                                                                               
*  0      1ST     16 BYTE LITERAL - ONSCREEN NAME FOR THE FIELD                 
*  16     2ND     ONE BYTE CHAR FOR ACTUAL LENGTH, FIXED LEN ('F') OR A         
*                 BLANK                                                         
*  17     3RD     ONE BYTE HEX ACTUAL LEN - TWO BYTE MAX OUTPUT                 
*  18     4TH     ONE BYTE HEX REQUESTED LEN - TWO BYTE MAX OUTPUT              
*  19     5TH     ONE BYTE CHAR FOR COLUMN #, PAGE BREAK ('P') OR ' '           
*  20     6TH     ONE BYTE HEX COLUMN # - TWO BYTE MAX OUTPUT                   
*  21     7TH     ONE BYTE ELEMENT CODE FOR THIS FIELD IN REPORT RECORD         
*  22     8TH     ONE BYTE ELEMENT CODE FOR THIS FILED IN CONRACT REC -         
*                 - X'00' IF IN THE KEY, X'FE' IF NOT IN CONTRACT REC           
*                 OR AN EXCEPTION                                               
*  23     9TH     DISPLACEMENT (IN CONTRACT REC) OF THIS FIELD FROM             
*                 KEY OR START OF ELEM -- 1 BYTE FIELD                          
*  24    10TH     MISC FLAGS:                                                   
*                 X'80' - PERIOD FIELD, ALLOW 'M' IN COLUMN FIELD               
*                                                                               
*  25 BYTES PER TABLE ENTRY --- 'TABENTQ'                                       
*                                                                               
**********************************************************************          
SCRTAB   DS    0H                                                               
OFFTAB   DS    0H                                                               
         DC    CL16'Office'                                                     
         DC    C'F'                                                             
         DC    X'02'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'10'                                                            
         DC    X'00'                                                            
         DC    AL1(RCONKOFF-RCONKEY)                                            
         DC    X'00'                                                            
*                                                                               
TABENTQ  EQU   *-SCRTAB            EQUATED LENGTH FOR A TABLE ENTRY             
*                                                                               
STATAB   EQU   *                                                                
         DC    CL16'Station'                                                    
         DC    C' '                                                             
         DC    X'05'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'12'                                                            
         DC    X'00'                                                            
         DC    AL1(RCONKSTA-RCONKEY)                                            
         DC    X'00'                                                            
*                                                                               
SALTAB   EQU   *                                                                
         DC    CL16'Salesperson'                                                
         DC    C' '                                                             
         DC    X'14'               20 CHAR MAX LENGTH                           
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'14'                                                            
         DC    X'FE'               *** WILL GET FROM SALESP REC                 
         DS    XL1                ***                                           
         DC    X'00'                                                            
*                                                                               
SALCTAB  EQU   *                                                                
         DC    CL16'Salesperson Code'                                           
         DC    C'F'                                                             
         DC    X'03'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'16'                                                            
         DC    X'01'                                                            
         DC    AL1(RCONSAL-RCONELEM)                                            
         DC    X'00'                                                            
*                                                                               
         DC    CL16'Agency'                                                     
         DC    C' '                                                             
         DC    X'14'               20 CHAR MAX LENGTH                           
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'18'                                                            
         DC    X'FE'                                                            
         DS    XL1                                                              
         DC    X'00'                                                            
*                                                                               
         DC    CL16'Advertiser'                                                 
         DC    C' '                                                             
         DC    X'14'               20 CHAR LENGTH                               
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'1A'                                                            
         DC    X'FE'                                                            
         DS    XL1                                                              
         DC    X'00'                                                            
*                                                                               
         DC    CL16'Product'                                                    
         DC    C' '                                                             
         DC    X'14'               20 CHAR LENGTH                               
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'1C'                                                            
         DC    X'FE'                                                            
         DS    XL1                                                              
         DC    X'00'                                                            
*                                                                               
         DC    CL16'Buyer'                                                      
         DC    C' '                                                             
         DC    X'14'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'1E'                                                            
         DC    X'01'                                                            
         DC    AL1(RCONBUYR-RCONELEM)                                           
         DC    X'00'                                                            
*                                                                               
         DC    CL16'Flight Start'                                               
         DC    C'F'                                                             
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'20'                                                            
         DC    X'01'                                                            
         DC    AL1(RCONDATE-RCONELEM)                                           
         DC    X'00'                                                            
*                                                                               
         DC    CL16'Flight End'                                                 
         DC    C'F'                                                             
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'22'                                                            
         DC    X'01'                                                            
         DC    AL1(RCONDATE+3-RCONELEM)                                         
         DC    X'00'                                                            
*                                                                               
         DC    CL16'Flight Weeks'                                               
         DC    C'F'                                                             
         DC    X'02'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'24'                                                            
         DC    X'01'                                                            
         DC    AL1(RCONWKS-RCONELEM)                                            
         DC    X'00'                                                            
*                                                                               
         DC    CL16'Primary Demo'                                               
         DC    C'F'                                                             
         DC    X'06'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'25'                                                            
         DC    X'12'               SAR ELEMENT                                  
         DC    AL1(RSARDEM-RSAREL) FIRST ONE                                    
         DC    X'00'                                                            
*                                                                               
         DC    CL16'Daypart Code'                                               
         DC    C' '                                                             
         DC    X'06'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'26'                                                            
         DC    X'41'                                                            
         DC    AL1(RCPRDPDP-RCPRDPEL)                                           
         DC    X'00'                                                            
*                                                                               
         DC    CL16'Create Date'                                                
         DC    C'F'                                                             
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'11'                                                            
         DC    X'01'                                                            
         DC    AL1(RCONCREA-RCONCODE)                                           
         DC    X'00'                                                            
*                                                                               
         DC    CL16'Last Update Date'                                           
         DC    C'F'                                                             
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'27'                                                            
         DC    X'12'               UNKNOWN                                      
         DC    AL1(RSARXLAD-RSARXEL)                                            
         DC    X'00'                                                            
*                                                                               
         DC    CL16'Status'                                                     
         DC    C' '                                                             
         DC    X'0A'               LENGTH 10                                    
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'13'                                                            
         DC    X'1F'                                                            
         DC    AL1(RCONSTAT-RCONXEL)                                            
         DC    X'00'                                                            
*                                                                               
         DC    CL16'Share Goal'                                                 
         DC    C'F'                                                             
         DC    X'04'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'15'                                                            
         DC    X'12'                                                            
         DC    AL1(RSARXSHG-RSARXEL)                                            
         DC    X'00'                                                            
*                                                                               
         DC    CL16'Market Bud'                                                 
         DC    C'F'                                                             
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'17'                                                            
         DC    X'12'               NEED STATION BUD AND SHARE GOAL              
         DS    XL1                                                              
         DC    X'00'                                                            
*                                                                               
         DC    CL16'Station Bud'                                                
         DC    C'F'                                                             
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'19'                                                            
         DC    X'12'                                                            
         DS    AL1(RSARXBGT-RSARXEL)                                            
         DC    X'00'                                                            
*                                                                               
         DC    CL16'Per1'                                                       
         DC    C'F'                                                             
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'1B'                                                            
         DC    X'FE'               ???? ELEM CODE UNKNOWN                       
         DS    XL1                 DISPLACEMENT UNKNOWN                         
         DC    X'80'               ALLOW 'M' IN COLUMN FLD                      
*                                                                               
         DC    CL16'Per2'                                                       
         DC    C'F'                                                             
         DC    X'08'               30 CHAR MAX LENGTH                           
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'1D'                                                            
         DC    X'FE'               ???? ELEM CODE UNKNOWN                       
         DS    XL1                 DISPLACEMENT UNKNOWN                         
         DC    X'80'               ALLOW 'M' IN COLUMN FLD                      
*                                                                               
         DC    CL16'Per3'                                                       
         DC    C'F'                                                             
         DC    X'08'               30 CHAR LENGTH                               
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'1F'                                                            
         DC    X'FE'               ???? ELEM CODE UNKNOWN                       
         DS    XL1                 DISPLACEMENT UNKNOWN                         
         DC    X'80'               ALLOW 'M' IN COLUMN FLD                      
*                                                                               
         DC    CL16'Per4'                                                       
         DC    C'F'                                                             
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'21'                                                            
         DC    X'FE'               ???? ELEM CODE UNKNOWN                       
         DS    XL1                 DISPLACEMENT UNKNOWN                         
         DC    X'80'               ALLOW 'M' IN COLUMN FLD                      
*                                                                               
         DC    CL16'Per5'                                                       
         DC    C'F'                                                             
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'23'                                                            
         DC    X'FE'               ???? ELEM CODE UNKNOWN                       
         DS    XL1                 DISPLACEMENT UNKNOWN                         
         DC    X'80'               ALLOW 'M' IN COLUMN FLD                      
*                                                                               
         DC    CL16'Contract Number'                                            
         DC    C'F'                ??? SHOULD BE FIXED??                        
         DC    X'08'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'29'                                                            
         DC    X'00'                                                            
         DC    AL1(RCONKCON-RCONKEY)                                            
         DC    X'00'                                                            
*                                                                               
         DC    CL16'Ready To Book'                                              
         DC    C'F'                ??? SHOULD BE FIXED??                        
         DC    X'03'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'30'                                                            
         DC    X'FE'                                                            
         DC    AL1(0)                                                           
         DC    X'00'                                                            
*                                                                               
         DC    CL16'Days to Flight'                                             
         DC    C'F'                ??? SHOULD BE FIXED??                        
         DC    X'04'                                                            
         DS    XL1                                                              
         DS    CL1                                                              
         DS    XL1                                                              
         DC    X'31'                                                            
         DC    X'FE'                                                            
         DC    AL1(0)                                                           
         DC    X'00'                                                            
*                                                                               
         DC    X'FF'                                                            
**************************************************                              
* ROUTINES AND SUBROUTINES                                                      
**************************************************                              
*                                                                               
*** ROUTINE TO BUMP TO NEXT SCREEN FIELD                                        
NEXTFLD  DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*** SUBROUTINE TO CLEAR TABLE OF REQ LGTH AND COL # ENTRIES                     
CLRTAB   NTR1                                                                   
         LA    R3,SCRTAB                                                        
*                                                                               
CT10     CLI   0(R3),X'FF'                                                      
         BE    CTX                                                              
         XC    18(3,R3),18(R3)     CLEARS 3 CONSECUTIVE BYTES                   
         LA    R3,TABENTQ(R3)                                                   
         B     CT10                                                             
CTX      B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*** SUBROUTINE TO CLEAR RECORD OF SIMILAR ENTRIES                               
CLRREC   NTR1                                                                   
         LA    R3,SCRTAB                                                        
         L     R6,AIO                                                           
*                                                                               
CR10     CLI   0(R3),X'FF'                                                      
         BE    CRX                                                              
         MVC   ELCODE,21(R3)                                                    
         GOTO1 REMELEM                                                          
         LA    R3,TABENTQ(R3)                                                   
         B     CR10                                                             
CRX      B     XIT                                                              
*                                                                               
         EJECT                                                                  
*** SUBROUTINE TO PRINT THE 'SIMILAR' FIELDS ON SCREEN                          
SHOWSCR  NTR1                                                                   
*        LA    R2,ARMDESCH         POINT TO DESCRIPTION FIELD                   
*                                                                               
* NOTE: BE CAREFUL IF YOU CHANGE THE SCREEN FIELDS!!!!                          
* THIS RTN ASSUMES THAT THE FIELD R2 PTS TO FIRST IS THE FIELD                  
* JUST PRIOR TO THE HEADER FIELDS, IF YOU CHANGES THE SCREEN,                   
* CHANGE THIS ROUTINE!!!!!!!!!                                                  
*                                                                               
         LA    R2,ARMCOLWH         POINT TO TOT COLUMN FIELD                    
         BAS   RE,NEXTFLD          BUMP TO COLUMN HEADER                        
         MVC   8(L'DISHEAD,R2),DISHEAD   DISPLAY COLUMN HEADER                  
         OI    6(R2),X'80'                                                      
         BAS   RE,NEXTFLD          BUMP TO NEXT HEADER                          
         MVC   8(L'DISHEAD,R2),DISHEAD2  DISPLAY COLUMN HEADER                  
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,ARMNAM1H         POINT TO FIRST LABEL FIELD                   
         LA    R3,SCRTAB                                                        
*                                                                               
SCR10    DS    0H                                                               
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    SCRX                YES, DONE                                    
         LA    R1,ARMCOMH                                                       
         CR    R2,R1               ARE WE PAST END OF 'NORMAL' INPUT?           
         BNL   SCRX                DONE, DONE                                   
*                                                                               
         BAS   RE,PRNTTAB          PRINT ONE TABLE ENTRY                        
*                                                                               
         L     R2,SCRNADD          LOAD SCREEN ADDRESS                          
         LA    R3,TABENTQ(R3)      BUMP TO NEXT TABLE ENTRY                     
         BAS   RE,NEXTFLD          GOTO NEXT PROTECTED FIELD                    
         B     SCR10               CHECK FOR END OF TABLE & PRNT NEXT           
*                                                                               
SCRX     DS    0H                                                               
         LA    R1,MAXCMLNQ                                                      
         EDIT  (R1),ARMACL                                                      
*        MVC   ARMACL,=C'60'                                                    
         OI    ARMACLH+6,X'80'                                                  
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*** ROUTINE TO PRINT CONTENTS OF A TABLE ENTRY                                  
*                                                                               
* ASSUMES R2 POINTS TO A PROTECTED FIELD CORRSPONDING TO TABLE ENTRY            
* AND THAT R3 POINTS TO BEGINING OF THE CORRECT TABLE ENTRY                     
*                                                                               
PRNTTAB  NTR1                                                                   
         MVC   8(L'ARMNAM1,R2),0(R3)   PRINT LITERAL TO PROT FIELD              
         OI    6(R2),X'80'                                                      
*                                                                               
         OC    18(1,R3),18(R3)     NO REQ LEN?                                  
         BZ    PT05                SKIP TOT ACCUMULATION                        
         L     R1,TOTCOLS          ACCUMULATE COLUMN TOTAL FOR DISPLAY          
         ZIC   R0,18(R3)           GET REQ LEN                                  
         AR    R1,R0                                                            
         LA    R1,1(R1)            INCREM. FOR SPACE                            
         ST    R1,TOTCOLS                                                       
*                                                                               
* PRINT 'MAX LGTH' FIELD                                                        
PT05     BAS   RE,NEXTFLD          POINT TO 'MAX LGTH' FIELD                    
         LA    R1,8(R2)            POINT TO BEGINING OF OUTPUT                  
         CLI   16(R3),C' '         IS FIELD BLANK?                              
         BNH   PT10                BLANK, SO DON'T PRINT CHARACTER              
         MVC   0(1,R1),16(R3)      PRINT CHARACTER - SHOULD BE 'F'              
         LA    R1,1(R1)            POINT TO NEXT BYTE ON SCREEN                 
*                                                                               
PT10     DS    0H                                                               
         EDIT  (1,17(R3)),(2,0(R1)),ALIGN=LEFT,ZERO=BLANK                       
         OI    6(R2),X'80'                                                      
*                                                                               
         BAS   RE,NEXTFLD          POINT TO 'REQ LGTH' FIELD                    
         LA    R1,8(R2)            POINT TO BEGINING OF OUTPUT                  
         EDIT  (1,18(R3)),(2,0(R1)),ALIGN=LEFT,ZERO=BLANK                       
         OI    6(R2),X'80'                                                      
*                                                                               
* PRINT 'COL #' FIELD                                                           
         BAS   RE,NEXTFLD          POINT TO 'COL #' FIELD                       
         LA    R1,8(R2)            POINT TO BEGINING OF OUTPUT                  
         CLI   19(R3),C' '         IS FIELD BLANK?                              
         BNH   PT20                BLANK, SO DON'T PRINT CHARACTER              
         MVC   0(1,R1),19(R3)      PRINT CHARACTER - SHOULD BE 'P'              
         LA    R1,1(R1)            POINT TO NEXT BYTE ON SCREEN                 
*                                                                               
PT20     DS    0H                                                               
         EDIT  (1,20(R3)),(2,0(R1)),ALIGN=LEFT,ZERO=BLANK                       
         OI    6(R2),X'80'                                                      
*                                                                               
         ST    R2,SCRNADD          SAVE SCREEN ADDRESS                          
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*** SUBROUTINE TO FILL TABLE WITH FIELDS FROM ACTIVITY RECORD                   
*                                                                               
LOADTAB  NTR1                                                                   
         LA    R3,SCRTAB                                                        
*                                                                               
LT10     DS    0H                                                               
         CLI   0(R3),X'FF'                                                      
         BE    LTX                 END OF TABLE, DONE                           
         L     R6,AIO                                                           
         MVC   ELCODE,21(R3)                                                    
         BAS   RE,GETEL                                                         
         BNE   LT20                SKIP TO NEXT TABLE ENTRY                     
*                                                                               
         USING RACTELTP,R6                                                      
         MVC   18(1,R3),RACTELRL   COPY REQ LEN TO TABLE                        
         MVC   19(1,R3),RACTELCP   COPY PAGE BREAK FLAG                         
         MVC   20(1,R3),RACTELCN   COPY COLUMN NUMBER                           
*                                                                               
LT20     LA    R3,TABENTQ(R3)          GOTO NEXT TABLE ENTRY                    
         B     LT10                                                             
LTX      B     XIT                                                              
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*** SUBROUTINE TO CHECK THE NUMBER OF COLUMNS ON A LINE                         
*                                                                               
COLNMCHK NTR1                                                                   
         L     R1,TOTCOLS                                                       
         SR    R0,R0                                                            
         CR    R1,R0               NO INPUT EXCEPT FOR DESCRIPTION?             
         BE    NOREPORT            YES, NEED AT LEAST ONE ENTRY                 
         LA    R0,MAXCLLNQ                                                      
         BCTR  R1,0                DECREMENT ONCE FOR LAST FIELD                
         CR    R1,R0               DOES TOTAL COLUMNS EXCEED 164?               
         BNH   CNCX                                                             
         EDIT  TOTCOLS,ARMCOLW,ZERO=NOBLANK                                     
         OI    ARMCOLWH+6,X'80'                                                 
         B     CLHIERR                                                          
*        BH    CLHIERR             # COLUMNS TOO HIGH                           
CNCX     B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*** SUBROUTINE TO CHECK IF COLUMN NUMBERS ARE IN SEQUENCE                       
*                                                                               
COLSQCHK NTR1                                                                   
         LA    RE,MAXINPQ                                                       
         LA    R3,COLNMTAB                                                      
         MVI   PRVNMFLG,C'Y'       PREVIOUS NUMBER FLAG                         
*                                                                               
CSC95    DS    0H                                                               
         CLI   0(R3),0             GAP?                                         
         BNE   *+12                NO                                           
         MVI   PRVNMFLG,C'N'       GAP, NO PREVIOUS NUMBER                      
         B     CSC99               CHECK NEXT ENTRY                             
         CLI   PRVNMFLG,C'Y'       NUMBER FOUND, PREVIOUS NUMBER FOUND?         
         BNE   CLSQERR             NO, COL #'S OUT OF SEQUENCE                  
CSC99    LA    R3,1(R3)                                                         
         BCT   RE,CSC95            CHECK NEXT TABLE ENTRY                       
CSCX     B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
SUBSQCHK NTR1                                                                   
         LA    RE,MAXINPQ                                                       
         LA    R3,SUBTTAB                                                       
         MVI   PRVSBFLG,C'Y'       PREVIOUS NUMBER FLAG                         
*                                                                               
SSC95    DS    0H                                                               
         CLI   0(R3),0             GAP?                                         
         BNE   *+12                NO                                           
         MVI   PRVSBFLG,C'N'       GAP, NO PREVIOUS NUMBER                      
         B     SSC99               CHECK NEXT ENTRY                             
         CLI   PRVSBFLG,C'Y'       NUMBER FOUND, PREVIOUS NUMBER FOUND?         
         BNE   SUBSQERR            NO, COL #'S OUT OF SEQUENCE                  
SSC99    LA    R3,1(R3)                                                         
         BCT   RE,SSC95            CHECK NEXT TABLE ENTRY                       
SSCX     B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*** SUBROUTINE TO VALIDATE PAGE BREAKS                                          
*                                                                               
VALPB    NTR1                                                                   
         LA    R0,OFFTAB                                                        
         CR    R3,R0               IS THE PB BK ON THE OFFICE FIELD?            
         BE    VPBX                YES, DONE                                    
         LA    R0,STATAB                                                        
         CR    R3,R0               IS THE PB BK ON THE STATION FIELD?           
         BE    VPBX                YES, DONE                                    
         LA    R0,SALTAB                                                        
         CR    R3,R0               IS THE PB BK ON THE S/P FIELD?               
         BE    VPBX                YES, DONE                                    
         LA    R0,SALCTAB                                                       
         CR    R3,R0               IS THE PB BK ON THE S/PCODE FIELD?           
         BNE   PBFLDERR            NO, ERROR                                    
VPBX     DS    0H                                                               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
* LOCAL STORAGE AREA                                                            
*                                                                               
MAXINPQ  EQU   27                  NUMBER OF 'SIMILAR' FLDS AND COL FLD         
MAXCMLNQ EQU   65                  MAXIMUM COMMENT LENGTH (FOR COLUMN)          
MAXCLLNQ EQU   164                 MAX # OF COLUMNS ON A LINE                   
SAVER2   DS    F                   SAVE POINTER TO STRT ROW OF EACH FLD         
SCRNADD  DS    F                   SAVED SCREEN ADDRESS                         
TOTCOLS  DS    F                   TOTAL NUMBER OF COLUMNS ON LINE              
COLNMTAB DS    (MAXINPQ)X          TABLE OF COLUMN NUMBERS                      
SUBTTAB  DS    (MAXINPQ)X          TABLE OF TOTAL/SUBT SEQ                      
PRVNMFLG DS    CL1                 PREVIOUS NUMBER FOUND FLAG, Y OR N           
PRVSBFLG DS    CL1                 PREVIOUS NUMBER FOUND FLAG, Y OR N           
PGBKFND  DS    CL1                 PAGE BREAK FOUND FLAG                        
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDPARSNIPD                                                     
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMB8D          (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMB9D          (OUR LIST SCREEN OVERLAY)                    
       ++INCLUDE RESFMWORKD                                                     
       ++INCLUDE REGENACT                                                       
       ++INCLUDE REGENCON                                                       
         PRINT ON                                                               
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE            APPLICATION SAVE STORAGE                     
PREVKEY  DS    CL(L'KEY)           FOR LIST TO RETURN TO                        
PREVFLAG DS    C                   (Y/N) PREVIOUS KEY USED INDICATOR            
ISBLANK  DS    C                   (Y/N) PREVENT BLANK LABELS                   
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LARNAME  DS    CL8                 LABEL NAME                                   
         DS    CL2                                                              
LARDESC  DS    CL47                LABEL DESCRIPTION                            
LARLEN   EQU   *-LARNAME                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'084RESFM37   05/01/02'                                      
         END                                                                    
