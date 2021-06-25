*          DATA SET DEDEMOMATH AT LEVEL 021 AS OF 07/01/19                      
*PHASE T00ADAB                                                                  
         SPACE 1                                                                
*        P1=A(COMMAND)                                                          
*        P2=A(INPUT RECORD)                                                     
*        P3=A(OUTPUT RECORD) *MAY BE SAME AS P2*                                
*        P4=A(MATHFAC BLOCK)                                                    
         TITLE 'MACRO ROUTINE TO MANIPULATE DEMO RECORDS'                       
DEMOMATH RSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 DMWRKX-DMWRK,DEMOMATH,RR=R5                                      
         USING DMWRK,RC            RC=A(W/S)                                    
         MVC   INPARAM,0(R1)       SAVE INPUT PARAMETERS                        
         L     RE,INPMFAC                                                       
         MVC   MATHFAC,0(RE)       SET CONTROL BLOCK VALUES                     
         L     R6,MTHCFACS                                                      
         USING DBLOCKD,R6          R6=A(DBLOCK)                                 
         L     R2,DBCOMFCS                                                      
         USING COMFACSD,R2         R2=A(COMMON FACILITIES)                      
         MVI   DBERROR,0                                                        
         MVI   DBERRMOD,EDEMOMTH                                                
         CLC   INPIN+1(3),INPOUT+1 TEST IF INADDR EQ OUTADDR                    
         BNE   *+10                                                             
         MVC   MTHOFIL,MTHIFIL     YES - SET OUTPUT FILE EQ INPUT               
         SPACE 2                                                                
         XC    OTPARAM,OTPARAM     BUILD OUTPUT FORMAT BLOCK                    
         MVC   OTSRC,MTHOSRC                                                    
         MVC   OTEFIL,MTHOFIL                                                   
         MVC   OTBOOK,=X'FFFF'     FORCE BOOK TO INPUT BOOK                     
         L     R1,INPIN            GET A(FIRST RECORD ELEMENTS)                 
         LA    RF,MTHIFIL                                                       
         BAS   RE,GETFSTEL                                                      
         ST    R1,ITAQUART                                                      
         MVC   IRECEND,RECEND      SAVE END OF INPUT RECORD                     
         L     R1,INPOUT                                                        
         LA    RF,MTHOFIL                                                       
         BAS   RE,GETFSTEL                                                      
         ST    R1,OTAQUART                                                      
         MVC   ORECEND,RECEND      SAVE END OF OUTPUT RECORD                    
         MVC   SVAQUART,DBAQUART   SAVE DBLOCK VALUES                           
         MVC   SVRADDR,DBAREC                                                   
         MVC   SVFILE,DBFILE                                                    
         MVC   SVINTFIL,DBINTFIL                                                
         MVC   SVACTBK,DBACTBK                                                  
         MVC   SVACTSRC,DBACTSRC                                                
         MVC   DBAREC,INPIN        SET DBLOCK VALUES FOR DEMAINT                
         MVC   DBFILE,MTHIFIL                                                   
         OC    DBAQUART,DBAQUART   TEST IF QRT HOUR ADDRESS SET                 
         BNZ   *+10                                                             
         MVC   DBAQUART,ITAQUART   NO - SET TO FIRST ELEMENT                    
         CLC   DBAQUART+1(3),INPIN+1  ELEMENT ADDR HIGHER THAN REC ADDR         
         BH    *+10                      YES - MIGHT BE OK                      
         MVC   DBAQUART,ITAQUART         NO  - FORCE TO FIRST ELEMENT           
         CLC   DBAQUART+1(3),IRECEND+1  ELEM ADDR LESS THAN REC END             
         BL    *+10                       YES - ACCEPT AS VALID                 
         MVC   DBAQUART,ITAQUART          NO  - FORCE TO FIRST ELEMENT          
         EJECT                                                                  
* GO TO MACRO ROUTINE BASED ON COMMAND                                          
*                                                                               
         MVC   BYTE1,INPCMND       SAVE 1ST BYTE OF PARAM1                      
         ZICM  RE,INPCMND+1,(7)                                                 
         MVC   COMMAND,0(RE)       SAVE INPUT COMMAND                           
         CLC   COMMAND,=C'CON'     CONVERT RECORD TO NEW FORMAT                 
         BE    CNV                                                              
         CLC   COMMAND,=C'REC'     RECALCULATE AND REPLACE RECORD               
         BE    REC                 KEEPING THE SAME FORMAT                      
         CLC   COMMAND,=C'RRO'     ROUND RATINGS AND PUTS ON A RECORD           
         BE    RROUND              (USED FOR REP INVENTORY RECS.)               
         CLC   COMMAND,=C'ADD'     ADD INPUT RECORD TO OUTPUT                   
         BE    ADD                                                              
         CLC   COMMAND,=C'MUL'     MULTIPLY RECORD BY FACTOR                    
         BE    MUL                                                              
         CLC   COMMAND,=C'MAD'     MULTIPLY & ADD RECORD TO OUTPUT              
         BE    MAD                                                              
         CLC   COMMAND,=C'DMA'     MULTIPLY & ADD IN DOUBLE-WORD SLOTS          
         BE    DMAD                                                             
         CLC   COMMAND,=C'DDI'     DIVIDE DOUBLE-WORD SLOTS BY DBDIVSOR         
         BE    DDIV                                                             
         CLC   COMMAND,=C'DIV'     DIVIDE RECORD BY DIVISOR                     
         BNE   *+14                                                             
         MVC   COMMAND,=C'RDI'                                                  
         B     MUL                                                              
         DC    H'0',C'INVALID COMMAND'                                          
         EJECT                                                                  
CNV      GOTO1 MAINT,DMCB,=C'GET',(R6),WRKREC,OTPARAM                           
         MVC   DBAREC,INPOUT                                                    
         MVC   DBAQUART,OTAQUART                                                
         MVC   DBFILE,MTHOFIL                                                   
         GOTO1 (RF),(R1),=C'REP',(R6),WRKREC,OTPARAM                            
         B     EXIT                                                             
         SPACE 2                                                                
REC      CLC   MTHIFIL,MTHOFIL     INPUT AND OUTPUT FILES MUST BE SAME          
         BE    *+12                                                             
         MVI   DBERROR,INVMTHB                                                  
         BAS   RE,CHKERRS          KILL THIS CALL                               
         GOTO1 MAINT,DMCB,=C'DSP',(R6),WRKREC,OTPARAM                           
         L     R3,16(R1)           A(INPUT RECORD DISP TABLE)                   
         LR    R4,R3                                                            
         USING DSPHDRD,R4                                                       
         SR    R7,R7                                                            
         ICM   R7,3,DSPLDE         ENTRY LENGTH                                 
         LA    R3,DSPHDRLN(R3)     POINT TO FIRST ENTRY IN DSP TAB              
         SR    R0,R0                                                            
         ICM   R0,7,DSPAET                                                      
         AR    R4,R0               POINT R4 AT END OF DSP TABLE                 
         GOTO1 CDEMOUT,(R1),(C'M',(R3)),(R6),WRKREC,(R4)                        
         BAS   RE,CHKERRS                                                       
         MVC   DBAREC,INPOUT                                                    
         MVC   DBAQUART,OTAQUART                                                
         STCM  R3,15,WORK          START OF TABLE                               
         STCM  R4,15,WORK+4        END OF TABLE                                 
         STCM  R7,3,WORK+8         LENGTH OF TABLE ENTRY                        
         SR    R4,R3               LENGTH OF ALL ENTRIES                        
         SRDL  R4,32               SET UP DIVIDEND                              
         DR    R4,R7               NUMBER OF VALUES IN RECORD                   
         STCM  R5,3,DBNUMVLS                                                    
         BAS   RE,ADJNUMV          EXCLUDE DUMMY ELEMENTS > X'60'               
*                                                                               
         GOTO1 MAINT,(R1),(BYTE1,=C'REP'),(R6),WRKREC,OTPARAM                   
**         GOTO1 MAINT,(R1),=C'REP',(R6),WRKREC,OTPARAM                         
         B     EXIT                                                             
         DROP  R4                                                               
         SPACE 2                                                                
ADD      GOTO1 MAINT,DMCB,(BYTE1,=C'GET'),(R6),WRKREC,OTPARAM                   
**D      GOTO1 MAINT,DMCB,=C'GET',(R6),WRKREC,OTPARAM                           
         MVC   DBAREC,INPOUT                                                    
         MVC   DBAQUART,OTAQUART                                                
         MVC   DBFILE,MTHOFIL                                                   
         GOTO1 (RF),(R1),(BYTE1,=C'ADD'),(R6),WRKREC,OTPARAM                    
         GOTO1 (RF),(R1),(BYTE1,=C'REP'),(R6),WRKREC,OTPARAM                    
**         GOTO1 (RF),(R1),=C'ADD',(R6),WRKREC,OTPARAM                          
**         GOTO1 (RF),(R1),=C'REP',(R6),WRKREC,OTPARAM                          
         B     EXIT                                                             
         SPACE 2                                                                
MUL     GOTO1 MAINT,DMCB,(BYTE1,=C'GET'),(R6),WRKREC,OTPARAM                    
**MUL      GOTO1 MAINT,DMCB,=C'GET',(R6),WRKREC,OTPARAM                         
*&&DO                                                                           
         CLI   DBSELMED,C'O'                                                    
         BNE   *+8                                                              
         BAS   RE,CLEARNEG                                                      
*&&                                                                             
         MVC   DBFILE,MTHOFIL      SET OUTPUT RECORD PARAMETERS                 
         MVC   DBINTFIL(2),OTIFIL  THIS WILL FORCE PROPER DISP TABLE            
         MVC   DBACTBK,OTBOOK      TO BE USED FOR MUL/DIV                       
         MVC   DBACTSRC,OTSRC                                                   
         L     R7,MTHFCTR                                                       
         GOTO1 (RF),(R1),(BYTE1,COMMAND),(R6),WRKREC,OTPARAM,(R7)               
**       GOTO1 (RF),(R1),COMMAND,(R6),WRKREC,OTPARAM,(R7)                       
         MVC   DBAREC,INPOUT                                                    
         MVC   DBAQUART,OTAQUART                                                
         MVC   DBFILE,MTHOFIL                                                   
         GOTO1 (RF),(R1),(BYTE1,=C'REP'),(R6),WRKREC,OTPARAM                    
**       GOTO1 (RF),(R1),=C'REP',(R6),WRKREC,OTPARAM                            
         B     EXIT                                                             
         SPACE 2                                                                
MAD      GOTO1 MAINT,DMCB,(BYTE1,=C'GET'),(R6),WRKREC,OTPARAM                   
*AD      GOTO1 MAINT,DMCB,=C'GET',(R6),WRKREC,OTPARAM                           
         MVC   DBFILE,MTHOFIL      SET OUTPUT RECORD PARAMETERS                 
         MVC   DBINTFIL(2),OTIFIL  THIS WILL FORCE PROPER DISP TABLE            
         MVC   DBACTBK,OTBOOK      TO BE USED FOR MUL/DIV                       
         MVC   DBACTSRC,OTSRC                                                   
         L     R7,MTHFCTR                                                       
         GOTO1 (RF),(R1),(BYTE1,=C'MUL'),(R6),WRKREC,OTPARAM,(R7)               
**       GOTO1 (RF),(R1),=C'MUL',(R6),WRKREC,OTPARAM,(R7)                       
         MVC   DBAREC,INPOUT                                                    
         MVC   DBAQUART,OTAQUART                                                
         MVC   DBFILE,MTHOFIL                                                   
         GOTO1 (RF),(R1),(BYTE1,=C'ADD'),(R6),WRKREC,OTPARAM                    
****     GOTO1 (RF),(R1),=C'ADD',(R6),WRKREC,OTPARAM                            
* HAVE TO CHECK WRKREC AREA TO SEE IF WE HAVE ANY NEGATIVE NUMBERS              
* IF WE HAVE NEGATIVE NUMBER THAT MEANS WE OVERFLOWED THE FULLWORD              
* BUFFERS HENCE WE SHOULD NOT RETURN ANY VALUES                                 
         CLI   DBSELMED,C'O'        ONLY DO THIS FOR OVERNIGHTS                 
         BNE   *+8                  REALLY FOR OVERNIGHTS ROLLING AVG           
         BAS   RE,CHKNEG                                                        
*                                                                               
         GOTO1 (RF),(R1),(BYTE1,=C'REP'),(R6),WRKREC,OTPARAM                    
**       GOTO1 (RF),(R1),=C'REP',(R6),WRKREC,OTPARAM                            
*                                                                               
         CLI   DBSELMED,C'O'        ONLY DO THIS FOR OVERNIGHTS                 
         BNE   EXIT                 REALLY FOR OVERNIGHTS ROLLING AVG           
         NI    DBERROR,X'40'    MAKE SURE ITS OFF AS DEFAULT                    
         CLI   NEGFLAG,C'Y'                                                     
         BNE   *+8                                                              
         MVI   DBERROR,X'40'                                                    
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
* ROUND RATINGS AND PUTS ON REP INVENTORY RECORD (IUN).  THIS CALL              
* WILL BE USED INITIALLY TO FORCE ROUNDED RATINGS AND PUTS                      
* ON INDEX UPGRADED INVENTORY RECORDS PARTICIPATING IN INVENTORY-               
* TO-INVENTORY TRANSFERS.                                                       
*                                                                               
RROUND   CLC   MTHIFIL,MTHOFIL     INPUT FILE = OUTPUT FILE                     
         BE    *+12                                                             
         MVI   DBERROR,INVMTHB                                                  
         BAS   RE,CHKERRS                                                       
*                                                                               
         GOTO1 MAINT,DMCB,=C'DSP',(R6),WRKREC,OTPARAM                           
         L     R3,20(R1)           GET A(DISPLACEMENT TABLE)                    
         USING DSPHDRD,R3                                                       
         LR    R5,R3                                                            
         SR    R4,R4                                                            
         ICM   R4,3,DSPLDE         R4=L'TABLE ENTRY                             
         SR    R0,R0                                                            
         ICM   R0,7,DSPAET         R0=DISPLACEMENT TO E-O-T                     
         AR    R5,R0               R5=A(E-O-T)                                  
         LA    R3,DSPHDRLN(R3)     R3=A(S-O-T)                                  
         USING DSPDTAD,R3                                                       
*                                  CALCULATE N'DISP. TABLE ENTRIES              
         STCM  R3,15,WORK          START OF TABLE                               
         STCM  R5,15,WORK+4        END OF TABLE                                 
         STCM  R4,3,WORK+8         LENGTH OF TABLE ENTRY                        
         LR    RF,R5               RF=A(E-O-T)                                  
         SR    RF,R3               RF=L'TABLE = A(E-O-T) - A(S-O-T)             
         SR    RE,RE                                                            
         DR    RE,R4               N'ENTRIES=L'TABLE/L'ENTRY                    
         STCM  RF,3,DBNUMVLS                                                    
         BAS   RE,ADJNUMV          EXCLUDE FAST DEMO ELEMENTS > X'60'           
*                                                                               
         GOTO1 CDEMOUT,DMCB,(C'M',(R3)),(R6),WRKREC,(R5)                        
         BAS   RE,CHKERRS                                                       
*                                                                               
         SR    R1,R1               R1=INDEX INTO WORK AREA                      
*                                                                               
RROUND2  CLI   DSPMOD,C'R'         TEST FOR RATING                              
         BE    *+12                                                             
         CLI   DSPMOD,C'P'         TEST FOR PUT                                 
         BNE   RROUND3                                                          
*                                                                               
         L     RF,WRKREC(R1)       NOW ROUND THE VALUE                          
         SR    RE,RE                                                            
         AH    RF,=H'5'                                                         
         D     RE,=F'10'                                                        
         M     RE,=F'10'                                                        
         ST    RF,WRKREC(R1)                                                    
*                                                                               
RROUND3  LA    R1,4(R1)                                                         
         BXLE  R3,R4,RROUND2                                                    
*                                                                               
RROUND4  MVC   DBAREC,INPOUT                                                    
         MVC   DBAQUART,OTAQUART                                                
         GOTO1 MAINT,DMCB,=C'REP',(R6),WRKREC,OTPARAM                           
         B     EXIT                                                             
         DROP  R3                                                               
         SPACE 2                                                                
* DOUBLE-WORD MULTIPLY AND ADD                                                  
* MULTIPLY DEMOS FROM RECORD BY DBFACTOR AND ADD TO DOUBLE WORD ACCUMS          
* !!! USE ONLY WITH 'IUN' RECORDS. OTHERWISE LOGIC FAILS WHEN WE                
* !!! DMAD RECORDS WITH DIFFERENT FORMATS (DIFF DISPLACEMENT TABLES)            
DMAD     GOTO1 MAINT,DMCB,=C'GET',(R6),WRKREC,OTPARAM                           
* DEMAINT DAD CALL USES DBAREC WITH THE DOUBLE WORD ACCUMLATOR                  
* BUT THE PROBLEM IS IT ALSO CALLS DEMEL TOO LOOK UP THE DEMDISP                
* SET DBSVAREC TO THE REAL DEMO RECORD SO THE DEMOMATH DAD CALL                 
* CAN FIND THE 5E ELEMENT                                                       
*                                                                               
* THE ONLY OTHER PROGRAM USING DOUBLE WORD ARITHMETIC IS REFETCH FOR            
* OVERNIGHT LOOKUPS                                                             
* WE GOT AWAY WITH THIS BUG FOR REFETCH OVERNIGHTS LOOKUPS BECAUSE              
* OVERNIGHTS ONLY HAS ONE QTR PER RECORD                                        
* SO IT NEVER NEEDED TO GO BACK TO THE DEMO RECORD TO FIND THE 1ST              
* 5E ELEMENT                                                                    
* CANADA HAS MULTIPLE QTR HOURS PER RECORD SO THIS IS A PROBLEM                 
*                                                                               
         CLI   DBSELMED,C'C'                                                    
         BNE   *+8                                                              
         CLI   DBSELSRC,C'A'                                                    
         BNE   *+10                                                             
         MVC   DBSVAREC,DBAREC   PASS REAL DBAREC-BOOKEL LOOKUP NEEDED          
*                                                                               
         MVC   DBFILE,MTHOFIL      SET OUTPUT RECORD PARAMETERS                 
         MVC   DBINTFIL(2),OTIFIL  THIS WILL FORCE PROPER DISP TABLE            
         MVC   DBACTBK,OTBOOK      TO BE USED FOR MUL/DIV                       
         MVC   DBACTSRC,OTSRC                                                   
         L     R7,MTHFCTR                                                       
         GOTO1 MAINT,DMCB,(BYTE1,=C'MUL'),(R6),WRKREC,OTPARAM,(R7)              
**       GOTO1 MAINT,DMCB,=C'MUL',(R6),WRKREC,OTPARAM,(R7)                      
         MVC   DBAREC,INPOUT       DOUBLE-WORD ACCUMULATOR AREA                 
         MVC   DBFILE,MTHOFIL      ADD VALUES IN WRKREC TO INPOUT               
***      GOTO1 MAINT,DMCB,=C'DAD',(R6),WRKREC,OTPARAM                           
         GOTO1 MAINT,DMCB,(BYTE1,=C'DAD'),(R6),WRKREC,OTPARAM                   
DMADX    B     EXIT                                                             
         SPACE 2                                                                
* DOUBLE-WORD DIVIDE                                                            
* DIVIDE DOUBLE-WORD VALUES BY DBDIVSOR AND ROUND                               
* RETURN A DEMO-STYLE RECORD                                                    
* !!! USE ONLY WITH 'IUN' RECORDS. LOGIC RELIES ON IT.                          
DDIV     MVC   DBFILE,MTHOFIL      SET OUTPUT RECORD PARAMETERS                 
*                                                                               
*                                                                               
* FOR CANADA DDI CALL WE NEED TO KEEP CALLERS DBAQUART DBAREC                   
* BECAUSE THE BUFFER IS NOW A DOUBLE WORD AREA                                  
* YOU CANT OVERRIDE DBAREC WITH THE BUFFER ADDRESS, DEMEL WILL                  
* LOOP FOREVER TO LOOK FOR ELEMENTS                                             
         CLI   DBSELMED,C'C'                                                    
         BNE   DDIV02                                                           
         CLI   DBSELSRC,C'A'                                                    
         BNE   DDIV02                                                           
         MVC   DBAQUART,SVAQUART                                                
         MVC   DBAREC,SVRADDR                                                   
**                                                                              
DDIV02   L     R7,MTHFCTR                                                       
         MVC   OTPARAM,IUNFRMT     DEFAULT IUN FORMAT                           
         CLI   DBTAPEP,C'Y'        IF DEMOS BASED ON IMPRESSIONS                
         BNE   *+10                                                             
         MVC   OTPARAM,IUNFRMTI    SWITCH FORMAT FIELDS                         
**                                                                              
** FOR CANADA USE TWA                                                           
         CLI   DBSELMED,C'C'                                                    
         BNE   DDIV04                                                           
         CLI   DBSELSRC,C'A'                                                    
         BNE   DDIV04                                                           
         MVC   OTPARAM,TWAFRMT                                                  
* NEWLY ADDED, FOR CANADA NEW DBACTBK SET TO THE DEMDISP BOOK                   
         MVC   DBFILE,MTHOFIL      SET OUTPUT RECORD PARAMETERS                 
         MVC   DBINTFIL(2),OTIFIL  THIS WILL FORCE PROPER DISP TABLE            
         MVC   DBACTBK,OTBOOK      TO BE USED FOR MUL/DIV                       
DDIV04   DS    0H                                                               
*                                                                               
***      GOTO1 MAINT,DMCB,=C'DRD',(R6),INPIN,OTPARAM,(R7)                       
         GOTO1 MAINT,DMCB,(BYTE1,=C'DRD'),(R6),INPIN,OTPARAM,(R7)               
*                                                                               
* SHRINK DOUBLE-WORD VALUES AT INPIN TO FULL-WORD VALUES ON WRKREC              
* DIVIDED VALUES ARE SMALLER AND WILL FIT IN FULL WORDS                         
         SR    R0,R0                                                            
         ICM   R0,3,DBNUMVLS                                                    
         BZ    DDIV20                                                           
         L     RE,INPIN                                                         
         LA    RF,WRKREC                                                        
DDIV10   MVC   0(4,RF),4(RE)                                                    
         LA    RE,8(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,DDIV10                                                        
*                                                                               
DDIV20   MVC   DBAREC,INPOUT       CREATE DEMO-STYLE RECORD AT INPOUT           
         MVC   DBAQUART,OTAQUART                                                
         MVC   DBFILE,MTHOFIL                                                   
         MVC   OTEFIL,MTHOFIL      RESTORE OUTPUT FORMAT                        
**                                                                              
** FOR CANADA USE TWA                                                           
         CLI   DBSELMED,C'C'                                                    
         BNE   DDIV24                                                           
         CLI   DBSELSRC,C'A'                                                    
         BNE   DDIV24                                                           
         MVC   OTPARAM,TWAFRMT                                                  
* NEWLY ADDED, FOR CANADA NEW DBACTBK SET TO THE DEMDISP BOOK                   
         MVC   DBFILE,MTHOFIL      SET OUTPUT RECORD PARAMETERS                 
         MVC   DBINTFIL(2),OTIFIL  THIS WILL FORCE PROPER DISP TABLE            
         MVC   DBACTBK,OTBOOK      TO BE USED FOR MUL/DIV                       
DDIV24   DS    0H                                                               
**                                                                              
*****    GOTO1 MAINT,DMCB,=C'REP',(R6),WRKREC,OTPARAM                           
         GOTO1 MAINT,DMCB,(BYTE1,=C'REP'),(R6),WRKREC,OTPARAM                   
         B     EXIT                                                             
         SPACE 2                                                                
                                                                                
         EJECT                                                                  
* GET A(FIRST ELEMENT FOR A RECORD)                                             
*                                                                               
GETFSTEL ST    R1,DMCB                                                          
         LA    R1,RECTAB                                                        
GETFST2  CLC   0(3,R1),=C'ALL'                                                  
         BE    GETFST4                                                          
         CLC   0(3,R1),0(RF)                                                    
         BE    *+12                                                             
         LA    R1,L'RECTAB(R1)                                                  
         B     GETFST2                                                          
GETFST4  SR    RF,RF                                                            
         ICM   RF,3,5(R1)          SET UP RECORD END                            
         A     RF,DMCB                                                          
         SR    R0,R0               NOW FIND END OF RECORD                       
         ICM   R0,3,0(RF)                                                       
         L     RF,DMCB                                                          
         AR    RF,R0                                                            
         ST    RF,RECEND           SAVE END OF RECORD                           
         SR    RF,RF               SET UP FIRST ELEMENT                         
         ICM   RF,3,3(R1)                                                       
         L     R1,DMCB                                                          
         LA    R1,0(RF,R1)                                                      
         BR    RE                                                               
         SPACE 2                                                                
* GO TO DEMO MODULE AND CHECK DBERROR ON RETURN                                 
*                                                                               
MAINT    STM   RE,RF,SAVEREGS                                                   
         L     RF,CDEMAINT                                                      
         BASR  RE,RF                                                            
         LM    RE,RF,SAVEREGS                                                   
         B     CHKERRS                                                          
         SPACE 1                                                                
* CHECK FOR DEMO SYSTEM ERRORS & DIE IF ANY ERRORS RETURNED                     
*                                                                               
CHKERRS  CLI   DBERROR,0                                                        
         BER   RE                                                               
         ZIC   R0,DBERROR                                                       
         DC    H'0',C'DEMO MODULE ERROR - ERROR NUMBER IN R0'                   
         SPACE 2                                                                
CHKNEG   STM   RE,RF,SAVEREGS                                                   
         LA    RF,WRKREC                                                        
         ZICM  RE,DBNUMVLS,(3)                                                  
         MVI   NEGFLAG,C'N'                                                     
CHKNEG02 L     R0,0(RF)                                                         
         TMH   R0,X'8000'             ALWAYS REST TO X'80000000'                
         BO    CHKNEG10               IF THE NUMBERS GOT OO BIG                 
                                                                                
         AHI   RF,4              BUMP TO NEXT FULLWORD VALUE                    
         BCT   RE,CHKNEG02                                                      
         MVI   NEGFLAG,C'N'                                                     
         B     CHKNEGX                                                          
*                                                                               
* WE HAVE FOUND A NEGATIVE VALUE IN OUR WORK AREA - CLEAR OUT                   
CHKNEG10 MVI   NEGFLAG,C'Y'       SET NEGATIVE FLAG                             
         LA    RE,WRKREC          CLEAR OUT THE WORKAREA                        
         ZICM  RF,DBNUMVLS,(3)                                                  
         MH    RF,=H'4'                                                         
         XCEF                                                                   
*                                                                               
CHKNEGX  LM    RE,RF,SAVEREGS                                                   
         BR    RE                                                               
*&&DO                                                                           
CLEARNEG STM   RE,RF,SAVEREGS                                                   
         LA    RF,WRKREC                                                        
         LA    RF,WRKREC                                                        
         ZICM  RE,DBNUMVLS,(3)                                                  
CLRNEG02 L     R0,0(RF)                                                         
***      TMH   R0,X'8000'             CLEAR OUT                                 
***      BNO   *+10                   IF NEG                                    
         CLC   =X'8FFFFFFF',0(RF)                                               
         BNE   *+10                                                             
         MVC   0(4,RF),=X'00000000'                                             
                                                                                
         AHI   RF,4              BUMP TO NEXT FULLWORD VALUE                    
         BCT   RE,CLRNEG02                                                      
         B     CLRNEGX                                                          
CLRNEGX  LM    RE,RF,SAVEREGS                                                   
         BR    RE                                                               
*&&                                                                             
* EXIT FROM MODULE                                                              
*                                                                               
EXIT     MVC   DBAREC,SVRADDR      RESTORE DBLOCK SAVED VALUES                  
         MVC   DBAQUART,SVAQUART                                                
         MVC   DBFILE,SVFILE                                                    
         MVC   DBACTBK,SVACTBK                                                  
         MVC   DBINTFIL(2),SVINTFIL                                             
         MVC   DBACTSRC,SVACTSRC                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* ADJUST DBNUMVLS TO INCLUDE ONLY ELEMENTS WITH CODE < X'60'                    
* INPUT:                                                                        
*  DBNUMVLS=TOTAL NUMBER OF DEMOS (INCLUDING '6X','7X'... ELEMENTS)             
*  WORK   = A(START OF TABLE)                                                   
*  WORK+4 = A(END OF TABLE)                                                     
*  WORK+8 = LENGTH OF TABLE ENTRY                                               
* OUTPUT:                                                                       
*  DBNUMVLS=NUM OF DEMOS ON ELEMENTS < X'60'                                    
*                                                                               
ADJNUMV  NTR1                                                                   
*                                                                               
         USING DBLOCKD,R6                                                       
         ICM   R2,15,WORK     S-O-T                                             
         ICM   R3,15,WORK+4   E-O-T                                             
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,WORK+8    L'ENTRY                                           
         SR    RF,RF                                                            
         ICM   RF,3,DBNUMVLS                                                    
*                                                                               
ADJNV10  SR    R3,R1          BACK OFF ONE ENTRY                                
         CR    R3,R2                                                            
         BL    ADJNV50                                                          
         USING DSPDTAD,R3                                                       
         CLI   DSPELCD,X'60'                                                    
         BL    ADJNV50                                                          
         BCTR  RF,0           DECREASE NO OF ACTUAL ENTRIES                     
         B     ADJNV10        BY THE NUMBER OF ELEMS > 60                       
         DROP  R3                                                               
*                                                                               
ADJNV50  STCM  RF,3,DBNUMVLS                                                    
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
* LITERALS & DSECTS                                                             
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
RECTAB   DS    0XL7                                                             
         DC    C'INV',AL2(34),AL2(27)                                           
         DC    C'ALL',AL2(23),AL2(20)                                           
         EJECT                                                                  
IUNFRMT  DC    C'IUNUIUN',X'530B00'       DEFAULT IUN FORMAT                    
IUNFRMTI DC    C'IUNUIUN',X'5A0B00'       IMPRESSION BASED IUN FORMAT           
TWAFRMT  DC    X'E3D74000E3E6C1682300'                                          
DMWRK    DSECT                                                                  
BYTE1    DS    C                                                                
DMCB     DS    6F                                                               
SAVEREGS DS    2F                                                               
MATHFAC  DS    0CL17                                                            
MTHCFACS DS    A                   A(DBLOCK)                                    
MTHFCTR  DS    F                   FACTOR FOR MULT OR DIV                       
MTHIFIL  DS    CL3                 INPUT FILE                                   
MTHOFIL  DS    CL3                 OUTPUT FILE                                  
MTHOSRC  DS    CL3                 OUTPUT SOURCE                                
COMMAND  DS    CL3                 DEMAINT COMMAND                              
         DS    0F                                                               
INPARAM  DS    0CL16                                                            
INPCMND  DS    A                   A(COMMAND)                                   
INPIN    DS    A                   A(INPUT RECORD)                              
INPOUT   DS    A                   A(OUTPUT RECORD)                             
INPMFAC  DS    A                   A(MATHFAC BLOCK)                             
         SPACE 1                                                                
OTPARAM  DS    0CL10               OUTPUT FORMAT BLOCK                          
OTEFIL   DS    CL3                 EXTERNAL FILE                                
OTEMED   DS    CL1                 EXTERNAL MEDIA                               
OTIFIL   DS    CL1                 INTERNAL FILE                                
OTIMED   DS    CL1                 INTERNAL MEDIA                               
OTSRC    DS    CL1                 SOURCE CODE                                  
OTBOOK   DS    CL2                 BOOK                                         
OTELFLT  DS    CL1                 ELEMENT CODE FILTER                          
ITAQUART DS    A                   A(INPUT RECORD QTR HR ELEMENT)               
OTAQUART DS    A                   A(OUTPUT RECORD QTR HR ELEMENT)              
SVAQUART DS    A                   SAVE DBLOCK QTR HR ADDRESS                   
SVRADDR  DS    A                   SAVE DBLOCK RECORD ADDRESS                   
SVFILE   DS    CL3                 SAVE DBFILE VALUE                            
SVINTFIL DS    CL2                 SAVE DBINTFIL/MED                            
SVACTBK  DS    CL2                 SAVE DBACTBK                                 
SVACTSRC DS    CL2                 SAVE DBACTSRC                                
RECEND   DS    F                   END OF CURRENT RECORD                        
IRECEND  DS    F                   END OF INPUT RECORD                          
ORECEND  DS    F                   END OF OUTPUT RECORD                         
WORK     DS    CL60                                                             
WRKREC   DS    2000C                                                            
WRKRECX  EQU   *                                                                
NEGFLAG  DS    C                                                                
*                                                                               
DMWRKX   EQU   *                                                                
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DEDBLOCK                                                                      
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMEQUS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021DEDEMOMATH07/01/19'                                      
         END                                                                    
