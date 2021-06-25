*          DATA SET DEDEMEL    AT LEVEL 030 AS OF 06/26/19                      
*PHASE T00ADBB                                                                  
         TITLE 'DEMEL - EXPLODE/CREATE DEMO ELEMENTS'                           
DEMEL    RSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 DEMELX-DEMELD,**DEMEL,RR=R2                                      
         USING DEMELD,RC           RC=A(W/S)                                    
         STAR  CLEAR=Y,ARS=OFF                                                  
*                                                                               
         ST    R2,RELO                                                          
         ST    R1,APARM            SAVE A(PARM LIST)                            
         MVC   ACTION,0(R1)                                                     
         LM    R3,R5,0(R1)         R3=A(FMT),R4=A(DBLOCK),R5=A(WORK)            
         USING DBLOCKD,R4                                                       
*                                                                               
         XC    DUB,DUB             SPECIAL CALL FOR SYSFACS                     
         MVC   DUB(4),=XL4'FEFFFFFF'                                            
         L     RF,DBCOMFCS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         LTR   RF,RF                                                            
         BNZ   DEMAL10                                                          
         ICM   RF,15,DBCOMFCS                                                   
         ICM   RF,15,CMASTC-COMFACSD(RF)                                        
         ICM   RF,15,MCSSB-MASTD(RF)       RF=ASSB                              
         MVC   ALET,SSBTBLET-SSBD(RF)                                           
         B     DEMALETX                                                         
*                                                                               
DEMAL10  GOTO1 (RF),DUB                                                         
         L     RF,0(R1)            RF=V(SYSFACS)                                
         L     RF,VSSB-SYSFACD(RF) RF=V(SSB)                                    
         MVC   ALET,SSBTBLET-SSBD(RF)                                           
*                                                                               
DEMALETX XC    DUB,DUB                                                          
         L     R1,APARM                                                         
         XC    OFORMAT,OFORMAT     CLEAR OUTPUT FORMAT BLOCK                    
         LA    R3,0(R3)                                                         
         LTR   R3,R3               TEST IF A(FMT) PASSED                        
         BZ    *+10                                                             
         MVC   OFORMAT,0(R3)       YES - SAVE                                   
*FOR BBM DIARY ALWAYS FORCE TO THE OLD DIARY FORMAT - PRE NOV04 FORMAT          
         CLI   DBSELSRC,C'A'                                                    
         BNE   BBMFMTX                                                          
         CLI   DBSELMED,C'C'                                                    
         BNE   BBMFMTX                                                          
         CLI   DBBTYPE,C'U'        CHECK FOR THE FORCED BOOKTYPE                
         BNE   BBMFMTX             BY UPGRADE ROUTINE                           
         MVC   OFORMAT,OBBMFMT     FORCE ALL BBM DIARY TO OLD FORMAT            
*                                                                               
BBMFMTX  MVI   DBERROR,0                                                        
         MVI   DBERRMOD,EDEMEL                                                  
         ST    R5,AOUTPUT          SAVE A(OUTPUT AREA)                          
*                                                                               
*                                  GET A(DISPTAB)/A(FMTAB)                      
         L     R6,=A(DEMTABCL)     A(LIST OF TABLES WITHIN PROGRAM)             
         A     R6,RELO                                                          
         USING DEMTABCL,R6                                                      
         MVC   ATABLES,ATABLESL    SET W/S FROM LIST WITHIN CSECT               
         OC    ADISPTAB+1(3),ADISPTAB+1  DID WE GET A(DEMDISP) ALREADY?         
         BNZ   DEM2                      YES                                    
*                                                                               
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMADDR-COMFACSD(RF)                                         
         GOTO1 (RF),DUB,(X'FF',ATABLES),DBCOMFCS                                
*                                                                               
         L     RF,DBCOMFCS                                                      
         ICM   RF,15,CPROTOFF-COMFACSD(RF)                                      
         JZ    *+6                                                              
         BASR  RE,RF               *** TURN STORAGE PROTECTION OFF ***          
*                                                                               
         MVC   ATABLESL(L'ATABLES),ATABLES     SAVE A(TABLES) IN CSECT          
*                                                                               
         L     RF,DBCOMFCS                                                      
         ICM   RF,15,CPROTON-COMFACSD(RF)                                       
         JZ    *+6                                                              
         BASR  RE,RF               *** STORAGE PROTECTION BACK ON ***           
*                                                                               
         B     DEM2                                                             
         DROP  R6                                                               
*                                  RETURN OUTPUT FORMAT BLOCK                   
DEMX     XC    OBOOK,=X'FFFF'                                                   
         L     R1,APARM                                                         
         MVC   12(4,R1),AIDISP     RETURN A(I/O FMT DISP TABLES)                
         MVC   16(4,R1),AODISP                                                  
         XR    R3,R3                                                            
         ICM   R3,7,1(R1)          TEST IF FORMAT BLOCK PASSED                  
         BNZ   *+16                                                             
         LA    R3,OFORMAT          NO - RETURN A(MY FORMAT BLOCK)               
         STCM  R3,7,1(R1)                                                       
         B     *+10                                                             
         MVC   0(L'OFORMAT,R3),OFORMAT                                          
*                                  EXIT FROM MODULE                             
DEMXMOD  REAR  ARS=OFF                                                          
         XMOD1 1                                                                
         EJECT                                                                  
* BUILD INPUT/OUTPUT BOOK FORMAT BLOCKS                                         
*                                                                               
DEM2     LA    R6,IFORMAT          R6=A(INPUT BOOK FORMAT BLOCK)                
         XC    IFORMAT,IFORMAT                                                  
         MVC   IFILE,DBFILE                                                     
*                                  TEST IF IREC HAS A BOOK ELEMENT              
         LA    RE,BKELTAB          RE=A(BOOK ELEMENT TABLE)                     
         MVI   BOOKEL,X'5E'        SET DEFAULT BOOK ELEMENT CODE                
DEM2A    CLI   0(RE),X'FF'         TEST E-O-T                                   
         BE    DEM2B                                                            
         CLC   0(3,RE),IFILE                                                    
         BE    *+12                                                             
         LA    RE,L'BKELTAB(RE)                                                 
         B     DEM2A                                                            
         CLI   3(RE),0             TEST IF RECORD HAS A BOOK ELEMENT            
         BE    DEM3                                                             
         MVC   BOOKEL,3(RE)                                                     
*                                                                               
DEM2B    ICM   RE,15,DBAQUART      FIND BOOK ELEMENT ON RECORD                  
         BZ    DEM3                MAKE SURE WE HAVE QH ELEMENT                 
         XR    R0,R0                OTHERWISE, SET VALUES FROM DBLOCK           
*                                                                               
DEM2C    CLI   0(RE),0             TEST E-O-R                                   
         BE    DEM3                                                             
         CLC   0(1,RE),BOOKEL                                                   
         BE    *+14                                                             
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     DEM2C                                                            
         CLI   1(RE),4             TEST IF COMPRESSED BOOK ELEMENT              
         BNE   DEM2D                                                            
         MVC   DUB(2),2(RE)                                                     
         NI    DUB,X'7F'           DUB=DISP TO REAL BOOK ELEMENT                
         L     RE,DBAREC                                                        
* FOR CANADA DOUBLE WORD ARTHMATIC DAD CALL FAILED FROM DEMOMATH                
*BECAUSE DBAREC IS SET WITH THE DOUBLE WORD BUFFER                              
*IF DBSVAREC IS PASSED USE IT INSTEAD                                           
*DEMOMATH PASSES THE DOUBLE WORD BUFFERS IN DBAREC                              
* USE DBSVAREC SET IN DEMOMATH WHICH POINTS TO THE REAL DEMO RECORD             
*                                                                               
         OC    DBSVAREC,DBSVAREC                                                
         BZ    *+8                                                              
         L     RE,DBSVAREC                                                      
*                                                                               
         LA    RE,0(RE)                                                         
         AH    RE,DUB                                                           
*                                                                               
DEM2D    MVC   IINTFIL(2),2(RE)    SET VALUES FROM BOOK ELEMENT                 
         MVC   ISOURCE,4(RE)                                                    
         CLC   2(3,RE),=C'CNN'                                                  
         BNE   DEM2E                                                            
         CLC   5(2,RE),=X'5D31'                                                 
         BL    DEM2E                                                            
         CLC   5(2,RE),=X'5D33'                                                 
         BH    DEM2E                                                            
         MVC   5(2,RE),=X'5D10'                                                 
DEM2E    MVC   IBOOK,5(RE)                                                      
         XC    IBOOK,=X'FFFF'                                                   
         B     DEM5                                                             
*                                  SET VALUES FROM DBLOCK                       
DEM3     MVC   IMED,DBACTMED                                                    
         CLI   DBACTMED,0                                                       
         BNE   *+10                                                             
         MVC   IMED,DBSELMED                                                    
         MVC   IINTFIL(2),DBINTFIL                                              
         OC    DBINTFIL(2),DBINTFIL                                             
         BNZ   DEM4                                                             
         BRAS  RE,GETFILE          GET INTERNAL FILE/MEDIA                      
         MVC   DBINTFIL(2),IINTFIL                                              
DEM4     MVC   ISOURCE,DBACTSRC                                                 
         CLI   DBACTSRC,0                                                       
         BNE   *+10                                                             
         MVC   ISOURCE,DBSELSRC                                                 
         CLI   ISOURCE,0                                                        
         BNE   *+8                                                              
         MVI   ISOURCE,C'N'                                                     
*                                                                               
         CLI   DBSELMED,C'N'       NHT FILE LOOK UP?                            
         BNE   DEM4Z                                                            
         MVC   DUB(1),DBACTSRC                                                  
         OC    DUB(1),DUB                                                       
         BNZ   *+10                                                             
         MVC   DUB(1),DBSELSRC                                                  
         CLI   DUB,C'H'                                                         
         BNE   *+10                                                             
         MVC   IINTFIL(3),=C'NHN'                                               
         CLI   DUB,C'D'                                                         
         BNE   *+10                                                             
         MVC   IINTFIL(3),=C'NNN'                                               
         CLI   DUB,C'K'                                                         
         BNE   *+10                                                             
         MVC   IINTFIL(3),=C'PNN'                                               
         CLI   DUB,C'C'                                                         
         BNE   *+10                                                             
         MVC   IINTFIL(3),=C'CNN'                                               
*                                                                               
DEM4Z    MVC   IBOOK,DBACTBK                                                    
         OC    IBOOK,IBOOK                                                      
         BNZ   *+10                                                             
         MVC   IBOOK,DBSELBK                                                    
         OC    IBOOK,IBOOK                                                      
         BZ    *+10                                                             
         XC    IBOOK,=X'FFFF'                                                   
         OC    IBOOK,IBOOK         TEST IF BOOK PASSED                          
         BNZ   *+8                                                              
         BRAS  RE,GETFBOOK         NO - GET FIRST BOOK FOR SOURCE/MEDIA         
*                                  SET A(MASTER DISP. TABLE)                    
DEM5     BRAS  RE,GETDISP                                                       
         ST    R1,AIDISP                                                        
*                                                                               
         LA    R6,OFORMAT          R6=A(OUTPUT BOOK FORMAT BLOCK)               
         OC    OFILE,OFILE                                                      
         BNZ   *+10                                                             
         MVC   OFILE,IFILE                                                      
*                                  TEST IF OREC HAS A BOOK ELEMENT              
         LA    RE,BKELTAB          RE=A(BOOK ELEMENT TABLE)                     
         MVI   BOOKEL,X'5E'        SET DEFAULT BOOK ELEMENT CODE                
DEM5A    CLI   0(RE),X'FF'         TEST E-O-T                                   
         BE    DEM5B                                                            
         CLC   0(3,RE),OFILE                                                    
         BE    *+12                                                             
         LA    RE,L'BKELTAB(RE)                                                 
         B     DEM5A                                                            
         MVC   BOOKEL,3(RE)                                                     
*                                                                               
DEM5B    CLI   OMED,0                                                           
         BNE   *+10                                                             
         MVC   OMED,IMED                                                        
         OC    OINTFIL(2),OINTFIL                                               
         BNZ   DEM6                                                             
         CLC   IFILE(IINTFIL-IFILE),OFILE                                       
         BNE   *+14                                                             
         MVC   OINTFIL(2),IINTFIL                                               
         B     DEM6                                                             
*                                  TEST IF A VARIABLE FILE                      
         LA    RE,EQUTAB           RE=A(FILE EQUATE TABLE)                      
DEM5C    CLI   0(RE),X'FF'         TEST E-O-T                                   
         BNE   *+12                                                             
         BRAS  RE,GETFILE          YES - FIND FMTAB ENTRY FOR FILE              
         B     DEM6                                                             
         CLC   OFILE,0(RE)                                                      
         BE    *+12                                                             
         LA    RE,L'EQUTAB(RE)                                                  
         B     DEM5C                                                            
         MVC   OINTFIL(2),IINTFIL  SET OUTPUT FILE/MEDIA FROM INPUT             
*                                                                               
DEM6     CLI   OSOURCE,0                                                        
         BNE   *+10                                                             
         MVC   OSOURCE,ISOURCE                                                  
         CLC   OBOOK,=X'FFFF'      TEST OUTPUT BOOK EQ INPUT BOOK               
         BNE   *+14                                                             
         MVC   OBOOK,IBOOK         YES - EQUATE                                 
         B     DEM8                                                             
         OC    OBOOK,OBOOK         TEST IF LATEST OUTPUT BOOK REQD.             
         BNZ   *+8                                                              
         BAS   RE,GETLBOOK         YES - GO FIND LATEST BOOK                    
         OC    OBOOK,OBOOK                                                      
         BNZ   *+14                                                             
         MVC   OBOOK,IBOOK                                                      
         B     DEM8                                                             
         XC    OBOOK,=X'FFFF'                                                   
*                                                                               
DEM8     MVC   AODISP,AIDISP                                                    
         CLC   OINTFIL(OFILTER-OINTFIL),IINTFIL                                 
         BE    *+12                                                             
         BRAS  RE,GETDISP                                                       
         ST    R1,AODISP           SET A(MASTER DISP. TABLE)                    
*                                                                               
         CLI   ACTION,C'D'         EXIT IF A(DISPATB) ONLY REQD                 
         BE    DEMX                                                             
*                                  GET A(START/END) OF MASTER DISP. TBL         
         L     R1,AODISP                                                        
         USING DSPHDRD,R1                                                       
         ICM   RF,7,DSPAET                                                      
         LA    RF,0(R1,RF)                                                      
         XR    RE,RE                                                            
         ICM   RE,3,DSPLDE                                                      
         AHI   R1,DSPHDRLN                                                      
         USING DSPDTAD,R1                                                       
*                                                                               
         CLI   OFILTER,0           TEST IF ELEMENT FILTER PASSED                
         BE    DEM10                                                            
         CLC   DSPELCD,OFILTER     YES - FIND FIRST ENTRY FOR ELEMENT           
         BE    DEM9                                                             
         BXLE  R1,RE,*-10                                                       
         MVI   DBERROR,INVELEM     ERROR EXIT IF NOT FOUND                      
         B     DEMX                                                             
*                                                                               
DEM9     LR    R0,R1               SAVE A(FIRST ENTRY)                          
         CLC   DSPELCD,OFILTER     FIND LAST ENTRY FOR ELEMENT                  
         BNE   *+8                                                              
         BXLE  R1,RE,*-10                                                       
         LR    RF,R1               RF=A(LAST ENTRY)                             
         LR    R1,R0               R1=A(FIRST ENTRY)                            
*                                                                               
DEM10    ST    R1,ADISPS           SAVE BXLE VALUES                             
         BCTR  RF,0                                                             
         ST    RF,ADISPE                                                        
         LA    RF,1(RF)                                                         
         ST    RE,DISPLN                                                        
         CLI   ACTION,C'C'         GO TO EXPLODE/COMPRESS ROUTINES              
         BE    DEMCND                                                           
         B     DEMEXP                                                           
         DROP  R1                                                               
         EJECT                                                                  
* ROUTINE TO EXPLODE DEMO ELEMENTS INTO WORK AREA                               
*                                                                               
* IF INPUT & OUTPUT BOOKS ARE OF THE SAME FORMAT FIELDS ARE EXTRACTED           
* DIRECTLY FROM RECORD INTO WORK AREA. IF DIFFERENT DEMOUT WILL BE              
* CALLED TO CALCULATE OUTPUT VALUES. IF MODULE IS CALLED TO TRANSFER            
* DEMCNV WILL BE ENTERED DIRECTLY AFTER WORK AREA HAS BEEN BUILT.               
*                                                                               
DEMEXP   SR    RF,R1               CLEAR WORK AREA TO BINARY ZEROES             
         LR    R0,RE                                                            
         SR    RE,RE                                                            
         DR    RE,R0                                                            
*        STCM  RF,3,DBNUMVLS       RETURN NUMBER OF WORK AREA VALUES            
         BAS   RE,SETNUMV          RETURN NUMBER OF WORK AREA VALUES            
         SLA   RF,2                                                             
         LR    RE,R5                                                            
         XCEF                                                                   
*                                                                               
         L     RE,AIDISP                                                        
         CLC   =C'CHN',0(RE)       HISPANIC CABLE NEEDS TO USE DBSPANAD         
         BE    *+14                                                             
         CLC   AIDISP+1(3),AODISP+1 TEST IF I/O FORMATS DIFFER                  
         BE    DEMEXP2                                                          
*                                  YES - CALL DEMOUT TO GET VALUES              
         MVC   DUB(2),DBSELAGY     SAVE AND CLEAR AGENCY IN DBLOCK              
         MVC   DUB+2(2),DBACTAGY                                                
         XC    DBSELAGY,DBSELAGY                                                
         XC    DBACTAGY,DBACTAGY                                                
         L     RF,DBCOMFCS                                                      
         L     RF,CDEMOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(C'M',ADISPS),DBLOCKD,(R5),ADISPE                      
         MVC   DBSELAGY,DUB        RESTORE AGENCY IN DBLOCK                     
         MVC   DBACTAGY,DUB+2                                                   
         B     DEMEXPX                                                          
*                                  EXTRACT VALUES FROM RECORD                   
DEMEXP2  LM    R7,R9,ADISPS        SET-UP BXLE REGISTERS                        
         USING DSPDTAD,R7                                                       
         MVI   LASTEL,0            SET FT SWITCH                                
*                                                                               
DEMEXP4  CLC   DSPELCD,LASTEL      TEST FOR CHANGE OF ELEMENT                   
         BE    DEMEXP18                                                         
         CLI   DSPELCD,HIELCD      BYPASS CALC ONLY ELEMENTS                    
         BH    DEMEXPX                                                          
         L     RE,DBAQUART         YES - SEARCH RECORD FOR ELEMENT              
         USING DREELEM,RE                                                       
         XR    RF,RF                                                            
*                                  LOOP TO FIND QTR HR/DEMO ELEMENT             
DEMEXP6  CLI   DRECODE,QHCODEQ                                                  
         BH    DEMEXP12                                                         
         CLI   DRECODE,0           TEST E-O-R                                   
         BNE   DEMEXP10                                                         
DEMEXP8  MVC   LASTEL,DSPELCD      FIND NEXT ELEMENT IN MASTER DISP TBL         
         B     *+14                                                             
         CLC   LASTEL,DSPELCD      LOOP TILL CODES DIFFER                       
         BNE   DEMEXP4                                                          
         LA    R5,4(R5)                                                         
         BXLE  R7,R8,*-14                                                       
         B     DEMEXPX                                                          
*                                  BUMP TO NEXT ELEMENT                         
DEMEXP10 IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     DEMEXP6                                                          
*                                  LOOP TO FIND CORRECT DEMO ELEMENT            
DEMEXP12 CLI   DRECODE,QHCODEQ                                                  
         BNH   DEMEXP8                                                          
         MVC   DUB(1),DRECODE      EXTRACT ELEMENT CODE                         
         CLC   IINTFIL(2),=C'CH'   CABLE HISP HAS EVEN ELEMENTS                 
         BE    *+10                                                             
         CLC   IINTFIL(2),=C'NH'   NHTI HAS EVEN ELEMENTS                       
         BE    *+8                                                              
         OI    DUB,X'01'           FORCE BIT 7 MATCH WITH TABLE                 
         CLC   DSPELCD,DUB         TEST IF CORRECT ELEMENT FOUND                
         BL    DEMEXP8                                                          
         BE    *+14                                                             
         IC    RF,1(RE)            BUMP TO NEXT ELEMENT                         
         AR    RE,RF                                                            
         B     DEMEXP12                                                         
*                                  GET A(ELEMENT) IF A DUPLICATE                
         MVC   LASTEL,DSPELCD                                                   
         TM    DREDUPA,X'80'                                                    
         BZ    DEMEXP14                                                         
         MVC   DUB(2),DREDUPA                                                   
         NI    DUB,X'FF'-X'80'     DUB(2)=DISP. TO ELEMENT                      
         SR    RE,RE                                                            
         ICM   RE,7,DBAREC+1       RE=A(RECORD)                                 
         AH    RE,DUB                                                           
*                                  EXPLODE ELEMENT INTO TEMP W/S                
DEMEXP14 MVC   0(4,RD),=C'TEMP'    STAMP W/S                                    
         LA    R6,12(RD)           R6=A(TEMPORARY W/S)                          
         XC    0(200,R6),0(R6)                                                  
         MVI   DUB,0               DUB+0(1)=CONTROL INDICATORS                  
         MVI   DUB+1,1             DUB+1(1)=FIELD LENGTH                        
         LA    RF,DRDATA           RF=A(FIRST DATA FIELD)                       
         CLC   IINTFIL(2),=C'CH'   CABLE HISP HAS EVEN ELEMENTS                 
         BE    *+10                                                             
         CLC   IINTFIL(2),=C'NH'   NHTI HAS EVEN ELEMENTS                       
         BE    *+12                                                             
         TM    DRECODE,X'01'       TEST IF ONE BYTE FORMAT                      
         BZ    DEMEXP15                                                         
         MVC   DUB(1),DREFCTRL     NO - EXTRACT CONTROL/LENGTH                  
         NI    DUB,X'F8'                                                        
         MVC   DUB+1(1),DREFCTRL                                                
         NI    DUB+1,X'07'                                                      
         LA    RF,DRDATA1                                                       
*                                  CALCULATE NUMBER OF DATA FIELDS              
DEMEXP15 ZIC   R1,DRELEN                                                        
         LR    R0,RF                                                            
         SR    R0,RE                                                            
         SR    R1,R0                                                            
         SR    R0,R0                                                            
         ZIC   RE,DUB+1                                                         
         DR    R0,RE                                                            
         LR    R0,R1               R0=NUMBER OF DATA FIELDS                     
         LR    R1,RE               R1=L'DATA FIELD                              
*                                  LOOP TO EXTRACT FIELDS INTO TEMP W/S         
DEMEXP16 SR    RE,RE                                                            
         TM    DUB,X'20'           TEST IF -VE FIELDS PRESENT                   
         BZ    *+14                                                             
         TM    0(RF),X'80'         YES - TEST IF THIS FIELD -VE                 
         BZ    *+6                                                              
         LCR   RE,RE               YES - SET RE TO ALL F'S                      
         IC    RE,ICMMASK-1(R1)    GET ICM MASK INTO LOW ORDER BYTE             
         EX    RE,*+8                                                           
         B     *+8                                                              
         ICM   RE,0,0(RF)          INSERT FIELD INTO RE                         
         TM    DUB,X'40'           TEST IF IMPLIED DECIMAL                      
         BO    *+8                                                              
         MH    RE,=H'10'                                                        
         ST    RE,0(R6)            SET VALUE IN TEMP W/S                        
         AR    RF,R1               BUMP TO NEXT FIELD                           
         LA    R6,4(R6)                                                         
         BCT   R0,DEMEXP16         DO FOR NUMBER OF FIELDS                      
         LA    R6,12(RD)           RESET TEMP W/S POINTER                       
*                                  MOVE FIELD FROM TEMP W/S TO WORK             
DEMEXP18 MVC   0(4,R5),0(R6)                                                    
         LA    R5,4(R5)                                                         
         LA    R6,4(R6)                                                         
         BXLE  R7,R8,DEMEXP4       LOOP UNTIL ALL FIELDS IN WORK                
*                                                                               
DEMEXPX  CLI   ACTION,C'E'         TEST IF TRANSFER                             
         BE    DEMX                NO - EXIT                                    
         CLI   DBERROR,0                                                        
         BNE   DEMX                                                             
         L     R5,AOUTPUT          YES- RESET WORK POINTER                      
         B     DEMCND              GO CREATE ELEMENTS                           
         DROP  R7                                                               
         EJECT                                                                  
* ROUTINE TO CREATE DEMO ELEMENTS FROM WORK AREA VALUES                         
*                                                                               
* ON EXIT WORK AREA CONTAINS TWO BYTE FIELD CONTAINING TOTAL LENGTH OF          
* DEMO ELEMENTS FOLLOWED BY CHAIN OF DEMO ELEMENTS DELIMITED BY X'00'.          
* IF RECORD SHOULD CONTAIN AN ORIGINAL BOOK ELEMENT ONE WILL BE                 
* CREATED AT THE END OF THE DEMO CHAIN.                                         
*                                                                               
DEMCND   LA    RE,DEMCNDX          FUDGE EXIT TO GET SOME LOCAL W/S             
         NTR1  WORK=(R6,DEMCNDDX-DEMCNDD)                                       
         USING DEMCNDD,R6          R6=A(LOCAL W/S)                              
         LM    R7,R9,ADISPS        SET-UP BXLE REGISTERS                        
         ST    R5,OLAST            SAVE WORK AREA POINTER                       
         USING DSPDTAD,R7                                                       
         XC    OAREA(3),OAREA      CLEAR OUTPUT LENGTH                          
         XC    OCONTROL,OCONTROL   CLEAR CONTROL BYTES                          
         B     DEMCND6                                                          
*                                                                               
DEMCND2  CLC   DSPELCD,OELEMENT    TEST FOR CHANGE OF ELEMENT                   
         BNE   DEMCND6                                                          
         STC   R1,OHINUM           SET HIGHEST FIELD NUMBER                     
         ICM   RF,15,0(R5)         RF=FIELD VALUE                               
         BZ    DEMCND4                                                          
         BP    *+10                                                             
         LPR   RF,RF                                                            
         OI    OCTRL,X'20'         SET -VE FIELDS CONTROL INDICATOR             
         STC   R1,OHINONZ          SET HIGHEST NON-ZERO FIELD NUMBER            
         CLM   RF,15,OHIVAL        TEST IF HIGHEST FIELD VALUE SO FAR           
         BL    *+8                                                              
         STCM  RF,15,OHIVAL        YES - SAVE                                   
*                                  BUMP TO NEXT FIELD                           
DEMCND4  LA    R5,4(R5)                                                         
         LA    R1,1(R1)                                                         
         BXLE  R7,R8,DEMCND2                                                    
         MVI   OFLAG,X'FF'         SET E-O-T INDICATOR                          
*                                                                               
DEMCND6  ST    R5,OTHIS            SAVE CURRENT WORK POINTER                    
         CLI   OHINONZ,0           ANY FIELDS PROCESSED                         
         BE    DEMCND16                                                         
*                                  YES - TRY TO DIVIDE FIELDS BY 10             
         L     RE,OLAST            RE=A(FIRST FIELD)                            
         LA    RF,OWORK            RF=A(FIRST DIVIDED FIELD)                    
         ZIC   R2,OHINONZ          R2=NUMBER OF FIELDS                          
         CHI   R2,6                NEED AT LEAST 5 VALUES                       
         BL    DEMCND9             TO MAKE THIS WORTHWHILE                      
DEMCND8  ICM   R0,15,0(RE)         R0=FIELD VALUE                               
         SRDA  R0,32                                                            
         D     R0,=F'10'                                                        
         LTR   R0,R0               TEST REMAINDER                               
         BZ    *+16                                                             
DEMCND9  OI    OCTRL,X'40'         IF NON-ZERO SET IMPLIED DECIMAL              
         L     RE,OLAST                                                         
         B     DEMCND10                                                         
*                                                                               
         STCM  R1,15,0(RF)         BUMP TO NEXT FIELD                           
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R2,DEMCND8          DO FOR NUMBER OF FIELDS                      
*                                  DIVIDE HIGHEST VALUE BY 10                   
         ICM   R0,15,OHIVAL                                                     
         SRDA  R0,32                                                            
         D     R0,=F'10'                                                        
         STCM  R1,15,OHIVAL                                                     
         LA    RE,OWORK                                                         
DEMCND10 ST    RE,DUB              SAVE INPUT POINTER                           
         ICM   RE,15,OHIVAL        GET LENGTH OF LONGEST FIELD IN R1            
         XR    RF,RF                                                            
         LA    R1,1                                                             
DEMCND12 SRDL  RE,8                                                             
         LTR   RE,RE                                                            
         BZ    *+12                                                             
         LA    R1,1(R1)                                                         
         B     DEMCND12                                                         
         STCM  RF,8,OHIVAL                                                      
         TM    OCTRL,X'20'         TEST IF -VE FIELDS PRESENT                   
         BZ    *+16                                                             
         TM    OHIVAL,X'80'        TEST IF TOP BIT OF HIGHEST VALUE ON          
         BZ    *+8                                                              
         LA    R1,1(R1)            YES - INCREASE FIELD LENGTH BY1              
         IC    R2,ICMMASK-1(R1)    R2=STCM MASK                                 
         LR    R5,R1               R5=OUTPUT FIELD LENGTH                       
         ZIC   R0,OHINONZ                                                       
         MR    R0,R0                                                            
         LA    R1,3(R1)            R1=TOTAL OUTPUT ELEMENT LENGTH               
         LA    RF,OAREA+2                                                       
         LH    RE,OAREA            RE=TOTAL ELEMENT LENGTHS SO FAR              
         AR    RF,RE               RF=A(NEXT OUTPUT SLOT)                       
         AR    RE,R1                                                            
         STH   RE,OAREA            SET NEW TOTAL OUTPUT LENGTH                  
         MVC   0(1,RF),OELEMENT    BUILD ELEMENT HEADER                         
         CHI   R1,X'FF'            MAXIMUM ELEMENT LENGTH EXCEEDED?             
         BNH   *+6                                                              
         DC    H'0'                YES                                          
         STC   R1,1(RF)                                                         
         STC   R5,2(RF)            SET FIELD LENGTH                             
         OC    2(1,RF),OCTRL       SET CONTROL BITS                             
*                                                                               
         LA    RF,3(RF)            RF=A(FIRST DATA FIELD IN ELEMENT)            
         L     RE,DUB              RE=A(INPUT VALUES)                           
         ZIC   R0,OHINONZ          R0=NUMBER OF DATA FIELDS                     
*                                  MOVE DATA FIELDS TO ELEMENT                  
DEMCND14 L     R1,0(RE)                                                         
         EX    R2,*+8                                                           
         B     *+8                                                              
         STCM  R1,0,0(RF)                                                       
         LA    RE,4(RE)            BUMP TO NEXT FIELD                           
         AR    RF,R5                                                            
         BCT   R0,DEMCND14         DO FOR NUMBER OF FIELDS                      
         MVI   0(RF),0             SET DELIMITER                                
*                                                                               
DEMCND16 CLI   OFLAG,X'FF'         TEST IF ALL FIELDS PROCESSED                 
         BE    DEMCND18                                                         
         CLI   DSPELCD,HIELCD                                                   
         BH    DEMCND18                                                         
         XC    OCONTROL,OCONTROL   NO - SET-UP FOR NEXT ELEMENT                 
         MVC   OELEMENT,DSPELCD                                                 
         LA    R1,1                                                             
         L     R5,OTHIS            SET POINTER TO NEXT FIELD                    
         MVC   OLAST,OTHIS         SET LAST POINTER TO CURRENT                  
         B     DEMCND2                                                          
*                                  ADD BOOK ELEMENT TO CHAIN                    
DEMCND18 CLI   BOOKEL,0            TEST IF ELEMENT S/B ADDED                    
         BE    DEMCND20                                                         
         SR    RE,RE                                                            
         ICM   RE,3,OAREA                                                       
         BZ    DEMCND20                                                         
         LA    RF,OAREA+2(RE)                                                   
         MVC   0(1,RF),BOOKEL                                                   
         MVI   1(RF),7                                                          
         MVC   2(2,RF),OINTFIL                                                  
         MVC   4(1,RF),OSOURCE                                                  
         MVC   5(2,RF),OBOOK                                                    
         XC    5(2,RF),=X'FFFF'                                                 
         MVI   7(RF),0                                                          
         ZIC   R0,1(RF)                                                         
         AR    RE,R0                                                            
         STH   RE,OAREA                                                         
*                                  MOVE ELEMENTS TO WORK AREA                   
DEMCND20 L     RE,AOUTPUT                                                       
         XC    0(3,RE),0(RE)                                                    
         XR    R1,R1                                                            
         ICM   R1,3,OAREA                                                       
         BZ    DEMCND22                                                         
         LA    R1,3(R1)                                                         
         LA    R0,OAREA                                                         
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         BNO   *+6                DESTRUCTIVE MOVE?                             
         DC    H'0'               YES!                                          
*                                                                               
*                                  EXIT TO DEMCNDX                              
DEMCND22 SAC   0                                                                
         LAM   AR7,AR7,=F'0'                                                    
         XIT1  ,                                                                
*                                                                               
DEMCNDX  B     DEMX                                                             
         DROP  R6,R7                                                            
         EJECT                                                                  
* GET INTERNAL FILE/MEDIA CODES FOR AN EXTERNAL FILE/MEDIA                      
*                                                                               
GETFILE  L     R1,AFMTAB                                                        
         LAM   AR1,AR1,ALET                                                     
         SAC   512                                                              
GETFILE2 CLI   0(R1),X'FF'                                                      
         BNE   GETFILE4                                                         
         SAC   0                                                                
         LAM   AR1,AR1,=F'0'                                                    
         MVI   DBERROR,INVFM                                                    
         B     DEMX                                                             
*                                                                               
GETFILE4 CLC   0(4,R1),IFILE-IFORMAT(R6)                                        
         BE    *+12                                                             
         LA    R1,6(R1)                                                         
         B     GETFILE2                                                         
         MVC   IINTFIL-IFORMAT(2,R6),4(R1)                                      
         SAC   0                                                                
         LAM   AR1,AR1,=F'0'                                                    
         BR    RE                                                               
         SPACE 1                                                                
* GET A(MASTER DISP. TABLE) FOR A FILE/MEDIA/SOURCE/BOOK                        
*                                                                               
         USING DSPHDRD,R1                                                       
GETDISP  L     R1,ADISPTAB                                                      
         XR    RF,RF                                                            
*                                                                               
GETDISP2 CLC   0(2,R1),=AL2(0)                                                  
         BNE   GETDISP4                                                         
         MVI   DBERROR,NODSPTAB                                                 
         B     DEMX                                                             
*                                                                               
GETDISP4 CLC   DSPFMS,IINTFIL-IFORMAT(R6)                                       
         BNE   *+14                                                             
         CLC   DSPSBOOK,IBOOK-IFORMAT(R6)                                       
         BNL   GETDISP6                                                         
         ICM   RF,7,DSPAET                                                      
         LA    R1,1(RF,R1)                                                      
         B     GETDISP2                                                         
*                                                                               
GETDISP6 BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
* GET EARLIEST BOOK FOR A FILE/MEDIA/SOURCE                                     
*                                                                               
         USING DSPHDRD,R1                                                       
GETFBOOK L     R1,ADISPTAB                                                      
         XC    DUB(4),DUB                                                       
*                                                                               
GETFBK2  CLC   0(2,R1),=AL2(0)                                                  
         BE    GETFBK4                                                          
         CLC   DSPFMS,IINTFIL-IFORMAT(R6)                                       
         BNE   *+10                                                             
         MVC   DUB(2),DSPSBOOK                                                  
         ICM   RF,7,DSPAET                                                      
         LA    R1,1(RF,R1)                                                      
         B     GETFBK2                                                          
*                                                                               
GETFBK4  OC    DUB(2),DUB                                                       
         BNZ   *+12                                                             
         MVI   DBERROR,NODSPTAB                                                 
         B     DEMX                                                             
         MVC   IBOOK-IFORMAT(2,R6),DUB                                          
         BR    RE                                                               
         DROP  R1                                                               
         SPACE 1                                                                
* GET LATEST BOOK FOR A SOURCE/MEDIA                                            
*                                                                               
         USING DSPHDRD,R1                                                       
GETLBOOK L     R1,ADISPTAB                                                      
*                                                                               
GETLBK2  CLC   0(2,R1),=AL2(0)                                                  
         BNE   GETLBK4                                                          
         MVI   DBERROR,NODSPTAB                                                 
         B     DEMX                                                             
*                                                                               
GETLBK4  CLC   DSPFMS,IINTFIL-IFORMAT(R6)                                       
         BNE   GETLBK6                                                          
         MVC   IBOOK-IFORMAT(2,R6),DSPSBOOK                                     
         XC    IBOOK-IFORMAT(2,R6),=X'FFFF'                                     
         BR    RE                                                               
*                                                                               
GETLBK6  ICM   RF,7,DSPAET                                                      
         LA    R1,1(RF,R1)                                                      
         B     GETLBK2                                                          
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
* SET DBNUMVLS=TOTAL NUMBER OF ENTRIES(DEMOS) IN DEMDISP TABLE                  
* DO NOT INCLUDE ELEMENTS > X'60'                                               
* RF = TOTAL NUMBER OF DEMOS (INCLUDING '6X','7X'... ELEMENTS)                  
* ADISPS = ADDRESS OF START OF TABLE                                            
* ADISPE = ADRESS OF END OF TABLE                                               
* DISPLN = LENGTH OF TABLE ENTRY                                                
SETNUMV  NTR1                                                                   
*                                                                               
         USING DBLOCKD,R4                                                       
         L     R2,ADISPE                                                        
         LA    R2,1(R2)                                                         
         L     RE,ADISPS                                                        
*                                                                               
SETNV10  S     R2,DISPLN      BACK OFF ONE ENTRY                                
         CR    R2,RE                                                            
         BL    SETNV50                                                          
         USING DSPDTAD,R2                                                       
         CLI   DSPELCD,X'60'                                                    
         BL    SETNV50                                                          
         BCTR  RF,0           DECREASE NO OF ACTUAL ENTRIES                     
         B     SETNV10        BY THE NUMBER OF ELEMS > 60                       
         DROP  R2                                                               
*                                                                               
SETNV50  STCM  RF,3,DBNUMVLS                                                    
XIT      XIT1                                                                   
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
HIELCD   EQU   X'5F'                                                            
         SPACE 1                                                                
ICMMASK  DC    X'0103070F'                                                      
         SPACE 1                                                                
* LIST OF FILE RECORDS THAT HAVE UNUSUAL (OR NO) BOOK ELEMENTS                  
*                                                                               
BKELTAB  DS    0XL4                                                             
         DC    C'EVN',X'5D'                                                     
         DC    X'FF'                                                            
         SPACE 1                                                                
* LIST OF FILES THAT MAY CONTAIN DEMOS FROM VARIOUS FILE/SOURCES                
* OUTPUT DEMO FORMAT FOR THESE FILES ARE DERIVED FROM THE INPUT                 
* FILE DEMO FORMAT IF NO OUTPUT FORMAT IS SPECIFIED BY THE CALLER               
*                                                                               
EQUTAB   DS    0CL3                                                             
         DC    C'INV'                                                           
         DC    X'FF'                                                            
*                                                                               
OBBMFMT  DC    C'TP CTCA',AL1(103,11,00)        OLD BBM DIARY FORMAT            
*                                                                               
         SPACE 3                                                                
DEMTABCL CSECT                                                                  
*                                                                               
ATABLESL DC    X'D0',3X'00'        DEMDISP                                      
         DC    X'E2',3X'00'        FILE/MEDIA TABLE                             
         DC    X'FF'               EOT                                          
TABLISTL EQU   *-ATABLESL                                                       
*                                                                               
         EJECT                                                                  
* DSECT TO COVER GLOBAL W/S                                                     
*                                                                               
DEMELD   DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
RELO     DS    F                                                                
APARM    DS    A                                                                
AOUTPUT  DS    A                                                                
ALET     DS    A                                                                
ATABLES  DS    XL(TABLISTL)                                                     
         ORG   ATABLES                                                          
ADISPTAB DS    A                                                                
AFMTAB   DS    A                                                                
         ORG                                                                    
AIDISP   DS    A                                                                
AODISP   DS    A                                                                
ADISPS   DS    A                                                                
DISPLN   DS    F                                                                
ADISPE   DS    A                                                                
ACTION   DS    CL1                                                              
LASTEL   DS    XL1                                                              
BOOKEL   DS    XL1                                                              
*                                  INPUT BOOK FORMAT BLOCK                      
IFORMAT  DS    0XL10                                                            
IFILE    DS    CL3                                                              
IMED     DS    CL1                                                              
IINTFIL  DS    CL1                                                              
IINTMED  DS    CL1                                                              
ISOURCE  DS    CL1                                                              
IBOOK    DS    XL2                                                              
IFILTER  DS    XL1                                                              
*                                  OUTPUT BOOK FORMAT BLOCK                     
OFORMAT  DS    0XL10                                                            
OFILE    DS    CL3                                                              
OMED     DS    CL1                                                              
OINTFIL  DS    CL1                                                              
OINTMED  DS    CL1                                                              
OSOURCE  DS    CL1                                                              
OBOOK    DS    XL2                                                              
OFILTER  DS    XL1                                                              
DEMELX   EQU   *                                                                
         EJECT                                                                  
* DSECT TO COVER DEMCND LOCAL W/S                                               
*                                                                               
DEMCNDD  DSECT                                                                  
OLAST    DS    A                                                                
OTHIS    DS    A                                                                
OCONTROL DS    0XL12               ELEMENT CONTROL BLOCK                        
OHINUM   DS    XL1                                                              
OHINONZ  DS    XL1                                                              
OELEMENT DS    XL1                                                              
OCTRL    DS    XL1                                                              
OHIVAL   DS    XL4                                                              
OFLAG    DS    XL1                                                              
         DS    XL3                                                              
OWORK    DS    100F                                                             
OAREA    DS    1800C                                                            
DEMCNDDX EQU   *                                                                
         EJECT                                                                  
* DEDBLOCK                                                                      
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DEDEMEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DEDEMTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030DEDEMEL   06/26/19'                                      
         END                                                                    
