*          DATA SET DDTSAR     AT LEVEL 007 AS OF 05/01/02                      
*=============================================================*                 
* THIS IS THE SOURCE CODE FOR THE RETIRED VERSION OF TSAR,    *                 
* AND IS NO LONGER IN USE.                                    *                 
* THE CODE FOR THE CURRENT IMPLEMENTATION IS IN DDTSARDINE    *                 
* MH  11/30/93                                                *                 
*=============================================================*                 
*PHASE T00A5DA,*                                                                
         TITLE 'TSAR - TEMPSTR SAVE AND RETRIEVE MODULE'                        
TSAR     CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TSAWRKX-TSAWRK,**TSAR**,RA,RR=RE,CLEAR=YES                       
         USING TSAWRK,RC           RC=A(W/S)                                    
         ST    RE,RELO                                                          
         LR    R2,R1                                                            
         USING TSARD,R2            R2=A(TSAR BLOCK)                             
         MVC   LENGTHS,TWASMALL                                                 
         TM    TSINDS,TSIXTTWA                                                  
         BZ    *+10                                                             
         MVC   LENGTHS,TWALARGE                                                 
         SR    R1,R1                                                            
         ICM   R1,7,TSABUF+1                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R1,ABUF                                                          
         SR    R1,R1                                                            
         ICM   R1,7,TSACOM+1                                                    
         MVC   VDATAMGR,CDATAMGR-COMFACSD(R1)                                   
         MVI   TSERRS,0                                                         
         SR    R1,R1                                                            
         IC    R1,TSKEYL           SET KEY LENGTH-1                             
         BCTR  R1,0                                                             
         STC   R1,KEYX                                                          
         MVC   FILE,TEMPSTR                                                     
         TM    TSINDS,TSIALLOC     TEST ALLOCATE C/I'S FROM TEMPEST             
         BZ    *+10                                                             
         MVC   FILE,TEMPEST                                                     
*                                                                               
         LA    RE,ACTNTAB                                                       
TS2      CLI   0(RE),0             TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,RE),TSACTN      MATCH ACTION TO TABLE                        
         BE    *+12                                                             
         LA    RE,L'ACTNTAB(RE)                                                 
         B     TS2                                                              
         SR    RF,RF                                                            
         ICM   RF,7,1(RE)                                                       
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
*                                                                               
TSX      CLI   TSERRS,0            SET CONDITION CODE FOR CALLER                
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INITIALSE TSARD & CLEAR TEMPSTR PAGES                    *         
***********************************************************************         
         SPACE 1                                                                
TSINI    NTR1  ,                                                                
         GOTO1 GETTERM                                                          
         L     R3,ABUF                                                          
         USING TSPAGED,R3          R3=A(TSAR BUFFER)                            
         CLI   TSPAGL,0                                                         
         BNE   *+8                                                              
         MVI   TSPAGL,1            SET LOW PAGE NUMBER IF NOT PASSED            
         CLI   TSPAGN,0                                                         
         BNE   *+8                                                              
         MVI   TSPAGN,1            SET NUMBER OF PAGES IF NOT PASSED            
         TM    TSPAGN,TSPAGNCI     TEST ALLOCATING C/I'S                        
         BNZ   *+14                                                             
         CLI   TSPAGN,TSPEXPN                                                   
         BNH   *+6                                                              
         DC    H'0'                NUMBER OF PAGES EXCEEDS MAXIMUM              
         SR    R1,R1                                                            
         ICM   R1,1,TSKEYL                                                      
         BNZ   *+6                                                              
         DC    H'0'                KEY LENGTH OF ZERO SPECIFIED                 
         CH    R1,=Y(L'TSPRECK)    TEST GREATER THAN MINIMUM KEY LENGTH         
         BH    *+8                                                              
         LH    R1,=Y(L'TSPRECK)    NO - SET TO MINIMUM LENGTH IN TABLE          
         LA    R1,TSPRECK-TSPTAB(R1)                                            
         STH   R1,TSPLTAB          SET WIDTH OF TSPTAB IN TSARD                 
         OC    TSRECL,TSRECL                                                    
         BNZ   *+6                                                              
         DC    H'0'                RECORD LENGTH NOT SPECIFIED                  
         CLC   TSRECL,=Y(TSPDATAL/8)                                            
         BNH   *+6                                                              
         DC    H'0'                RECORD LENGTH TOO LONG                       
         LH    R1,PAGELEN                                                       
         LA    R0,TSPAGED                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR TEMPSTR PAGE TO ZEROES                 
*                                                                               
         TM    TSINDS,TSIREUSE     TEST RE-USE ALREADY ALLOCATED C/I            
         BNZ   TSINI4                                                           
         TM    TSINDS,TSIALLOC     TEST ALLOC FROM TEMPEST                      
         BZ    TSINI4                                                           
         MVC   FLAG,TSPAGN                                                      
         NI    FLAG,255-TSPAGNCI                                                
         SR    R0,R0                                                            
         ICM   R0,8,FLAG           R0=NUMBER OF PAGES OR C/I'S                  
         TM    TSPAGN,TSPAGNCI     TEST C/I'S REQUESTED                         
         BNZ   *+8                                                              
         ICM   R0,4,=C'P'          NO - RESERVE PAGES                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRSRV,FILE,(R0),TSPAGED                           
         BE    *+12                                                             
         MVI   TSERRS,TSEEOF       SET E-O-F ON ALLOCATE FAILURE                
         B     TSINIX                                                           
         LH    R0,10(R1)           GET NUMBER OF OF PAGES PER TRACK             
         MH    R0,12(R1)           MULTIPLY BY PREVIOUS C/I'S ALLOCATED         
         AH    R0,=H'1'            PLUS ONE                                     
         STC   R0,TSPAGL           =LOW PAGE NUMBER IN THIS ALLOCATION          
         TM    TSPAGN,TSPAGNCI     TEST PAGES REQUESTED                         
         BZ    TSINI4                                                           
         LH    R0,10(R1)           GET NUMBER OF PAGES PER TRACK                
         MH    R0,08(R1)           MULTIPLY BY C/I'S ALLOCATED                  
         STC   R0,TSPAGN           =NUMBER OF PAGES ALLOCATED                   
*                                                                               
TSINI4   LA    R0,TSPTAB           CLEAR TSPTAB TO BINARY ZEROES                
         SR    R1,R1                                                            
         IC    R1,TSPAGN                                                        
         MH    R1,TSPLTAB                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         SR    R0,R0                                                            
         IC    R0,TSPAGN           R0=NUMBER OF TEMPSTR PAGES                   
         SR    R5,R5                                                            
         IC    R5,TSPAGL                                                        
         AR    R5,R0                                                            
         BCTR  R5,0                R5=HIGHEST PAGE NUMBER TO BE USED            
         STC   R5,TSPAGH                                                        
         LR    R4,R0                                                            
         BCTR  R4,0                                                             
         MH    R4,TSPLTAB                                                       
         LA    R4,TSPTAB(R4)                                                    
         USING TSPTAB,R4           R4=A(LAST TSPTAB ENTRY)                      
         MVC   TSPRECI,TSRECI                                                   
         MVC   TSPKEYL,TSKEYL                                                   
         MVC   TSPRECL,TSRECL                                                   
         MVC   TSPTERM,TERM                                                     
*                                                                               
TSINI6   STC   R5,TSPAGEN          READ NEXT TEMPSTR PAGE                       
         STC   R5,TSPPAGE                                                       
         MVC   TSPFREE,TOTFREE                                                  
         MVI   TSPRECK,X'FF'       SET HIGH KEY TO X'FF...FF'                   
         SR    R1,R1                                                            
         IC    R1,TSKEYL                                                        
         SH    R1,=H'2'            R1=KEYLEN-2                                  
         BM    *+14                                                             
         EX    R1,*+4                                                           
         MVC   TSPRECK+1(0),TSPRECK                                             
         GOTO1 VDATAMGR,DMCB,DMWRITE,FILE,(TSPPAGE,0),TSPAGED                   
         BE    TSINI8                                                           
         TM    TSINDS,TSIALLOC+TSIRTNAF  ALLOCATION FAILURE-                    
         BO    *+6                       TEST WHETHER TO RETURN                 
         DC    H'0'                                                             
         MVI   TSERRS,TSEALF       YES-SET RETURN CODE AND RETURN               
         B     TSX                                                              
*                                                                               
TSINI8   SH    R4,TSPLTAB                                                       
         BCTR  R5,0                DECREMENT PAGE NUMBER                        
         BCT   R0,TSINI6                                                        
         DROP  R4                                                               
*                                                                               
         MVI   TSPINDS,1           SET LOW PAGE IN BUFFER                       
         MVI   TSUBUF,1            SET 1 BUFFER USED                            
         OI    TSINDS,TSIINIOK                                                  
TSINIX   B     TSX                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A RECORD BY KEY                                      *         
***********************************************************************         
         SPACE 1                                                                
TSADD    NTR1  ,                                                                
         BAS   RE,TSRDH            READ HIGH FOR KEY                            
         BNE   *+12                                                             
         MVI   TSERRS,TSEDUP       IF FOUND SET DUPLICATE KEY                   
         B     TSADDX                                                           
         MVC   ERRS,TSERRS         SAVE ERROR BYTE                              
         MVI   TSERRS,0            RESET ERROR BYTE                             
         L     R4,TABA                                                          
*                                                                               
         TM    TSINDS,TSIANYAD     TEST ANY RECORDS ADDED                       
         BNZ   *+8                                                              
         LA    R4,TSPTAB                                                        
         USING TSPTAB,R4                                                        
         GOTO1 GETP,TSPTAB         GET PAGE INTO BUFFER                         
         ICM   R4,15,TABA          R4=A(ENTRY FOR LAST PAGE WITH DATA)          
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,TSAREC+1       RE=A(RECORD TO BE ADDED)                     
         BNZ   *+6                                                              
         DC    H'0'                RECORD ADDRESS NOT PASSED                    
         ST    RE,AREC             SAVE A(RECORD TO BE ADDED)                   
         LR    RF,RE                                                            
         LH    R1,TSRECL                                                        
         TM    TSRECI,TSRVAR       TEST VARIABLE LENGTH RECORDS                 
         BZ    *+12                                                             
         LA    RF,2(RF)                                                         
         ICM   R1,3,0(RE)                                                       
         ST    RF,KEYA                                                          
         STH   R1,RECL             SAVE RECORD LENGTH                           
         CH    R1,TSRECL           ENSURE RECORD NOT TOO BIG                    
         BNH   *+6                                                              
         DC    H'0'                                                             
         CH    R1,TSPFREE          TEST ENOUGH FREE SPACE AVAILABLE             
         BH    TSADD2                                                           
         CLC   TSPFREE,TOTFREE     TEST THIS PAGE IS EMPTY                      
         BNE   *+10                                                             
         MVC   RECA,DATA           YES - ADD RECORD TO START OF BLOCK           
         BAS   RE,ADDR                                                          
         TM    TSINDS,TSIANYAD     TEST FIRST RECORD ADDED                      
         BNZ   *+10                                                             
         MVC   TSRNUM,=H'1'        YES - SET RECORD NUMBER                      
         OI    TSINDS,TSIANYAD                                                  
         B     TSADDX                                                           
*                                                                               
TSADD2   TM    ERRS,TSEEOF         TEST NEW FILE HIGH KEY TO ADD                
         BZ    TSADD3                                                           
         CLC   TSPPAGE,TSPAGH      TEST THIS IS HIGHEST PAGE                    
         BNE   *+12                                                             
         MVI   TSERRS,TSEEOF       YES - SET END-OF-FILE & EXIT                 
         B     TSADDX                                                           
         AH    R4,TSPLTAB          POINT TO NEXT PAGE TABLE ENTRY               
         CLC   TSPFREE,TOTFREE     THIS PAGE MUST BE EMPTY                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETP,TSPTAB         GET LAST PAGE INTO BUFFER                    
         MVC   RECA,DATA           THIS IS FRIST RECORD IN BLOCK                
         BAS   RE,ADDR                                                          
         B     TSADDX                                                           
         DROP  R4                                                               
*                                                                               
TSADD3   L     RE,KEYA             NEW RECORD WON'T FIT IN PAGE                 
         SR    R1,R1                                                            
         IC    R1,KEYX                                                          
         L     RF,DATA                                                          
         TM    TSRECI,TSRVAR                                                    
         BZ    *+8                                                              
         LA    RF,2(RF)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)       IS KEY LOWER THAN FIRST IN PAGE              
         BNL   TSADD4                                                           
         CLC   TSPPAGE,TSPAGL      TEST THIS IS THE FIRST PAGE                  
         BE    TSADD4                                                           
         SH    R4,TSPLTAB          BACK-UP TO PREVIOUS PAGE                     
         USING TSPTAB,R4                                                        
         CLC   RECL,TSPFREE        TEST ENOUGH FREE SPACE HERE                  
         BNH   *+12                                                             
         AH    R4,TSPLTAB                                                       
         B     TSADD4                                                           
         GOTO1 GETP,TSPTAB         YES - GET PAGE INTO BUFFER                   
         L     RE,DATA                                                          
         AH    RE,TOTFREE                                                       
         SH    RE,TSPFREE                                                       
         ST    RE,RECA             SET RECORD INSERT ADDRESS                    
         BAS   RE,ADDR             ADD RECORD TO PAGE                           
         B     TSADDX                                                           
*                                                                               
TSADD4   ST    R4,SAVE             SAVE A(PAGE TABLE ENTRY)                     
         MVC   RNUM,=H'1'                                                       
         LH    R0,TSRECL                                                        
         TM    TSRECI,TSRVAR                                                    
         BZ    *+8                                                              
         SLL   R0,3                                                             
         MVI   FLAG,0                                                           
*                                                                               
TSADD6   CH    R0,TSPFREE          TEST IF ENOUGH SPACE IN ANY PAGE             
         BNH   TSADD8              YES                                          
         CLC   TSPPAGE,TSPAGH      TEST LAST PAGE TABLE ENTRY                   
         BE    *+12                                                             
         AH    R4,TSPLTAB          NO - BUMP TO NEXT                            
         B     TSADD6                                                           
*                                                                               
         CLI   FLAG,0              TEST COMPRESSION DONE                        
         BE    *+12                                                             
         MVI   TSERRS,TSEEOF       YES - RECORD WILL NOT FIT                    
         B     TSADDX                                                           
         BAS   RE,TSCPS            COMPRESS DATA                                
         BAS   RE,TSRDH            READ HIGH FOR RECORD                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R4,TABA             MAY NOT BE SAME PAGE AS LAST TIME            
         ST    R4,SAVE                                                          
         MVI   FLAG,1              SET COMPRESSION DONE                         
         B     TSADD6                                                           
*                                                                               
TSADD8   L     R4,SAVE             POINT TO INSERT PAGE ENTRY                   
         L     RE,4(RD)            ACQUIRE TSRECL*4 BYTES OF W/S                
         ST    RD,IREC                                                          
         AR    RD,R0                                                            
         ST    RD,OREC                                                          
         AR    RD,R0                                                            
         LA    RD,7(RD)                                                         
         SRL   RD,3                                                             
         SLL   RD,3                                                             
         ST    RD,8(RE)            SET NEW FORWARD LINK                         
         ST    RE,4(RD)            SET NEW BACKWARD LINK                        
*                                                                               
TSADD10  L     RE,RECA                                                          
         S     RE,DATA             RE=DISPLACEMENT TO NEW RECORD                
         AH    RE,RECL             ADD ON RECORD LENGTH                         
         CH    RE,TOTFREE          WILL NEW RECORD FIT INTO CURRENT             
         BNH   TSADD12             PAGE AT LOGICAL KEY INSERTION POINT          
         L     R0,IREC             NO - MUST GO INTO NEXT PAGE                  
         LH    R1,RECL                                                          
         L     RE,AREC                                                          
         C     RE,OREC                                                          
         BNE   *+6                                                              
         DC    H'0'                MOVED DATA DOES NOT FIT IN PAGE              
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE RECORD TO INPUT RECORD AREA             
         MVC   ILEN,RECL           SAVE NEW RECORD LENGTH                       
         L     R0,IREC                                                          
         AH    R0,ILEN                                                          
         L     RE,RECA                                                          
         LR    R1,RE                                                            
         S     R1,DATA                                                          
         AH    R1,TSPFREE                                                       
         LNR   R1,R1                                                            
         AH    R1,TOTFREE          R1=LENGTH TO BE REMOVED                      
         ST    R1,SAVE                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R1,SAVE                                                          
         AH    R1,ILEN                                                          
         STH   R1,ILEN                                                          
         L     R1,SAVE                                                          
         AH    R1,TSPFREE                                                       
         STH   R1,TSPFREE                                                       
         L     R1,SAVE             CLEAR MOVED AREA TO BINARY ZEROES            
         L     R0,RECA                                                          
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LH    R0,TSPRECN          DECREMENT NUMBER OF RECORDS IN PAGE          
         SH    R0,RNUM                                                          
         BCTR  R0,0                                                             
         STH   R0,TSPRECN                                                       
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO RECORDS REMAINING IN PAGE                 
         BAS   RE,SETK             SET HIGHEST KEY IN PAGE                      
         OI    TSPINDS,TSPIPWP                                                  
         B     TSADD18                                                          
*                                                                               
TSADD12  CLC   RECL,TSPFREE        TEST ENOUGH FREE SPACE IN PAGE               
         BH    TSADD14                                                          
         CLC   AREC,OREC           YES - MUST BE A STAGING RECORD               
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,ADDR             YES - ADD RECORD(S) TO PAGE                  
         B     TSADDX                                                           
*                                                                               
TSADD14  L     R1,DATA             R1=INSERTION POINT OF NEW RECORD             
         LR    RE,R1                                                            
         AH    RE,TOTFREE          RE=A(END OF PAGE+1)                          
         SH    RE,RECL                                                          
         LH    R0,TSRECL                                                        
         LH    RF,TSPRECN                                                       
*                                                                               
TSADD16  TM    TSRECI,TSRVAR       ADD REC LENS TILL PAGE SIZE EXCEEDED         
         BZ    *+8                                                              
         ICM   R0,3,0(R1)                                                       
         BCTR  RF,0                                                             
         AR    R1,R0                                                            
         CR    R1,RE                                                            
         BNH   TSADD16                                                          
         LA    RF,1(RF)                                                         
         STH   RF,RNUM             SET NUMBER OF RECORDS TO REMOVE              
         SR    R1,R0               R1=A(FIRST RECORD TO BE REMOVED)             
         LR    RE,R1                                                            
         S     R1,DATA             R1=DISPLACEMENT TO MOVE POINT                
         AH    R1,TSPFREE                                                       
         LNR   R1,R1                                                            
         AH    R1,TOTFREE                                                       
         STH   R1,ILEN             R1=LENGTH TO BE REMOVED FROM PAGE            
         L     R0,IREC                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE RECORD TO STAGING AREA                  
         LH    R1,TSPFREE                                                       
         AH    R1,ILEN                                                          
         STH   R1,TSPFREE          INCREMENT FREE SPACE                         
         LH    R0,TSPRECN                                                       
         SH    R0,RNUM                                                          
         STH   R0,TSPRECN                                                       
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO RECORDS REMAINING IN BLOCK                
         BAS   RE,ADDR             ADD USER RECORD TO PAGE                      
*                                                                               
TSADD18  AH    R4,TSPLTAB          POINT TO NEXT PAGE TABLE ENTRY               
         GOTO1 GETP,TSPTAB                                                      
         MVC   RECA,DATA           SET ADDRESS TO FIRST RECORD                  
         SR    R1,R1                                                            
         ICM   R1,3,ILEN                                                        
         STCM  R1,3,OLEN                                                        
         L     R0,OREC                                                          
         L     RE,IREC                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVC   RECL,OLEN                                                        
         MVC   AREC,OREC                                                        
         MVC   RNUM,=H'1'                                                       
         B     TSADD10                                                          
*                                                                               
TSADDX   B     TSX                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO COMRESS DATA SET                                         *         
***********************************************************************         
         SPACE 1                                                                
TSCPS    CLI   TSNBUF,1            TEST AT LEAST TWO BUFFERS AVAILABLE          
         BER   RE                                                               
         NTR1  ,                                                                
TSCPSX   B     TSX                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT A RECORD BY NUMBER                                   *         
***********************************************************************         
         SPACE 1                                                                
TSPUT    OC    TSRNUM,TSRNUM       TEST RECORD NUMBER SET                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BAS   RE,TSGET            GET RECORD                                   
         BNE   *+8                                                              
         BAS   RE,TSUPD            UPDATE RECORD IF FOUND                       
TSPUTX   B     TSX                                                              
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO PUT A RECORD BY KEY                                      *         
***********************************************************************         
         SPACE 1                                                                
TSWRT    BAS   RE,TSRDH            READ HIGH FOR KEY                            
         BNE   *+8                                                              
         BAS   RE,TSUPD            UPDATE RECORD IF FOUND                       
TSWRTX   B     TSX                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UPDATE A RECORD IN PAGE                                  *         
***********************************************************************         
         SPACE 1                                                                
TSUPD    NTR1  ,                                                                
         L     R4,TABA             R4=A(PAGE TABLE ENTRY)                       
         USING TSPTAB,R4                                                        
         L     RF,RECA             RF=A(RECORD IN PAGE BUFFER)                  
         L     RE,TSAREC           RE=A(NEW RECORD)                             
         TM    TSRECI,TSRVAR       TEST VARIABLE LENGTH RECORDS                 
         BZ    *+12                                                             
         LA    RE,2(RE)            YES - POINT TO KEY FIELDS                    
         LA    RF,2(RF)                                                         
         SR    R1,R1                                                            
         IC    R1,KEYX                                                          
         EX    R1,*+8              MATCH OLD/NEW KEY VALUES                     
         B     *+10                                                             
         CLC   0(0,RF),0(RE)                                                    
         BE    *+12                                                             
         MVI   TSERRS,TSERNF       IF KEY CHANGED SET RECORD NOT FOUND          
         B     TSUPDX                                                           
         LH    R1,TSRECL           NO - MOVE RECORD TO PAGE                     
         TM    TSRECI,TSRVAR       TEST VARIABLE LENGTH RECORDS                 
         BNZ   *+12                                                             
         LR    R0,RF                                                            
         LR    RF,R1                                                            
         B     TSUPD2                                                           
         L     RF,RECA             RF=A(RECORD IN PAGE BUFFER)                  
         L     RE,TSAREC           RE=A(NEW RECORD)                             
         CLC   0(2,RE),0(RF)       TEST RECORD LENGTH HAS CHANGED               
         BNE   TSUPD4                                                           
         LR    R0,RF                                                            
         SR    R1,R1               NO - UPDATE RECORD IN PAGE                   
         ICM   R1,3,0(RE)                                                       
         LR    RF,R1                                                            
*                                                                               
TSUPD2   MVCL  R0,RE               MOVE UPDATED RECORD TO PAGE                  
         OI    TSPINDS,TSPIPWP     SET PAGE WRITE PENDING                       
         B     TSUPDX                                                           
*                                                                               
TSUPD4   BAS   RE,TSDEL            DELETE THIS RECORD                           
         BAS   RE,TSADD            ADD RECORD WITH NEW LENGTH                   
*                                                                               
TSUPDX   B     TSX                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A RECORD BY NUMBER (IF RECORD NUMBER IS SET) OR   *         
* BY KEY - DELETED RECORD IS RETURNED IN CALLERS BUFFER               *         
***********************************************************************         
         SPACE 1                                                                
TSDEL    NTR1  ,                                                                
         ICM   RF,15,RECA          TEST GET PREVIOUSLY DONE                     
         BNZ   TSDEL2                                                           
         LA    RF,TSGET                                                         
         OC    TSRNUM,TSRNUM       TEST RECORD NUMBER GIVEN                     
         BNZ   *+8                                                              
         LA    RF,TSRDH            NO - DO READ FOR KEY                         
         BASR  RE,RF               GET RECORD ADDRESS                           
         BNE   TSDELX              EXIT IF NOT FOUND                            
         L     RF,RECA                                                          
*                                                                               
TSDEL2   L     R4,TABA                                                          
         USING TSPTAB,R4                                                        
         LR    R0,RF               R0=TO ADDRESS                                
         LR    R1,RF                                                            
         S     R1,DATA                                                          
         LNR   R1,R1                                                            
         AH    R1,TOTFREE          R1=TO LENGTH                                 
         LH    RE,TSRECL                                                        
         TM    TSRECI,TSRVAR                                                    
         BZ    *+8                                                              
         ICM   RE,3,0(RF)                                                       
         STH   RE,RECL                                                          
         AR    RE,RF               RE=FROM ADDRESS                              
         LR    RF,RE                                                            
         S     RF,DATA                                                          
         LNR   RF,RF                                                            
         AH    RF,TOTFREE          RF=FROM LENGTH                               
         MVCL  R0,RE               DELETE THIS RECORD                           
*                                                                               
         OI    TSPINDS,TSPIPWP     SET PAGE WRITE PENDING                       
         LH    R0,TSPFREE          INCREMENT FREE BYTES IN PAGE                 
         AH    R0,RECL                                                          
         STH   R0,TSPFREE                                                       
         LH    R0,TSPRECN          DECREMENT RECORDS IN PAGE                    
         BCTR  R0,0                                                             
         STH   R0,TSPRECN                                                       
*                                                                               
         L     R1,DATA                                                          
         LH    RE,TSRECL                                                        
         LTR   R0,R0               TEST ANY RECORDS LEFT IN PAGE                
         BZ    *+12                                                             
         BAS   RE,SETK             SET HIGHEST KEY IN PAGE                      
         B     TSDEL4                                                           
         MVI   TSPRECK,X'FF'       NO SET DUMMY HIGH KEY                        
         SR    R1,R1                                                            
         IC    R1,TSKEYL                                                        
         SH    R1,=H'2'            R1=KEYLEN-2                                  
         BM    *+14                                                             
         EX    R1,*+4                                                           
         MVC   TSPRECK+1(0),TSPRECK                                             
         DROP  R4                                                               
*                                                                               
TSDEL4   LA    R4,TSPTAB           TEST ANY RECORD REMAINING                    
         USING TSPTAB,R4                                                        
         SR    R0,R0                                                            
         IC    R0,TSPAGN                                                        
         OC    TSPRECN,TSPRECN                                                  
         BNZ   TSDELX                                                           
         AH    R4,TSPLTAB                                                       
         BCT   R0,*-14                                                          
         NI    TSINDS,255-TSIANYAD                                              
TSDELX   B     TSX                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ HIGH FOR A KEY - IF KEY IS NOT FOUND TSERNF IS SET  *         
***********************************************************************         
         SPACE 1                                                                
TSRDH    NTR1  ,                                                                
         MVI   TSERRS,0                                                         
         LA    R4,TSPTAB                                                        
         USING TSPTAB,R4           R4=A(PAGE TABLE)                             
         XC    TABA,TABA           CLEAR ENTRY POINTER                          
         SR    R0,R0                                                            
         IC    R0,TSPAGN           R0=NUMBER OF PAGE TABLE ENTRIES              
         SR    RE,RE                                                            
         ICM   RE,7,TSAREC+1       RE=A(RECORD)                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    TSRECI,TSRVAR                                                    
         BZ    *+8                                                              
         LA    RE,2(RE)            RE=A(RECORD KEY)                             
         SR    RF,RF                                                            
         IC    RF,KEYX                                                          
         EX    RF,*+4                                                           
         MVC   RKEY(0),0(RE)       SAVE RECORD KEY                              
         SR    R5,R5               R5=RECORD NUMBER COUNTER                     
*                                                                               
TSRDH2   OC    TSPRECN,TSPRECN     TEST ANY RECORD IN PAGE                      
         BZ    TSRDH4                                                           
         AH    R5,TSPRECN          INCREMENT N'RECORDS SO FAR                   
         ST    R4,TABA             SAVE A LAST PAGE CONTAINING DATA             
         EX    RF,*+8              TEST KEY LOWER THAN HIGHEST IN PAGE          
         B     *+10                                                             
         CLC   RKEY(0),TSPRECK                                                  
         BNH   TSRDH6                                                           
*                                                                               
TSRDH4   AH    R4,TSPLTAB          BUMP TO NEXT PAGE                            
         BCT   R0,TSRDH2                                                        
         MVI   TSERRS,TSEEOF+TSERNF                                             
         ICM   R4,15,TABA          POINT TO LAST PAGE ENTRY WITH DATA           
         BZ    TSRDHX              EXIT IF ALL PAGES ARE EMPTY                  
*                                                                               
TSRDH6   SH    R5,TSPRECN          SUBTRACT N'RECORDS IN LAST PAGE              
         GOTO1 GETP,TSPTAB         READ PAGE INTO BUFFER                        
         L     R1,DATA             R1=A(FIRST RECORD IN PAGE)                   
         LH    R0,TSPRECN          R0=NUMBER OF RECORDS IN PAGE                 
         SR    RF,RF                                                            
         IC    RF,KEYX                                                          
*                                                                               
TSRDH8   LA    R5,1(R5)            BUMP RECORD NUMBER                           
         LR    RE,R1                                                            
         TM    TSRECI,TSRVAR       TEST VARIABLE LENGTH RECORDS                 
         BZ    *+8                                                              
         LA    RE,2(RE)                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   RKEY(0),0(RE)       MATCH REQ KEY TO BUFFER KEY                  
         BE    TSRDH12             EQUAL MEANS FOUND                            
         BL    TSRDH10             LOW MEANS NOT FOUND                          
         LH    RE,TSRECL           CONTINUE SEARCH                              
         TM    TSRECI,TSRVAR                                                    
         BZ    *+8                                                              
         ICM   RE,3,0(R1)          BUMP TO NEXT RECORD IN PAGE                  
         AR    R1,RE                                                            
         BCT   R0,TSRDH8                                                        
         LA    R5,1(R5)            R5=NEXT RECORD NUMBER (FOR ADD)              
TSRDH10  OI    TSERRS,TSERNF       SET RECORD NOT FOUND                         
*                                                                               
TSRDH12  LR    RE,R1               RETURN RECORD IN CALLERS BUFFER              
         ST    RE,RECA             SET RECORD NUMBER                            
         STH   R5,TSRNUM           SET RECORD ADDRESS                           
         CLI   TSACTN,TSAWRT       TEST WRITE ACTION                            
         BE    TSRDHX              YES - EXIT                                   
         CLI   TSACTN,TSAADD       TEST ADD ACTION                              
         BE    TSRDHX              YES - EXIT                                   
         LH    R1,TSRECL                                                        
         TM    TSRECI,TSRVAR                                                    
         BZ    *+8                                                              
         ICM   R1,3,0(RE)                                                       
         SR    R0,R0                                                            
         ICM   R0,7,TSAREC+1                                                    
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE RECORD TO CALLER'S I/O AREA             
*                                                                               
TSRDHX   B     TSX                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET A RECORD BY NUMBER OR NEXT RECORD BY NUMBER          *         
***********************************************************************         
         SPACE 1                                                                
TSNXT    LH    RF,TSRNUM                                                        
         LA    RF,1(RF)                                                         
         STH   RF,TSRNUM                                                        
         SPACE 1                                                                
TSGET    NTR1  ,                                                                
         LA    R4,TSPTAB           R4=A(PAGE TABLE)                             
         USING TSPTAB,R4                                                        
         SR    R0,R0                                                            
         IC    R0,TSPAGN           R0=NUMBER OF PAGE TABLE ENTRIES              
         SR    RE,RE                                                            
         AH    RE,TSPRECN                                                       
         CH    RE,TSRNUM           TEST RECORD WITHIN THIS PAGE                 
         BNL   TSGET2                                                           
         AH    R4,TSPLTAB          BUMP TO NEXT PAGE                            
         BCT   R0,*-16                                                          
         MVI   TSERRS,TSEEOF+TSERNF                                             
         B     TSGETX                                                           
*                                                                               
TSGET2   SH    RE,TSPRECN          RE=LOW RECORD NUMBER THIS PAGE               
         LH    R0,TSRNUM                                                        
         SR    R0,RE               R0=RECORD NUMBER WITHIN PAGE                 
         GOTO1 GETP,TSPTAB         GET PAGE INTO CORE BUFFER                    
         L     RE,DATA             RE=A(FIRST RECORD IN PAGE)                   
         LH    R1,TSRECL           R1=RECORD LENGTH                             
*                                                                               
TSGET4   TM    TSRECI,TSRVAR       TEST VARIABLE LENGTH RECORDS                 
         BZ    *+8                                                              
         ICM   R1,3,0(RE)          R1=RECORD LENGTH                             
         BCT   R0,*+8                                                           
         B     *+10                                                             
         AR    RE,R1               R1=A(NEXT RECORD IN PAGE)                    
         B     TSGET4                                                           
*                                                                               
         ST    RE,RECA             RETURN RECORD IN CALLERS I/O AREA            
         CLI   TSACTN,TSAPUT       TEST PUT ACTION                              
         BE    TSRDHX                                                           
         CLI   TSACTN,TSAWRT       TEST WRITE ACTION                            
         BE    TSRDHX                                                           
         SR    R0,R0                                                            
         ICM   R0,7,TSAREC+1                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
TSGETX   B     TSX                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WRITE UPDATED TEMPSTR PAGES TO DISK FOR LATER RESTORE    *         
***********************************************************************         
         SPACE 1                                                                
TSSAV    NTR1  ,                                                                
         LA    R4,TSPTAB                                                        
         USING TSPTAB,R4           R4=A(PAGE TABLE)                             
         SR    R0,R0                                                            
         IC    R0,TSPAGN           R0=NUMBER OF PAGE TABLE ENTRIES              
*                                                                               
TSSAV2   TM    TSPINDS,TSPIPWP     TEST PAGE WRITE PENDING                      
         BZ    TSSAV4                                                           
         MVC   BUFN,TSPINDS        YES - GET CORE BUFFER ADDRESS                
         NI    BUFN,TSPIBUF                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         IC    RF,BUFN                                                          
         BCTR  RF,0                                                             
         MH    RF,PAGELEN                                                       
         A     RF,ABUF                                                          
         MVC   TSPRECS-TSPAGED(,RF),TSPRECN                                     
         GOTO1 VDATAMGR,DMCB,DMWRITE,FILE,(TSPPAGE,0),(RF)                      
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    TSPINDS,255-TSPIPWP-TSPIHLD                                      
*                                                                               
TSSAV4   AH    R4,TSPLTAB          BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,TSSAV2                                                        
TSSAVX   B     TSX                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO RESTORE TSARD FROM SAVED TEMPSTR PAGES                   *         
***********************************************************************         
         SPACE 1                                                                
TSRES    NTR1  ,                                                                
         GOTO1 GETTERM                                                          
         L     R3,ABUF                                                          
         USING TSPAGED,R3                                                       
         LA    R1,TSPDATA          SET A(FIRST RECORD IN PAGE)                  
         ST    R1,DATA                                                          
         CLI   TSPAGL,0                                                         
         BNE   *+8                                                              
         MVI   TSPAGL,1            SET LOW PAGE NUMBER IF NOT PASSED            
         CLI   TSPAGN,0                                                         
         BNE   *+8                                                              
         MVI   TSPAGN,1            SET NUMBER OF PAGES IF NOT PASSED            
         CLI   TSPAGN,TSPEXPN                                                   
         BNH   *+6                                                              
         DC    H'0'                NUMBER OF PAGES EXCEEDS MAXIMUM              
         SR    R1,R1                                                            
         ICM   R1,1,TSKEYL                                                      
         BNZ   *+6                                                              
         DC    H'0'                KEY LENGTH OF ZERO SPECIFIED                 
         CH    R1,=Y(L'TSPRECK)    TEST GREATER THAN MINIMUM KEY LENGTH         
         BH    *+8                                                              
         LH    R1,=Y(L'TSPRECK)    NO - SET TO MINIMUM LENGTH IN TABLE          
         LA    R1,TSPRECK-TSPTAB(R1)                                            
         STH   R1,TSPLTAB          SET WIDTH OF TSPTAB IN TSARD                 
         XC    TSRECI(TSUBUF-TSRECI),TSRECI                                     
         LA    R0,TSPTAB           CLEAR TSPTAB TO BINARY ZEROES                
         SR    R1,R1                                                            
         IC    R1,TSPAGN                                                        
         MH    R1,TSPLTAB                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         SR    R6,R6                                                            
         IC    R6,TSPAGN           R6=NUMBER OF PAGES                           
         SR    R5,R5                                                            
         IC    R5,TSPAGL                                                        
         AR    R5,R6                                                            
         BCTR  R5,0                                                             
         STC   R5,TSPAGH           R5=HIGHEST PAGE NUMBER                       
         LR    R4,R6                                                            
         BCTR  R4,0                                                             
         MH    R4,TSPLTAB                                                       
         LA    R4,TSPTAB(R4)                                                    
         USING TSPTAB,R4                                                        
*                                                                               
TSRES2   STC   R5,TSPPAGE          READ NEXT TEMPSTR PAGE                       
         ICM   RF,12,=C'L='                                                     
         ICM   RF,3,PAGELEN                                                     
         GOTO1 VDATAMGR,DMCB,DMREAD,FILE,(TSPPAGE,0),TSPAGED,,(RF)              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TSPPAGE,TSPAGEN     MATCH PAGE NUMBER                            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLM   R5,1,TSPAGH         TEST FIRST PAGE READ                         
         BNE   TSRES4                                                           
         MVC   TSRECI(TSRNUM-TSRECI),TSPRECI                                    
         SR    R1,R1                                                            
         IC    R1,TSKEYL           SET KEY LENGTH-1                             
         BCTR  R1,0                                                             
         STC   R1,KEYX                                                          
*                                                                               
TSRES4   CLC   TSRECI(TSRNUM-TSRECI),TSPRECI                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TSPPAGE,TSPAGEN     SET PAGE NUMBER                              
         OC    TSPRECN,TSPRECS     SET NUMBER OF RECORDS                        
         BNZ   TSRES6                                                           
         MVI   TSPRECK,X'FF'                                                    
         SR    R1,R1                                                            
         IC    R1,TSKEYL                                                        
         SH    R1,=H'2'            R1=KEYLEN-2                                  
         BM    *+14                                                             
         EX    R1,*+4                                                           
         MVC   TSPRECK+1(0),TSPRECK                                             
         MVC   TSPFREE,TOTFREE                                                  
         B     TSRES8                                                           
*                                                                               
TSRES6   BAS   RE,SETK             SET HIGHEST KEY & COUNT USED BYTES           
         LH    RE,TOTFREE          SET NUMBER OF FREE BYTES IN PAGE             
         SH    RE,USED                                                          
         BNM   *+6                                                              
         DC    H'0'                DIE IF FREE SPACE NEGATIVE                   
         STH   RE,TSPFREE                                                       
         OI    TSINDS,TSIANYAD     SET AT LEAST ONE RECORD ADDED                
*                                                                               
TSRES8   SH    R4,TSPLTAB                                                       
         BCTR  R5,0                DECREMENT PAGE NUMBER                        
         BCT   R6,TSRES2                                                        
         DROP  R4                                                               
*                                                                               
         MVI   TSPINDS,1           SET FIRST PAGE IN CORE BUFFER                
         MVI   TSUBUF,1                                                         
         OI    TSINDS,TSIINIOK     SET INITIALISATION OK                        
TSRESX   B     TSX                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD ONE OR MORE RECORDS TO A PAGE                        *         
*                                                                     *         
* NTRY - R4=A(PAGE TABLE ENTRY)                                       *         
*        DATA=A(FIRST RECORD IN DATA PAGE)                            *         
*        AREC=A(RECORD OR RECORDS TO BE ADDED TO PAGE)                *         
*        RECL=RECORD LENGTH                                           *         
*        RECA=INSERTION POINT OF RECORD TO BE ADDED                   *         
*                                                                     *         
* EXIT - TSPFREE CONTAINS NUMBER OF FREE BYTES IN PAGE                *         
*        TSPRECN CONTAINS NUMBER OF RECORDS IN PAGE                   *         
*        TSPINDS TSPIPWP BIT SET TO INDICATE PAGE WRITE PENDING       *         
*        TSPRECK CONTAINS KEY OF LAST RECORD IN PAGE                  *         
*        NREC CONTAINS NUMBER OF RECORDS ADDED TO PAGE                *         
***********************************************************************         
         SPACE 1                                                                
ADDR     NTR1  ,                                                                
         OC    RECA,RECA           ENSURE RECORD ADDRESS SET                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING TSPTAB,R4                                                        
         L     RE,AREC             COUNT NUMBER OF RECORDS TO BE ADDED          
         SR    R0,R0                                                            
         ICM   R0,3,RECL           R0=LENGTH OF RECORD(S) TO BE ADDED           
         BNZ   *+6                                                              
         DC    H'0'                INVALID RECORD LENGTH                        
         LH    R1,TSRECL           R1=FIXED RECORD LENGTH                       
         LA    RF,1                RF=NUMBER OF RECORDS TO ADD                  
*                                                                               
ADDR2    TM    TSRECI,TSRVAR       TEST VARIABLE LENGTH RECORDS                 
         BZ    *+14                                                             
         ICM   R1,3,0(RE)          R1=VARIABLE RECORD LENGTH                    
         BNZ   *+6                                                              
         DC    H'0'                ZERO LENGTH RECORD                           
         SR    R0,R1               DECREMENT TOTAL RECORD LENGTH                
         BZ    ADDR4               UNTIL LENGTH RUN OUT                         
         BP    *+6                                                              
         DC    H'0'                BAD TOTAL LENGTH                             
         AR    RE,R1               BUMP TO NEXT RECORD                          
         LA    RF,1(RF)            INCREMENT RECORD COUNT                       
         B     ADDR2                                                            
*                                                                               
ADDR4    STH   RF,NREC             SAVE NUMBER OF RECORDS TO ADD                
         L     RE,DATA                                                          
         AH    RE,TOTFREE                                                       
         SH    RE,TSPFREE                                                       
         LR    RF,RE               RF=A(OLD END OF PAGE)                        
         AH    RE,RECL             RE=A(NEW END OF PAGE)                        
         L     R0,DATA                                                          
         AH    R0,TOTFREE                                                       
         CR    RE,R0                                                            
         BNH   *+6                                                              
         DC    H'0'                RECORD(S) WILL NOT FIT IN PAGE               
         LH    R1,RECL                                                          
         CH    R1,=H'256'                                                       
         BL    *+8                                                              
         LH    R1,=H'256'                                                       
         LR    R0,R1               R0=RECORD LENGTH   (OR 256)                  
         BCTR  R1,0                R1=RECORD LENGTH-1 (OR 255)                  
*                                                                               
ADDR6    SR    RE,R0               CREATE SPACE IN PAGE FOR NEW RECORD          
         SR    RF,R0                                                            
         EX    R1,*+4                                                           
         MVC   0(0,RE),0(RF)                                                    
         C     RF,RECA                                                          
         BH    ADDR6                                                            
*                                                                               
         L     RE,AREC             INSERT NEW RECORD INTO PAGE                  
         LH    R1,RECL                                                          
         L     R0,RECA                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE RECORD(S) TO PAGE                       
         LH    R1,TSPFREE          DECREMENT FREE SPACE IN PAGE                 
         SH    R1,RECL                                                          
         STH   R1,TSPFREE                                                       
         LH    R1,TSPRECN          INCREMENT NUMBER OF RECORD IN PAGE           
         AH    R1,NREC                                                          
         STH   R1,TSPRECN                                                       
         BAS   RE,SETK             SET HIGHEST KEY IN PAGE                      
         OI    TSPINDS,TSPIPWP                                                  
ADDRX    B     TSX                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET HIGHEST KEY VALUE FOR A PAGE IN TSPTAB               *         
***********************************************************************         
         SPACE 1                                                                
         USING TSPTAB,R4                                                        
SETK     ST    RE,12(RD)           SAVE RE                                      
         LH    R0,TSPRECN          R0=NUMBER OF RECORDS IN PAGE                 
         L     R1,DATA             R1=A(FIRST RECORD IN PAGE)                   
         LH    RF,TSRECL           RF=FIXED RECORD LENGTH                       
         SR    RE,RE                                                            
         XC    USED,USED                                                        
*                                                                               
SETK2    TM    TSRECI,TSRVAR                                                    
         BZ    *+8                                                              
         ICM   RF,3,0(R1)          RF=VARIABLE RECORD LENGTH                    
         AR    RE,RF                                                            
         BCT   R0,*+8              LOOP TO LAST RECORD IN PAGE                  
         B     *+10                                                             
         AR    R1,RF                                                            
         B     SETK2                                                            
         STH   RE,USED                                                          
         SR    RF,RF                                                            
         IC    RF,KEYX             RF=KEY LENGTH-1                              
         TM    TSRECI,TSRVAR                                                    
         BZ    *+8                                                              
         LA    R1,2(R1)            POINT TO KEY IF VARIABLE                     
         EX    RF,*+4                                                           
         MVC   TSPRECK(0),0(R1)    SET HIGHEST KEY VALUE                        
SETKX    L     RE,12(RD)                                                        
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET A TEMPSTR PAGE INTO CORE BUFFER                      *         
*                                                                     *         
* NTRY - R1=A(TSPTAB ENTRY FOR REQUIRED PAGE)                         *         
*                                                                     *         
* EXIT - PAGN=REQUIRED PAGE NUMBER                                    *         
*        BUFN=CORE BUFFER NUMBER ALLOCATED                            *         
*        TABA=A(TSTPAB ENTRY FOR REQUIRED PAGE)                       *         
*        BUFA=A(CORE BUFFER)                                          *         
*        DATA=A(FIRST DATA BYTE IN BUFFER)                            *         
*                                                                     *         
* NOTE - REQUIRED PAGE WILL NOT BE READ FROM TEMPSTR IF IT IS ALREADY *         
*        IN CORE BUFFER. IF NO FREE BUFFERS ARE AVAILABLE A BUFFER    *         
*        PAGE THAT HAS NOT BEEN UPDATED WILL BE USED, ELSE A PAGE     *         
*        THAT HAS BEEN UPDATED WILL BE WRITTEN TO TEMPSTR AND BUFFER  *         
*        WILL BE OVERLAYED WITH REQUIRED PAGE.                        *         
***********************************************************************         
         SPACE 1                                                                
GETP     NTR1  ,                                                                
         LR    R4,R1                                                            
         USING TSPTAB,R4                                                        
         ST    R4,TABA                                                          
         MVC   PAGN,TSPPAGE        SET REQUIRED PAGE                            
         MVC   BUFN,TSPINDS                                                     
         NI    BUFN,TSPIBUF        TEST TEMPSTR PAGE IN CORE                    
         BZ    GETP2                                                            
         SR    RF,RF                                                            
         IC    RF,BUFN             YES - RETURN CORE BUFFER ADDRESS             
         BCTR  RF,0                                                             
         MH    RF,PAGELEN                                                       
         A     RF,ABUF                                                          
         ST    RF,BUFA                                                          
         AH    RF,=Y(TSPDATA-TSPAGED)                                           
         ST    RF,DATA                                                          
         B     GETPX                                                            
*                                                                               
GETP2    CLC   TSUBUF,TSNBUF       TEST ANY FREE BUFFERS AVAILABLE              
         BNL   GETP4                                                            
         SR    RF,RF                                                            
         IC    RF,TSUBUF           YES - ALLOCATE TO THIS PAGE                  
         LA    R0,1(RF)                                                         
         STC   R0,TSUBUF                                                        
         STC   R0,BUFN                                                          
         MH    RF,PAGELEN                                                       
         A     RF,ABUF                                                          
         ST    RF,BUFA                                                          
         AH    RF,=Y(TSPDATA-TSPAGED)                                           
         ST    RF,DATA                                                          
         B     GETP12                                                           
         DROP  R4                                                               
*                                                                               
GETP4    ST    R4,SAVE             SAVE A(TSPTAB ENTRY)                         
         LA    R4,TSPTAB           LOCATE A NON-UPDATED PAGE                    
         USING TSPTAB,R4                                                        
         SR    R0,R0                                                            
         IC    R0,TSPAGN           OR FIRST UPDATED PAGE                        
         SR    RF,RF                                                            
*                                                                               
GETP6    TM    TSPINDS,TSPIBUF     TEST PAGE IN CORE BUFFER                     
         BZ    *+14                                                             
         TM    TSPINDS,TSPIPWP     TEST PAGE WRITE PENDING                      
         BZ    GETP8               NO - USE THIS CORE BUFFER                    
         LR    RF,R4               SAVE A(FIRST UPDATED PAGE ENTRY)             
         AH    R4,TSPLTAB                                                       
         BCT   R0,GETP6                                                         
         LTR   R4,RF               POINT TO FIRST UPDATED PAGE ENTRY            
         BNZ   *+6                                                              
         DC    H'0'                NO BUFFER AVAILABLE                          
*                                                                               
GETP8    MVC   BUFN,TSPINDS        SET BUFFER NUMBER & ADDRESS                  
         NI    BUFN,TSPIBUF                                                     
         SR    R0,R0                                                            
         IC    R0,BUFN                                                          
         BCTR  R0,0                                                             
         MH    R0,PAGELEN                                                       
         A     R0,ABUF                                                          
         ST    R0,BUFA                                                          
         LR    RF,R0                                                            
         MVC   TSPRECS-TSPAGED(,RF),TSPRECN                                     
         AH    RF,=Y(TSPDATA-TSPAGED)                                           
         ST    RF,DATA                                                          
         NI    TSPINDS,255-TSPIBUF                                              
         TM    TSPINDS,TSPIPWP     TEST PAGE WRITE PENDING                      
         BZ    GETP10                                                           
         GOTO1 VDATAMGR,DMCB,DMWRITE,FILE,(TSPPAGE,0),(R0)                      
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    TSPINDS,255-TSPIPWP                                              
*                                                                               
GETP10   L     R4,SAVE             RESTORE A(TSPTAB ENTRY)                      
*                                                                               
GETP12   L     R3,BUFA             POINT TO BUFFER & READ PAGE                  
         USING TSPAGED,R3                                                       
         ICM   RF,12,=C'L='                                                     
         ICM   RF,3,PAGELEN                                                     
         GOTO1 VDATAMGR,DMCB,DMREAD,FILE,(TSPPAGE,0),TSPAGED,,(RF)              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TSPPAGE,TSPAGEN     MATCH PAGE NUMBER                            
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    TSPINDS,255-TSPIPWP-TSPIBUF                                      
         OC    TSPINDS,BUFN        SET BUFFER ALLOCATED TO PAGE                 
GETPX    B     TSX                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ESTABLISH TERMINAL NUMBER OF USER                        *         
***********************************************************************         
         SPACE 1                                                                
GETTERM  LR    R0,RE                                                            
         L     RF,TSACOM                                                        
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,X'FFFFFFFF'                                            
         L     R1,0(R1)                                                         
         MVC   TERM,TNUM-UTLD(R1)                                               
GETTERMX LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
TWASMALL DC    Y(TSPDATAL,TSPAGEL)                                              
TWALARGE DC    Y(TSPDATAX,TSPAGEX)                                              
         SPACE 1                                                                
DMREAD   DC    C'DMREAD '                                                       
DMWRITE  DC    C'DMWRT  '                                                       
DMRSRV   DC    C'DMRSRV '                                                       
TEMPSTR  DC    C'TEMPSTR'                                                       
TEMPEST  DC    C'TEMPEST'                                                       
         SPACE 1                                                                
ACTNTAB  DS    0XL4                                                             
         DC    AL1(TSAADD),AL3(TSADD)                                           
         DC    AL1(TSARDH),AL3(TSRDH)                                           
         DC    AL1(TSAGET),AL3(TSGET)                                           
         DC    AL1(TSANXT),AL3(TSNXT)                                           
         DC    AL1(TSAPUT),AL3(TSPUT)                                           
         DC    AL1(TSAWRT),AL3(TSWRT)                                           
         DC    AL1(TSADEL),AL3(TSDEL)                                           
         DC    AL1(TSAINI),AL3(TSINI)                                           
         DC    AL1(TSASAV),AL3(TSSAV)                                           
         DC    AL1(TSARES),AL3(TSRES)                                           
         DC    AL1(TSACPS),AL3(TSCPS)                                           
         DC    AL1(0)                                                           
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
         SPACE 1                                                                
TSAWRK   DSECT                     ** TSAR WORKING STORAGE **                   
RELO     DS    A                                                                
VDATAMGR DS    V                                                                
DMCB     DS    6F                                                               
ABUF     DS    A                                                                
AREC     DS    A                                                                
SAVE     DS    A                                                                
TABA     DS    A                                                                
BUFA     DS    A                                                                
DATA     DS    A                                                                
RECA     DS    A                                                                
KEYA     DS    A                                                                
IREC     DS    A                                                                
OREC     DS    A                                                                
LENGTHS  DS    0XL4                                                             
TOTFREE  DS    H                                                                
PAGELEN  DS    H                                                                
TERM     DS    H                                                                
ILEN     DS    H                                                                
OLEN     DS    H                                                                
USED     DS    H                                                                
NREC     DS    H                                                                
RNUM     DS    H                                                                
RECL     DS    H                                                                
PAGN     DS    X                                                                
BUFN     DS    X                                                                
KEYX     DS    X                                                                
FLAG     DS    X                                                                
ERRS     DS    X                                                                
RKEY     DS    XL256                                                            
FILE     DS    CL7                                                              
TSAWRKX  EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007DDTSAR    05/01/02'                                      
         END                                                                    
