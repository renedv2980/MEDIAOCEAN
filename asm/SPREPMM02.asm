*          DATA SET SPREPMM02  AT LEVEL 004 AS OF 12/16/02                      
*PHASE SPMM02A                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE TIMVAL                                                                 
*INCLUDE EZMMPCDS                                                               
         TITLE 'SPMM02 - MMP CONVERSION TO WORKER FILE'                         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- INPUT TAPE RECORD                              *         
*                R3 -- OUTPUT WORKER FILE RECORD                      *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- WORK                                           *         
*                R7 -- WORK                                           *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
SPMM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPMM02,R8                                                      
                                                                                
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
                                                                                
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
                                                                                
MM10     DS    0H                                                               
         CLC   QAREA+49(6),SPACES  IS THERE A LINE # TO START AT?               
         BNE   MM20                                                             
         OI    RECFLAG2,FNDLINE    NO, SO DON'T WORRY ABOUT IT                  
         B     MM30                                                             
                                                                                
MM20     DS    0H                                                               
         PACK  DUB,QAREA+49(6)                                                  
         CVB   R1,DUB                                                           
         ST    R1,STARTLN                                                       
                                                                                
MM30     DS    0H                                                               
         CLC   QAREA+55(6),SPACES  IS THERE A LINE # TO END AT?                 
         BE    MM40                                                             
         PACK  DUB,QAREA+55(6)                                                  
         CVB   R1,DUB                                                           
         ST    R1,LASTLN                                                        
                                                                                
* PREPROCESS THE UPLOAD TO MAKE A TABLE WITHOUT DUPLICATE DATA                  
MM40     DS    0H                                                               
         MVI   FRSTREAD,C'Y'                                                    
         OI    RECFLAG,FIRSTA1                                                  
         OPEN  (TIN,INPUT)                    OPEN MM+ UPLOAD                   
                                                                                
         XC    HDRCOUNT,HDRCOUNT                                                
         XC    NUMENTRY,NUMENTRY                                                
         LA    R4,TABENTRY                                                      
         USING TABLED,R4                                                        
         LA    R2,IO                                                            
         LA    R0,TABLEMAX                                                      
         GOTO1 ,BINPARMS,,ATABLE,0,L'TABENTRY,(0,L'TABENTRY-2),(R0)             
                                                                                
MM50     DS    0H                                                               
         GET   TIN,(R2)                                                         
         CLC   0(2,R2),=C'H1'      H1 HEADER                                    
         BE    MM70                                                             
         CLC   0(2,R2),=C'H2'                                                   
         BE    MM74                                                             
         CLC   0(2,R2),=C'H3'                                                   
         BE    MM76                                                             
         CLC   0(2,R2),=C'B1'      BUYS                                         
         BE    MM80                                                             
         CLC   0(2,R2),=C'A1'      CLIENT                                       
         BNE   MM50                OTHERWISE, IGNORE IT                         
                                                                                
MM60     BAS   RE,HDRDBUY                                                       
         MVC   LASTREC,=C'A1'                                                   
         MVC   CLNT,2(R2)                                                       
         TM    RECFLAG,FIRSTA1                                                  
         BNO   MM50                                                             
         MVC   SVTMSTMP,8(R2)      SAVE TIME STAMP                              
         NI    RECFLAG,X'FF'-FIRSTA1                                            
         B     MM50                                                             
                                                                                
MM70     DS    0H                                                               
         USING UH1,R2                                                           
         BAS   RE,HDRDBUY                                                       
         MVC   LASTREC,=C'H1'                                                   
         XC    LASTMED,LASTMED                                                  
         NI    RECFLAG2,X'FF'-HDR99                                             
         LH    R1,HDRCOUNT                                                      
         LA    R1,1(R1)                                                         
         STH   R1,HDRCOUNT                                                      
         XC    TABENTRY,TABENTRY                                                
         MVC   TABCLT,CLNT                                                      
         MVC   TABCODES,H1EST                                                   
         MVC   TABSTATN,H1STATN                                                 
         STCM  R1,3,TABCOUNT                                                    
         CLC   H1DEL,=C'99'                                                     
         BNE   MM50                                                             
         OI    RECFLAG2,HDR99      FOUND A HEADER WITH A 99 FOR DELETES         
         B     MM50                                                             
                                                                                
MM74     DS    0H                                                               
         USING UH2,R2                                                           
         MVC   LASTMED,H2MEDIA                                                  
         B     MM50                                                             
         DROP  R2                                                               
                                                                                
MM76     DS    0H                                                               
         CLC   =C'CA',LASTMED                                                   
         BNE   MM50                                                             
         USING UH3,R2                                                           
         MVC   TABSTATN,SPACES                                                  
         MVC   TABSTATN(L'H3STA),H3STA                                          
         B     MM50                                                             
         DROP  R2                                                               
                                                                                
MM80     DS    0H                                                               
         MVC   LASTREC,=C'B1'                                                   
         CLC   =C'DEL',2(R2)       IGNORE B1'S WITH DEL IN THEM                 
         BE    MM50                                                             
         GOTO1 BINSRCH,BINPARMS,(1,TABENTRY)                                    
         OC    BINPARMS,BINPARMS   TABLE FULL?                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   BINPARMS,X'01'      RECORD NOT FOUND?                            
         BE    MM50                NOT FOUND, IN TABLE NOW                      
         L     R1,BINPARMS         FOUND, UPDATE LAST HEADER LOCATION           
         MVC   TABCOUNT-TABLED(2,R1),HDRCOUNT                                   
         B     MM50                                                             
         DROP  R4                                                               
                                                                                
MM90     DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(1,TDAY)                                       
         BAS   RE,WRKROPEN                                                      
         XC    HDRCOUNT,HDRCOUNT                                                
         XC    CLNT,CLNT                                                        
         XC    LASTREC,LASTREC                                                  
         OPEN  (TIN,INPUT)                    OPEN MM+ UPLOAD                   
         LA    R2,IO                                                            
                                                                                
MM100    DS    0H                                                               
         GET   TIN,(R2)                                                         
                                                                                
         L     R1,LINENUM          COUNT LINE NUMBER IN UPLOAD                  
         LA    R1,1(R1)                                                         
         ST    R1,LINENUM                                                       
                                                                                
         TM    RECFLAG2,FNDLINE    FOUND START LINE ALREAD?                     
         BO    MM110               YES                                          
         L     R1,STARTLN          NO, COMPARE CURRENT LINE NUMBER              
         C     R1,LINENUM                                                       
         BNE   MM110               HAVEN'T REACHED IT YET                       
                                                                                
         OI    RECFLAG2,FNDLINE    CANNOT START ON A B2                         
         CLC   0(2,R2),=C'B2'                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
MM110    DS    0H                                                               
         ZICM  R1,LASTLN,4         WAS A LAST LINE TO PROCESS ENTERED?          
         BZ    *+12                                                             
         C     R1,LINENUM          YES, SO COMPARE W/ CURRENT LINE              
         BL    MMX                 IT'S LOWER, SO DONE                          
                                                                                
         LA    R3,IO2+10                                                        
         LA    R1,IO2L                                                          
         XC    0(14,R1),0(R1)                                                   
                                                                                
         CLC   0(2,R2),=C'A1'      CLIENT                                       
         BE    A1R                                                              
         CLC   0(2,R2),=C'H1'      HEADER                                       
         BE    H1R                                                              
         CLC   0(2,R2),=C'H2'      HEADER DEMOS                                 
         BE    H2R                                                              
         CLC   0(2,R2),=C'H3'      H3 HAS CABLE NETWORK                         
         BE    H3R                                                              
         TM    RECFLAG2,FNDLINE    ONLY LOOK AT A1, H1, H2, & H3                
         BNO   MM100               IF HAVEN'T STARTED PROCESSING YET            
         CLC   0(2,R2),=C'B1'      BUY                                          
         BE    B1R                                                              
         CLC   0(2,R2),=C'B2'      DEMO VALUES                                  
         BE    B2R                                                              
         B     MM100                                                            
                                                                                
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*  A1 RECORD - CONTAINS CLIENT CODE                                   *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
A1R      DS    0H                                                               
         CLC   LASTREC,=C'H2'               CHECK IF THERE WAS A HDR            
         BE    A1R10                        WITHOUT BUYS                        
         CLC   LASTREC,=C'H3'                                                   
         BNE   A1R20                                                            
A1R10    OI    RECFLAG,HDRWOBS                                                  
         BAS   RE,BLDHDR                    BUILD HEADER                        
                                                                                
A1R20    MVC   SAVEA1,0(R2)                                                     
         MVI   ERRFLAG,0                    CLEAR ALL ERRORS                    
         MVC   LASTREC,=C'A1'                                                   
         CLC   2(3,R2),SPACES                                                   
         BE    A1CLTER1                                                         
         MVC   CLNT,2(R2)                                                       
         B     MM100                                                            
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* HEADER RECORD - CHECK FOR ERRORS, SAVE AND PROCESS LATER            *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
H1R      DS    0H                                                               
         CLC   LASTREC,=C'H2'               CHECK IF THERE WAS A HDR            
         BE    H1R10                        WITHOUT BUYS                        
         CLC   LASTREC,=C'H3'                                                   
         BNE   H1R20                                                            
H1R10    OI    RECFLAG,HDRWOBS                                                  
         BAS   RE,BLDHDR                                                        
                                                                                
H1R20    MVC   SAVEH1,0(R2)        SAVE AND PROCESS LATER                       
         MVC   SAVEH3,SPACES       CLEAR BECAUSE NOT REQUIRED                   
         XC    LASTMED,LASTMED                                                  
         CLC   LASTREC,=C'B1'      LAST RECORD READ B1?                         
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         NI    ERRFLAG,X'FF'-HDRERR-HDR2ERR-HDR3ERR-BUYERR                      
         TM    RECFLAG,FIRSTHDR                                                 
         BO    *+8                                                              
         BAS   RE,EOB                                                           
         OI    RECFLAG,FIRSTBUY                                                 
                                                                                
         LH    R1,HDRCOUNT                                                      
         LA    R1,1(R1)                                                         
         STH   R1,HDRCOUNT                                                      
         LA    R1,300                                                           
         C     R1,SEQNUM           IF AT LEAST 300 RECORDS,                     
         BH    *+12                 OPEN ANOTHER WORKER FILE                    
         BAS   RE,WRKRCLSE                                                      
         BAS   RE,WRKROPEN                                                      
                                                                                
         MVC   LASTREC,=C'H1'                                                   
         OC    CLNT,CLNT           CHECK IF THERE'S A CLIENT                    
         BZ    A1CLTER2                                                         
         B     MM100                                                            
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* HEADER 2 RECORD - CHECK FOR ERRORS, SAVE AND PROCESS LATER          *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
H2R      DS    0H                                                               
         USING UH2,R2                                                           
         MVC   SAVEH2,0(R2)                                                     
                                                                                
         CLC   LASTREC,=C'H1'      LAST RECORD                                  
         BE    H2R10                                                            
         MVC   P2+PRERRMSG-PRERRHD(17),=C'MISSING H1 RECORD'                    
         MVC   SAVEH1,SPACES                                                    
         B     HD2ERROR                                                         
                                                                                
H2R10    MVC   LASTREC,=C'H2'                                                   
         CLI   ERRFLAG,0           CHECK IF THERE'S ALREADY AN ERROR            
         BNE   MM100                                                            
         MVC   LASTMED,H2MEDIA     SAVE MEDIA CODE                              
         B     MM100                                                            
         DROP  R2                                                               
         EJECT                                                                  
*=====================================================================*         
* H3 RECORD CONTAINS CABLE STATION - MOVE IT TO H1 DATA               *         
*=====================================================================*         
         SPACE 1                                                                
H3R      DS    0H                                                               
         MVC   SAVEH3,0(R2)                                                     
         USING UH3,R2                                                           
*                                                                               
         CLC   LASTREC,=C'H2'      LAST RECORD READ                             
         BE    H3R10                                                            
         MVC   P2+PRERRMSG-PRERRHD(17),=C'MISSING H2 RECORD'                    
         MVC   SAVEH2,SPACES                                                    
         B     HD3ERROR                                                         
*                                                                               
H3R10    MVC   LASTREC,=C'H3'      LAST RECORD READ                             
         CLC   LASTMED,=C'CA'      TEST CABLE                                   
         BNE   H3RX                                                             
         LA    RE,H1STATN-UH1+SAVEH1                                            
         MVC   0(8,RE),SPACES                                                   
         MVC   0(5,RE),H3STA       OVERWRITE H1 STATION                         
*                                                                               
H3RX     B     MM100                                                            
         DROP  R2                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                         BUY RECORD                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
B1R      DS    0H                                                               
         MVC   NEXTALPH,=A(ALPHABET)                                            
         MVC   LASTREC,=C'B1'                                                   
         TM    ERRFLAG,A1ERR+HDRERR+HDR2ERR+HDR3ERR                             
         BNZ   MM100                                                            
                                                                                
         TM    ERRFLAG,BUYERR                                                   
         BNZ   MMBUY20                                                          
         TM    RECFLAG,FIRSTHDR+FIRSTBUY                                        
         BNZ   MMBUY10                                                          
         BAS   RE,EOB                                                           
         B     MMBUY20                                                          
                                                                                
MMBUY10  NI    RECFLAG,X'FF'-FIRSTHDR                                           
         BAS   RE,BLDHDR                                                        
         CLI   ERRFLAG,0                                                        
         BNE   MM100                                                            
                                                                                
MMBUY20  DS    0H                                                               
         NI    ERRFLAG,X'FF'-BUYERR                                             
         USING UB1,R2                                                           
         USING SBUYD,R3                                                         
         CLC   =C'DEL',B1BUYID     IGNORE B1'S WITH DEL IN THEM                 
         BNE   *+12                                                             
         OI    ERRFLAG,BUYERR                                                   
         B     MM100                                                            
         XC    SBUYODAT(SBUYACTN-SBUYODAT),SBUYODAT                             
         MVI   SBUYSTRT,C' '       PAD WITH SPACES                              
         MVC   SBUYSTRT+1(SBUYRLNQ-1),SBUYSTRT                                  
                                                                                
         MVC   SBUYLEN,=AL2(SBUYLENQ)                                           
         MVC   SBUYTYPE,=C'BUY*'   OBJECT TYPE                                  
         MVC   SBUYSTA,SVSTATN     STATION                                      
                                                                                
         CLC   B1ROTATN,SPACES                                                  
         BNE   *+14                                                             
         MVC   P2+PRERRMSG-PRERRHD(11),=C'NO ROTATION'                          
         B     BUYERROR                                                         
                                                                                
         LA    R4,B1ROTATN+6                                                    
         LA    R5,SBUYROT+6                                                     
         LA    R6,7                                                             
MMBUY30  MVI   0(R5),C'N'                                                       
         CLI   0(R4),C'-'                                                       
         BE    *+12                                                             
         MVI   0(R5),C'Y'                                                       
         STC   R6,BYTE             SAVE FIRST DATE                              
         BCTR  R4,0                                                             
         BCTR  R5,0                                                             
         BCT   R6,MMBUY30                                                       
                                                                                
         ZIC   R4,BYTE                                                          
         CVD   R4,DUB                                                           
         UNPK  SBUYRDAY,DUB                                                     
         OI    SBUYRDAY,X'30'                                                   
                                                                                
         CLC   =C'1200A',B1STTIME      MM+ USES 12A                             
         BNE   *+10                                                             
         MVC   B1STTIME(5),=C'1200M'   BUT TIMVAL NEEDS 12M - MIDNIGHT          
         CLC   =C'1200A',B1ENTIME                                               
         BNE   *+10                                                             
         MVC   B1ENTIME(5),=C'1200M'                                            
                                                                                
         CLC   =C'1200P',B1STTIME      MM+ USES 12P                             
         BNE   *+10                                                             
         MVC   B1STTIME(5),=C'1200N'   BUT TIMVAL NEEDS 12N - NOON              
         CLC   =C'1200P',B1ENTIME                                               
         BNE   *+10                                                             
         MVC   B1ENTIME(5),=C'1200N'                                            
                                                                                
         CLI   SVMED,C'R'                  IF MEDIA IS RADIO                    
         BNE   *+20                                                             
         CLC   =C'0500A',B1ENTIME          AND END TIME IS 5A                   
         BNE   *+10                                                             
         MVC   B1ENTIME(5),=C'0459A'       THEY REALLY WANT 4:59A               
                                                                                
         CLC   B1STTIME,SPACES                                                  
         BNE   *+14                                                             
         MVC   P2+PRERRMSG-PRERRHD(13),=C'NO START TIME'                        
         B     BUYERROR                                                         
                                                                                
         MVC   STARTIME,B1STTIME                                                
         CLC   B1ENTIME,SPACES                                                  
         BNE   *+14                                                             
         MVC   P2+PRERRMSG-PRERRHD(11),=C'NO END TIME'                          
         B     BUYERROR                                                         
                                                                                
         MVC   ENDTIME,B1ENTIME                                                 
         GOTO1 =V(TIMVAL),DMCB,(11,STARTIME),BSTTIME                            
         CLI   0(R1),X'FF'                                                      
         BNE   MMBUY35                                                          
         MVC   P2+PRERRMSG-PRERRHD(13),=C'INVALID TIMES'                        
         MVC   P2+PRERRFLD-PRERRHD(10),B1STTIME                                 
         B     BUYERROR                                                         
                                                                                
MMBUY35  ZICM  R4,BSTTIME,2        START TIME                                   
         CVD   R4,DUB                                                           
         UNPK  SBUYSTIM,DUB                                                     
         OI    SBUYSTIM+3,X'30'                                                 
                                                                                
         ZICM  R4,BENTIME,2        END TIME                                     
         CVD   R4,DUB                                                           
         UNPK  SBUYETIM,DUB                                                     
         OI    SBUYETIM+3,X'30'                                                 
                                                                                
         LA    RE,DAYPRTAB                                                      
MMBUY40  CLI   0(RE),X'FF'                                                      
         BNE   *+12                                                             
         MVI   SBUYDPT,C'Q'        DAY PART                                     
         B     MMBUY50                                                          
                                                                                
         CLC   B1DAYPRT,0(RE)                                                   
         BE    *+12                                                             
         LA    RE,3(RE)                                                         
         B     MMBUY40                                                          
         MVC   SBUYDPT,2(RE)       DAY PART                                     
                                                                                
MMBUY50  DS    0H                                                               
         MVC   SBUYSLEN,B1SPTLEN   SPOT LENGTH                                  
         CLC   B1SPTLEN,SPACES                                                  
         BE    MMBUY52                                                          
         CLC   B1SPTLEN,=C'000'                                                 
         BNE   MMBUY55                                                          
                                                                                
MMBUY52  DS    0H                                                               
         MVC   SBUYSLEN,=C'060'    RADIO 60 SECONDS                             
         CLI   SVMED,C'R'                                                       
         BE    MMBUY55                                                          
         MVC   SBUYSLEN,=C'030'    TV 30 SECONDS                                
                                                                                
MMBUY55  DS    0H                                                               
         MVI   SBUYLUNT,C'S'       SPOT LENGTH UNITS ALWAYS SECONDS             
                                                                                
         MVC   SBUYPROG(L'B1PRGMNM),B1PRGMNM   PROGRAM NAME                     
         CLC   B1PRGMNM,SPACES                                                  
         BNE   *+10                                                             
         MVC   SBUYPROG(7),=C'UNKNOWN'                                          
                                                                                
         LA    R4,L'B1PRGMNM       GET RID OF ALL COMMAS                        
         LA    R5,SBUYPROG                                                      
MMBUY60  DS    0H                                                               
         CLI   0(R5),C','                                                       
         BNE   *+8                                                              
         MVI   0(R5),C' '                                                       
         LA    R5,1(R5)                                                         
         BCT   R4,MMBUY60                                                       
                                                                                
         MVI   SBUYCOST,C'0'                   PAD COST WITH ZEROS              
         MVC   SBUYCOST+1(L'SBUYCOST-1),SBUYCOST                                
         LA    R4,B1SPCOST+L'B1SPCOST-1                                         
                                                                                
         LA    R5,B1SPCOST                                                      
         LA    R6,L'B1SPCOST                                                    
MMBUY70  CLI   0(R5),C'.'               CHECK IF THERE ARE ANY DECIMALS         
         BE    MMBUY80                                                          
         LA    R5,1(R5)                                                         
         BCT   R6,MMBUY70                                                       
         LA    R5,SBUYCOST+L'SBUYCOST-3   LEAVE 2 0'S FOR DECIMALS              
         B     MMBUY86                                                          
                                                                                
MMBUY80  DS    0H                                                               
         CLI   0(R4),C' '                 FIND LAST DIGIT                       
         BH    MMBUY84                                                          
         BCTR  R4,0                                                             
         B     MMBUY80                                                          
MMBUY84  SR    R4,R5                      = # OF ACTUAL DECIMAL PLACES          
         LA    R5,SBUYCOST+L'SBUYCOST-3   ASSUME NO DECIMAL PLACES              
         AR    R5,R4                      ADD TO GET START POSITION             
         LA    R4,B1SPCOST+L'B1SPCOST-1   RESET R4 TO END                       
MMBUY86  LA    R6,L'B1SPCOST                                                    
MMBUY90  CLI   0(R4),C' '                                                       
         BE    MMBUY100                                                         
         CLI   0(R4),C'.'                                                       
         BE    MMBUY100                                                         
         MVC   0(1,R5),0(R4)                                                    
         BCTR  R5,0                                                             
MMBUY100 BCTR  R4,0                                                             
         BCT   R6,MMBUY90                                                       
                                                                                
         MVC   SBUYUID(4),SVFLRFNO       FILE REF NO(4)                         
         MVC   SBUYUID+4(3),B1BUYID      BUY #(3)                               
         MVC   SACNNUM,SVACN                                                    
         MVC   IO2L(2),=Y(SBUYLENQ+16)                                          
         BAS   RE,WRKR                                                          
         MVC   SVBUY,IO2L                                                       
         DROP  R3                                                               
                                                                                
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        SCHEDULE RECORD                              *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
MMSKD    DS    0H                                                               
         USING SSKDD,R3                                                         
         XC    SSKDODAT(SSKDSTRT-SSKDODAT),SSKDODAT                             
         MVI   SSKDSTRT,C' '       PAD WITH SPACES                              
         MVC   SSKDSTRT+1(SSKDLNQ-1),SSKDSTRT                                   
                                                                                
         MVC   SSKDLEN,=AL2(SSKDLENQ)                                           
         MVC   SSKDTYPE,=C'SKD*'   OBJECT TYPE                                  
                                                                                
         MVC   SSKDSDT,SVSTDATE    START DATE                                   
                                                                                
* MAKE A TABLE OF SKD PACKED                                                    
         TM    RECFLAG2,PROCSKD    FIRST TIME PROCESSING SKD?                   
         BO    MMSKD40             NO, SO DON'T DO THIS                         
         LA    RE,B1SKD                                                         
         LA    RF,PSKDTAB                                                       
         LA    R6,14               14 SKD VALUES                                
MMSKD10  DS    0H                                                               
         SR    R1,R1               FIGURE OUT LENGTH                            
         LR    R4,RE                                                            
         LA    R5,L'B1SKD                                                       
                                                                                
MMSKD20  CLI   0(R4),C' '                                                       
         BE    MMSKD30                                                          
         LA    R1,1(R1)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,MMSKD20                                                       
                                                                                
MMSKD30  DS    0H                                                               
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  0(L'PSKDTAB,RF),0(0,RE)                                          
         LA    RE,L'B1SKD(RE)                                                   
         LA    RF,L'PSKDTAB(RF)                                                 
         BCT   R6,MMSKD10                                                       
                                                                                
* NOW MAKE SURE THERE ARE NO MORE THAN 4 SPOTS/WEEK                             
MMSKD40  DS    0H                                                               
         NI    RECFLAG2,X'FF'-PROCSKD                                           
         LA    R4,PSKDTAB                                                       
         LA    R5,14               14 SKD VALUES                                
         LA    R6,SSKDCNTR                                                      
                                                                                
MMSKD50  DS    0H                                                               
         ZAP   PSKD,0(L'PSKDTAB,R4)                                             
         SP    PSKD,=P'4'                                                       
         BNM   MMSKD60                                                          
         UNPK  0(L'SSKDCNTR,R6),0(L'PSKDTAB,R4)                                 
         ZAP   0(L'PSKDTAB,R4),=P'0'                                            
         B     MMSKD70                                                          
                                                                                
MMSKD60  DS    0H                                                               
         BZ    *+8                                                              
         OI    RECFLAG2,PROCSKD    MORE THAN 4 SO MAKE ANOTHER BUY              
         ZAP   0(L'PSKDTAB,R4),PSKD                                             
         UNPK  0(L'SSKDCNTR,R6),=PL2'4'                                         
                                                                                
MMSKD70  DS    0H                                                               
         OI    L'SSKDCNTR-1(R6),X'F0'                                           
         LA    R6,L'SSKDCNTR(R6)                                                
         LA    R4,L'PSKDTAB(R4)                                                 
         BCT   R5,MMSKD50                                                       
                                                                                
         MVC   IO2L(2),=Y(SSKDLENQ+16)                                          
         BAS   RE,WRKR                                                          
                                                                                
         OI    RECFLAG,GOODBUY                                                  
         NI    RECFLAG,X'FF'-FIRSTBUY                                           
                                                                                
         TM    RECFLAG2,PROCSKD2                                                
         BNO   MM100                                                            
         MVC   LASTREC,=C'B1'                                                   
         NI    RECFLAG2,X'FF'-PROCSKD2                                          
         DROP  R2,R3                                                            
                                                                                
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        DEMO VALUES                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
B2R      DS    0H                                                               
         CLC   LASTREC,=C'B1'      LAST RECORD READ B1?                         
         BE    *+14                                                             
         MVC   P2+PRERRMSG-PRERRHD(17),=C'MISSING B1 RECORD'                    
         B     BUYERROR                                                         
                                                                                
         MVC   LASTREC,=C'B2'                                                   
         CLI   ERRFLAG,0                                                        
         BNE   MM100                                                            
                                                                                
         USING UB2,R2                                                           
         USING SDEMD,R3                                                         
         XC    SDEMODAT(SDEMSTRT-SDEMODAT),SDEMODAT                             
         MVI   SDEMDEM,C' '        PAD WITH SPACES                              
         MVC   SDEMDEM+1(SDEMRLNQ-4),SDEMDEM                                    
                                                                                
         MVC   SDEMLEN,=AL2(SDEMLENQ)                                           
         MVC   SDEMTYPE,=C'DEM*'   OBJECT TYPE                                  
                                                                                
         MVI   SDEMDEM,C'0'                    PAD DEMOS WITH ZEROS             
         MVC   SDEMDEM+1(83),SDEMDEM                                            
         LA    R4,B2DEMOS+L'B2DEMOS-1   ADDRESS OF "FROM" DEMOS                 
         LA    R5,SDEMDEM               ADDRESS OF "TO" DEMOS                   
         LA    R7,6                     6 DEMOS                                 
                                                                                
         B     MMDEMV20                                                         
                                                                                
MMDEMV10 L     R4,ADEMO1                                                        
         LA    R4,L'B2DEMOS(R4)                                                 
         L     R5,ADEMO2                                                        
         LA    R5,L'SDEMDEM(R5)                                                 
                                                                                
MMDEMV20 ST    R4,ADEMO1                                                        
         ST    R5,ADEMO2                                                        
                                                                                
         LR    R6,R4                                                            
         LA    R1,L'B2DEMOS                                                     
MMDEMV30 CLI   0(R6),C'.'               CHECK IF THERE ARE ANY DECIMALS         
         BE    MMDEMV40                                                         
         BCTR  R6,0                                                             
         BCT   R1,MMDEMV30                                                      
                                                                                
         LA    R5,L'SDEMDEM-2(R5)       LEAVE 1 0 FOR DECIMAL                   
         B     MMDEMV50                                                         
                                                                                
MMDEMV40 LA    R5,L'SDEMDEM-1(R5)                                               
MMDEMV50 LA    R6,L'B2DEMOS             LENGTH OF "FROM" FIELD                  
                                                                                
MMDEMV60 CLI   0(R4),C' '                                                       
         BE    MMDEMV70                                                         
         CLI   0(R4),C'.'                                                       
         BE    MMDEMV70                                                         
         MVC   0(1,R5),0(R4)                                                    
         BCTR  R5,0                                                             
MMDEMV70 BCTR  R4,0                                                             
         BCT   R6,MMDEMV60                                                      
         BCT   R7,MMDEMV10                                                      
                                                                                
         MVC   IO2L(2),=Y(SDEMLENQ+16)                                          
         BAS   RE,WRKR                                                          
                                                                                
         TM    RECFLAG2,PROCSKD    CHECK IF NEED TO MAKE ANOTHER BUY            
         BNO   MM100               NO                                           
         BAS   RE,EOB                                                           
         MVC   IO2L(L'SVBUY),SVBUY                                              
                                                                                
         L     R1,NEXTALPH         ADDRESS OF NEXT LETTER TO USE                
         CLI   0(R1),X'FF'           FOR UNIQUE BUY ID                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   IO2+10+SBUYUID-SBUYD+L'SBUYUID-1(1),0(R1)                        
         LA    R1,1(R1)                                                         
         ST    R1,NEXTALPH                                                      
                                                                                
         BAS   RE,WRKR                                                          
         OI    RECFLAG2,PROCSKD2                                                
         B     MMSKD                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* BUILD HDR* RECORD FOR WORKER FILE                                   *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
BLDHDR   NTR1                                                                   
         CLI   ERRFLAG,0                                                        
         BNE   BLDHDRX                                                          
         LA    R7,SAVEH1                                                        
         USING UH1,R7                                                           
         USING SHDRD,R3                                                         
                                                                                
* CHECK TABLE TO SEE IF IT'S DUPLICATE DATA                                     
         USING TABLED,R4                                                        
         LA    R4,TABENTRY                                                      
         XC    TABENTRY,TABENTRY                                                
         MVC   TABCLT,CLNT                                                      
         MVC   TABCODES,H1EST                                                   
         MVC   TABSTATN,H1STATN                                                 
         GOTO1 BINSRCH,BINPARMS,(0,TABENTRY)                                    
         CLI   BINPARMS,X'01'      RECORD NOT FOUND?                            
         BE    BHDR10                                                           
         L     R1,BINPARMS         FOUND, COMPARE HEADER COUNT                  
         CLC   HDRCOUNT,TABCOUNT-TABLED(R1)                                     
         BE    BHDR20                                                           
BHDR10   OI    ERRFLAG,HDRERR      NOT PROCESSING THIS HEADER                   
         B     BLDHDRX                                                          
         DROP  R4                                                               
                                                                                
BHDR20   XC    SHDRERNO(SHDRTYPE-SHDRODAT),SHDRERNO                             
         MVI   SHDRSTRT,C' '       PAD WITH SPACES                              
         MVC   SHDRSTRT+1(SHDRRLNQ-1),SHDRSTRT                                  
                                                                                
         MVC   IO2L(2),=Y(SHDRLENQ+16)                                          
         MVC   SHDRLEN,=AL2(SHDRLENQ)                                           
         MVC   SHDRTYPE,=C'HDR*'   OBJECT TYPE                                  
         MVI   SHDRSYS,C'S'        SYSTEM                                       
         MVC   SHDRUTYP,=C'BUY'    UPLOAD TYPE                                  
         MVC   SHDRSRCE,=C'MM'     MMPLUS UPLOAD                                
                                                                                
         CLC   H1STATN,SPACES      CHECK FOR A STATION                          
         BNE   BHDR30                                                           
         MVC   P2+PRERRMSG-PRERRHD(10),=C'NO STATION'                           
         CLC   LASTMED,=C'CA'      TEST CABLE, THEN H3 ERROR                    
         BE    HD3ERR2                                                          
         B     HDRERR2                                                          
                                                                                
BHDR30   LA    RE,H1STATN                                                       
         LA    R1,5                                                             
BHDR40   CLI   0(RE),C'+'                                                       
         BNE   BHDR50                                                           
         MVC   0(4,RE),=C'-TV '                                                 
         B     BHDR60                                                           
BHDR50   LA    RE,1(RE)                                                         
         BCT   R1,BHDR40                                                        
                                                                                
BHDR60   CLI   H1STATN,C'!'                                                     
         BNE   BHDR80                                                           
         LA    RE,STATAB                                                        
BHDR70   CLI   0(RE),X'FF'                                                      
         BE    BHDR130             STATION NOT IN TABLE                         
         CLC   H1STATN+1(L'H1STATN-1),1(RE)                                     
         BNE   *+14                                                             
         MVC   H1STATN(1),0(RE)    REPLACE ! W/ LETTER                          
         B     BHDR160                                                          
         LA    RE,L'H1STATN(RE)                                                 
         B     BHDR70                                                           
                                                                                
* CHECK THAT FIRST 3 LETTERS ARE ALPHA CHARACTERS                               
BHDR80   LA    RE,H1STATN                                                       
         LA    R5,3                                                             
BHDR90   DS    0H                                                               
         CLI   0(RE),C'A'                                                       
         BL    BHDR110             CHECK LOW POWER STATION TABLE                
         CLI   0(RE),C'Z'                                                       
         BNH   BHDR100             OK                                           
         CLI   0(RE),X'81'         LOWER CASE A                                 
         BL    BHDR110             CHECK LOW POWER STATION TABLE                
         CLI   0(RE),X'A9'         LOWER CASE Z                                 
         BH    BHDR110             CHECK LOW                                    
BHDR100  LA    RE,1(RE)                                                         
         BCT   R5,BHDR90                                                        
         B     BHDR160                                                          
*                                                                               
* CHECK IF FIRST 4 CHARS ARE NUMERIC                                            
BHDR110  LA    RE,H1STATN                                                       
         LA    R5,4                                                             
BHDR120  CLI   0(RE),C'0'          CHECK FOR NUMERIC CABLE STATION              
         BL    BHDR130                                                          
         CLI   0(RE),C'9'                                                       
         BH    BHDR130             OK                                           
         LA    RE,1(RE)                                                         
         BCT   R5,BHDR120                                                       
         B     BHDR160                                                          
*                                                                               
* IF NOT ALL ALPHA CHECK FOR LOW POWER STATION                                  
BHDR130  LA    RE,LOWSTAB          TABLE OF LOW POWER STATIONS                  
*                                                                               
BHDR140  CLI   0(RE),X'FF'                                                      
         BE    BHDR150             STATION NOT IN TABLE= INVALID                
         CLC   H1STATN(4),0(RE)    MATCH ON 4                                   
         BNE   *+14                                                             
         MVC   H1STATN,4(RE)       REPLACE WITH 8                               
         B     BHDR160                                                          
         LA    RE,12(RE)                                                        
         B     BHDR140                                                          
                                                                                
BHDR150  DS    0H                                                               
         MVC   P2+PRERRMSG-PRERRHD(15),=C'INVALID STATION'                      
         MVC   P2+PRERRFLD-PRERRHD(8),H1STATN                                   
         CLC   LASTMED,=C'CA'      TEST CABLE, THEN H3 ERROR                    
         BE    HD3ERR2                                                          
         B     HDRERR2                                                          
                                                                                
* FIGURE OUT WHAT THE MEDIA IS                                                  
BHDR160  DS    0H                                                               
         MVC   SVSTATN,H1STATN     SAVE STATION                                 
         MVI   SHDRMED,C'T'        MEDIA                                        
         LA    RE,SVSTATN                                                       
         LA    R1,6                                                             
                                                                                
* FIGURE OUT WHAT THE MEDIA IS                                                  
BHDR170  DS    0H                                                               
         CLI   0(RE),C'-'                                                       
         BE    BHDR180                                                          
         LA    RE,1(RE)                                                         
         BCT   R1,BHDR170                                                       
         B     BHDR210                                                          
                                                                                
BHDR180  LA    R1,RADIOTAB         CHECK TABLE IF A RADIO STATION               
                                                                                
BHDR190  CLI   0(R1),X'FF'                                                      
         BE    BHDR210                                                          
                                                                                
         CLC   1(2,RE),0(R1)                                                    
         BE    BHDR200                                                          
         LA    R1,4(R1)                                                         
         B     BHDR190                                                          
                                                                                
BHDR200  DS    0H                                                               
         OC    2(2,R1),2(R1)       CHECK IF THERE IS A REPLACEMENT              
         BZ    *+10                                                             
         MVC   1(2,RE),2(R1)       YES                                          
         MVI   SHDRMED,C'R'                                                     
         B     BHDR220                                                          
                                                                                
BHDR210  DS    0H                                                               
         LA    R1,SVSTATN+L'SVSTATN                                             
         SR    R1,RE                                                            
         EX    R1,*+4                                                           
         MVC   0(0,RE),SPACES                                                   
                                                                                
BHDR220  DS    0H                                                               
         MVC   SVMED,SHDRMED                                                    
         CLC   H1EST,SPACES                                                     
         BNE   *+14                                                             
         MVC   P2+PRERRMSG-PRERRHD(19),=C'NO PRD AND EST CODE'                  
         B     HDRERR2                                                          
                                                                                
         GOTO1 =V(EZMMPCDS),DMCB,H1EST,CLTPRDES                                 
         MVC   SHDRCLT,CLNT                                                     
                                                                                
         OC    CLTPRDES+3(3),CLTPRDES+3                                         
         BNZ   BHDR225                                                          
         MVC   P2+PRERRMSG-PRERRHD(20),=C'INVALID PRODUCT CODE'                 
         MVC   P2+PRERRFLD-PRERRHD(2),H1EST                                     
         B     HDRERR2                                                          
                                                                                
BHDR225  OC    CLTPRDES+6(3),CLTPRDES+6                                         
         BZ    BHDR230                                                          
         CLC   =C'000',CLTPRDES+6                                               
         BNE   BHDR235                                                          
                                                                                
BHDR230  DS    0H                                                               
         MVC   P2+PRERRMSG-PRERRHD(21),=C'INVALID ESTIMATE CODE'                
         MVC   P2+PRERRFLD-PRERRHD(4),H1EST+3                                   
         B     HDRERR2                                                          
                                                                                
BHDR235  MVC   SHDRPRD,CLTPRDES+3                                               
         MVC   SHDREST,CLTPRDES+6                                               
         CLI   SHDREST,X'40'                                                    
         BH    *+8                                                              
         MVI   SHDREST,C'0'                                                     
         CLI   SHDREST+1,X'40'                                                  
         BH    *+8                                                              
         MVI   SHDREST+1,C'0'                                                   
                                                                                
BHDR240  DS    0H                                                               
         MVC   SHDRSTPD+2(4),H1STDATE             START MMDD                    
         MVC   SHDRSTPD(1),H1STYEAR               START YEAR (DECADE)           
         MVC   SHDRSTPD+1(1),H1STDATE+4           START YEAR                    
         MVC   SHDRENPD+2(4),H1ENDATE             END MMDD                      
         MVC   SHDRENPD(1),H1STYEAR               END YEAR DECADE ONLY          
         MVC   SHDRENPD+1(1),H1ENDATE+4           END YEAR                      
                                                                                
         CLC   SHDRSTPD+1(1),SHDRENPD+1                                         
         BNH   BHDR260                                                          
                                                                                
         CLC   SHDRSTPD+1(1),H1STYEAR+1                                         
         BE    BHDR250                                                          
                                                                                
* FIGURE OUT DECADE                                                             
         PACK  DUB,H1STYEAR                       YEAR(2)                       
         AP    DUB,=P'99'                         ADD 100 - 1                   
         UNPK  WORK(2),DUB                                                      
         MVC   SHDRSTPD(1),WORK                                                 
                                                                                
BHDR250  CLC   SHDRENPD+1(1),H1STYEAR+1                                         
         BE    BHDR260                                                          
                                                                                
         PACK  DUB,H1STYEAR                                                     
         AP    DUB,=P'1'                                                        
         UNPK  WORK(2),DUB                                                      
         MVC   SHDRENPD(1),WORK                                                 
                                                                                
BHDR260  DS    0H                                                               
         MVC   SVSTDATE,SHDRSTPD   SAVE START DATE                              
         MVC   SVACN,H1ACN         SAVE ACN                                     
                                                                                
         CLC   H1FLRFNO,SPACES     CHECK FOR A FILE REF NO                      
         BNE   *+14                                                             
         MVC   P2+PRERRMSG-PRERRHD(24),=C'NO FILE REFERENCE NUMBER'             
         B     HDRERR2                                                          
                                                                                
         SR    R1,R1               FIGURE OUT LENGTH                            
         LA    R4,H1FLRFNO                                                      
         LA    R5,L'H1FLRFNO                                                    
BHDR270  CLI   0(R4),C' '                                                       
         BE    BHDR280                                                          
         LA    R1,1(R1)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,BHDR270                                                       
                                                                                
BHDR280  DS    0H                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  WORK(3),H1FLRFNO(0)                                              
         UNPK  SVFLRFNO,WORK(3)                                                 
                                                                                
* PROCESS H2 - DEMOS                                                            
         USING UH2,R7                                                           
         LA    R7,SAVEH2                                                        
         CLC   H2DEMOS,SPACES                                                   
         BNE   *+14                                                             
         MVC   P2+PRERRMSG-PRERRHD(8),=C'NO DEMOS'                              
         B     HD2ERR2                                                          
         LA    R4,H2DEMOS                                                       
         LA    R5,SHDRDEMO                                                      
         LA    RE,6                6 DEMOS TO PROCESS                           
         B     BHDR300                                                          
                                                                                
BHDR290  DS    0H                                                               
         LA    R4,1(R1)            INCREMENT R4                                 
                                                                                
         L     R5,ADEMO1           ADDRESS OF DEMO JUST PROCESSED               
         LA    R5,L'SHDRDEMO(R5)   POINT TO NEXT DEMO                           
                                                                                
BHDR300  DS    0H                                                               
         STC   RE,NUMDEMOS         # OF DEMOS LEFT TO PROCESS                   
         LA    R6,7                MAX LENGTH OF DEMO                           
         ST    R5,ADEMO1           ADDRESS OF DEMO                              
         MVI   RFLAG,C'N'                                                       
         LA    R1,7(R4)            1 BEYOND LAST CHARACTER TO PROCESS           
         CLI   7(R4),C'R'          IF LAST IS 'R' MOVE IT TO FRONT              
         BNE   BHDR340                                                          
         MVI   0(R5),C'R'                                                       
         MVI   RFLAG,C'Y'                                                       
         LA    R5,1(R5)                                                         
         BCTR  R6,0                                                             
                                                                                
         LA    RE,DEMTAB                                                        
BHDR310  CLI   0(RE),X'FF'                                                      
         BE    BHDR320                                                          
         CLC   0(L'H2DEMOS-1,R4),0(RE)                                          
         BE    *+12                                                             
         LA    RE,L'H2DEMOS-1+L'SHDRDEMO(RE)                                    
         B     BHDR310                                                          
         MVC   0(L'SHDRDEMO,R5),L'H2DEMOS-1(RE)                                 
         B     BHDR460                                                          
                                                                                
BHDR320  CLC   0(6,R4),=C'WW 18+'  CHECK THE 3 DEMOS THAT DON'T                 
         BE    BHDR330               CONVERT TO A DDS DEMO CODE                 
         CLC   0(6,R4),=C'HW 18+'                                               
         BE    BHDR330                                                          
         CLC   0(7,R4),=C'P 35-64'                                              
         BNE   BHDR340                                                          
                                                                                
BHDR330  CLI   RFLAG,C'Y'          WAS THERE AN R?                              
         BNE   *+6                                                              
         BCTR  R5,0                TAKE OUT THE R JUST PUT IN                   
         MVI   0(R5),C' '                                                       
         B     BHDR460                                                          
                                                                                
BHDR340  CLI   0(R4),C'C'          CHILD                                        
         BNE   BHDR350                                                          
         MVC   0(2,R5),=C'CH'                                                   
         B     BHDR360                                                          
                                                                                
BHDR350  CLI   0(R5),C'T'          TEENAGER                                     
         BNE   BHDR370                                                          
         MVC   0(2,R5),=C'TN'                                                   
BHDR360  LA    R5,2(R5)                                                         
         B     BHDR420                                                          
                                                                                
BHDR370  CLI   0(R4),C'K'          KIDS                                         
         BNE   BHDR380                                                          
         MVI   0(R5),C'V'          VIEWER                                       
         B     BHDR410                                                          
                                                                                
BHDR380  CLI   0(R4),C'P'          PERSON                                       
         BNE   BHDR400                                                          
         MVI   0(R5),C'A'          ADULT (CHECK AGE LATER TO SEE IF             
         B     BHDR410                    IT SHOULD REALLY BE VIEWER)           
                                                                                
BHDR390  LA    R4,1(R4)                                                         
BHDR400  CR    R4,R1               END OF DEMO?                                 
         BNL   BHDR440                                                          
         CLI   0(R4),C' '          SKIP SPACES AND '-'                          
         BE    BHDR390                                                          
         CLI   0(R4),C'-'                                                       
         BE    BHDR390                                                          
         MVC   0(1,R5),0(R4)                                                    
                                                                                
BHDR410  LA    R5,1(R5)                                                         
BHDR420  LA    R4,1(R4)                                                         
BHDR430  BCT   R6,BHDR400                                                       
                                                                                
BHDR440  L     R5,ADEMO1           ADDRESS OF DEMO JUST PROCESSED               
         CLI   0(R5),C'R'          AN 'R' IN FIRST POSITION?                    
         BNE   *+8                                                              
         LA    R5,1(R5)            YES, SO BUMP R5                              
         CLI   0(R5),C'A'          CATEGORY ADULT?                              
         BNE   BHDR460                                                          
         CLI   2(R5),C'0'          CHECK IF NUMBER HAS LENGTH 1                 
         BL    BHDR450             YES, SO AGE IS < 10 SO CHANGE ADULT          
         CLI   2(R5),C'9'          TO VIEWER                                    
         BH    BHDR450                                                          
         CLC   1(2,R5),=C'18'      MUST BE AT LEAST 18 TO BE ADULT              
         BNL   BHDR460                                                          
BHDR450  MVI   0(R5),C'V'                                                       
         CLI   SVMED,C'R'                                                       
         BNE   BHDR460                                                          
         MVI   0(R5),C'L'                                                       
BHDR460  ZIC   RE,NUMDEMOS                                                      
         BCT   RE,BHDR290                                                       
         TM    RECFLAG,HDRWOBS                                                  
         BO    HDRWOB                                                           
         BAS   RE,WRKR                                                          
BLDHDRX  NI    RECFLAG,X'FF'-HDRWOBS                                            
         B     EXIT                                                             
                                                                                
HDRERR2  OI    ERRFLAG,HDRERR                                                   
         B     BHDRERX                                                          
                                                                                
HD2ERR2  OI    ERRFLAG,HDR2ERR                                                  
         B     BHDRERX                                                          
                                                                                
HD3ERR2  OI    ERRFLAG,HDR3ERR                                                  
         B     BHDRERX                                                          
                                                                                
BHDRERX  BAS   RE,PRNTERR                                                       
         NI    RECFLAG,X'FF'-HDRWOBS                                            
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        EXIT ROUNTINE                                *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
MMX      DS    0H                                                               
         CLI   FRSTREAD,C'Y'                                                    
         BNE   MMX10                                                            
         MVI   FRSTREAD,C'N'                                                    
         BAS   RE,HDRDBUY                                                       
         CLOSE TIN                                                              
         B     MM90                                                             
                                                                                
MMX10    DS    0H                                                               
         CLC   LASTREC,=C'H2'               CHECK IF THERE WAS A HDR            
         BE    MMX20                        WITHOUT BUYS                        
         CLC   LASTREC,=C'H3'                                                   
         BNE   MMX30                                                            
MMX20    OI    RECFLAG,HDRWOBS              THERE IS A HEADER W/O BUYS          
         BAS   RE,BLDHDR                                                        
                                                                                
MMX30    LA    R1,4                PURGE IF NO BUYS                             
         C     R1,SEQNUM                                                        
         BL    *+8                                                              
         OI    RECFLAG2,PRGWRKR    DELETE LAST WORKER FILE                      
         BAS   RE,WRKRCLSE                                                      
         TM    RECFLAG2,PRGWRKR                                                 
         BNO   MMX40                                                            
         GOTO1 DATAMGR,DMCB,=C'PURGE   ',WRKFILEN,WRKRINDX,IO2,AWKBUFF          
                                                                                
MMX40    DS    0H                                                               
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                             ERRORS                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
A1CLTER1 MVC   P2+PRERRMSG-PRERRHD(9),=C'NO CLIENT'                             
         B     A1ERROR                                                          
                                                                                
A1CLTER2 MVC   P2+PRERRMSG-PRERRHD(17),=C'MISSING A1 RECORD'                    
         B     A1ERROR                                                          
                                                                                
A1ERROR  OI    ERRFLAG,A1ERR                                                    
         B     ERR10                                                            
                                                                                
HD2ERROR OI    ERRFLAG,HDR2ERR                                                  
         B     ERR10                                                            
                                                                                
HD3ERROR OI    ERRFLAG,HDR3ERR                                                  
         B     ERR10                                                            
                                                                                
BUYERROR OI    ERRFLAG,BUYERR                                                   
                                                                                
ERR10    BAS   RE,PRNTERR                                                       
         B     MM100                                                            
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* PRINT ERROR MESSAGES - GOES TO PRINT QUEUE SMM REPORT               *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PRNTERR  NTR1                                                                   
         MVI   P1,0                                                             
         LA    R5,P2                                                            
         USING PRERRD,R5                                                        
         MVC   PRERRHD,=C'ERROR  LINE NUMBER'                                   
         EDIT  LINENUM,(5,PRERRNO)                                              
         MVC   P3(L'SAVEA1),SAVEA1                                              
         TM    ERRFLAG,A1ERR                                                    
         BO    PERR10                                                           
                                                                                
         MVC   P4,SAVEH1                                                        
         MVC   P5+5(124),SAVEH1+132                                             
         TM    ERRFLAG,HDRERR                                                   
         BO    PERR10                                                           
                                                                                
         MVC   P6,SAVEH2                                                        
         MVC   P7+5(124),SAVEH2+132                                             
         TM    ERRFLAG,HDR2ERR                                                  
         BO    PERR10                                                           
*                                                                               
         MVC   P8,SAVEH3                                                        
         MVC   P9+5(124),SAVEH3+132                                             
         TM    ERRFLAG,HDR3ERR                                                  
         BO    PERR10                                                           
                                                                                
         MVC   P10,0(R2)                   MUST BE A BUYERR                     
         MVC   P11+5(124),123(R2)                                               
                                                                                
PERR10   GOTO1 REPORT                                                           
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                         END-OF-BUY RECORD                           *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
EOB      NTR1                                                                   
         TM    RECFLAG,GOODBUY                                                  
         BNO   EXIT                                                             
         NI    RECFLAG,X'FF'-GOODBUY                                            
         TM    ERRFLAG,A1ERR+HDRERR+HDR2ERR+HDR3ERR+BUYERR                      
         BNZ   EXIT                                                             
         LA    R3,IO2+10                                                        
         USING SEBYD,R3                                                         
         XC    SEBYODAT(SEBYSTRT-SEBYODAT),SEBYODAT                             
                                                                                
         MVC   SEBYLEN,=AL2(SEBYLENQ)                                           
         MVC   SEBYTYPE,=C'EBY*'   OBJECT TYPE                                  
                                                                                
         MVC   IO2L(2),=Y(SEBYLENQ+16)                                          
         BAS   RE,WRKR                                                          
                                                                                
         DROP  R3                                                               
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   OPEN WORKER FILE                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKROPEN NTR1                                                                   
         L     R0,AWKBUFF          CLEAR WKFILE BUFFER                          
         L     R1,=A(WKBUFFX-WKBUFF)                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         XC    SEQNUM,SEQNUM                                                    
         USING WLHDRD,R4                                                        
         LA    R4,IO2                                                           
         XC    0(255,R4),0(R4)     BUILD HEADER                                 
         MVC   WLSOFLAB,=C'*SOFSOF*'                                            
         MVC   WLUSRID,RCORIGID    COKEAT                                       
         MVC   WLSYSPRG(3),=C'CCX'                                              
         MVI   WLSUBPRG,C'1'       SET FACPAK1                                  
         CLI   QOPT1,C'T'          OPTION TO GENERATE ON TST                    
         BNE   *+8                                                              
         MVI   WLSUBPRG,C'T'       SET FACTST                                   
         MVC   WLDAY,TDAY+2                                                     
         MVI   WLCLASS,C'T'        CLASS T FOR WRKF SCRIPTS                     
         MVI   WLTYPE,C'A'         TYPE A FOR IMMEDIATE EXEC                    
         MVC   WLPSWD,SPACES                                                    
         OI    WLATTB,WLATOBJ                                                   
                                                                                
         LA    R3,IO2                                                           
         MVI   FIXED,C'Y'                                                       
         BAS   RE,WRKR                                                          
*                                                                               
         XC    SEQNUM,SEQNUM                                                    
         XC    WRKRINDX,WRKRINDX                                                
         LA    R5,WRKRINDX                                                      
         USING UKRECD,R5                                                        
         MVC   UKUSRID,RCORIGID                                                 
         MVC   UKSYSPRG(3),=C'CCX'                                              
         MVC   UKFILENO,WLREPRNO   WORKER FILE NUMBER                           
         DROP  R5                                                               
                                                                                
         MVC   WRKFNO,WLREPRNO     WORKER FILE NUMBER                           
         MVI   FIXED,C'N'                                                       
                                                                                
         MVC   P(16),=C'WORKER FILE ID ='                                       
         EDIT  (B2,WLUSRID),(4,P+20)                                            
         MVI   P+24,C','                                                        
         MVC   P+25(4),WLFILEID                                                 
         GOTO1 HEXOUT,DMCB,WLDAY,P+29,1,=C'TOG'                                 
         MVC   P+31(1),WLCLASS                                                  
         MVI   P+32,C','                                                        
         EDIT  WLREPRNO,(5,P+33),0,ALIGN=LEFT                                   
         MVC   P+40(L'SVTMSTMP),SVTMSTMP    TIME STAMP                          
         GOTO1 REPORT                                                           
         DROP  R4                                                               
                                                                                
         LA    R1,IO2                                                           
         XC    0(256,R1),0(R1)                                                  
         MVC   0(4,R1),=F'2101'                                                 
         MVC   4(6,R1),=C'SCRIPT'                                               
         MVC   10(8,R1),=C'SPCOKUPL'                                            
         MVI   18(R1),C'S'         INSERT MODE/USER SETS FINAL STATUS           
         MVC   30(5,R1),=C'00006'    HEADER LENGTH                              
         MVC   35(5,R1),=C'00100'    INPUT  DATA LENGTH                         
         MVC   40(5,R1),=C'00020'    OUTPUT DATA LENGTH                         
         MVI   45(R1),C'N'         DO NOT INSERT ERRORS AT FILE END             
         MVC   59(L'SVTMSTMP,R1),SVTMSTMP  TIME STAMP                           
         MVC   IO2L(2),=H'76'        72 + 4 BYTES FOR QSAM                      
         BAS   RE,WRKR                                                          
                                                                                
         LA    R1,IO2                                                           
         XC    0(256,R1),0(R1)                                                  
         MVC   10(20,R1),=CL20'SIGN-ON INFORMATION '                            
         MVC   30(6,R1),=C'COKEAT'                                              
         MVC   38(3,R1),=C'DDS'                                                 
                                                                                
         MVC   IO2L(2),=H'50'        LEAVE ROOM (LEN + 4 FOR QSAM)              
         BAS   RE,WRKR                                                          
                                                                                
         LA    R1,IO2                                                           
         XC    0(256,R1),0(R1)                                                  
         MVC   30(4,R1),=C'@#$%'                 MEDIA                          
         EDIT  WRKFNO,(10,34(R1)),0,ALIGN=LEFT   BUYER(WORKER FILE NO.)         
                                                                                
         MVC   IO2L(2),=H'47'                 43 + 4 BYTES FOR QSAM             
         BAS   RE,WRKR                                                          
                                                                                
         OI    RECFLAG,FIRSTHDR                                                 
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   CLOSE WORKER FILE                                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKRCLSE NTR1                                                                   
         BAS   RE,EOB                                                           
         CLC   LASTREC,=C'H1'                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING WLHDRD,R4                                                        
         LA    R4,IO2                                                           
         XC    0(255,R4),0(R4)     BUILD HEADER                                 
         MVC   WLSOFLAB,=C'*EOFEOF*'                                            
         MVI   FIXED,C'Y'                                                       
         XC    SEQNUM,SEQNUM                                                    
         BAS   RE,WRKR                                                          
         CLI   QOPT2,C'Y'          OPTION TO SET TO STATUS HOLD                 
         BNE   EXIT                                                             
         GOTO1 DATAMGR,DMCB,=C'HOLD    ',WRKFILEN,WRKRINDX,IO2,AWKBUFF          
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                    ADD LINE TO WORKER FILE                          *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKR     NTR1                                                                   
         OC    SEQNUM,SEQNUM                                                    
         BZ    WRKR10                                                           
         MVC   IO2(4),=F'2102'                                                  
         EDIT  SEQNUM,(6,IO2+4),0,FILL=0                                        
                                                                                
WRKR10   DS    0H                                                               
         LA    R3,IO2                                                           
         CLI   FIXED,C'Y'                                                       
         BE    *+8                                                              
         LA    R3,IO2L                                                          
                                                                                
         GOTO1 DATAMGR,DMCB,DMPRINT,WRKFILE,0,(R3),AWKBUFF                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         ST    R1,SEQNUM                                                        
         CLI   QOPT3,C'Y'                                                       
         BNE   EXIT                                                             
         MVC   P,IO2                                                            
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* ADD TABLE ENTRY FOR THIS HEADER W/O B1'S TO DELETE BUYS IF DELETED  *         
* FLIGHT FLAG ON (99 IN COLUMN 201 OF H1 HEADER)                      *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
HDRDBUY  NTR1                                                                   
         CLC   LASTREC,=C'H1'                                                   
         BNE   HDRDBUYX                                                         
         TM    RECFLAG2,HDR99                                                   
         BNO   HDRDBUYX                                                         
         GOTO1 BINSRCH,BINPARMS,(1,TABENTRY)                                    
         OC    BINPARMS,BINPARMS   TABLE FULL?                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
HDRDBUYX B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                    PUT OUT HEADER WITHOUT BUYS                      *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
HDRWOB   NI    RECFLAG,X'FF'-HDRWOBS                                            
         TM    ERRFLAG,A1ERR+HDRERR+HDR2ERR+HDR3ERR                             
         BNZ   EXIT                                                             
         CLC   SAVEH1+H1DEL-UH1(2),=C'99'   ONLY IF THEY WANT ALL THE           
         BNE   EXIT                         BUYS DELETED                        
         USING SHDRD,R3                                                         
         MVC   SHDRLEN,=AL2(SHDRLENQ)                                           
         MVC   SHDRTYPE,=C'HDR*'   OBJECT TYPE                                  
         MVC   IO2L(2),=Y(SHDRLENQ+16)                                          
         BAS   RE,WRKR                                                          
         NI    RECFLAG,X'FF'-FIRSTHDR                                           
         DROP  R3                                                               
                                                                                
         USING SBUYD,R3                                                         
         XC    SBUYODAT(SBUYACTN-SBUYODAT),SBUYODAT                             
         MVI   SBUYSTRT,C' '                PAD WITH SPACES                     
         MVC   SBUYSTRT+1(SBUYRLNQ-1),SBUYSTRT                                  
                                                                                
         MVC   SBUYLEN,=AL2(SBUYLENQ)                                           
         MVC   SBUYTYPE,=C'BUY*'            OBJECT TYPE                         
         MVC   IO2L(2),=Y(SBUYLENQ+16)                                          
         MVC   SBUYSTA,SVSTATN              STATION                             
         MVC   SACNNUM,SVACN                ACN                                 
         MVC   SBUYUID(4),SVFLRFNO       FILE REF NO(4)                         
         MVI   SBUYDEL,C'Y'                                                     
         BAS   RE,WRKR                                                          
         DROP  R3                                                               
         OI    RECFLAG,GOODBUY                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
TIN      DCB   DDNAME=TIN,DSORG=PS,MACRF=GM,EODAD=MMX                           
                                                                                
         LTORG                                                                  
DEMTAB   DC    C'MET1 HH',C'META   '                                            
         DC    C'MET2 HH',C'METB   '                                            
         DC    C'DMA  HH',C'HOMES  '                                            
         DC    C'DMAHH  ',C'HOMES  '                                            
         DC    C'TOT  HH',C'HOMES  '                                            
         DC    X'FF'                                                            
                                                                                
DAYPRTAB DC    C'DT',C'D'          DAY                                          
         DC    C'EF',C'E'          EARLY FRINGE                                 
         DC    C'PA',C'A'          PRIME ACCESS                                 
         DC    C'AC',C'A'          "                                            
         DC    C'PR',C'P'          PRIME                                        
         DC    C'PT',C'P'          "                                            
         DC    C'IP',C'P'          "                                            
         DC    C'LF',C'L'          LATE FRINGE                                  
         DC    C'SP',C'S'          SPORTS                                       
         DC    C'OT',C'Z'          OTHER                                        
         DC    C'GF',C'G'          GIFT                                         
         DC    X'FF'                                                            
                                                                                
RADIOTAB DC    C'CO',C'FM'         TABLE FOR RADIO STATIONS                     
         DC    C'AF',C'FM'         BAND(2),REPLACEMENT BAND(2)                  
         DC    C'FF',C'FM'                                                      
         DC    C'FA',XL2'00'                                                    
         DC    C'AM',XL2'00'                                                    
         DC    C'FM',XL2'00'                                                    
         DC    C'F ',XL2'00'                                                    
         DC    C'A ',XL2'00'                                                    
         DC    X'FF'                                                            
*                                                                               
LOWSTAB  DS    0H                     LOW POWER STATION TABLE                   
         DC    C'KM58',C'KMPH-L  '    MATCH (4), REPLACEMENT (8)                
         DC    X'FF'                                                            
*                                                                               
STATAB   DC    C'KBMY-TV '                                                      
         DC    C'KFYR-TV '                                                      
         DC    C'KXMC-TV '                                                      
         DC    C'KPOM-TV '                                                      
         DC    C'KHBS-TV '                                                      
         DC    C'KREX-TV '                                                      
         DC    C'KOTA-TV '                                                      
         DC    C'KOAT-TV '                                                      
         DC    C'KOB-TV  '                                                      
         DC    C'KSVI-TV '                                                      
         DC    C'KECI-TV '                                                      
         DC    C'KPAX-TV '                                                      
         DC    C'KELO-TV '                                                      
         DC    C'KCAU-TV '                                                      
         DC    C'KSFY-TV '                                                      
         DC    C'KTTW-TV '                                                      
         DC    C'KGWN-TV '                                                      
         DC    C'KPVI-TV '                                                      
         DC    C'KPBI-TV '                                                      
         DC    C'KFSM-TV '                                                      
         DC    C'WXLV-TV '                                                      
         DC    C'WFXI-TV '                                                      
         DC    C'WSAZ-TV '                                                      
         DC    C'WFXR-TV '                                                      
         DC    X'FF'                                                            
                                                                                
ALPHABET DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',X'FF'                    
         EJECT                                                                  
DMPRINT  DC    CL8'DMPRINT'                                                     
WRKFILE  DC    CL8'WRKF1  '                                                     
WRKFILEN DC    CL8'WRKFILE'                                                     
WRKFN    DC    C'SBUY'             WRKF=WRKF                                    
AWKBUFF  DC    A(WKBUFF)                                                        
ATABLE   DC    A(TABLE)                                                         
BINPARMS DS    6F                  BINSRCH PARAMETERS                           
ADEMO1   DS    F                   ADDRESS OF DEMO                              
ADEMO2   DS    F                   ADDRESS OF DEMO                              
SEQNUM   DS    F                   SEQUENCE NUMBER OF RECORDS                   
NUMENTRY DS    F                   NUMBER OF HEADER ENTRIES IN TABLE            
LINENUM  DS    F                   LINE NUMBER IN WORKER FILE                   
COUNT    DS    F                   NUMBER OF RECORDS PUT OUT                    
STARTLN  DS    F                   LINE TO START AT IN UPLOAD                   
LASTLN   DS    F                   LINE TO END AT IN UPLOAD                     
NEXTALPH DS    F                   ADDRESS OF NEXT LETTER TO USE                
HDRCOUNT DS    H                   COUNT OF HEADERS                             
FRSTREAD DS    C                   Y=FIRST TIME READING UPLOAD                  
*                                                                               
RECFLAG  DS    X                                                                
FIRSTA1  EQU   X'80'               THIS IS THE FIRST A1 RECORD                  
FIRSTHDR EQU   X'40'               THIS IS THE FIRST HDR OF WRKR FILE           
FIRSTBUY EQU   X'20'               NEXT BUY IS FIRST BUY AFTER HEADER           
HDRWOBS  EQU   X'10'               HEADER WAS SENT WITHOUT BUYS                 
GOODBUY  EQU   X'01'               A GOOD BUY WAS JUST PUT OUT                  
*                                                                               
ERRFLAG  DS    X                                                                
A1ERR    EQU   X'10'               ERROR IN A1 RECORD                           
HDRERR   EQU   X'08'               ERROR IN H1 RECORD                           
HDR2ERR  EQU   X'04'               ERROR IN H2 RECORD                           
BUYERR   EQU   X'02'               ERROR IN B1 RECORD                           
HDR3ERR  EQU   X'01'               ERROR IN H3 RECORD                           
*                                                                               
RECFLAG2 DS    X                                                                
FNDLINE  EQU   X'80'               FOUND LINE IN UPLOAD TO START AT             
PROCSKD  EQU   X'40'               PROCESS SKD DATA AGAIN                       
PROCSKD2 EQU   X'20'               IN MIDDLE OF PROCESS SKD DATA AGAIN          
PRGWRKR  EQU   X'10'               PURGE WORKER FILE                            
HDR99    EQU   X'08'               HEADER HAS A 99 FOR DELETES                  
*                                                                               
WRKFNO   DS    XL2                 WORKER FILE NUMBER                           
TDAY     DS    CL3                 YYMMDD PWOS                                  
FIXED    DS    CL1                 FIXED LENGTH                                 
SIGNON2H DS    XL2                 2 BYTE HEX AGENCY ID                         
NUMDEMOS DS    X                   NUMBER OF DEMOS IN H2 TO PROCESS             
RFLAG    DS    CL1                                                              
LASTREC  DS    CL2                                                              
LASTMED  DS    CL2                                                              
STARTIME DS    CL5                 START TIME                                   
         DC    C'-'                                                             
ENDTIME  DS    CL5                 END TIME                                     
BSTTIME  DS    XL2                                                              
BENTIME  DS    XL2                                                              
CLNT     DS    CL3                                                              
CLTPRDES DS    CL9                 CLIENT/PRODUCT/ESTIMATE                      
SVTMSTMP DS    CL13                SAVE TIME STAMP ON UPLOAD                    
SVMED    DS    C                   SAVE MEDIA                                   
SVSTDATE DS    CL6                 SAVE START DATE                              
SVSTATN  DS    CL8                 SAVE STATION                                 
SVACN    DS    CL5                 SAVE ACN                                     
SVFLRFNO DS    CL4                 SAVE FILE REF # (RIGHT ALIGN,0 FILL)         
PSKDTAB  DS    14PL2               TABLE OF PACKED SKEDULE VALUES               
PSKD     DS    PL2                                                              
DMACTN   DS    CL5                                                              
WRKRCMD  DS    CL7                                                              
WRKRINDX DS    CL42                                                             
TABENTRY DS    XL21                CLT(3),PRD/EST(8),STATION(8),LINE(2)         
*                                                                               
         DS    0D                                                               
SAVEA1   DS    CL7                                                              
         DS    0D                                                               
SAVEH1   DS    CL256                                                            
SAVEH2   DS    CL256                                                            
SAVEH3   DS    CL256                                                            
SVBUY    DS    XL(SBUYLENQ+20)                                                  
ELEM     DS    CL256                                                            
IO       DS    XL400               IO AREA                                      
IO2L     DS    F                                                                
IO2      DS    XL400               IO AREA                                      
*                                                                               
TABLE    DS    4095XL(L'TABENTRY)                                               
TABLEMAX EQU   (*-TABLE)/(L'TABENTRY)    MAX ENTRIES                            
         DC    X'FF'                                                            
*                                                                               
WKBUFF   DS    14336C                                                           
WKBUFFX  EQU   *                                                                
                                                                                
TABLED   DSECT                                                                  
TABCLT   DS    CL3                                                              
TABCODES DS    XL8                                                              
TABSTATN DS    CL8                                                              
TABCOUNT DS    XL2                                                              
                                                                                
UH1      DSECT                                                                  
         DS    CL127                                                            
H1STATN  DS    CL8                                                              
H1STDATE DS    CL5                                                              
H1ENDATE DS    CL5                                                              
H1STYEAR DS    CL2                                                              
         DS    C                                                                
H1EST    DS    CL7                                                              
         DS    C                                                                
H1ACN    DS    CL5                                                              
         DS    CL35                                                             
H1FLRFNO DS    CL4                                                              
H1DEL    DS    CL2                                                              
*                                                                               
UH2      DSECT                                                                  
         DS    CL100                                                            
H2DEMOS  DS    6CL8                                                             
         DS    CL105                                                            
H2MEDIA  DS    CL2                 TV/RD/CA                                     
         DS    CL1                 UNUSED MEDIA BYTE                            
*                                                                               
UH3      DSECT                                                                  
         DS    CL67                                                             
H3STA    DS    CL5                                                              
*                                                                               
UB1      DSECT                                                                  
         DS    CL2                                                              
B1BUYID  DS    CL3                                                              
B1PRGMNM DS    CL14                                                             
B1DAYPRT DS    CL2                                                              
         DS    CL16                                                             
B1STTIME DS    CL5                                                              
B1ENTIME DS    CL5                                                              
B1ROTATN DS    CL7                                                              
B1SPTLEN DS    CL3                                                              
         DS    CL6                                                              
B1SPCOST DS    CL10                                                             
         DS    CL10                                                             
B1SKD    DS    52CL3                                                            
*                                                                               
UB2      DSECT                                                                  
         DS    CL2                                                              
B2DEMOS  DS    6CL6                                                             
*                                                                               
PRERRD   DSECT                                                                  
PRERRHD  DS    CL18                                                             
         DS    C                                                                
PRERRNO  DS    CL5                                                              
         DS    CL3                                                              
PRERRMSG DS    CL25                                                             
         DS    CL5                                                              
PRERRFLD DS    CL18                                                             
*                                                                               
PRHDERRD DSECT                                                                  
         DS    CL20                                                             
PRHTYPE  DS    CL2                                                              
         DS    CL3                                                              
PRHSTATN DS    CL8                                                              
         DS    CL3                                                              
PRHSTDAT DS    CL5                                                              
         DS    CL3                                                              
PRHENDAT DS    CL5                                                              
         DS    CL3                                                              
PRHEST   DS    CL7                                                              
         DS    CL3                                                              
PRHACN   DS    CL5                                                              
*                                                                               
         DS    CL3                                                              
PRBPRGRM DS    CL14                                                             
         DS    CL3                                                              
PRBDAYPT DS    CL2                                                              
         DS    CL3                                                              
PRBSTTIM DS    CL5                                                              
         DS    CL3                                                              
PRBENTIM DS    CL5                                                              
         DS    CL3                                                              
PRBROTAN DS    CL7                                                              
         DS    CL3                                                              
PRBSPLEN DS    CL3                                                              
         DS    CL3                                                              
PRBCOST  DS    CL10                                                             
         EJECT                                                                  
                                                                                
         PRINT OFF                                                              
       ++INCLUDE SPTUPLOADD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE DMWRKFK                                                        
       ++INCLUDE CTGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPREPMM02 12/16/02'                                      
         END                                                                    
