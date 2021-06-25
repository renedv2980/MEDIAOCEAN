*          DATA SET SPLFM24    AT LEVEL 115 AS OF 05/01/02                      
*PHASE T21924B,+0                                                               
*INCLUDE SCINKEY                                                                
*INCLUDE XSORT                                                                  
         TITLE 'SPLFM24  MARKET RECORD  T21924'                                 
T21924   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21924                                                         
         L     RC,0(R1)                                                         
         LA    R7,2048(RC)                                                      
         LA    R7,2048(R7)                                                      
         USING GENOLD,RC,R7                                                     
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING MKTRECD,R8                                                       
         RELOC RELO                                                             
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADIAN                                
         BNE   INITX                                                            
         CLC   MARRST2(3),=C'BBM'  TEST TITLE FIXED                             
         BE    INITX                                                            
         MVC   MARRST2(3),=C'BBM'  FIX RTG SVC ID                               
         OI    MARRST2H+6,X'80'    AND XMT THE FIELD                            
*                                                                               
INITX    CLI   SVFMTSW,0                     TEST FORMAT OR EDIT                
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 RDSTA                                                            
         FOUT  MARNAMEH,MKTNAME,24                                              
         FOUT  MARTZONH,MKTZONE,1                                               
         FOUT  MARRANKH,MKTRANK,3                                               
         FOUT  MARHOMSH,MKTHOMES,8                                              
         FOUT  MARNTAH,MKTNTA,2                                                 
         XC    MARWT,MARWT                                                      
         OC    MKTWT,MKTWT                                                      
         BZ    FMT1                                                             
         CLC   MKTWT,=C'0000'                                                   
         BE    FMT1                                                             
         EDIT  (C4,MKTWT),(5,MARWT),2,ALIGN=LEFT                                
*                                                                               
FMT1     FOUT  MARWTH                                                           
         XC    MARSHR,MARSHR                                                    
         OC    MKTSHR,MKTSHR                                                    
         BZ    FMT2                                                             
         CLC   MKTSHR,=C'0000'                                                  
         BE    FMT2                                                             
         EDIT  (C4,MKTSHR),(5,MARSHR),2,ALIGN=LEFT                              
*                                                                               
FMT2     FOUT  MARSHRH                                                          
*                                                                               
FMT3     DS    0H                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(1),MKTRS1       MOVE RTG SVC ID                              
         MVC   DUB+1(2),MKTRSM1    MOVE RTG SVC MKT NUM                         
         MVC   DUB+3(1),MKTCLAS1   MOVE RTG SVC SWEEP CLASS                     
         OI    DUB+3,X'F0'                                                      
         MVC   DUB+4(1),MKTRS2                                                  
         MVC   DUB+5(2),MKTRSM2                                                 
         MVC   DUB+7(1),MKTCLAS2                                                
         OI    DUB+7,X'F0'                                                      
*                                                                               
         CLI   DUB,C'0'                                                         
         BNE   FMT4A                                                            
         MVI   DUB+4,C'1'                                                       
         B     FMT4X                                                            
*                                                                               
FMT4A    CLI   DUB,C'1'                                                         
         BNE   FMT4B                                                            
         MVI   DUB+4,C'0'                                                       
         B     FMT4X                                                            
*                                                                               
FMT4B    CLI   DUB+4,C'0'                                                       
         BNE   FMT4C                                                            
         MVI   DUB,C'1'                                                         
         B     FMT4X                                                            
*                                                                               
FMT4C    CLI   DUB+4,C'1'                                                       
         BNE   FMT4D                                                            
         MVI   DUB,C'0'                                                         
         B     FMT4X                                                            
*                                                                               
FMT4D    MVI   DUB,C'0'                                                         
         MVI   DUB+4,C'1'                                                       
*                                                                               
FMT4X    XC    ELEM(8),ELEM                                                     
*                                                                               
         CLI   SVAPROF,C'0'        TEST NSI AGENCY                              
         BE    *+12                                                             
         CLI   SVAPROF,C'2'        OR BOTH                                      
         BNE   FMT6                                                             
*                                                                               
         MVC   ELEM(4),DUB                                                      
         CLI   DUB,C'0'            TEST NSI DATA                                
         BE    FMT6                                                             
         MVC   ELEM(4),DUB+4                                                    
*                                                                               
FMT6     CLI   SVAPROF,C'1'        TEST ARB AGENCY                              
         BE    *+12                                                             
         CLI   SVAPROF,C'2'        OR BOTH                                      
         BNE   FMT8                                                             
*                                                                               
         MVC   ELEM+4(4),DUB+4                                                  
         CLI   DUB+4,C'1'                                                       
         BE    FMT8                                                             
         MVC   ELEM+4(4),DUB                                                    
*                                                                               
FMT8     LA    R2,MARRSM1H                                                      
         LA    R3,ELEM                                                          
         BAS   RE,FMTRS                                                         
*                                                                               
         LA    R2,MARRSM2H                                                      
         LA    R3,ELEM+4                                                        
         BAS   RE,FMTRS                                                         
*                                                                               
FMT10    DS    0H                                                               
         FOUT  MARLOCKH,MKTLTACC,3                                              
*                                                                               
         LA    R2,MARMKTSH                                                      
         BAS   RE,DSPMKT                DISPLAY MKTS                            
*                                                                               
FMTX     DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
* *********************************************************************         
* FMTRS- FORMAT RATING SERVICE MKT/SWEEP CLASSS                                 
* ON ENTRY R2 POINTS TO NSI  OR ARB(BBM) FLDHDR                                 
* R3 POINTS TO 4 BYTE DATA VALUES                                               
* *********************************************************************         
         SPACE 1                                                                
FMTRS    DS    0H                                                               
         XC    8(4,R2),8(R2)       CLEAR MARKET NUMBER                          
         OI    6(R2),X'80'                                                      
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)          GET MKT NUM                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(4,R2),DUB                                                      
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO SWEEP CLASS                         
         MVI   8(R2),0             CLEAR IT                                     
         OI    6(R2),X'80'         AND XMT                                      
         MVC   8(1,R2),3(R3)       MOVE SWEEP CLASS                             
         OI    8(R2),X'F0'         GUARANTEE EBCDIC                             
*                                                                               
FMTRSX   BR    RE                                                               
         EJECT                                                                  
* *********************************************************************         
* DSPMKT-DISPLAY MARKETS                                                        
* *********************************************************************         
         SPACE 1                                                                
DSPMKT   NTR1                                                                   
         XC    MARMKTS,MARMKTS     CLEAR SCREEN FIELD                           
         OI    MARMKTSH+6,X'80'    TRANSMIT                                     
         SR    R6,R6               R6=N'ENTRIES IN TABLE                        
         LA    R5,MKTALST          PT TO MKTS IN RECORD                         
*                                                                               
DSPMKT10 CH    R6,=H'9'            MAX 9 MKTS                                   
         BNL   DSPMK15                                                          
         OC    0(3,R5),0(R5)       NON ZERO?                                    
         BZ    DSPMK15                                                          
         CLC   0(3,R5),=C'0000'                                                 
         BE    DSPMK15                                                          
         LA    R6,1(R6)                                                         
         LA    R5,3(R5)                                                         
         B     DSPMKT10                                                         
*                                  FORMAT SCAN BLOCK INTO TWA                   
DSPMK15  DS    0H                                                               
         LTR   R6,R6                                                            
         BZ    DSPMKTX                                                          
*                                                                               
         BCTR  R6,0                DON'T INCLUDE PRIMARY (1ST) ALPHA            
         LTR   R6,R6                MARKET INTO SORT.                           
         BZ    DSPMK20                                                          
*                                                                               
         L     RF,=V(XSORT)                                                     
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(0,MKTALST+3),(R6),3,3,0  SORT BEFORE DIS-             
*                                    PLAY, LEAVE 1ST ALPHA MKT ALONE.           
DSPMK20  LA    R6,1(R6)            DISPLAY ALL ALPHA MARKETS.                   
         L     RF,=V(SCINKEY)                                                   
         A     RF,RELO                                                          
         LA    R4,MARMKTSH                                                      
         GOTO1 (RF),DMCB,(1,(R4)),(3,MKTALST),(R6)                              
*                                                                               
DSPMKTX  B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
* *********************************************************************         
* EDT    -     VALIDATE INPUT                                                   
* *********************************************************************         
EDT      CLI   SVACT,C'A'                                                       
         BE    EDT0                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 RDSTA                                                            
         MVC   PRIMARK,MKTALST     PRIMARK=PRIMARY ALPHA MKT.                   
EDT0     LA    R2,MARNAMEH                                                      
         GOTO1 ANY                                                              
         MVC   MKTNAME,8(R2)                                                    
         LA    R2,MARTZONH                                                      
         MVI   MKTZONE,0                                                        
         CLI   5(R2),0                                                          
         BE    EDT10                                                            
         GOTO1 ANY                                                              
         MVC   MKTZONE,8(R2)                                                    
*                                                                               
EDT10    LA    R2,MARRANKH                                                      
         XC    MKTRANK,MKTRANK                                                  
         CLI   5(R2),0                                                          
         BNE   EDT10A                                                           
         CLC   AGYALPHA,=C'BO'                                                  
         BNE   EDT15                                                            
         MVI   ERRCD,MSSNGERR                                                   
         GOTO1 LFMERR        REQUIRED FOR BO                                    
*                                                                               
EDT10A   GOTO1 ANY                                                              
         GOTO1 PACK                                                             
         MVI   ERRCD,INVERR                                                     
         CP    DUB,=P'0'                                                        
         BE    LFMERR                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKTRANK,DUB                                                      
         B     EDT15                                                            
*                                                                               
EDT15    LA    R2,MARHOMSH                                                      
         MVI   ERRCD,INVERR                                                     
         XC    MKTHOMES,MKTHOMES                                                
         CLI   5(R2),0                                                          
         BE    EDT20                                                            
         GOTO1 ANY                                                              
         GOTO1 PACK                                                             
         CP    DUB,=P'0'                                                        
         BE    LFMERR                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKTHOMES,DUB                                                     
*                                                                               
EDT20    DS    0H                                                               
         B     EDT40              REGION CODE GOES HERE                         
*                                                                               
EDT40    LA    R2,MARNTAH                                                       
         XC    MKTNTA,MKTNTA                                                    
         CLI   5(R2),0                                                          
         BNE   EDT40A                                                           
         CLC   AGYALPHA,=C'BO'                                                  
         BNE   EDT45                                                            
         MVI   ERRCD,MSSNGERR                                                   
         B    LFMERR                                                            
*                                                                               
EDT40A   GOTO1 ANY                                                              
         GOTO1 PACK                                                             
         CP    DUB,=P'0'                                                        
         BE    LFMERR                                                           
         CP    DUB,=P'29'                                                       
         BH    LFMERR                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKTNTA,DUB                                                       
*                                                                               
EDT45    LA    R2,MARWTH                                                        
         XC    MKTWT,MKTWT                                                      
         CLI   5(R2),0                                                          
         BE    EDT50                                                            
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,MARWT),(R5)                                     
         CLI   DMCB,X'FF'                                                       
         BE    LFMERR                                                           
         L     R5,DMCB+4                                                        
         CVD   R5,DUB                                                           
         CP    DUB,=P'9999'         MAX IS 99.99                                
         BH    LFMERR                                                           
         CP    DUB,=P'0'                                                        
         BE    LFMERR                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKTWT,DUB                                                        
*                                                                               
EDT50    LA    R2,MARSHRH                                                       
         XC    MKTSHR,MKTSHR                                                    
         CLI   5(R2),0                                                          
         BE    EDT60                                                            
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,MARSHR),(R5)                                    
         CLI   DMCB,X'FF'                                                       
         BE    LFMERR                                                           
         L     R5,DMCB+4                                                        
         CVD   R5,DUB                                                           
         CP    DUB,=P'0'                                                        
         BNH   LFMERR                                                           
         CP    DUB,=P'9999'           MAX IS 99.99                              
         BH    LFMERR                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKTSHR,DUB                                                       
*                                                                               
EDT60    XC    MKTRS1(3),MKTRS1    CLEAR RTG SVC AND MKT                        
         MVI   MKTCLAS1,0          AND SWEEP CLASS                              
         MVI   MKTRS1,C'0'         FORCE RTG SVC = NSI                          
*                                                                               
         LA    R2,MARRSM1H         RATING SERVICE MARKET                        
         CLI   5(R2),0             TEST DATA                                    
         BE    EDT65               NO                                           
*                                                                               
         LA    R2,MARRSM1H         EDIT MARKET NUMBER                           
         GOTO1 PACK                                                             
         STH   R0,HALF                                                          
         MVC   MKTRSM1,HALF                                                     
*                                                                               
EDT65    LA    R2,MARSCL1H         EDIT SWEEP CLASS                             
         CLI   5(R2),0             TEST DATA                                    
         BE    EDT70                                                            
         GOTO1 PACK                                                             
         MVC   MKTCLAS1,8(R2)                                                   
*                                                                               
EDT70    XC    MKTRS2(3),MKTRS2    CLEAR RTG SVC AND MKT                        
         MVI   MKTCLAS2,0          AND SWEEP CLASS                              
         MVI   MKTRS2,C'1'         FORCE RTG SVC = ARB/BBM                      
*                                                                               
         LA    R2,MARRSM2H         RATING SERVICE MARKET                        
         CLI   5(R2),0             TEST DATA                                    
         BE    EDT75               NO                                           
*                                                                               
         LA    R2,MARRSM2H         EDIT MARKET NUMBER                           
         GOTO1 PACK                                                             
         STH   R0,HALF                                                          
         MVC   MKTRSM2,HALF                                                     
*                                                                               
EDT75    LA    R2,MARSCL2H         EDIT SWEEP CLASS                             
         CLI   5(R2),0             TEST DATA                                    
         BE    EDT80                                                            
         GOTO1 PACK                                                             
         MVC   MKTCLAS2,8(R2)                                                   
*                                                                               
EDT80    XC    MKTLTACC,MKTLTACC                                                
         LA    R2,MARLOCKH                                                      
         CLI   5(R2),0                                                          
         BE    EDT90                                                            
         CLI   5(R2),3                                                          
         BH    LFMERR                                                           
         MVC   MKTLTACC,8(R2)                                                   
         LA    R4,MKTLTACC                                                      
         LA    R5,3                                                             
EDT80B   CLI   0(R4),0                                                          
         BE    EDT80C                                                           
         CLI   0(R4),C'A'                                                       
         BL    LFMERR                                                           
         CLI   0(R4),C'9'                                                       
         BH    LFMERR                                                           
EDT80C   LA    R4,1(R4)                                                         
         BCT   R5,EDT80B                                                        
*                                                                               
EDT90    MVI   MKTNODST,0                                                       
         LA    R2,MARDSTH                                                       
         CLI   5(R2),0                                                          
         BE    EDT100                                                           
         MVC   MKTNODST,8(R2)                                                   
         CLI   MKTNODST,C'Y'                                                    
         BNE   LFMERR                                                           
*                                                                               
EDT100   XC    MKTALST,MKTALST                                                  
         LA    R2,MARMKTSH                                                      
         CLI   5(R2),0                                                          
         BE    EDT102                                                           
         BAS   RE,VALMRKT                                                       
         BZ    LFMERR                                                           
*                                                                               
*----------------------- PROCESSING 'L'-RECORDS ----------------------*         
*                                                                               
EDT102   CLI   SVACT,C'A'                                                       
         BNE   EDT105                                                           
         OC    MKTALST,MKTALST     ANY ALPHA MARKETS?                           
         BZ    EDT110              NO, EXIT 'L'-RECORD PROCESSING.              
         BAS   RE,BLDANKEY         BUILD KEY OF 'L'-RECORD.                     
         USING ANMRECD,R1                                                       
         MVC   ANMKAMRK,MKTALST    PUT 1ST ALPHA MKT INTO KEY.                  
         DROP  R1                                                               
         XCEF  REC2,2000           CLEAR REC2.                                  
         MVC   REC2(ANMKEYLQ),MYKEY  MOVE KEY INTO RECORD.                      
         MVC   REC2+(ANMRCLEN-ANMRECD)(2),=H'20'                                
         MVC   COMMAND,=C'DMADD   '                                             
         BAS   RE,EDT107                                                        
*                                                                               
         CLI   SVEBCMED,C'T'       IF MEDIA 'T', THEN WORRY ABOUT               
         BNE   EDT110               CANADIAN AGENCIES.                          
*                                                                               
         CLI   SVAPROF+7,C'C'      CHECK FOR CANADIAN AGENCY.                   
         BNE   EDT110                                                           
         BAS   RE,MKCANADC         MAKE A MEDIA 'C' KEY.                        
         BAS   RE,EDT107                                                        
         BAS   RE,MKCANADN         MAKE A MEDIA 'N' KEY.                        
         BAS   RE,EDT107                                                        
*                                                                               
         MVC   MYKEY+(ANMKMED-ANMKEYD)(1),SVEBCMED  ORIGINAL MEDIA.             
         B     EDT110                                                           
*                                                                               
EDT105   CLI   SVACT,C'C'                                                       
         BNE   EDT110             EXIT PROCESSING IF SVACT<>'C' EITHER.         
         CLC   PRIMARK,MKTALST     DID PRIMARY ALPHA MARKET CHANGE?             
         BE    EDT110              NO, EXIT PROCESSING.                         
         BAS   RE,BLDANKEY         YES, PREPARE TO DELETE THE OLD               
*                                   'L'-RECORD OF MARKET.                       
         OC    PRIMARK,PRIMARK     CHECK IF PRIMARK IS NULLS.  IF YES,          
         BZ    EDT105C              THEN THERE IS NOTHING TO DELETE.            
         USING ANMRECD,R1                                                       
         MVC   ANMKAMRK,PRIMARK    PUT OLD ALPHA MKT INTO KEY.                  
         DROP  R1                                                               
*                                                                               
         MVC   MEDIA,SVEBCMED                                                   
         BAS   RE,EDLSTEP1         DEL 'L'-RECD W/. SVEBCMED MEDIA              
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   EDT105C              NOPE, DO NEXT THING                         
         CLI   SVEBCMED,C'T'       WORRY ABOUT MEDIAS 'C' & 'N'?                
         BNE   EDT105C              NOPE, DO NEXT THING                         
         MVI   MEDIA,C'C'          MEDIA 'C' FOR CANADIAN AGENCY,               
         BAS   RE,EDLSTEP1          DELETE 'L'-RECORD FOR IT                    
         MVI   MEDIA,C'N'          MEDIA 'N' FOR CANADIAN AGENCY,               
         BAS   RE,EDLSTEP1          DELETE 'L'-RECORD FOR IT                    
*                                                                               
         MVC   MYKEY+(ANMKMED-ANMKEYD)(1),SVEBCMED  ORIGINAL MEDIA.             
*                                                                               
EDT105C  OC    MKTALST,MKTALST     ANY ALPHA MARKETS?                           
         BZ    EDT110              NO, DON'T CREATE 'L'-RECORD.                 
         LA    R1,MYKEY            BUILD THE NEW 'L'-RECORD TO BE               
         USING ANMRECD,R1           CONSISTENT W/. UPDATED                      
         MVC   ANMKAMRK,MKTALST     MARKET-RECORD.                              
         DROP  R1                                                               
*                                                                               
         MVC   MEDIA,SVEBCMED      MEDIA = 'T' OR 'R'                           
         BAS   RE,EDLSTEP2         ADD/RSTR 'L'-RECD W/. SVEBCMED MEDIA         
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   EDT110               NOPE, DONE WITH 'L'-RECORDS                 
         CLI   SVEBCMED,C'T'       WORRY ABOUT MEDIAS 'C' & 'N'?                
         BNE   EDT110               NOPE, DONE WITH 'L'-RECORDS                 
         MVI   MEDIA,C'C'          MEDIA 'C' FOR CANADIAN AGENCY,               
         BAS   RE,EDLSTEP2          ADD/RSTR 'L'-RECORD FOR IT                  
         MVI   MEDIA,C'N'          MEDIA 'N' FOR CANADIAN AGENCY,               
         BAS   RE,EDLSTEP2          ADD/RSTR 'L'-RECORD FOR IT                  
*                                                                               
         MVC   MYKEY+(ANMKMED-ANMKEYD)(1),SVEBCMED  ORIGINAL MEDIA.             
         B     EDT110                                                           
*                                                                               
         SPACE 3                                                                
EDT107   NTR1                                                                   
         GOTO1 VDATAMGR,DMCB,COMMAND,=C'STATION',MYKEY,REC2                     
         XIT1                                                                   
*                                                                               
*---------------------------------------------------------------------*         
*                                                                               
         EJECT                                                                  
EDT110   BAS   RE,DSPMKT                                                        
*                                                                               
EDTX     ST    R8,AREC             RESET AREC TO                                
         MVC   KEY,SVKEY                                                        
         CLI   SVACT,C'A'          TEST ADD                                     
         BNE   WRITE                                                            
         MVC   REC(17),SVKEY                                                    
*                                                                               
***********************************************************************         
*ATTENTION:  THE BELOW MVC INSTRUCTION IS INSERTED FOR A FILE-FIX ON            
*            THE STATION FILES.  THEY ARE BEING CONVERTED FROM 17-BYTES         
* 10/22/92   KEY, FIXED-LENGTH RECORDS TO 15-BYTES KEY AND VARIABLE-            
*            LENGTH RECORDS.  THE LENGTH OF THE NEW RECORDS IS PLACED           
*            IN THE 16TH AND 17TH BYTES, AND THE LENGTH OF THE                  
*            M-TYPE RECORDS IS TO BE 144.                                       
*                                                                               
         MVC   REC+15(2),=H'144'                                                
*                                                                               
***********************************************************************         
*                                                                               
         MVC   COMMAND,=C'DMADD   '                                             
         GOTO1 STA                                                              
         CLC   KEY+2(4),=C'0000'   DON'T DO NETWORK FOR MKT 0000                
         BE    REQREC                                                           
         GOTO1 CNADDSTA                                                         
         B     REQREC                                                           
*                                                                               
WRITE    LA    R1,REC2                                                          
         ST    R1,AREC                                                          
         GOTO1 RDSTA               REREAD REC BEFORE WRITE                      
         TM    DMCB+8,X'50'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
***********************************************************************         
*ATTENTION:  THE BELOW MVC INSTRUCTION IS INSERTED FOR A FILE-FIX ON            
*            THE STATION FILES.  THEY ARE BEING CONVERTED FROM 17-BYTES         
* 10/22/92   KEY, FIXED-LENGTH RECORDS TO 15-BYTES KEY AND VARIABLE-            
*            LENGTH RECORDS.  THE LENGTH OF THE NEW RECORDS IS PLACED           
*            IN THE 16TH AND 17TH BYTES, AND THE LENGTH OF THE                  
*            M-TYPE RECORDS IS TO BE 144.                                       
*                                                                               
         MVC   REC+15(2),=H'144'                                                
*                                                                               
***********************************************************************         
*                                                                               
         ST    R8,AREC                                                          
         MVC   COMMAND,=C'DMWRT   '                                             
         GOTO1 STA                                                              
         CLC   KEY+2(4),=C'0000'   DON'T DO NETWORK FOR MKT 0000                
         BE    REQREC                                                           
         GOTO1 CNCHASTA                                                         
         B     REQREC                                                           
         EJECT                                                                  
REQREC   XC    REC(110),REC             GENERATE REQUEST RECORD                 
         MVI   REC+10,45                                                        
         MVI   REC+14,106                                                       
         MVI   REC+26,X'40'                                                     
         MVC   REC+27(79),REC+26                                                
         MVC   REC+26(2),=C'45'                                                 
         MVC   REC+28(2),AGYALPHA                                               
         MVC   REC+30(1),SVEBCMED                                               
         MVC   REC+31(3),=C'ALL'                                                
         MVI   REC+36,C'M'                                                      
         MVC   REC+40(4),SVKEY+2                                                
         MVC   REC+94(7),=C'CONTROL'                                            
         MVC   REC+93(1),SVACT                                                  
         GOTO1 VDATAMGR,DMCB,=C'DMADD   ',=C'REQUEST',REC,REC                   
         TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXXMOD                                                           
         EJECT                                                                  
********************************************************************            
*VALMKT- VALIDATE MKT LIST                                                      
********************************************************************            
         SPACE 1                                                                
VALMRKT  NTR1                                                                   
         XC    TEMP,TEMP           BUILD BASIC ELEMENT                          
         XC    MKTALST,MKTALST     CLEAR PREV VALUES                            
*                                  VALIDATE AN INPUT FIELD                      
VALM2    LA    R2,MARMKTSH                                                      
         GOTO1 VSCANNER,DMCB,(R2),(X'80',SCANTBL)                               
         CLI   4(R1),0                                                          
         BE    VALERR              NO INPUT                                     
         MVC   NLINES,4(R1)        SAVE NUMBER OF -INPUT FIELDS                 
         MVI   FNDX,1                                                           
         LA    R5,SCANTBL          R5=A(SCAN BLOCK ENTRY)                       
*                                                                               
VALM4    CLC   FNDX,NLINES                                                      
         BH    VALOK                                                            
         CLI   0(R5),0             L'FLD                                        
         BE    VALERR                                                           
         CLI   0(R5),3             L'FLD                                        
         BH    VALERR                                                           
*                                  VALIDATE MARKET NUMBERS                      
VALM8    TM    2(R5),X'40'         C'LHS (ALPHA)- MUST BE CHAR                  
         BO    *+12                                                             
         MVI   ERRCD,4             ALPHA DATA ONLY ALLOWED IN FIELD             
         B     VALERR                                                           
*                                  ENSURE VALID MARKET                          
         LA    R1,12(R5)           R1=PTS TO MKT CODE                           
         BAS   RE,FNDMKT           FND MKT ON FILE                              
         BNZ   *+12                                                             
         MVI   ERRCD,17            INVALID MARKET                               
         B     VALERR                                                           
*                                  ENSURE MARKET NOT PREVIOUSLY DEFINED         
VALM9A   LA    RE,MKTALST                                                       
         LA    RF,L'MKTALST(RE)                                                 
VALM10   OC    0(3,RE),0(RE)                                                    
         BZ    VALM12                                                           
         CLC   12(3,R5),0(RE)                                                   
         BNE   *+12                                                             
         MVI   ERRCD,245           ERROR DUPLICATE ENTRY                        
         B     VALERR                                                           
         LA    RE,3(RE)                                                         
         CR    RE,RF                                                            
         BL    VALM10                                                           
*        B     VALERR              TOO MANY MKTS                                
*                                                                               
VALM12   MVC   0(3,RE),12(R5)      ADD MARKET TO LIST                           
*                                  BUMP TO NEXT SCAN BLOCK ENTRY                
VALM14   ZIC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R5,32(R5)                                                        
         B     VALM4                                                            
*                                  ADD ELEMENT TO RECORD                        
VALERR   SR    R1,R1               ERROR OCCURED                                
         B     VALX                                                             
*                                                                               
VALOK    LA    R1,1                CLEAN EXIT                                   
*                                                                               
VALX     LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
********************************************************************            
*FNDMKT- LOOK UP MARKET ON FILE                                                 
********************************************************************            
         SPACE 1                                                                
FNDMKT   NTR1                                                                   
         XC    MYKEY,MYKEY                                                      
         LA    R3,MYKEY                                                         
         USING CTDMREC,R3                                                       
         MVI   CTDMKTYP,CTDMKTEQ                                                
         MVI   CTDMKTY2,CTDMKT2E                                                
         MVC   CTDMKMED,SVKEY+1                                                 
         MVC   CTDMKMKT,0(R1)                                                   
         LA    R4,1                FOUND- CC= NON-ZERO                          
*                                                                               
         MVI   CTDMKSRC,C'A'                                                    
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',MYKEY,REC2                
         CLC   MYKEY(22),REC2                                                   
         BE    FNDMKX                                                           
*                                                                               
FNDMK5   MVI   CTDMKSRC,C'N'       NEILSON?                                     
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',MYKEY,REC2                
         CLC   MYKEY(22),REC2                                                   
         BE    FNDMKX                                                           
*                                                                               
         SR    R4,R4               NOT FOUND                                    
*                                                                               
FNDMKX   DS    0H                                                               
         LTR   R4,R4                                                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
********************************************************************            
LFMERR   GOTO1 ERROR                                                            
         SPACE 4                                                                
********************************************************************            
*=================== PROCEDURES FOR 'L'-RECORD'S ==================*            
*                                                                               
*--------------------- BUILD 'L'-RECORD'S KEY ---------------------*            
BLDANKEY DS    0H                                                               
         MVI   MYKEY,C'0'                                                       
         MVC   MYKEY+1(L'MYKEY-1),MYKEY                                         
         LA    R1,MYKEY                                                         
         USING ANMRECD,R1                                                       
         LA    R8,SVKEY            R8 USED BY MKTRECD                           
         MVI   ANMKTYPE,C'L'       TYPE.                                        
         MVC   ANMKAGCY,MKTKAGY    AGENCY.                                      
         MVC   ANMKMED,MKTKMED     MEDIA.                                       
         MVC   ANMKNMRK,MKTKMKT    NUMERIC MARKET.                              
         DROP  R1,R8                                                            
         LA    R8,REC              RESTORE R8-->REC.                            
         BR    RE                  RETURN.                                      
         SPACE 4                                                                
*                                                                               
*----------------------------- MKCANADC ------------------------------*         
*                                                                               
MKCANADC DS    0H                                                               
*         MYKEY = KEY OF 'L'-RECORD.                                            
*         REC2  = 'L'-RECORD.                                                   
*                                                                               
         MVI   MYKEY+(ANMKMED-ANMKEYD),C'C'                                     
         MVI   REC2+(ANMKMED-ANMKEYD),C'C'                                      
*                                                                               
         BR    RE                                                               
         SPACE 4                                                                
*                                                                               
*----------------------------- MKCANADN ------------------------------*         
*                                                                               
MKCANADN DS    0H                                                               
*         MYKEY = KEY OF 'L'-RECORD.                                            
*         REC2  = 'L'-RECORD.                                                   
*                                                                               
         MVI   MYKEY+(ANMKMED-ANMKEYD),C'N'                                     
         MVI   REC2+(ANMKMED-ANMKEYD),C'N'                                      
*                                                                               
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*----------------------------- EDLSTEP1 ------------------------------*         
*                                                                               
EDLSTEP1 NTR1                                                                   
*         DELETES THE 'L'-RECORD W/. OLD ALPHA-MARKET                           
*         REC2 = 'L'-RECORD.                                                    
*                                                                               
         CLC   MEDIA,SVEBCMED      MEDIA 'T' OR 'R'?                            
         BE    EDL100                                                           
         CLI   MEDIA,C'C'          MEDIA 'C'? (CANADIAN)                        
         BNE   *+12                                                             
         BAS   RE,MKCANADC                                                      
         B     EDL100                                                           
         CLI   MEDIA,C'N'          MEDIA 'N'? (CANADIAN)                        
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,MKCANADN                                                      
*                                                                               
EDL100   GOTO1 VDATAMGR,DMCB,(X'08',=C'DMRDHI  '),=C'STATION ',MYKEY,  +        
               REC2                                                             
         CLC   MYKEY(ANMKEYLQ),REC2   CHECK IF RECORD EXISTS.                   
         BE    EDL110              RECORD EXISTS.                               
EDL105   MVI   ERRCD,NOFNDERR      RECORD DOESN'T EXIST => ERROR.               
         B     LFMERR                                                           
EDL110   OI    REC2+(ANMCNTL-ANMRECD),X'80'   MARKED FOR DELETION.              
         MVC   COMMAND,=C'DMWRT   '                                             
         BAS   RE,EDT107           WRITE BACK TO FILE                           
*                                                                               
XEDLSTP1 B     XIT                                                              
         EJECT                                                                  
*                                                                               
*----------------------------- EDLSTEP2 ------------------------------*         
*                                                                               
EDLSTEP2 NTR1                                                                   
*         ADDS/RESTORES THE 'L'-RECORD W/. PRIMARK                              
*         REC2 = 'L'-RECORD.                                                    
*                                                                               
         CLC   MEDIA,SVEBCMED      MEDIA 'T' OR 'R'?                            
         BE    EDL200                                                           
         CLI   MEDIA,C'C'          MEDIA 'C'? (CANADIAN)                        
         BNE   *+12                                                             
         BAS   RE,MKCANADC                                                      
         B     EDL200                                                           
         CLI   MEDIA,C'N'          MEDIA 'N'? (CANADIAN)                        
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,MKCANADN                                                      
*                                                                               
EDL200   GOTO1 VDATAMGR,DMCB,(X'08',=C'DMRDHI  '),=C'STATION ',MYKEY,  +        
               REC2                                                             
*                          CHECK IF THE RECORD WAS MARKED FOR DELETION.         
         CLC   MYKEY(ANMKEYLQ),REC2                                             
         BE    EDL210              YES, SO RESTORE IT.                          
         XCEF  REC2,2000           NO, SO BUILD NEW RECORD AND ADD IT.          
         MVC   REC2(ANMKEYLQ),MYKEY                                             
         MVC   REC2+(ANMRCLEN-ANMRECD)(2),=H'20'                                
         MVC   COMMAND,=C'DMADD   '                                             
         BAS   RE,EDT107                                                        
         B     XEDLSTP2                                                         
EDL210   NI    REC2+(ANMCNTL-ANMRECD),X'FF'-X'80'   RESTORE RECORD.             
         MVC   REC2+(ANMRCLEN-ANMRECD)(2),=H'20'                                
         MVC   COMMAND,=C'DMWRT   '                                             
         BAS   RE,EDT107                                                        
*                                                                               
XEDLSTP2 B     XIT                                                              
***********************************************************************         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPLFMWRK                                                       
         ORG   LFMTABH                                                          
*SPLFME4                                                                        
       ++INCLUDE SPLFME4D                                                       
       EJECT                                                                    
********************************************************************            
*CTGENFILE                                                                      
       ++INCLUDE CTGENFILE                                                      
********************************************************************            
         EJECT                                                                  
********************************************************************            
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
********************************************************************            
         EJECT                                                                  
********************************************************************            
       ++INCLUDE SPGENANMK                                                      
********************************************************************            
         EJECT                                                                  
********************************************************************            
*VARIABLES                                                                      
********************************************************************            
GENOLD   DSECT                                                                  
         ORG   DEMAREA                                                          
FNDX     DS    X                                                                
TEMP     DS    X                                                                
NLINES   DS    X                                                                
MEDIA    DS    CL1                                                              
PRIMARK  DS    CL3                 HOLDS PRIMARY ALPHA MARKET.                  
RELO     DS    A                                                                
MYKEY    DS    CL(L'KEY)                                                        
SCANTBL  DS    XL256                                                            
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'115SPLFM24   05/01/02'                                      
         END                                                                    
