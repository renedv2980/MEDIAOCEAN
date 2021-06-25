*          DATA SET DDRCVSTRIP AT LEVEL 002 AS OF 10/07/11                      
*&&      SET   DB=N,DB1=N                                                       
*PHASE RCVSTRPA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE CUREDIT                                                                
*INCLUDE SORTER                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DECODE                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE CASHVAL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE PRTREC                                                                 
         EJECT                                                                  
         TITLE 'STRIP AND SORT RECOVERY FILE'                                   
         PRINT NOGEN                                                            
STRIP    CSECT                                                                  
         NBASE WORKL,STRIP***,VREGSVE,RA,CLEAR=YES                              
         USING WORKD,RC            LOCAL W/S                                    
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         USING PLINED,P                                                         
         L     R8,VBOXAREA                                                      
         USING BOXD,R8                                                          
         ST    RD,SAVERD                                                        
*                                                                               
         BRAS  RE,INIT                                                          
         BNE   XBASE                                                            
         BRAS  RE,SORTONE          READ FILE. FORMAT IT AND PUT TO SORT         
         BRAS  RE,PRNTACCS         PRINT INPUT TOTALS                           
         LA    RF,PROCONE                                                       
         CLI   ADDS,C'Y'                                                        
         BNE   *+8                                                              
         LA    RF,PROC1ADD                                                      
         BASR  RE,RF                                                            
         BRAS  RE,PRNTFLTS         PRINT FILTERED TOTALS                        
*                                                                               
         BRAS  RE,SORTTWO          SECOND SORT                                  
         BRAS  RE,PRNTOUTS         PRINT FILTERED TOTALS                        
         BRAS  RE,PROCTWO          PROCESS SORTED RECORDS                       
         B     XBASE                                                            
*                                                                               
VREGSVE  DC    V(REGSAVE)                                                       
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         MVC   TITLE,CTITLE                                                     
         ZAP   LINE,P99                                                         
*                                                                               
         L     R3,ASVCARD          READ IN ALL CARDS                            
INIT02   GOTO1 VCARDS,DMCB,(R3),=C'RE00'                                        
         CLC   =C'/*',0(R3)        END OF CARDS?                                
         BE    *+12                YES                                          
         AHI   R3,80                                                            
         B     INIT02                                                           
*                                                                               
         L     R3,ASVCARD          NOW PROCESS CARDS INDIVIDUALLY               
INIT04   CLC   =C'/*',0(R3)                                                     
         BE    INITX                                                            
         MVC   P(80),0(R3)                                                      
         GOTO1 VPRINTER            PRINT PARAMETER CARD                         
         MVC   CARD,0(R3)                                                       
         BRAS  RE,CARDVAL          VALIDATE KEYWORD=VALUE                       
         BE    *+8                                                              
         BRAS  RE,ERROUT           OUTPUT ERROR MESSAGE                         
         AHI   R3,80                                                            
         B     INIT04                                                           
*                                                                               
INITX    MVI   FERN,0                                                           
         OC    ERRCNT,ERRCNT       ERRORS?                                      
         BNZ   EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* READ RECOVERY FILE FROM TAPE FILTERING OUT UNWANTED DATA            *         
***********************************************************************         
         SPACE 1                                                                
SORTONE  NTR1  ,                                                                
         XC    SEQNUM,SEQNUM                                                    
         MVC   TITLE+10(L'RKINP),RKINP                                          
         GOTO1 VSORTER,PLIST,SORTCARD,RECCARD,0                                 
*&&DB1                                                                          
         OPEN  (DEBUG,OUTPUT)                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         OPEN  (RCVTAPE,INPUT)                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SONE02   GET   RCVTAPE                                                          
         LR    R7,R1                                                            
         USING RECDS,R7                                                         
*                                                                               
         CLI   RSIN,X'FF'          IGNORE DELETED RECORDS                       
         BE    SONE02                                                           
         TM    RRECTY,X'80'        IGNORE POINTER COPIES/CHANGES                
         BO    SONE02                                                           
*                                                                               
         L     RF,AINPACCS         TABLE OF INPUT ACCUMULATORS                  
         USING INPACCSD,RF                                                      
SONE04   CLC   RFILTY,INPNUM       TEST MATCH ON FILE TYPE                      
         BE    SONE06                                                           
         AHI   RF,INPACCSL         BUMP TO NEXT ROW OF ACCUMULATORS             
         CLI   0(RF),0             TEST FOR END OF TABLE                        
         BNE   SONE04                                                           
*                                                                               
SONE06   XR    R1,R1                                                            
         IC    R1,RRECTY           UPDATE INPUT ACCUMULATORS                    
         BCTR  R1,0                                                             
         MHI   R1,L'INPTCPY                                                     
         LA    R1,INPTCPY(R1)                                                   
         AP    0(8,R1),PONE        INCREMENT INPUT ACCUMULATOR FOR TYPE         
         AP    INPTTOT,PONE        INCREMENT FILE TOTAL                         
         AP    INPTOTS,PONE        INCREMENT GRAND TOTAL                        
         DROP  RF                                                               
*                                                                               
SONE08   BRAS  RE,FILTER                                                        
         BNE   SONE02                                                           
*                                                                               
         L     RF,AINPACCS         TABLE OF INPUT ACCUMULATORS                  
         USING INPACCSD,RF                                                      
SONE10   CLC   RFILTY,INPNUM       TEST MATCH ON FILE TYPE                      
         BE    SONE12                                                           
         AHI   RF,INPACCSL         BUMP TO NEXT ROW OF ACCUMULATORS             
         CLI   0(RF),0             TEST FOR END OF TABLE                        
         BNE   SONE10                                                           
*                                                                               
SONE12   OC    INPKLEN,INPKLEN     UNLESS KEY LEN SET                           
         BZ    SONE02              YOU CAN'T COMPRESS THESE                     
*                                                                               
         XR    R1,R1                                                            
         IC    R1,RRECTY           UPDATE INPUT ACCUMULATORS                    
         BCTR  R1,0                                                             
         MHI   R1,L'INPTCPY                                                     
         LA    R1,FLTTCPY(R1)                                                   
         AP    0(8,R1),PONE        INCREMENT INPUT ACCUMULATOR FOR TYPE         
         AP    FLTTTOT,PONE        INCREMENT FILE TOTAL                         
         AP    FLTTOTS,PONE        INCREMENT GRAND TOTAL                        
*                                                                               
         XR    R1,R1               GET KEY LENGTH                               
         ICM   R1,3,INPKLEN                                                     
         BCTR  R1,0                                                             
         DROP  RF                                                               
*                                                                               
         L     R2,AIOSORT      *** BUILD SORT RECORD HERE                       
         USING SRTKEYD,R2                                                       
         XC    SRTKEYD(SRTKEYL),SRTKEYD                                         
         LH    R0,RECLN                                                         
         AHI   R0,SRTKEYL                                                       
         SLL   R0,16                                                            
         ST    R0,SRTLEN                                                        
*--->                                                                           
         CLI   RFILTY,X'21'        TEST SPTFILE                                 
         BNE   SONE13                                                           
         CLI   RCVFRST,X'10'       TEST BUYREC                                  
         BL    SONE13                                                           
         CLI   RCVFRST+11,0        FIX BUYKEY BUG!                              
         BNE   *+8                                                              
         MVI   RCVFRST+11,1                                                     
*--->                                                                           
SONE13   EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRTKEY(0),RCVFRST   MOVE IN RECORD KEY                           
         MVC   SRTFILE,RFILTY                                                   
*                                                                               
         CLI   ADDS,C'Y'           SQUASHING ADDS?                              
         BNE   SONE14              NO                                           
         CLI   SRTFILE+1,3                                                      
         BNE   SONE14                                                           
         MVI   SRTFILE+1,0         MAKE ADD SORT FIRST                          
*                                                                               
SONE14   L     R0,SEQNUM                                                        
         AHI   R0,1                                                             
         ST    R0,SEQNUM           SET UNIQUE SEQUENCE NUMBER                   
         STCM  R0,15,SRTSEQ                                                     
*                                                                               
         LA    R0,SRTREC                                                        
         LH    R1,0(R7)                                                         
         LR    RE,R7                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE RECORD                                  
*                                                                               
         GOTO1 VSORTER,PLIST,PUT,(R2)                                           
*&&DB1                                                                          
         PUT   DEBUG,(R2)                                                       
*&&                                                                             
         B     SONE02                                                           
*                                                                               
SONEEND  DS    0H                                                               
*&&DB1                                                                          
         CLOSE (DEBUG)                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         CLOSE (RCVTAPE)                                                        
         LTR   RF,RF                                                            
         BZ    EXITOK                                                           
         DC    H'0'                                                             
         DROP  R2,R7                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS RECORDS - AFTER FIRST SORT                                  *         
***********************************************************************         
         SPACE 1                                                                
PROCONE  NTR1  ,                                                                
         OPEN  (TEMPOUT,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*&&DB                                                                           
         OPEN  (DEBUG,OUTPUT)                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
*                                                                               
         MVI   INKEY,C'N'          CHANGE OF KEY FLAG                           
         L     R2,AIOSORT          R2 = NEW RECORD FROM SORT                    
SRT      USING SRTKEYD,R2                                                       
         L     R3,AIOTEMP          R3 = OLD RECORD                              
SVE      USING SRTKEYD,R3                                                       
*                                                                               
PONE02   BRAS  RE,GETFRSRT         GET A RECORD FROM THE SORT                   
         BNE   PONE10              FINISHED WITH SORT                           
*&&DB                                                                           
         PUT   DEBUG,(R2)                                                       
*&&                                                                             
         OC    SVE.SRTFILE,SVE.SRTFILE                                          
         BZ    PONE04                                                           
         CLC   SRT.SRTFILE,SVE.SRTFILE                                          
         BNE   PONE08                                                           
*                                                                               
PONE04   CLI   INKEY,C'Y'          ALREADY HAVE A KEY?                          
         BE    PONE06              YES                                          
         MVI   INKEY,C'Y'                                                       
*                                                                               
         LR    R0,R3                                                            
         LHI   R1,IOL                                                           
         LA    RE,SRT.SRTKEYD      MOVE RECORD ACROSS                           
         LH    RF,SRT.SRTLEN                                                    
         MVCL  R0,RE                                                            
         B     PONE02                                                           
*                                                                               
PONE06   L     RF,AINPACCS         TABLE OF INPUT ACCUMULATORS                  
         USING INPACCSD,RF                                                      
PONE06A  CLC   SRT.SRTFILE(1),INPNUM     TEST MATCH ON FILE TYPE                
         BE    PONE06B                                                          
         AHI   RF,INPACCSL         BUMP TO NEXT ROW OF ACCUMULATORS             
         CLI   0(RF),0             TEST FOR END OF TABLE                        
         BNE   PONE06A                                                          
*                                                                               
PONE06B  XR    R1,R1               UNLESS KEY LEN SET                           
         ICM   R1,3,INPKLEN                                                     
         BCTR  R1,0                                                             
         DROP  RF                                                               
*                                                                               
PONE07   EX    R1,PONECLC          KEYS STILL MATCH?                            
         BNE   PONE08              NO                                           
         CLI   SRT.SRTFILE+1,1     COPY?                                        
         BE    PONE02              YES - KEEP FIRST COPY                        
*                                                                               
         LA    R0,SVE.SRTREC       MOVE LATER OVER EARLIER CHANGE               
         LHI   R1,IOL                                                           
         AHI   R1,-(SRTKEYL)                                                    
         LA    RE,SRT.SRTREC       MOVE RECORD ACROSS                           
         LH    RF,SRT.SRTLEN                                                    
         AHI   RF,-(SRTKEYL)                                                    
         MVCL  R0,RE                                                            
         LH    RF,SRT.SRTLEN                                                    
         STH   RF,SVE.SRTLEN                                                    
         B     PONE02                                                           
*                                                                               
PONECLC  CLC   SRT.SRTKEY(0),SVE.SRTKEY                                         
*                                                                               
PONE08   PUT   TEMPOUT,(R3)        PUT PRIOR RECORD TO TAPE                     
         MVI   INKEY,C'N'                                                       
         B     PONE04              AND PROCESS THIS RECORD                      
*                                                                               
PONE10   CLI   INKEY,C'Y'          PUT ANY PRIOR RECORD TO TAPE                 
         BNE   PONE12                                                           
         PUT   TEMPOUT,(R3)                                                     
*                                                                               
PONE12   GOTO1 VSORTER,PLIST,END   END SORT                                     
*&&DB                                                                           
         CLOSE DEBUG                                                            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         CLOSE TEMPOUT             CLOSE FILE                                   
         LTR   RF,RF                                                            
         BZ    EXITOK              FIRST PASS FINISHED                          
         DC    H'0'                                                             
         DROP  SVE,SRT                                                          
         EJECT                                                                  
***********************************************************************         
* PROCESS RECORDS - AFTER FIRST SORT - COMPRESS OUT ADDS              *         
***********************************************************************         
         SPACE 1                                                                
PROC1ADD NTR1  ,                                                                
         OPEN  (TEMPOUT,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*&&DB                                                                           
         OPEN  (DEBUG,OUTPUT)                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         MVI   INKEY,C'N'          CHANGE OF KEY FLAG                           
         MVI   INADD,C'N'          FIRST KEY IS ADD                             
*                                                                               
         L     R2,AIOSORT          R2 = NEW RECORD FROM SORT                    
SRT      USING SRTKEYD,R2                                                       
         L     R3,AIOTEMP          R3 = OLD RECORD                              
SVE      USING SRTKEYD,R3                                                       
*                                                                               
PRAD02   BRAS  RE,GETFRSRT         GET A RECORD FROM THE SORT                   
         BNE   PRAD20              FINISHED WITH SORT                           
*&&DB                                                                           
         PUT   DEBUG,(R2)                                                       
*&&                                                                             
PRAD04   CLI   INKEY,C'Y'          ALREADY HAVE A KEY?                          
         BE    PRAD08              YES                                          
         MVI   INKEY,C'Y'                                                       
*                                                                               
         CLI   SRT.SRTFILE+1,0     FIRST OF SET IS AN ADD?                      
         BNE   PRAD06              NO                                           
         MVI   INADD,C'Y'                                                       
         MVI   SRT.SRTFILE+1,3                                                  
         MVI   SRT.SRTREC+5,3                                                   
*                                                                               
PRAD06   LA    R0,SVE.SRTKEYD                                                   
         LHI   R1,IOL                                                           
         LA    RE,SRT.SRTKEYD      MOVE RECORD ACROSS                           
         LH    RF,SRT.SRTLEN                                                    
         MVCL  R0,RE                                                            
         B     PRAD02                                                           
*                                                                               
PRAD08   L     RF,AINPACCS         COUNT INPUT FROM SORT                        
         USING INPACCSD,RF                                                      
PRAD10   CLC   INPNUM,SRT.SRTFILE  TEST MATCH ON FILE TYPE                      
         BE    PRAD12                                                           
         AHI   RF,INPACCSL         BUMP TO NEXT ROW OF ACCUMULATORS             
         CLI   0(RF),0             TEST FOR END OF TABLE                        
         BNE   PRAD10                                                           
*                                                                               
PRAD12   XR    R1,R1               GET KEY LEN SET                              
         ICM   R1,3,INPKLEN                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BNE   PRAD18              KEY DIFFERENT                                
         CLC   SRT.SRTKEY(0),SVE.SRTKEY                                         
         DROP  RF                                                               
*                                                                               
         CLI   INADD,C'Y'          FIRST WAS ADD?                               
         BE    PRAD14              YES                                          
         CLC   SVE.SRTFILE,SRT.SRTFILE                                          
         BNE   PRAD18              FILE+TYPE DIFFERENT                          
         B     PRAD16                                                           
*                                                                               
PRAD14   CLC   SVE.SRTFILE(1),SRT.SRTFILE                                       
         BNE   PRAD18              FILE DIFFERENT ON ADD                        
*                                                                               
PRAD16   CLI   SVE.SRTFILE+1,1     COPY?                                        
         BE    PRAD02              YES - KEEP FIRST COPY                        
*                                                                               
         LA    R0,SVE.SRTREC       MOVE LATER OVER EARLIER CHANGE               
         LHI   R1,IOL                                                           
         AHI   R1,-(SRTKEYL)                                                    
         LA    RE,SRT.SRTREC       MOVE RECORD ACROSS                           
         LH    RF,SRT.SRTLEN                                                    
         AHI   RF,-(SRTKEYL)                                                    
         MVCL  R0,RE                                                            
         LH    RF,SRT.SRTLEN                                                    
         STH   RF,SVE.SRTLEN                                                    
*                                                                               
         CLI   INADD,C'Y'                                                       
         BNE   PRAD02                                                           
         MVI   SVE.SRTFILE+1,3                                                  
         MVI   SVE.SRTREC+5,3                                                   
         B     PRAD02                                                           
*                                                                               
PRAD18   PUT   TEMPOUT,(R3)        PUT PRIOR RECORD TO TAPE                     
         MVI   INKEY,C'N'                                                       
         MVI   INADD,C'N'                                                       
         B     PRAD04              AND PROCESS THIS RECORD                      
*                                                                               
PRAD20   CLI   INKEY,C'Y'          PUT ANY PRIOR RECORD TO TAPE                 
         BNE   PRAD22                                                           
         PUT   TEMPOUT,(R3)                                                     
*                                                                               
PRAD22   GOTO1 VSORTER,PLIST,END   END SORT                                     
*&&DB                                                                           
         CLOSE DEBUG                                                            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         CLOSE TEMPOUT             CLOSE FILE                                   
         LTR   RF,RF                                                            
         BZ    EXITOK              FIRST PASS FINISHED                          
         DC    H'0'                                                             
         DROP  SVE,SRT                                                          
         EJECT                                                                  
***********************************************************************         
* READ PROCESSED FILE FROM TEMPOUT AND RE-SORT IT                     *         
***********************************************************************         
         SPACE 1                                                                
SORTTWO  NTR1  ,                                                                
         MVC   TITLE+10(L'RKOUT),RKOUT                                          
         GOTO1 VSORTER,PLIST,SORTCARD,RECCARD,0                                 
*                                                                               
         L     R2,AIOSORT           STRIPPED AND SORTED TEMP FILE               
         USING SRTKEYD,R2                                                       
         OPEN  (TEMPOUT,INPUT)                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
STWO04   GET   TEMPOUT,(R2)                                                     
         LA    R7,SRTREC                                                        
         USING RECDS,R7                                                         
*                                                                               
         L     RF,AINPACCS         TABLE OF INPUT ACCUMULATORS                  
         USING INPACCSD,RF                                                      
STWO06   CLC   RFILTY,INPNUM       TEST MATCH ON FILE TYPE                      
         BE    STWO08                                                           
         AHI   RF,INPACCSL         BUMP TO NEXT ROW OF ACCUMULATORS             
         CLI   0(RF),0             TEST FOR END OF TABLE                        
         BNE   STWO06                                                           
*                                                                               
STWO08   XR    R1,R1                                                            
         IC    R1,RRECTY           UPDATE INPUT ACCUMULATORS                    
         BCTR  R1,0                                                             
         MHI   R1,L'INPTCPY                                                     
         LA    R1,OUTTCPY(R1)                                                   
         AP    0(8,R1),PONE        INCREMENT INPUT ACCUMULATOR FOR TYPE         
         AP    OUTTTOT,PONE        INCREMENT FILE TOTAL                         
         AP    OUTTOTS,PONE        INCREMENT GRAND TOTAL                        
         DROP  RF                                                               
*                                                                               
         GOTO1 VSORTER,PLIST,PUT,(R2)                                           
         B     STWO04                                                           
*                                                                               
STWOEND  CLOSE (TEMPOUT)                                                        
         LTR   RF,RF                                                            
         BZ    EXITOK                                                           
         DC    H'0'                                                             
         DROP  R2,R7                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD FINAL OUTPUT FILE - AFTER SECOND SORT                         *         
***********************************************************************         
         SPACE 1                                                                
PROCTWO  NTR1  ,                                                                
         OPEN  (RCVOUT,OUTPUT)                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOSORT          R2 = NEW RECORD FROM SORT                    
         USING SRTKEYD,R2                                                       
PTWO02   BRAS  RE,GETFRSRT         GET A RECORD FROM THE SORT                   
         BNE   PTWO04              FINISHED WITH SORT                           
         LA    R3,SRTREC                                                        
         PUT   RCVOUT,(R3)         PUT THIS RECORD TO TAPE                      
         B     PTWO02              AND PROCESS                                  
*                                                                               
PTWO04   CLOSE RCVOUT                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VSORTER,PLIST,END   END SORT                                     
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET A RECORD FROM THE SORT INTO AIOSORT                             *         
***********************************************************************         
         SPACE 1                                                                
GETFRSRT NTR1  ,                                                                
         GOTO1 VSORTER,PLIST,GET,0                                              
         ICM   RE,15,PLIST+4       END OF FILE?                                 
         BZ    EXITL                                                            
*                                                                               
         L     R0,AIOSORT          MOVE IT INTO SORT I/O AREA                   
         LHI   R1,IOL                                                           
         LH    RF,0(RE)                                                         
         MVCL  R0,RE                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE INPUT CARDS                                     *         
***********************************************************************         
         SPACE 1                                                                
CARDVAL  NTR1  ,                                                                
         LA    R2,CARD             R2=A(CARD START)                             
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITOK                                                           
*                                                                               
         GOTO1 VSCANNER,DMCB,(C'C',(R2)),(10,SCANBLK),0                         
         CLI   4(R1),0                                                          
         BNE   *+12                INVALID LINE                                 
         MVI   FERN,01                                                          
         B     EXITL                                                            
*                                                                               
         LA    R2,SCANBLK                                                       
         USING SCANBLKD,R2                                                      
         L     R3,ACARDTAB                                                      
         USING CARDTABD,R3                                                      
         LHI   R1,CARDTABL                                                      
         XR    RF,RF                                                            
*                                                                               
CARDV02  CLI   CNAME,CARDEOT       EOT                                          
         BNE   *+12                                                             
         MVI   FERN,2              INVALID KEYWORD                              
         B     EXITL                                                            
*                                                                               
         IC    RF,CXLEN                                                         
         EX    RF,CARDVCLC         MATCH KEYWORD IN TABLE                       
         BE    CARDV04                                                          
         BXH   R3,R1,CARDV02                                                    
*                                                                               
CARDVCLC CLC   SC1STFLD(0),CNAME   COMPARE KEYWORD                              
*                                                                               
CARDV04  CLI   CTYPE,CTNUM     *** NUMERIC INPUT?                               
         BNE   CARDV06             NO                                           
         CLI   SC2NDLEN,0          ENSURE SECOND VALUE                          
         BNE   *+12                                                             
         MVI   FERN,3              MISSING VALUE                                
         B     EXITL                                                            
         TM    SC2NDVAL,SCNUMQ     ENSURE VALUE IS NUMERIC                      
         BO    *+12                                                             
         MVI   FERN,4              NOT A NUMBER                                 
         B     EXITL                                                            
         CLC   SC2NDNUM,CMIN       SCOPE FOR MAX/MIN VALUES                     
         BNL   *+12                                                             
         MVI   FERN,5              TOO SMALL                                    
         B     EXITL                                                            
         CLC   SC2NDNUM,CMAX                                                    
         BNH   *+12                                                             
         MVI   FERN,6              TOO BIG                                      
         B     EXITL                                                            
*                                                                               
         ICM   RF,15,COUT          GET A(OUTPUT AREA)                           
         MVC   0(4,RF),SC2NDNUM    SET VALUE INTO OUTPUT AREA                   
         B     EXITOK                                                           
*                                                                               
CARDV06  CLI   CTYPE,CTCHR     *** CHARACTER INPUT                              
         BNE   CARDV08             NO                                           
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN       ENSURE SECOND VALUE                          
         BNZ   *+12                                                             
         MVI   FERN,3              MISSING VALUE                                
         B     EXITL                                                            
         C     RF,CMIN             SCOPE FOR LENGTH OF TEXT INPUT               
         BNL   *+12                                                             
         MVI   FERN,7              TOO SHORT                                    
         B     EXITL                                                            
         C     RF,CMAX                                                          
         BNH   *+12                                                             
         MVI   FERN,8              TOO LONG                                     
         B     EXITL                                                            
*                                                                               
         ICM   RE,15,COUT          MOVE IN FIELD                                
         IC    RF,CLEN                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SC2NDFLD                                                 
         B     EXITOK                                                           
*                                                                               
CARDV08  CLI   CTYPE,CTHEX     *** HEX INPUT                                    
         BNE   CARDV10             NO                                           
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN       ENSURE SECOND VALUE                          
         BNZ   *+12                                                             
         MVI   FERN,3              MISSING VALUE                                
         B     EXITL                                                            
         TM    SC2NDVAL,SCHEXQ     ENSURE DATA IS VALID HEX                     
         BO    *+12                                                             
         MVI   FERN,9              INVALID HEX DATA                             
         B     EXITL                                                            
         C     RF,CMIN             SCOPE FOR LENGTH OF HEX INPUT                
         BNL   *+12                                                             
         MVI   FERN,10             TOO SHORT                                    
         B     EXITL                                                            
         C     RF,CMAX                                                          
         BNH   *+12                                                             
         MVI   FERN,11             TOO LONG                                     
         B     EXITL                                                            
*                                                                               
         MVI   WORK,C'0'           ZERO FILL TEMP AREA                          
         MVC   WORK+1(L'WORK-1),WORK                                            
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,CLEN                                                        
         SLL   RF,1                LENGTH OF OUTPUT AREA AS EBCDIC              
         LR    R1,RF                                                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,SC2NDLEN                                                    
         SR    R1,RE               R1=DISPLACEMENT INTO WORK FOR MOVE           
         LA    R1,WORK(R1)                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SC2NDFLD    RIGHT ALIGN INPUT DATA                       
*                                                                               
         ICM   R0,15,COUT                                                       
         GOTO1 VHEXIN,DMCB,WORK,(R0),(RF)                                       
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   EXITOK                                                           
         DC    H'0'                                                             
*                                                                               
CARDV10  CLI   CTYPE,CTRTN     *** SELF VALIDATING ROUTINE                      
         BNE   CARDV12             NO                                           
         ICM   RF,15,COUT                                                       
         BASR  RE,RF                                                            
         B     EXIT                                                             
*                                                                               
CARDV12  DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT ERROR MESSAGE (FROM FERN) TO PRINT LINE           *         
***********************************************************************         
         SPACE 1                                                                
ERROUT   NTR1  ,                                                                
         MVC   P(15),=CL15'!! ***ERROR*** '                                     
         XR    R1,R1                                                            
         IC    R1,FERN                                                          
         BCTR  R1,0                                                             
         MHI   R1,50                                                            
         A     R1,AERRTAB                                                       
         MVC   P+15(50),0(R1)                                                   
         GOTO1 VPRINTER                                                         
         LH    R0,ERRCNT                                                        
         AHI   R0,1                                                             
         STH   R0,ERRCNT                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY FILTER                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING SCANBLKD,R2                                                      
VALKEY   NTR1  ,                                                                
         TM    FLAG,FLAGKYL+FLAGKY                                              
         BZ    VKEY02                                                           
         MVI   FERN,20             PRIOR MATCH KEY                              
         TM    FLAG,FLAGKY                                                      
         BO    *+8                                                              
         MVI   FERN,19             PRIOR START KEY                              
         B     EXITL                                                            
*                                                                               
VKEY02   OI    FLAG,FLAGKY         FOUND KEY CARD FLAG                          
         GOTO1 VDECODE,DMCB,SC2NDFLD,FKEY                                       
         MVC   KLEN,DMCB+10        KEY LENGTH                                   
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE LOWKEY FILTER                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING SCANBLKD,R2                                                      
VALKYL   NTR1  ,                                                                
         TM    FLAG,FLAGKYL+FLAGKY                                              
         BZ    VKYL02                                                           
         MVI   FERN,20             PRIOR MATCH KEY                              
         TM    FLAG,FLAGKY                                                      
         BO    *+8                                                              
         MVI   FERN,19             PRIOR START KEY                              
         B     EXITL                                                            
*                                                                               
VKYL02   OI    FLAG,FLAGKYL        FOUND LOWKEY CARD FLAG                       
         GOTO1 VDECODE,DMCB,SC2NDFLD,FLKEY                                      
         MVC   LKLEN,DMCB+10       KEY LENGTH                                   
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE HIGHKEY FILTER                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING SCANBLKD,R2                                                      
VALKYH   NTR1  ,                                                                
         TM    FLAG,FLAGKY                                                      
         BZ    VKYH02                                                           
         MVI   FERN,20             PRIOR MATCH KEY                              
         B     EXITL                                                            
*                                                                               
VKYH02   OI    FLAG,FLAGKYH        FOUND LOWKEY CARD FLAG                       
         GOTO1 VDECODE,DMCB,SC2NDFLD,FHKEY                                      
         MVC   HKLEN,DMCB+10       KEY LENGTH                                   
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE OUTPUT TYPE                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING SCANBLKD,R2                                                      
VALOP    NTR1  ,                                                                
         TM    FLAG,FLAGOP         ANOTHER OUTPUT= CARD INPUT?                  
         BZ    *+12                NO                                           
         MVI   FERN,12                                                          
         B     EXITL                                                            
*                                                                               
         OI    FLAG,FLAGOP         SET OUTPUT CARD FOUND                        
*                                                                               
         LA    R3,VALOPTAB         MATCH TEXT INPUT                             
         LHI   RF,L'VALOPTAB                                                    
         XR    R1,R1                                                            
         ICM   R1,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,3              NO SECOND INPUT                              
         B     EXITL                                                            
*                                                                               
VALOP02  CLI   0(R3),EOT           EOT?                                         
         BNE   *+12                                                             
         MVI   FERN,13             BAD VALUE                                    
         B     EXITL                                                            
         EX    R1,VOPCLC           MATCH VALUE                                  
         BE    VALOP04                                                          
         BXH   R3,RF,VALOP02                                                    
*                                                                               
VOPCLC   CLC   SC2NDFLD(0),0(R3)                                                
*                                                                               
VALOP04  OC    FLAG,7(R3)          SET OUTPUT TYPES                             
         B     EXITOK                                                           
*                                                                               
VALOPTAB DS    0XL8                                                             
         DC    CL7'PRINT  ',AL1(FLAGPR)                                         
         DC    CL7'DISK   ',AL1(FLAGDS)                                         
         DC    CL7'BOTH   ',AL1(FLAGPR+FLAGDS)                                  
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE RECORD TYPE                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING SCANBLKD,R2                                                      
VALRTY   NTR1  ,                                                                
         LA    R3,VALRTTAB         MATCH TEXT INPUT                             
         LHI   RF,L'VALRTTAB                                                    
         XR    R1,R1                                                            
         ICM   R1,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,3              NO SECOND INPUT                              
         B     EXITL                                                            
*                                                                               
VALRT02  CLI   0(R3),EOT           EOT?                                         
         BNE   *+12                                                             
         MVI   FERN,14             BAD VALUE                                    
         B     EXITL                                                            
         EX    R1,VRTCLC           MATCH VALUE                                  
         BE    VALRT04                                                          
         BXH   R3,RF,VALOP02                                                    
*                                                                               
VRTCLC   CLC   SC2NDFLD(0),0(R3)                                                
*                                                                               
VALRT04  MVC   RECTYPE,7(R3)       SET RECORD TYPE                              
         B     EXITOK                                                           
*                                                                               
VALRTTAB DS    0XL8                                                             
         DC    CL7'COPY   ',AL1(1)                                              
         DC    CL7'CHANGE ',AL1(2)                                              
         DC    CL7'ADD    ',AL1(3)                                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE FILE LIST                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALFILE  NTR1  ,                                                                
         MVI   CARD+5,C','         SET FILES= TO FILES  THEN RESCAN             
         GOTO1 VSCANNER,DMCB,(C'C',CARD),SCANBLK                                
         XR    R0,R0                                                            
         ICM   R0,1,DMCB+4                                                      
         CHI   R0,2                                                             
         BNL   *+12                                                             
         MVI   FERN,3                                                           
         B     EXITL                                                            
*                                                                               
         LA    R2,SCANBLK                                                       
         AHI   R2,SCANROW          GO PAST FILES= CARD                          
         AHI   R0,-1                                                            
         USING SCANBLKD,R2                                                      
         LA    R3,FILETBL                                                       
*                                                                               
VFIL02   CLI   SC1STLEN,2          MUST BE LENGTH 2                             
         BNE   VFIL04                                                           
         CLI   SC2NDLEN,0          NO SECOND INPUT                              
         BNE   VFIL04                                                           
         TM    SC1STVAL,SCHEXQ     AND HEX                                      
         BZ    VFIL04                                                           
         GOTO1 VHEXIN,DMCB,SC1STFLD,0(R3),2                                     
         B     VFIL10                                                           
*                                                                               
VFIL04   CLI   SC1STLEN,7          TRY TO MATCH NAME                            
         BH    VFIL12                                                           
         L     R5,AFILTAB          R5 = A(ENTRY)                                
         USING FILTABD,R5                                                       
         XR    RF,RF                                                            
         IC    RF,SC1STLEN                                                      
         BCTR  RF,0                                                             
*                                                                               
VFIL06   EX    RF,VFILCLC          MATCH FILENAME                               
         BE    VFIL08                                                           
         CLI   DMFLNUM,X'FF'       REACHED END OF TABLE?                        
         BE    *+12                YES                                          
         AHI   R5,DMFLLEN                                                       
         B     VFIL06                                                           
*                                                                               
         LH    R0,ERRCNT           INCREMENT ERROR COUNT                        
         AHI   R0,1                                                             
         STH   R0,ERRCNT                                                        
         MVC   P(15),=CL15'!! ***ERROR*** '                                     
         MVC   P+15(27),=CL27'Unable to find this file - '                      
         MVC   P+42(07),SC1STFLD                                                
         GOTO1 VPRINTER                                                         
         B     VFIL10                                                           
*                                                                               
VFILCLC  CLC   SC1STFLD(0),DMFLNAME                                             
*                                                                               
VFIL08   MVC   0(1,R3),DMFLNUM     SAVE FILE NUMBER                             
         B     VFIL10                                                           
         DROP  R5                                                               
*                                                                               
VFIL10   AHI   R2,SCANROW                                                       
         AHI   R3,1                                                             
         BCT   R0,VFIL02                                                        
         MVI   0(R3),FF                                                         
         OI    FLAG,FLAGFL                                                      
         B     EXITOK                                                           
*                                                                               
VFIL12   MVI   FERN,21                                                          
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE TERMINAL NUMBER                                            *         
***********************************************************************         
         SPACE 1                                                                
VALTRM   NTR1  ,                                                                
         MVI   FERN,18             NO LONGER SUPPORTED                          
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PROCESS A START TIME                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING SCANBLKD,R2                                                      
VALSTIM  NTR1  ,                                                                
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,3                                                           
         B     EXITL                                                            
*                                                                               
         MVC   WORK,SPACES                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SC2NDFLD                                                 
*                                                                               
         BRAS  RE,SETTIME                                                       
         BNE   EXITL                                                            
         MVC   LTIME,FULL                                                       
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PROCESS AN END TIME                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING SCANBLKD,R2                                                      
VALETIM  NTR1  ,                                                                
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BNZ   *+12                                                             
         MVI   FERN,3                                                           
         B     EXITL                                                            
*                                                                               
         MVC   WORK,SPACES                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),SC2NDFLD                                                 
*                                                                               
         BRAS  RE,SETTIME                                                       
         BNE   EXITL                                                            
         MVC   HTIME,FULL                                                       
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS A TIME VALUE - 00:00:00 TO 23:59:59 VALID                   *         
* NTRY: WORK   = HH:MM:SS EBCDIC SPACE FILLED (SS IS OPTIONAL)        *         
* EXIT: FULL   = 0HHMMSS0 BINARY                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING SCANBLKD,R2                                                      
SETTIME  NTR1  ,                                                                
         GOTO1 VSCANNER,DMCB,(C'C',WORK),SCANBLK,C',=:='                        
         XR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BNZ   *+12                                                             
         MVI   FERN,3                                                           
         B     EXITL                                                            
*                                                                               
         MVC   DUB1,ZEROS                                                       
         LA    R2,SCANBLK                                                       
         USING SCANBLKD,R2                                                      
         CLI   SC1STLEN,2          LENGTH OF 1 OR 2                             
         BH    STERR                                                            
         TM    SC1STVAL,SCNUMQ     AND VALID NUMBER                             
         BZ    STERR                                                            
         ICM   RF,15,SC1STNUM                                                   
         CHI   RF,23               00:00:00 TO 23:59:59 VALID                   
         BH    STERR1                                                           
         LA    RF,SC1STFLD                                                      
         CLI   SC1STLEN,1                                                       
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         MVI   0(RF),C'0'                                                       
         MVC   DUB1+00(2),0(RF)    SET HOURS VALUE                              
*                                                                               
         AHI   R0,-1               ANY MINUTES?                                 
         CHI   R0,0                                                             
         BE    STVAL02             NO                                           
*                                                                               
         AHI   R2,SCANROW                                                       
         CLI   SC1STLEN,2          LENGTH OF 1 OR 2                             
         BH    STERR                                                            
         TM    SC1STVAL,SCNUMQ     AND VALID NUMBER                             
         BZ    STERR                                                            
         ICM   RF,15,SC1STNUM                                                   
         CHI   RF,59               00 TO 59 VALID FOR MINUTE                    
         BH    STERR1                                                           
         LA    RF,SC1STFLD                                                      
         CLI   SC1STLEN,1                                                       
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         MVI   0(RF),C'0'                                                       
         MVC   DUB1+02(2),0(RF)    SET MINUTES VALUE                            
*                                                                               
         AHI   R0,-1               ANY SECONDS?                                 
         CHI   R0,0                                                             
         BE    STVAL02             NO                                           
*                                                                               
         AHI   R2,SCANROW                                                       
         CLI   SC1STLEN,2          LENGTH OF 1 OR 2                             
         BH    STERR                                                            
         TM    SC1STVAL,SCNUMQ     AND VALID NUMBER                             
         BZ    STERR                                                            
         ICM   RF,15,SC1STNUM                                                   
         CHI   RF,59               00 TO 59 VALID FOR SECONDS                   
         BH    STERR1                                                           
         LA    RF,SC1STFLD                                                      
         CLI   SC1STLEN,1                                                       
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         MVI   0(RF),C'0'                                                       
         MVC   DUB1+04(2),0(RF)    SET SECONDS VALUE                            
*                                                                               
STVAL02  PACK  DUB,DUB1(6)                                                      
         ICM   RF,15,DUB+4         RF=0HHMMSSC                                  
         N     RF,=X'0FFFFFF0'                                                  
         ST    RF,FULL             FULL=0HHMMSS0                                
         B     EXITOK                                                           
*                                                                               
STERR    MVI   FERN,16             FORMAT HH:MM:SS                              
         B     EXITL                                                            
*                                                                               
STERR1   MVI   FERN,17             ONLY VALUES 00:00:00 - 23:59:59              
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* FILTERING ROUTINE                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING RECDS,R7                                                         
FILTER   NTR1  ,                                                                
         LA    R1,FILETBL          FILTER ON FILE NUMBER                        
         LHI   RF,1                                                             
FILT02   CLI   0(R1),X'FF'         EOT                                          
         BE    EXITL               YES - FAIL                                   
         CLC   RFILTY,0(R1)        MATCH FILE NUMBER                            
         BE    *+8                 YES                                          
         BXH   R1,RF,FILT02                                                     
*                                                                               
         TM    FLAG,FLAGKY         EXACT KEY?                                   
         BZ    FILT04              NO                                           
*                                                                               
         LH    RF,KLEN             KEY LENGTH                                   
         BCTR  RF,0                                                             
         EX    RF,*+12                                                          
         BNE   EXITL                                                            
         B     FILT06                                                           
         CLC   RCVFRST(0),FKEY                                                  
*                                                                               
FILT04   TM    FLAG,FLAGKYL        LOW-HIGH KEY RANGE?                          
         BZ    FILT06              NO                                           
         LH    RF,LKLEN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BL    EXITL                                                            
         CLC   RCVFRST(0),FLKEY    MATCH ON LOW KEY                             
*                                                                               
         TM    FLAG,FLAGKYH        HIGH KEY INPUT?                              
         BZ    FILT06              NO                                           
         LH    RF,HKLEN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BH    EXITL                                                            
         CLC   RCVFRST(0),FHKEY    MATCH ON HIGH KEY                            
*                                                                               
FILT06   OC    FLUID,FLUID         LUID FILTER?                                 
         BZ    FILT08                                                           
         TM    RTIME,X'40'         HAS TRAILER IN RECOVERY RECORD?              
         BZ    FILT08              NO TRAILER                                   
*                                                                               
         LA    RE,RECLN            POINT TO RECORD LENGTH                       
         AH    RE,0(RE)            ADD RECORD LENGTH                            
         BCTR  RE,0                BACK UP TO THE LAST BYTE OF TRAILER          
         ZIC   RF,0(RE)            GET LENGTH OF TAILER                         
         BCTR  RF,0                                                             
         SR    RE,RF               BACK UP TO THE RECOVERY EXTENSION            
         USING RECVEXTD,RE                                                      
         CLC   FLUID,RLUID                                                      
         BNE   EXITL                                                            
         DROP  RE                                                               
*                                                                               
FILT08   OC    LTIME,LTIME         LOWTIME?                                     
         BZ    FILT10                                                           
         MVC   TTIME,RTIME                                                      
         NC    TTIME,=X'3FFFFFF0'  TURN OFF X'80' AND X'40' BITS                
         ICM   RE,15,TTIME                                                      
         TM    RTIME,X'80'                                                      
         BNO   *+12                                                             
         SRL   RE,8                                                             
         SLL   RE,4                RE=0HHMMSS0                                  
         C     RE,LTIME                                                         
         BL    EXITL                                                            
*                                                                               
FILT10   OC    HTIME,HTIME         HIGHTIME?                                    
         BZ    FILT12                                                           
         MVC   TTIME,RTIME                                                      
         NC    TTIME,=X'3FFFFFF0'  TURN OFF X'80' AND X'40' BITS                
         L     RE,TTIME                                                         
         TM    RTIME,X'80'                                                      
         BNO   *+12                                                             
         SRL   RE,8                                                             
         SLL   RE,4                RE=0HHMMSS0                                  
         C     RE,HTIME                                                         
         BH    EXITL                                                            
*                                                                               
FILT12   OC    DSKADR,DSKADR                                                    
         BZ    *+14                                                             
         CLC   RVCHR,DSKADR                                                     
         BNE   EXITL                                                            
*                                                                               
         OC    SYSINP,SYSINP                                                    
         BZ    *+14                                                             
         CLC   RSIN+1(3),SYSINP+1                                               
         BNE   EXITL                                                            
*                                                                               
         OC    PROGNUM(3),PROGNUM  ANY PROGRAM = FILTERS SET?                   
         BZ    FILT14              NO                                           
*                                                                               
         OC    PROGNUM,PROGNUM                                                  
         BZ    *+14                                                             
         CLC   RPRG,PROGNUM                                                     
         BE    FILT14                                                           
*                                                                               
         OC    PROGNUM2,PROGNUM2                                                
         BZ    *+14                                                             
         CLC   RPRG,PROGNUM2                                                    
         BE    FILT14                                                           
*                                                                               
         OC    PROGNUM3,PROGNUM3                                                
         BZ    *+14                                                             
         CLC   RPRG,PROGNUM3                                                    
         BE    FILT14                                                           
*                                                                               
         B     EXITL               SET BUT NO MATCH                             
*                                                                               
FILT14   OC    XPRGNUM1,XPRGNUM1                                                
         BZ    *+14                                                             
         CLC   RPRG,XPRGNUM1                                                    
         BE    EXITL                                                            
*                                                                               
         OC    XPRGNUM2,XPRGNUM2                                                
         BZ    *+14                                                             
         CLC   RPRG,XPRGNUM2                                                    
         BE    EXITL                                                            
*                                                                               
         OC    XPRGNUM3,XPRGNUM3                                                
         BZ    *+14                                                             
         CLC   RPRG,XPRGNUM3                                                    
         BE    EXITL                                                            
*                                                                               
         OC    USER,USER                                                        
         BZ    *+14                                                             
         CLC   RUSER,USER+2                                                     
         BNE   EXITL                                                            
*                                                                               
         CLI   RECTYPE,0                                                        
         BE    *+14                                                             
         CLC   RRECTY,RECTYPE                                                   
         BNE   EXITL                                                            
*                                                                               
         B     EXITOK                                                           
         DROP  R2,R7                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT OF RECOVERY RECORD INPUT TOTALS                        *         
***********************************************************************         
         SPACE 1                                                                
PRNTACCS NTR1  ,                                                                
PC       USING PLINED,BOXCOLS                                                   
         ZAP   PAGE,PONE                                                        
         ZAP   LINE,P99            ENSURE PAGE THROW                            
         GOTO1 VPRINTER            PRINT TITLES                                 
*                                                                               
         OI    BOXDDCTL,BOXDDLC                                                 
         MVI   BOXYORN,YES                                                      
         MVI   BOXOFF,NO                                                        
         MVI   BOXREQ,C'O'                                                      
*                                                                               
         MVC   BOXCOLS,SPACES      CLEAR DOWN COLUMNS                           
         MVI   PC.PILEFT,C'L'      SET INPUT COLUMNS                            
         MVI   PC.PIC1,C'C'                                                     
         MVI   PC.PIC2,C'C'                                                     
         MVI   PC.PIC3,C'C'                                                     
         MVI   PC.PIC4,C'C'                                                     
         MVI   PC.PIRIGHT,C'R'                                                  
         GOTO1 VPRINTER                                                         
         DROP  PC                                                               
*                                                                               
         MVC   PIFILE,IPIFILE   SET UP HEADERS                                  
         MVC   PICPY,IPICPY                                                     
         MVC   PICHG,IPICHG                                                     
         MVC   PIADD,IPIADD                                                     
         MVC   PITOT,IPITOT                                                     
         GOTO1 VPRINTER            DO HEADINGS                                  
         MVI   BOXREQ,C'B'                                                      
         GOTO1 VPRINTER            DRAW LINE UNDER HEADINGS                     
*                                                                               
         ZAP   CARD+00(8),PZERO    COPY ACCUMULATORS                            
         ZAP   CARD+08(8),PZERO    CHANGE ACCUMULATORS                          
         ZAP   CARD+16(8),PZERO    ADD ACCUMULATORS                             
         ZAP   CARD+24(8),PZERO    TOTALS ACCUMULATORS                          
*                                                                               
         L     R3,AINPACCS                                                      
         USING INPACCSD,R3                                                      
PACC02   CLI   INPNUM,255          EOT?                                         
         BE    PACC04                                                           
         AP    CARD+00(8),INPTCPY  ADD UP COPY ACCUMULATORS                     
         AP    CARD+08(8),INPTCHA  ADD UP CHANGE ACCUMULATORS                   
         AP    CARD+16(8),INPTADD  ADD UP ADD ACCUMULATORS                      
         AP    CARD+24(8),INPTTOT  ADD UP TOTALS ACCUMULATORS                   
*                                                                               
         MVC   PIFILE,INPNAME      FILE NAME                                    
         CURED INPTCPY,(8,PICPY),0,ZERO=NOBLANK                                 
         CURED INPTCHA,(8,PICHG),0,ZERO=NOBLANK                                 
         CURED INPTADD,(8,PIADD),0,ZERO=NOBLANK                                 
         CURED INPTTOT,(8,PITOT),0,ZERO=NOBLANK                                 
         GOTO1 VPRINTER            PRINT A ROW OF ACCUMULATORS                  
         AHI   R3,INPACCSL                                                      
         B     PACC02                                                           
*                                                                               
PACC04   MVC   PIFILE,IPIERRS                                                   
         CURED (P8,INPERRS),(8,PITOT),0,ZERO=NOBLANK                            
         GOTO1 VPRINTER                                                         
         MVI   BOXREQ,C'B'                                                      
         GOTO1 VPRINTER            DRAW LINE UNDER FILE COUNTS                  
*                                                                               
         MVC   PIFILE,IPITOTS      NOW OUTPUT TOTALS                            
         CURED (P8,CARD+00),(8,PICPY),0,ZERO=NOBLANK                            
         CURED (P8,CARD+08),(8,PICHG),0,ZERO=NOBLANK                            
         CURED (P8,CARD+16),(8,PIADD),0,ZERO=NOBLANK                            
         CURED (P8,CARD+24),(8,PITOT),0,ZERO=NOBLANK                            
         GOTO1 VPRINTER            PRINT TOTALS                                 
         MVI   BOXREQ,C'C'                                                      
         GOTO1 VPRINTER            CLOSE BOX                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT OF RECOVERY RECORD FILTER TOTALS                       *         
***********************************************************************         
         SPACE 1                                                                
PRNTFLTS NTR1  ,                                                                
PC       USING PLINED,BOXCOLS                                                   
         ZAP   PAGE,PONE                                                        
         ZAP   LINE,P99            ENSURE PAGE THROW                            
         GOTO1 VPRINTER            PRINT TITLES                                 
*                                                                               
         OI    BOXDDCTL,BOXDDLC                                                 
         MVI   BOXYORN,YES                                                      
         MVI   BOXOFF,NO                                                        
         MVI   BOXREQ,C'O'                                                      
*                                                                               
         MVC   BOXCOLS,SPACES      CLEAR DOWN COLUMNS                           
         MVI   PC.PILEFT,C'L'      SET INPUT COLUMNS                            
         MVI   PC.PIC1,C'C'                                                     
         MVI   PC.PIC2,C'C'                                                     
         MVI   PC.PIC3,C'C'                                                     
         MVI   PC.PIC4,C'C'                                                     
         MVI   PC.PIRIGHT,C'R'                                                  
         GOTO1 VPRINTER                                                         
         DROP  PC                                                               
*                                                                               
         MVC   PIFILE,IPIFILE   SET UP HEADERS                                  
         MVC   PICPY,IPICPY                                                     
         MVC   PICHG,IPICHG                                                     
         MVC   PIADD,IPIADD                                                     
         MVC   PITOT,IPITOT                                                     
         GOTO1 VPRINTER            DO HEADINGS                                  
         MVI   BOXREQ,C'B'                                                      
         GOTO1 VPRINTER            DRAW LINE UNDER HEADINGS                     
*                                                                               
         ZAP   CARD+00(8),PZERO    COPY ACCUMULATORS                            
         ZAP   CARD+08(8),PZERO    CHANGE ACCUMULATORS                          
         ZAP   CARD+16(8),PZERO    ADD ACCUMULATORS                             
         ZAP   CARD+24(8),PZERO    TOTALS ACCUMULATORS                          
*                                                                               
         L     R3,AINPACCS                                                      
         USING INPACCSD,R3                                                      
PFLT02   CLI   INPNUM,255          EOT?                                         
         BE    PFLT04                                                           
         AP    CARD+00(8),FLTTCPY  ADD UP COPY ACCUMULATORS                     
         AP    CARD+08(8),FLTTCHA  ADD UP CHANGE ACCUMULATORS                   
         AP    CARD+16(8),FLTTADD  ADD UP ADD ACCUMULATORS                      
         AP    CARD+24(8),FLTTTOT  ADD UP TOTALS ACCUMULATORS                   
*                                                                               
         MVC   PIFILE,INPNAME      FILE NAME                                    
         CURED FLTTCPY,(8,PICPY),0,ZERO=NOBLANK                                 
         CURED FLTTCHA,(8,PICHG),0,ZERO=NOBLANK                                 
         CURED FLTTADD,(8,PIADD),0,ZERO=NOBLANK                                 
         CURED FLTTTOT,(8,PITOT),0,ZERO=NOBLANK                                 
         GOTO1 VPRINTER            PRINT A ROW OF ACCUMULATORS                  
         AHI   R3,INPACCSL                                                      
         B     PFLT02                                                           
*                                                                               
PFLT04   MVC   PIFILE,IPIERRS                                                   
         CURED (P8,FLTERRS),(8,PITOT),0,ZERO=NOBLANK                            
         GOTO1 VPRINTER                                                         
         MVI   BOXREQ,C'B'                                                      
         GOTO1 VPRINTER            DRAW LINE UNDER FILE COUNTS                  
*                                                                               
         MVC   PIFILE,IPITOTS      NOW OUTPUT TOTALS                            
         CURED (P8,CARD+00),(8,PICPY),0,ZERO=NOBLANK                            
         CURED (P8,CARD+08),(8,PICHG),0,ZERO=NOBLANK                            
         CURED (P8,CARD+16),(8,PIADD),0,ZERO=NOBLANK                            
         CURED (P8,CARD+24),(8,PITOT),0,ZERO=NOBLANK                            
         GOTO1 VPRINTER            PRINT TOTALS                                 
         MVI   BOXREQ,C'C'                                                      
         GOTO1 VPRINTER            CLOSE BOX                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT OF RECOVERY RECORD FILTER TOTALS                       *         
***********************************************************************         
         SPACE 1                                                                
PRNTOUTS NTR1  ,                                                                
PC       USING PLINED,BOXCOLS                                                   
         ZAP   PAGE,PONE                                                        
         ZAP   LINE,P99            ENSURE PAGE THROW                            
         GOTO1 VPRINTER            PRINT TITLES                                 
*                                                                               
         OI    BOXDDCTL,BOXDDLC                                                 
         MVI   BOXYORN,YES                                                      
         MVI   BOXOFF,NO                                                        
         MVI   BOXREQ,C'O'                                                      
*                                                                               
         MVC   BOXCOLS,SPACES      CLEAR DOWN COLUMNS                           
         MVI   PC.PILEFT,C'L'      SET INPUT COLUMNS                            
         MVI   PC.PIC1,C'C'                                                     
         MVI   PC.PIC2,C'C'                                                     
         MVI   PC.PIC3,C'C'                                                     
         MVI   PC.PIC4,C'C'                                                     
         MVI   PC.PIRIGHT,C'R'                                                  
         GOTO1 VPRINTER                                                         
         DROP  PC                                                               
*                                                                               
         MVC   PIFILE,IPIFILE   SET UP HEADERS                                  
         MVC   PICPY,IPICPY                                                     
         MVC   PICHG,IPICHG                                                     
         MVC   PIADD,IPIADD                                                     
         MVC   PITOT,IPITOT                                                     
         GOTO1 VPRINTER            DO HEADINGS                                  
         MVI   BOXREQ,C'B'                                                      
         GOTO1 VPRINTER            DRAW LINE UNDER HEADINGS                     
*                                                                               
         ZAP   CARD+00(8),PZERO    COPY ACCUMULATORS                            
         ZAP   CARD+08(8),PZERO    CHANGE ACCUMULATORS                          
         ZAP   CARD+16(8),PZERO    ADD ACCUMULATORS                             
         ZAP   CARD+24(8),PZERO    TOTALS ACCUMULATORS                          
*                                                                               
         L     R3,AINPACCS                                                      
         USING INPACCSD,R3                                                      
POUT02   CLI   INPNUM,255          EOT?                                         
         BE    POUT04                                                           
         AP    CARD+00(8),OUTTCPY  ADD UP COPY ACCUMULATORS                     
         AP    CARD+08(8),OUTTCHA  ADD UP CHANGE ACCUMULATORS                   
         AP    CARD+16(8),OUTTADD  ADD UP ADD ACCUMULATORS                      
         AP    CARD+24(8),OUTTTOT  ADD UP TOTALS ACCUMULATORS                   
*                                                                               
         MVC   PIFILE,INPNAME      FILE NAME                                    
         CURED OUTTCPY,(8,PICPY),0,ZERO=NOBLANK                                 
         CURED OUTTCHA,(8,PICHG),0,ZERO=NOBLANK                                 
         CURED OUTTADD,(8,PIADD),0,ZERO=NOBLANK                                 
         CURED OUTTTOT,(8,PITOT),0,ZERO=NOBLANK                                 
         GOTO1 VPRINTER            PRINT A ROW OF ACCUMULATORS                  
         AHI   R3,INPACCSL                                                      
         B     POUT02                                                           
*                                                                               
POUT04   MVC   PIFILE,IPIERRS                                                   
         CURED (P8,FLTERRS),(8,PITOT),0,ZERO=NOBLANK                            
         GOTO1 VPRINTER                                                         
         MVI   BOXREQ,C'B'                                                      
         GOTO1 VPRINTER            DRAW LINE UNDER FILE COUNTS                  
*                                                                               
         MVC   PIFILE,IPITOTS      NOW OUTPUT TOTALS                            
         CURED (P8,CARD+00),(8,PICPY),0,ZERO=NOBLANK                            
         CURED (P8,CARD+08),(8,PICHG),0,ZERO=NOBLANK                            
         CURED (P8,CARD+16),(8,PIADD),0,ZERO=NOBLANK                            
         CURED (P8,CARD+24),(8,PITOT),0,ZERO=NOBLANK                            
         GOTO1 VPRINTER            PRINT TOTALS                                 
         MVI   BOXREQ,C'C'                                                      
         GOTO1 VPRINTER            CLOSE BOX                                    
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* COMMON ROUTINES                                                    *          
**********************************************************************          
         SPACE 1                                                                
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XBASE    L     RD,SAVERD                                                        
         XBASE                                                                  
         EJECT                                                                  
**********************************************************************          
* CONSTANTS/EQUATES AND LITERALS                                     *          
**********************************************************************          
         SPACE 1                                                                
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
IOL      EQU   8096                LENGTH OF AN IO AREA                         
FF       EQU   X'FF'                                                            
EOT      EQU   X'FF'                                                            
SCANROW  EQU   82                                                               
SCANLHS  EQU   10                                                               
SCANRHS  EQU   60                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
AINPACCS DC    A(INPACCS)                                                       
AIOSORT  DC    A(IO1)                                                           
AIOCOPY  DC    A(IO2)                                                           
AIOTEMP  DC    A(IO3)                                                           
ASVCARD  DC    A(SVCARD)                                                        
AERRTAB  DC    A(ERRTAB)                                                        
AFILTAB  DC    A(FILTAB)                                                        
AFACITAB DC    A(FACIDTAB)                                                      
ACARDTAB DC    A(CARDTAB)                                                       
*                                                                               
VCARDS   DC    V(CARDS)                                                         
VCASHVAL DC    V(CASHVAL)                                                       
VDATCON  DC    V(DATCON)                                                        
VDECODE  DC    V(DECODE)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRTREC  DC    V(PRTREC)                                                        
VPRNTBL  DC    V(PRNTBL)                                                        
VSCANNER DC    V(SCANNER)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VPRINTER DC    V(PRINTER)                                                       
VSORTER  DC    V(SORTER)                                                        
VBOXAREA DC    V(BOXAREA)                                                       
CUREDIT  DC    V(CUREDIT)                                                       
*                                                                               
PUT      DC    CL4'PUT '                                                        
GET      DC    CL4'GET '                                                        
END      DC    CL4'END '                                                        
*                                                                               
IPTITLE  DC    CL(L'TITLE)'Stripper - Recovery File Input Count'                
CTITLE   DC    CL(L'TITLE)'Stripper - Processing Input Parameters'              
IPIFILE  DC    CL(L'PIFILE)'Filename'                                           
IPIERRS  DC    CL(L'PIFILE)'Errors'                                             
IPITOTS  DC    CL(L'PIFILE)'Totals'                                             
IPICPY   DC    CL(L'PICPY)'  Copies'                                            
IPICHG   DC    CL(L'PICHG)' Changes'                                            
IPIADD   DC    CL(L'PIADD)'    Adds'                                            
IPITOT   DC    CL(L'PITOT)'   Total'                                            
*                                                                               
RKINP    DC    C'Recovery file input record dump'                               
RKOUT    DC    C'Recovery file output record dump'                              
*                                                                               
EFFS     DC    16X'FF'                                                          
QUERY    DC    16C'?'                                                           
ZEROS    DC    16C'0'                                                           
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
P99      DC    P'99'                                                            
*                                                                               
KLEN     DS    H                   KEY LENGTHS                                  
LKLEN    DS    H                                                                
HKLEN    DS    H                                                                
FKEY     DS    CL64                                                             
FLKEY    DS    CL64                                                             
FHKEY    DS    CL64                                                             
*                                                                               
FILETBL  DS    XL20                                                             
FLUID    DS    CL8                                                              
DSKADR   DS    F                                                                
SYSINP   DS    F                                                                
LTIME    DS    PL4                                                              
HTIME    DS    PL4                                                              
USER     DS    XL4                                                              
TTIME    DS    F                                                                
RECTYPE  DC    X'00'                                                            
PROGNUM  DS    XL1                                                              
PROGNUM2 DS    XL1                                                              
PROGNUM3 DS    XL1                                                              
XPRGNUM1 DS    XL1                                                              
XPRGNUM2 DS    XL1                                                              
XPRGNUM3 DS    XL1                                                              
DALEN    DS    XL1                                                              
SILEN    DS    XL1                                                              
EXTLEN   DS    H                                                                
ADDS     DC    C'N'                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(05,38,A),FORMAT=BI,WORK=1 '                    
SORTCAR2 DC    CL80'SORT FIELDS=(39,04,A),FORMAT=BI,WORK=1 '                    
*                                                                               
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=(8144,,,,) '                           
*                                                                               
RCVTAPE  DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GL),RECFM=VB,             +        
               BLKSIZE=0,LRECL=8200,BUFNO=2,EODAD=SONEEND                       
*                                                                               
TEMPOUT  DCB   DDNAME=TEMPOUT,DSORG=PS,RECFM=VB,LRECL=8200,            +        
               BLKSIZE=27648,MACRF=(GM,PM),EODAD=STWOEND                        
*                                                                               
DEBUG    DCB   DDNAME=DEBUG,DSORG=PS,RECFM=VB,LRECL=8200,              +        
               BLKSIZE=27648,MACRF=(PM)                                         
*                                                                               
RCVOUT   DCB   DDNAME=TAPEOUT,DSORG=PS,RECFM=VB,LRECL=8200,            +        
               BLKSIZE=27648,MACRF=(PM)                                         
         EJECT                                                                  
**********************************************************************          
* TABLE OF INPUT ACCUMULATORS                                        *          
**********************************************************************          
         SPACE 1                                                                
INPERRS  DC    PL8'0'              INPUT ERROR COUNT                            
FLTERRS  DC    PL8'0'              INPUT ERROR COUNT                            
OUTERRS  DC    PL8'0'              INPUT ERROR COUNT                            
INPTOTS  DC    PL8'0'              INPUT TOTALS COUNT                           
OUTTOTS  DC    PL8'0'              INPUT TOTALS COUNT                           
FLTTOTS  DC    PL8'0'              INPUT TOTALS COUNT                           
*                                                                               
INPACCS  DC    X'23',AL2(0013),CL7'SPTDIR ',12PL8'0'                            
         DC    X'21',AL2(0013),CL7'SPTFILE',12PL8'0'                            
         DC    X'36',AL2(0032),CL7'XSPDIR ',12PL8'0'                            
         DC    X'37',AL2(0032),CL7'XSPFILE',12PL8'0'                            
         DC    X'22',AL2(0015),CL7'STATION',12PL8'0'                            
         DC    X'25',AL2(0000),CL7'REQUEST',12PL8'0'                            
         DC    X'24',AL2(0000),CL7'RECOVER',12PL8'0'                            
         DC    X'27',AL2(0020),CL7'UNTDIR ',12PL8'0'                            
         DC    X'2A',AL2(0020),CL7'UNTFILE',12PL8'0'                            
         DC    X'00',AL2(0000),CL7'OTHER  ',12PL8'0'                            
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* NON-ADDRESSIBLE STORAGE AREAS                                      *          
**********************************************************************          
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'AIOSORT*'                                                    
IO1      DS    (IOL)X                                                           
         DC    CL8'AIOCOPY*'                                                    
IO2      DS    (IOL)X                                                           
         DC    CL8'AIOTEMP*'                                                    
IO3      DS    (IOL)X                                                           
*                                                                               
SVCARD   DC    50CL80' '           SAVED INPUT CARDS                            
         EJECT                                                                  
***********************************************************************         
* INPUT CARDS TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
CARDTAB  DC    CL8'OUTPUT  ',AL4(0,0)                                           
         DC    AL1(05,CTRTN,0,0),AL4(VALOP)                                     
         DC    CL8'KEY     ',AL4(0,0)                                           
         DC    AL1(02,CTRTN,0,0),AL4(VALKEY)                                    
         DC    CL8'LOWKEY  ',AL4(0,0)                                           
         DC    AL1(05,CTRTN,0,0),AL4(VALKYL)                                    
         DC    CL8'HIGHKEY ',AL4(0,0)                                           
         DC    AL1(06,CTRTN,0,0),AL4(VALKYH)                                    
         DC    CL8'RTYPE   ',AL4(0,0)                                           
         DC    AL1(04,CTRTN,0,0),AL4(VALRTY)                                    
         DC    CL8'FILES   ',AL4(0,0)                                           
         DC    AL1(04,CTRTN,0,0),AL4(VALFILE)                                   
         DC    CL8'TERMINAL',AL4(0,0)                                           
         DC    AL1(07,CTRTN,0,0),AL4(VALTRM)                                    
         DC    CL8'LOWTIME ',AL4(0,0)                                           
         DC    AL1(06,CTRTN,0,0),AL4(VALSTIM)                                   
         DC    CL8'HIGHTIME',AL4(0,0)                                           
         DC    AL1(07,CTRTN,0,0),AL4(VALETIM)                                   
         DC    CL8'LUID    ',F'008',F'008'                                      
         DC    AL1(03,CTCHR,L'FLUID,0),AL4(FLUID)                               
         DC    CL8'FIXADDS ',F'001',F'001'                                      
         DC    AL1(06,CTCHR,L'ADDS,0),AL4(ADDS)                                 
         DC    CL8'PROG2   ',F'001',F'002'                                      
         DC    AL1(04,CTHEX,L'PROGNUM2,0),AL4(PROGNUM2)                         
         DC    CL8'PROG3   ',F'001',F'002'                                      
         DC    AL1(04,CTHEX,L'PROGNUM3,0),AL4(PROGNUM3)                         
         DC    CL8'PROG    ',F'001',F'002'                                      
         DC    AL1(03,CTHEX,L'PROGNUM,0),AL4(PROGNUM)                           
         DC    CL8'XPRG2   ',F'001',F'002'                                      
         DC    AL1(04,CTHEX,L'XPRGNUM2,0),AL4(XPRGNUM2)                         
         DC    CL8'XPRG3   ',F'001',F'002'                                      
         DC    AL1(04,CTHEX,L'XPRGNUM3,0),AL4(XPRGNUM3)                         
         DC    CL8'XPRG    ',F'001',F'002'                                      
         DC    AL1(03,CTHEX,L'XPRGNUM1,0),AL4(XPRGNUM1)                         
         DC    CL8'PROGRAM ',F'001',F'002'                                      
         DC    AL1(03,CTHEX,L'PROGNUM,0),AL4(PROGNUM)                           
         DC    CL8'PROGRAM2',F'001',F'002'                                      
         DC    AL1(07,CTHEX,L'PROGNUM2,0),AL4(PROGNUM2)                         
         DC    CL8'PROGRAM3',F'001',F'002'                                      
         DC    AL1(07,CTHEX,L'PROGNUM3,0),AL4(PROGNUM3)                         
         DC    CL8'USER    ',F'001',F'65536'                                    
         DC    AL1(03,CTNUM,0,0),AL4(USER)                                      
         DC    CL8'USERID  ',F'001',F'65536'                                    
         DC    AL1(05,CTNUM,0,0),AL4(USER)                                      
         DC    CL8'HUSER   ',F'001',F'004'                                      
         DC    AL1(04,CTHEX,L'USER,0),AL4(USER)                                 
         DC    CL8'HUSERID ',F'001',F'004'                                      
         DC    AL1(06,CTHEX,L'USER,0),AL4(USER)                                 
         DC    CL8'SYSIN   ',F'001',F'008'                                      
         DC    AL1(06,CTHEX,L'SYSINP,0),AL4(SYSINP)                             
         DC    CL8'DISKADR ',F'004',F'008'                                      
         DC    AL1(06,CTHEX,L'DSKADR,0),AL4(DSKADR)                             
CARDTABX DC    AL1(CARDEOT)                                                     
         EJECT                                                                  
***********************************************************************         
* CARD TABLE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
CARDTABD DSECT                                                                  
CNAME    DS    CL8                 INPUT CARD                                   
CMIN     DS    F                   MINIMUM (VALUE OR LENGTH)                    
CMAX     DS    F                   MAXIMUM (VALUE OR LENGTH)                    
CXLEN    DS    AL1                 LEN-1 OF CNAME VALUE FOR COMPARE             
CTYPE    DS    AL1                 INPUT TYPE                                   
CTNUM    EQU   1                   NUMERIC                                      
CTCHR    EQU   2                   CHARACTER                                    
CTHEX    EQU   3                   HEX                                          
CTRTN    EQU   4                   VALIDATION ROUTINE                           
CLEN     DS    AL1                 OUTPUT AREA LENGTH (CHAR ONLY)               
         DS    AL1                 N/D                                          
COUT     DS    AL4                 A(OUTPUT AREA)                               
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
CARDEOT  EQU   X'FF'                                                            
*                                                                               
STRIP    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ERROR TABLE                                                         *         
***********************************************************************         
         SPACE 1                                                                
ERRTAB   DS    0D                                                               
ERR01    DC    CL50'Invalid line format                               '         
ERR02    DC    CL50'Invalid Keyword                                   '         
ERR03    DC    CL50'Missing value for Keyword=Value parameter         '         
ERR04    DC    CL50'Value not a valid number                          '         
ERR05    DC    CL50'Numeric value too small                           '         
ERR06    DC    CL50'Numeric value too large                           '         
ERR07    DC    CL50'Length of input string too short                  '         
ERR08    DC    CL50'Length of input string too long                   '         
ERR09    DC    CL50'Value not a valid hex string                      '         
ERR10    DC    CL50'Length of hex string too short                    '         
ERR11    DC    CL50'Length of hex string too long                     '         
ERR12    DC    CL50'This card is a duplicate - not sure what to do now'         
ERR13    DC    CL50'Bad Value - Only 1 of PRINT/DISK/BOTH is ok here  '         
ERR14    DC    CL50'Bad Value - Only 1 of COPY/CHANGE/ADD is ok here  '         
ERR15    DC    CL50'Bad Value - ALL or a 2 DP number only valid here  '         
ERR16    DC    CL50'Bad Format - needs to be HH:MM:SS - SS optional   '         
ERR17    DC    CL50'Bad Format - HH:MM:SS cannot be more than 23:59:59'         
ERR18    DC    CL50'No longer supported - Use LUID= instead           '         
ERR19    DC    CL50'Prior start key (LOWKEY=) makes this card invalid '         
ERR20    DC    CL50'Prior match key (KEY=) makes this card invalid    '         
ERR21    DC    CL50'Input must be =XX,XX,.. XX is File Name or Hex #'           
ERR22    DC    CL50'Required card OUTPUT=XX not found                 '         
ERR23    DC    CL50'Required card FILES=XX not found                  '         
ERR24    DC    CL50'You must specify a Key a Lowkey or another filter '         
         EJECT                                                                  
***********************************************************************         
* FACIDTAB                                                            *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE FACIDTAB                                                       
         EJECT                                                                  
***********************************************************************         
* DMFILTAB                                                            *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'DMFILTAB'                                                      
       ++INCLUDE DMFILTAB                                                       
         EJECT                                                                  
**********************************************************************          
* DSECTS                                                             *          
**********************************************************************          
         SPACE 1                                                                
WORKD    DSECT                     WORKING STORAGE                              
DUB      DS    D                                                                
DUB1     DS    D                                                                
PLIST    DS    XL24                                                             
DMCB     DS    XL24                                                             
*                                                                               
SAVERD   DS    A                                                                
SEQNUM   DS    F                                                                
*                                                                               
HALF     DS    H                                                                
BYTE     DS    X                                                                
INKEY    DS    X                                                                
INADD    DS    X                                                                
*                                                                               
FULL     DS    F                                                                
ERRCNT   DS    H                                                                
FERN     DS    X                                                                
FLAG     DS    XL1                                                              
FLAGPR   EQU   X'80'               OUTPUT TO PRINT                              
FLAGDS   EQU   X'40'               OUTPUT TO DISK                               
FLAGKY   EQU   X'20'               KEY= PARAMETER CARD FOUND                    
FLAGKYL  EQU   X'10'               LOWKEY= PARAMETER CARD FOUND                 
FLAGKYH  EQU   X'08'               HIGHKEY= PARAMETER CARD FOUND                
FLAGFL   EQU   X'04'               FILES= PARAMETER CARD FOUND                  
FLAGOP   EQU   X'02'               OUTPUT= PARAMETER CARD FOUND                 
*                                                                               
CARD     DS    CL80                                                             
SCANBLK  DS    CL820                                                            
WORK     DS    CL132                                                            
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
SRTKEYD  DSECT                                                                  
SRTLEN   DS    F                                                                
SRTKEY   DS    XL32                                                             
SRTFILE  DS    XL2                                                              
SRTSEQ   DS    XL4                                                              
SRTREC   DS    0X                                                               
SRTKEYL  EQU   *-SRTKEYD                                                        
*                                                                               
PLINED   DSECT                     PRINT LINE                                   
PLINE    DS    0CL132                                                           
PILEFT   DS    X                                                                
         DS    X                                                                
PIFILE   DS    CL8                                                              
         DS    X                                                                
PIC1     DS    X                                                                
         DS    X                                                                
PICPY    DS    CL8                                                              
         DS    X                                                                
PIC2     DS    X                                                                
         DS    X                                                                
PICHG    DS    CL8                                                              
         DS    X                                                                
PIC3     DS    X                                                                
         DS    X                                                                
PIADD    DS    CL8                                                              
         DS    X                                                                
PIC4     DS    X                                                                
         DS    X                                                                
PITOT    DS    CL8                                                              
         DS    X                                                                
PIRIGHT  DS    X                   INPUT LINE RIGHT                             
         EJECT                                                                  
INPACCSD DSECT                                                                  
INPNUM   DS    XL1                 DMFILES INTERNAL NUMBER FOR FILE             
INPKLEN  DS    XL2                 DMFILES INTERNAL NUMBER FOR FILE             
INPNAME  DS    CL7                 FILENAME                                     
INPTCPY  DS    PL8                                                              
INPTCHA  DS    PL8                 COUNTERS                                     
INPTADD  DS    PL8                                                              
INPTTOT  DS    PL8                                                              
*                                                                               
FLTTCPY  DS    PL8                                                              
FLTTCHA  DS    PL8                 COUNTERS                                     
FLTTADD  DS    PL8                                                              
FLTTTOT  DS    PL8                                                              
*                                                                               
OUTTCPY  DS    PL8                                                              
OUTTCHA  DS    PL8                 COUNTERS                                     
OUTTADD  DS    PL8                                                              
OUTTTOT  DS    PL8                                                              
INPACCSL EQU   *-INPACCSD                                                       
         EJECT                                                                  
***********************************************************************         
* DMRCVRHDR                                                           *         
***********************************************************************         
         SPACE 1                                                                
RECDS    DSECT                                                                  
RECLN    DS    CL4                                                              
       ++INCLUDE DMRCVRHDR                                                      
RCVFRST  DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* DMRCVREXT                                                                     
         PRINT OFF                                                              
RECVEXTD DSECT                                                                  
       ++INCLUDE DMRCVREXT                                                      
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
*FACIDTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
*DMFILTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMFILTABD                                                      
         PRINT ON                                                               
*                                                                               
         TITLE 'VARIABLE SCAN MODULE - REPLACES SCANNER'                        
***********************************************************************         
* SCANNER REPLACEMENT - ROW LENGTH IS 82 RHS IS 60                    *         
***********************************************************************         
         SPACE 1                                                                
SCANNER  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 SWORKL,**SCAN**                                                  
         USING SWORKD,RC                                                        
         LR    R9,R1               R9=A(PARAMETER LIST)                         
         LM    R2,R3,0(R9)         R2=A(DATA STRING) R3=A(BLOCK)                
         MVC   MAXLINES,4(R9)                                                   
         XC    SDISP,SDISP                                                      
         SR    R4,R4                                                            
         IC    R4,5(R2)            L'DATA IF SCREEN FIELD                       
         LA    R2,8(R2)                                                         
         MVC   LROW,=AL2(SCANROW)  PRESET DEFAULT LENGTHS                       
         MVC   LRIGHT,=AL2(SCANRHS)                                             
         MVC   LBOTH,=AL2(SCANLHS+SCANRHS)                                      
         MVC   COMMA(2),COMMAEQL                                                
         CLC   8(2,R9),COMMAEQL                                                 
         BNE   *+10                                                             
         MVC   COMMA(2),10(R1)                                                  
*                                                                               
SCAN1    SH    R2,=H'8'                                                         
         LA    R4,80                                                            
         CLC   0(80,R2),SSPACES                                                 
         BE    ERROR2                                                           
         LA    R5,79(R2)                                                        
         SPACE 2                                                                
SCAN2    CLI   0(R5),C' '                                                       
         BNE   SCAN4                                                            
         BCTR  R5,0                                                             
         BCT   R4,SCAN2                                                         
         SPACE 2                                                                
SCAN4    LA    R5,0(R2,R4)         L'DATA IN R4                                 
         MVC   BORROW,0(R5)        SAVE THE NEXT CHARACTER                      
         MVC   0(1,R5),COMMA       AND POP IN A COMMA TO SIMPLIFY               
         SR    R6,R6               R6=NUMBER OF LINES USED                      
         EJECT                                                                  
*HANDLE LINES OF DATA                                                           
*                                                                               
SCAN6    XC    0(12,R3),0(R3)      PRESET A LINE                                
         LH    RF,LBOTH                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R3),SSPACES                                                 
         MVC   2(2,R3),=X'E0E0'                                                 
         BRAS  RE,GETL                                                          
         CLI   0(R3),0                                                          
         BNE   *+8                                                              
         MVI   2(R3),0                                                          
         CLI   1(R3),0                                                          
         BNE   *+8                                                              
         MVI   3(R3),0                                                          
         CLC   0(1,R3),LBOTH+1                                                  
         BH    ERROR                                                            
         CLC   1(1,R3),LRIGHT+1                                                 
         BH    ERROR                                                            
         CLI   1(R3),0                                                          
         BE    SCAN8                                                            
         CLI   0(R3),10                                                         
         BH    ERROR                                                            
         SPACE 2                                                                
SCAN8    SR    R7,R7                                                            
         IC    R7,0(R3)                                                         
         LTR   R7,R7                                                            
         BZ    SCAN18                                                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R3),0(R2)                                                   
         TM    2(R3),X'80'                                                      
         BZ    SCAN10                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN10                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,5(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING DISPS.               
         BO    SCAN10                                                           
         ST    R8,4(R3)                                                         
         SPACE 2                                                                
SCAN10   LA    R2,2(R2,R7)                                                      
         IC    R7,1(R3)                                                         
         LTR   R7,R7                                                            
         BZ    SCAN20                                                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   22(0,R3),0(R2)                                                   
         TM    3(R3),X'80'                                                      
         BZ    SCAN12                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN12                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,9(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING SDISPS.              
         BO    SCAN12                                                           
         ST    R8,8(R3)                                                         
         SPACE 2                                                                
SCAN12   LA    R2,2(R2,R7)                                                      
         B     SCAN20                                                           
         SPACE 2                                                                
VARPAK   PACK  SDUB,0(0,R2)                                                     
         SPACE 2                                                                
SCAN18   LA    R2,1(R2)                                                         
         CLI   1(R3),0                                                          
         BNE   ERROR                                                            
         SPACE 2                                                                
SCAN20   LA    R6,1(R6)            BUMP N'LINES                                 
         AH    R3,LROW             BUMP TO NEXT LINE IN BLOCK                   
         CR    R2,R5               ARE WE NOW PAST LAST 'COMMA'                 
         BH    OK                                                               
         IC    R7,MAXLINES                                                      
         LTR   R7,R7                                                            
         BZ    SCAN6                                                            
         CR    R6,R7               HAVE WE REACHED MAX N'LINES                  
         BNE   SCAN6                                                            
         SPACE 2                                                                
OK       MVC   0(1,R5),BORROW      RETURN THE BYTE                              
         STC   R6,4(R9)            SET NUMBER OF LINES USED                     
         B     XIT                                                              
         SPACE 2                                                                
ERROR    MVI   4(R9),0                                                          
         MVC   0(1,R5),BORROW                                                   
         MVC   2(2,R3),=X'FFFF'                                                 
         B     XIT                                                              
         SPACE 2                                                                
ERROR2   MVI   4(R9),0                                                          
         MVC   2(2,R3),=X'FFFF'                                                 
         SPACE 2                                                                
XIT      XMOD1 1                                                                
         EJECT                                                                  
*VALIDATE AND GET LENGTHS                                                       
*                                                                               
GETL     NTR1                                                                   
         LR    R4,R3                                                            
         SR    R5,R5                                                            
         TM    4(R9),X'80'                                                      
         BZ    GETL2                                                            
         MVC   4(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
         SPACE 2                                                                
GETL2    CLC   0(1,R2),COMMA       TEST FIELD SEPERATOR                         
         BE    GETL12                                                           
         CLC   0(1,R2),EQUAL                                                    
         BE    GETL14                                                           
         SPACE 2                                                                
GETL3    LA    R5,1(R5)                                                         
         CLI   0(R2),C'9'                                                       
         BNH   *+8                                                              
         MVI   2(R4),0             (ALL INVALID)                                
         CLI   0(R2),C'0'                                                       
         BL    GETL4                                                            
         NI    2(R4),X'BF'         (INVALID ALPHA)                              
         B     GETL10                                                           
         SPACE 2                                                                
GETL4    NI    2(R4),X'7F'         (INVALID NUM)                                
         CLI   0(R2),C'Z'                                                       
         BNH   GETL6                                                            
         MVI   2(R4),0             Z-0 = ALL INVALID                            
         B     GETL10                                                           
         SPACE 2                                                                
GETL6    CLI   0(R2),C'A'          LESS THAN A = ALL INVALID                    
         BNL   GETL8                                                            
         MVI   2(R4),0                                                          
         B     GETL10                                                           
         SPACE 2                                                                
GETL8    CLI   0(R2),C'F'          OK FOR ALPHA                                 
         BNH   GETL10                                                           
         NI    2(R4),X'DF'         G-Z = INVALID HEX                            
         SPACE 2                                                                
GETL10   LA    R2,1(R2)                                                         
         B     GETL2                                                            
         SPACE 2                                                                
GETL12   STC   R5,0(R4)            COMMA FOUND                                  
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         B     XIT                                                              
         SPACE 2                                                                
GETL14   CR    R4,R3               EQUAL FOUND - IS THIS THE FIRST ONE?         
         BNE   GETL3               TREAT AS NORMAL CHARACTER IF NOT             
         STC   R5,0(R4)            NOW STORE L1                                 
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         TM    4(R9),X'80'                                                      
         BZ    GETL16                                                           
         MVC   8(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
         SPACE 2                                                                
GETL16   LA    R4,1(R4)            POINT TO FIELD2 DATA                         
         SR    R5,R5               CLEAR L2                                     
         LA    R2,1(R2)            POINT PAST EQUAL SIGN                        
         B     GETL2                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
COMMAEQL DC    C',='                                                            
SSPACES  DC    CL80' '                                                          
         EJECT                                                                  
***********************************************************************         
* SCANNER WORKING STORAGE                                             *         
***********************************************************************         
         EJECT                                                                  
SWORKD   DSECT                                                                  
SDUB     DS    D                                                                
SWORK    DS    CL32                                                             
LASTSTOP DS    F                                                                
BORROW   DS    CL1                                                              
MAXLINES DS    CL1                                                              
LROW     DS    H                                                                
LRIGHT   DS    H                                                                
LBOTH    DS    H                                                                
SDISP    DS    H                                                                
COMMA    DS    C                                                                
EQUAL    DS    C                                                                
*                                                                               
SWORKL   EQU   *-SWORKD                                                         
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DDRCVSTRIP10/07/11'                                      
         END                                                                    
