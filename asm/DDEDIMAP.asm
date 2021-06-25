*          DATA SET DDEDIMAP   AT LEVEL 100 AS OF 03/03/20                      
*PHASE T00AB6A                                                                  
*INCLUDE SORTER                                                                 
T00AB6   TITLE 'MEDIA EDI MAPPING'                                              
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-43978  03/03/20 HONOR EST UDEF DESC "ATTENTION NAME"      *         
* AKAT SPEC-39299  09/19/19 INCREASE ID ELEMENT CAPACITY TO 6K        *         
* AKAT SPEC-27820  10/15/18 GROUPM/IBM MOVING FROM 3040 TO 4010       *         
***********************************************************************         
**********************************************************************          
* THIS OVERLAY IS USED BY SPOT,PRINT AND NET EB FOR EDI TRANSMISSION *          
* ------------------------------------------------------------------ *          
* PERSON LEVEL IMDB#   DESCRIPTION                                   *          
* ------ ----- -----   -----------                                   *          
*                                                                    *          
* SMUR  05/01/18  SPEC-17729 NEW PRINT MEDIA (D)IGITAL AUDIO         *          
*                                                                    *          
* BPLA   66    2407641 FUNDING SOURCE CHANGED IN BURGER KING TABLE   *          
* RGUP   67    2245741 MCCANN TO NESTLE CHANGED TO ZENITH TO NESTLE  *          
* RGUP   68    447903  CHANGES FOR OMD TO MCDONALD - SKIP TXI SEGMNT *          
* BPLA   69    2554611 NEW MINDSHARE BURGER KING CLT CODE - BK       *          
*                      RADIO ONLY FOR NOW                            *          
* BPLA   69    2566991 TAX ONLY INVOICE FIX (SPOT)                   *          
*                      RESTORE TAX TO NET FOR MINDSHARE NET BASIS    *          
*                                                                               
* BPLA   3/06 7666351 ADD CLIENT BV3 TO H7ACCTAB (MINDSHARE BK)                 
*                                                                               
* BPLA   70    2492081 CHANGES TO COKE                               *          
*                      REPORT "REAL" NET (INSTEASD OF GROSS)         *          
*                      AND NO COMMISSION LOOP                        *          
*                      COKE - FOR MOS IN 2006 DON'T SEND                        
*                      COST CENTER (BC IN IT1035) - MCREFC                      
*                      NOR KBKS # (REF050 FOR DP) - MCRGN                       
*                      USE SKIPREC TO NOT SEND THE ONE NOT NEEDED               
*                      FOR INTERNAL ORDER NUMBER:                               
*                      IF 2006 USE EST UDEF1                                    
*                      IF 2005 USE EST UDEF2                                    
*                      USE TABLE FOR COKE GL CODES                              
*                      INSTEAD OF UDEF                                          
*                                                                               
*                      CHANGE ROUNDING CHECK FROM 10 CENTS TO 15     *          
*                      THERE ACTUALLY WAS A DUMP FOR 11 CENTS                   
*                      IGNORE MT RECORDS WITH 'TOTAL' IN MOS                    
*                      TAX FIX FOR MINDSHARE - BURGER KING                      
*                                                                               
*                      BROMLEY BURGER KING CLIENT BBN -NETWORK                  
*                      SPECIAL CODE                                             
*                                                                               
*                      ADD ENTRIES FOR EDI CLIENT CODES THAT                    
*                      THE NET BILLING PROGRAM INADVENTENTLY CHANGED            
*                      H9 (BBN TO 225) M1 (BKC TO 223)                          
*                                                                               
*  BPLA   70           OMDUSA - ELI LILY (ELO)  PRELIMINARY CODE                
*                      ONLY - SPEC QUESTIONS REMAIN                             
*                                                                               
*  BPLA   71   2638641 CHANGE TO HANDLE ZERO STATION DATA FOR H9                
*                      NETWORK                                                  
*         71   2817571 NEW MINDSHARE BK CLIENT (BV4)                            
*                                                                               
*  BPLA   73   2872701 NEW MINDSAHRE BK CLIENT (BV5)                            
*                                                                               
*  BPLA   74           ELI LILLY - MORE CHANGES                                 
*                                                                               
*  BPLA   75   0109112N NEW MINDSAHRE BK CLIENT (BV6)                           
*                       NEW G/L FOR COKE                                        
*  BPLA                 COKE G/L CODE CHANGE                                    
*                       891410000 FOR OUTDOOR                                   
*                       891412000 FOR INTERACTIVE                               
*                                                                               
*  BPLA   76   0113300N CHECK FOR NEW ESTIMATE USER 1 ANNOTATION                
*                       FOR COKE - CURRENT IO#                                  
*                                                                               
*  BPLA   77            ADD ENTRY FOR COKE EDI CLIENT 024                       
*                       - IT WAS ENTERED ON THE B1A PROFILE                     
*                         FOR CLIENT CG7 IN ERROR (SHOULD HAVE                  
*                         BEEN 002)                                             
*                                                                               
*  BPLA   78   0118493N BURGER KING FUNDING SOURCE AND G/L CODE CHGS            
*                                                                               
*  BPLA   79   0101035T MCDONALDS (OMD) ATTENTION FROM UCOM OR UDEF             
*                                                                               
*  BPLA   80   FIX SPOT/NET OMDUSA ELI-LILLY                                    
*                                                                               
*  BPLA   81   0133745N 2 NEW BURGER KING CLIENTS BV8 AND BV9                   
*                                                                               
*  BPLA   82   0134563N NEW ENTRIES IN THUFTAB FOR CABLE/SYNDICATION,           
*                       CUT-INS AND COPY SPLITS                                 
*                       MORE ELI-LILLY CHANGES - SKIP COMMISSION                
*                       LINE ALLOCATION AS IT DOESN'T WORK FOR                  
*                       SPOT/NET MANUAL BILL CREDITING                          
*                                                                               
*  BPLA   82   0144031N RESTORE FUNDING SOURCE FOR BURGER KING CLIENT           
*                       BI2 TO 023 (FROM 020)                                   
*                                                                               
*  BPLA   83   0147769N NEW BURGER KING CLIENT - BI3 LIKE BI2                   
*                                                                               
*  BPLA   84   0149802N VENDOR# CHANGE FOR BURGER KING BROMLEY                  
*                       OLD - 33574  NEW - 73689                                
*                                                                               
*  BPLA   85   0134563N CHANGE VENDER ID FOR ZENITH TO 100359930                
*                       FROM 879432557 *** ON HOLD ***                          
*                       OLD CODE USED                                           
*                                                                               
*  BPLA   85   0167618N MEDIACOM DELL                                           
*              0181553N NEW MINDSHARE BK CLIENT BV0                             
*                       (LIKE BV9)                                              
*                                                                               
*  YKVA   86   0189892N 2 NEW BK CLIENTS BG0 AND BG1 SAME AS BV9                
*                                                                               
*  BPLA   88   0186274N CARAT - PFIZER EDI                                      
*              0205126N MINSHARE - NEW BK CLIENT BG2 (LIKE BV9)                 
*                                                                               
*  BPLA   89   MODIFICATIONS TO PFIZER EDI                                      
*              ALSO IGNORE STATION AND PUB RECORDS WITH MOS = TOTAL             
*              THOSE CAN CAUSE PROBLEMS WITH DETAIL COMMISSION CALCS            
*              PFIZER - RETURN $1.00 IN IT104 -CLIFF MAY NOT LIKE THIS          
*              CHANGED TO 100 (PRECEDED BY ZEROS)                               
*                                                                               
*              INITIAL CODING FOR SHELL - MEDIACOM                              
*                                                                               
*   BPLA   06/11/14     CHANGES FOR MEDIA L                                     
*                                                                               
*   BPLA   06/30/15     CHANGES FOR NEW PRINT MEDIA CODES B,V,W                 
*                                                                               
**********************************************************************          
MEDIM    CSECT                                                                  
         PRINT NOGEN                                                            
EDI      NMOD1 (LWSX-LWSD),*EDIM*,CLEAR=YES,RA                                  
         LR    R9,RC                                                            
         USING LWSD,R9                                                          
         MVC   PARMS(PARMSLNQ),0(R1)                                            
         L     R2,COMFAC                                                        
         USING COMFACSD,R2                                                      
         MVC   DATCON,CDATCON                                                   
         MVC   ADDAY,CADDAY                                                     
         DROP  R2                                                               
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVI   ZEROS,C'0'                                                       
         MVC   ZEROS+1(L'ZEROS-1),ZEROS                                         
         ZAP   PZERO,PACKZ                                                      
         MVC   AIDPTAB,ADIDPTAB    ID RECORD DETAIL                             
         MVC   ALINTAB,ADLINTAB    LINE ITEM DETAIL                             
         MVC   AMITTAB,ADMITTAB    MONTHLY INVOICE TOTAL TABLE                  
         MVC   ASORTAB,ADSORTAB    INVOICE SORT TABLE                           
         MVC   VSORTER,ADSORTER    SORTER                                       
*                                                                               
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         CP    MCNDUMPS,PZERO      TEST ALREADY DIED                            
         JNE   XIT                 YES, JUST LEAVE                              
         DROP  RF                                                               
                                                                                
         TM    FCS,FCSTEMP         IS INPUT FILE OPEN                           
         BO    EDIM3                                                            
         OPEN  (TEMPEB,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    FCS,FCSTEMP                                                      
*                                                                               
EDIM3    L     R2,AEDINME          EDI FILE NAME (SPTTAPE.SP0EBXX)              
         MVC   MAPNME,0(R2)                                                     
         MVC   MAPPGM,=C'EM'       OUTPUT IS SPTTAPE.SP0EMXX                    
         XC    INVNUMB,INVNUMB                                                  
*                                                                               
EDIM5    TM    FCS,FCSTEMP         TEST TEMP FILE STILL OPEN                    
         BNO   EDIM15              NO, MUST BE FINISHED                         
         LA    R1,TEMPEB                                                        
         L     R0,AOUT                                                          
         GET   (R1),(R0)                                                        
*                                                                               
EDIM7    L     R3,AOUT                                                          
         LA    R3,4(R3)            GET PASSED LENGTH                            
         USING FHD,R3              GET FILE HEADER                              
         CLI   FHRT,C'H'                                                        
         BNE   EDIM5                                                            
         CLI   FHSTD,C'X'                                                       
         BNE   EDIM5                                                            
         CLC   FHDOCT,=C'810'                                                   
         BNE   EDIM5                                                            
         MVC   AGYALPHA,FHAGY                                                   
         L     R2,AAGYTAB                                                       
         USING AGCD,R2                                                          
*                                                                               
EDIM9    CLC   FHAGY,AGCAGY        MATCH AGENCY                                 
         BNE   EDIM11                                                           
         CLI   AGCSYS,C' '         TEST 'ALL SYSTEMS'                           
         BE    *+14                                                             
         CLC   FHSYS,AGCSYS        SYSTEM                                       
         BNE   EDIM11                                                           
         CLC   FHCLI,AGCCLI        CLIENT                                       
         BE    EDIM13                                                           
*                                                                               
EDIM11   LA    R2,AGCLNQ(R2)                                                    
         CLI   0(R2),EOT                                                        
         BNE   EDIM9                                                            
         B     EDIM5                                                            
*                                                                               
EDIM13   TM    FCS,FCSSORE         SORT ENDED?                                  
         BO    EDIM14              . YES                                        
         TM    AGCOPT,AGCSORT      NEED TO SORT?                                
         BZ    EDIM14              . NO                                         
         BAS   RE,SRTINV                                                        
*                                                                               
EDIM14   ST    R2,AAGC                                                          
         MVC   SYSTM,FHSYS                                                      
         CLI   SYSTM,C'N'          MAKE NETWORK                                 
         BNE   *+8                                                              
         MVI   SYSTM,C'S'          WORK LIKE SPOT                               
         MVC   CLI,FHCLI                                                        
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,3,AGCCSC         DISPLACEMENT TO CONTROL TABLE                
         AR    R5,RB                                                            
         ST    R5,AAGYSTAB         A(AGENCY SYSTEM TABLE)                       
         USING CSCD,R5                                                          
         SR    RF,RF                                                            
         ICM   RF,3,CSCROUT        SPECIAL HOOK ROUTINES                        
         AR    RF,RB                                                            
         ST    RF,CONROUT          SAVE A(CONTROL ROUTINE)                      
*                                                                               
         GOTO1 CONROUT,BEFRQ       FIRST TIME INITIALIZATION                    
         BAS   RE,PROC             GET AND PROCESS THE INPUT DATA               
         GOTO1 CONROUT,AFTRQ       FIRST TIME FOR INVOICE                       
         CLI   SKIPINV,C'Y'        TEST HOOK SET 'SKIP INVOICE'                 
         BE    EDIM19                                                           
*                                                                               
EDIM15   BAS   RE,LINT             GET DETAIL LINE TOTALS                       
         OC    INVNUMB,INVNUMB     ANY INVOICE                                  
         BZ    EDIM19                                                           
         LA    RF,CSCMAPS                                                       
         LA    R0,CSCNMAP                                                       
*                                                                               
EDIM17   SR    R3,R3                                                            
         ICM   R3,3,0(RF)          DISPLACEMENT TO MAPS                         
         BZ    *+10                                                             
         AR    R3,RB               R3=A(RECORD RECORDS)                         
         BAS   RE,PUTR             PUT RECORDS                                  
*                                                                               
         LA    RF,L'CSCMAPS(RF)                                                 
         BCT   R0,EDIM17                                                        
*                                                                               
EDIM19   XC    INVNUMB,INVNUMB     INVOICE PROCESSED                            
*                                                                               
         TM    FCS,FCSTEMP         TEST TEMP STILL OPEN                         
         BO    EDIM7                                                            
*                                                                               
         TM    FCS,FCSEDI          TEST EDI OPEN                                
         BNO   EDIM24                                                           
         LA    RF,CSCMXTR                                                       
         CLI   0(RF),EOTQ          ANY ALL INVOICE TOTAL MAP                    
         BE    EDIM22              . NO                                         
         SR    R3,R3                                                            
         ICM   R3,3,0(RF)          DISPLACEMENT TO MAP                          
         BZ    *+10                                                             
         AR    R3,RB               R3=A(RECORD RECORDS)                         
         BAS   RE,PUTR             PUT RECORDS                                  
*                                                                               
EDIM22   TM    FCS,FCSEDI          TEST EDI OPEN                                
         BNO   EDIM24                                                           
         CLOSE EDIMAP                                                           
         NI    FCS,ALL-(FCSEDI)                                                 
                                                                                
EDIM24   TM    FCS,FCSEIO          OUTPUT SUMMARY?                              
         BZ    *+8                 . NO                                         
         BRAS  RE,EIOTOT                                                        
                                                                                
         J     XIT                                                              
*                                                                               
CLOSE    CLOSE TEMPEB              CLOSE INPUT                                  
         NI    FCS,ALL-(FCSTEMP)                                                
         TM    FCS,FCSSORT         TEST IN SORT ROUTINE                         
         BO    SI75                . GO BACK TO SORT                            
         TM    FCS,FCSPROC         TEST IN PROCESS ROUTINE                      
         BNO   EDIM15              PROCESS LAST                                 
         NI    FCS,ALL-(FCSPROC)                                                
         J     XIT                 EXIT PROC ROUTINE                            
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
*                                                                               
*********************************************************************           
* SORT INVOICES                                                     *           
*********************************************************************           
         USING SORTRECD,R2                                                      
SRTINV   NTR1                                                                   
         OI    FCS,FCSSORT          SORTING                                     
         LHI   RE,1                                                             
         STH   RE,HALF                                                          
*                                                                               
         L     R2,ASORTAB           R2=A(SORT TABLE ENTRY)                      
         LR    RE,R2                CLEAR SORTAB                                
         L     RF,=A(MXSRT*MXSLIN)                                              
         XCEF                                                                   
*                                                                               
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD,0                                  
*                                                                               
         TM    FCS,FCSTEMP         IS INPUT FILE OPEN                           
         BO    SI22                                                             
         DC    H'0'                                                             
*----------------------------------                                             
* READ FROM TEMP AND ADD TO BUFFER                                              
*----------------------------------                                             
SI20     LA    R1,TEMPEB                                                        
         L     R0,AOUT                                                          
         GET   (R1),(R0)                                                        
SI22     L     R3,AOUT             R3=A(TEMPEB OUTPUT)                          
         LA    R4,4(R3)            R4=A(TEMP OUTPUT BEYOND THE LENGTH)          
*                                                                               
         LH    RE,HALF                                                          
         CHI   RE,1                                                             
         BE    SI25                . YES                                        
*                                                                               
         USING FHD,R4                                                           
         CLI   FHRT,C'H'           HEADER RECORD                                
         BNE   *+8                                                              
         BAS   RE,OUTSORT                                                       
*                                                                               
SI25     XC    0(SORTLKQ,R2),0(R2)                                              
*                                                                               
         USING IHD,R4                                                           
SI30     CLC   IHRT,=C'IH'                                                      
         BNE   SI40                                                             
         MVC   INVNUMB(L'IHINVN),IHINVN                                         
*                                                                               
         USING ADD,R4                                                           
SI40     CLC   ADRT,=C'AD'                                                      
         BNE   SI50                                                             
*                                                                               
         MVC   INVTYPE,=C'AD'                                                   
         CLI   ADDUEAMT,C'-'                                                    
         BNE   *+10                                                             
         MVC   INVTYPE,=C'CR'                                                   
*                                                                               
SI50     SR    R5,R5                                                            
         ICM   R5,3,0(R3)            LENGTH OF DATA                             
*                                                                               
         SHI   R5,1                                                             
         BP    *+6                   DATA?                                      
         DC    H'0'                                                             
         CHI   R5,L'SORTAB-SORTLKQ   FITS IN SORT ENTRY?                        
         BL    *+6                   . YES                                      
         DC    H'0'                                                             
*                                                                               
         EX    R5,*+8                MOVE INTO TABLE ENTRY                      
         B     *+10                                                             
         MVC   SORTLKQ(0,R2),0(R3)                                              
         AHI   R5,SORTLKQ+1                                                     
         STCM  R5,3,SORTLEN                                                     
         MVC   SORTSEQ,HALF                                                     
*                                                                               
         LH    R1,HALF                                                          
         AHI   R1,1                                                             
         CHI   R1,MXSRT            MAX NUMBER OF SORT ENTRIES?                  
         BNH   *+6                 . NO                                         
         DC    H'0'                                                             
         STH   R1,HALF                                                          
*                                                                               
         LA    R2,L'SORTAB(R2)                                                  
         B     SI20                                                             
         DROP  R4                                                               
*                                                                               
SI75     BAS   RE,OUTSORT                                                       
*----------------------------------                                             
* READ FROM SORTER AND ADD TO TEMP                                              
*----------------------------------                                             
         OPEN  (TEMPEB,(OUTPUT))                                                
         OI    FCS,FCSTEMP                                                      
                                                                                
         L     R2,ASORTAB          R2=A(SORT TABLE ENTRY)                       
SI80     XC    SORTLEN,SORTLEN                                                  
*                                                                               
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         L     R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    SI99                                                             
                                                                                
         SR    R5,R5                                                            
         ICM   R5,3,0(R4)                R5=LENGTH OF SORT RECORD               
         SHI   R5,SORTLKQ+1              SUBTRACT LEN,KEY,& 1 FOR MOVE          
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R5,*+8                    MOVE DATA AFTER LENGTH                 
         B     *+10                                                             
         MVC   SORTLEN(0),SORTLKQ(R4)                                           
         PUT   TEMPEB,(R2)                                                      
         B     SI80                                                             
*                                                                               
SI99     GOTO1 VSORTER,DMCB,=C'END',0                                           
         CLOSE TEMPEB                                                           
         NI    FCS,ALL-FCSSORT                                                  
                                                                                
         OPEN  (TEMPEB,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R1,TEMPEB                                                        
         L     R0,AOUT                                                          
         GET   (R1),(R0)                                                        
*                                                                               
         OI    FCS,FCSSORE         SORT ENDED                                   
         J     XIT                                                              
*------------------------------------                                           
* READ FROM BUFFER AND ADD TO SORTER                                            
*------------------------------------                                           
OUTSORT  ST    RE,SAVRE                                                         
         L     R2,ASORTAB          R2=A(SORT TABLE ENTRY)                       
                                                                                
OSO10    OC    SORTSEQ,SORTSEQ                                                  
         BZ    OSO20                                                            
*                                                                               
         MVC   SORTINV,INVNUMB                                                  
         MVC   SORTDEF(L'INVTYPE),INVTYPE                                       
         GOTO1 VSORTER,DMCB,=C'PUT',(R2)                                        
         LA    R2,L'SORTAB(R2)                                                  
         B     OSO10                                                            
*                                                                               
OSO20    DS    0H                                                               
         LHI   RE,1                                                             
         STH   RE,HALF                                                          
         L     R2,ASORTAB          R2=A(SORT TABLE ENTRY)                       
         LR    RE,R2               CLEAR SORTAB                                 
         L     RF,=A(MXSRT*MXSLIN)                                              
         XCEF                                                                   
         L     RE,SAVRE                                                         
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*********************************************************************           
* PROCESS FLAT FILE RECORDS FROM MEDIA BILLING                      *           
*********************************************************************           
PROC     NTR1  ,                                                                
PROC3    LA    R1,TEMPEB                                                        
         L     R0,AOUT                                                          
         OI    FCS,FCSPROC         SET IN PROCESS                               
         GET   (R1),(R0)                                                        
         NI    FCS,ALL-(FCSPROC)                                                
         L     R3,AOUT                                                          
         LA    R3,4(R3)            GET PASSED LENGTH                            
         USING FHD,R3              TEST FOR NEXT HEADER                         
         CLI   FHRT,C'H'                                                        
         BNE   PROC5                                                            
         CLI   FHSTD,C'X'                                                       
         BNE   PROC5                                                            
         CLC   FHDOCT,=C'810'                                                   
         BNE   PROC5                                                            
         J     XIT                                                              
*                                                                               
PROC5    L     R2,AFLATAB                                                       
         USING FFRD,R2                                                          
PROC7    XR    R4,R4                                                            
         ICM   R4,3,FFRLN                                                       
         AR    R4,R2               R4=NEXT ENTRY                                
         CLC   0(2,R3),FFRID       MATCH RECORD TO TABLE                        
         BE    PROC9                                                            
         LR    R2,R4                                                            
         CLI   0(R2),EOT           SKIP IF NOT IN TABLE                         
         BNE   PROC7                                                            
         B     PROC3                                                            
*                                                                               
PROC9    BCTR  R4,0                R4=END OF THIS ENTRY                         
         LA    R5,FFRDATA                                                       
         MVI   PASSCON,C'Y'                                                     
*                                                                               
         USING FFRDATA,R5                                                       
PROC11   TM    FFRCDLN,CONDQ       TEST CONDITIONAL                             
         BNO   PROC13              NO, PROCESS DATA                             
*                                                                               
         XR    R1,R1               R1=LENGTH                                    
         IC    R1,FFRCDLN                                                       
         LA    R0,CONDQ+1          STRIP CONDITION TEST FROM LENGTH             
         TM    FFRCDLN,CNDSQ                                                    
         BNO   *+8                                                              
         LA    R0,CNDSQ+1                                                       
         SR    R1,R0                                                            
         XR    RE,RE               RE=DISP. TO CONDITIONAL DATA                 
         ICM   RE,3,FFRCDDSP                                                    
         LR    R0,R3               R0=BASE                                      
         TM    FFRCDLN,CNDSQ                                                    
         BNO   *+8                                                              
         LA    R0,LWSD             BASE IS LOCAL STORAGE                        
         AR    RE,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),FFRCDDAT                                                 
         BE    *+8                                                              
         MVI   PASSCON,C'N'        DID NOT PASS CONDITION TEST                  
         LA    R5,FFRCDDAT-FFRDATA+1(R1,R5)                                     
         B     PROC11                                                           
*                                                                               
PROC13   CLI   PASSCON,C'Y'        TEST PASSED CONDITIONALS                     
         BNE   PROC17                                                           
         OC    FFRROUID,FFRROUID   TEST ROUTINE                                 
         BNZ   PROC15                                                           
         XR    RF,RF                                                            
         ICM   RF,3,FFRROU                                                      
         AR    RF,RB               GET ADDRESS OF ROUTINE                       
         BASR  RE,RF               PROCESS RECORD                               
         B     PROC17                                                           
*                                                                               
PROC15   XR    R1,R1               R1=LENGTH                                    
         IC    R1,FFRDLN                                                        
         BCTR  R1,0                                                             
         XR    RE,RE               RE=DESTINATION                               
         ICM   RE,3,FFRDDST                                                     
         LA    RE,LWSD(RE)                                                      
         XR    RF,RF               RF=SOURCE                                    
         ICM   RF,3,FFRDSRC                                                     
         AR    RF,R3                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
*                                                                               
PROC17   LA    R5,L'FFRDATA(R5)                                                 
PROC19   CR    R5,R4               TEST PASSED END OF ENTRY                     
         BH    PROC3               YES, GET NEXT CARD                           
         CLI   0(R5),CONDQ         TEST RESET CONDITION                         
         BNE   PROC11              NO, PROCESS NEXT ENTRY                       
         MVI   PASSCON,C'Y'        RESET CONDITION SWITCH                       
         LA    R5,1(R5)                                                         
         B     PROC19                                                           
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
*********************************************************************           
* INVOICE HEADER  ROUTINE                                           *           
*********************************************************************           
         USING IHD,R3                                                           
IHR      NTR1  ,                   INVOICE HEADER                               
         MVC   INVNUMB,IHINVN      INVOICE NUMBER                               
         MVC   INVDATE,IHINVD      INVOICE DATE                                 
         GOTOR CNVD,DMCB,(5,INVDATE),(20,INVDCYMD)                              
*                                                                               
         XC    AIDPITM,AIDPITM     CLEAR A(NEXT ID ITEM)                        
         XC    IDPCNT,IDPCNT       CLEAR NUMEBR IN TABLE                        
         L     R1,AIDPTAB                                                       
         MVI   0(R1),EOT                                                        
*                                                                               
         XC    ALINITM,ALINITM     CLEAR A(NEXT LINE ITEM)                      
         XC    LINCNT,LINCNT       CLEAR NUMEBR IN TABLE                        
*                                                                               
         XC    SVLINSTA,SVLINSTA                                                
         XC    SVLINMOS,SVLINMOS                                                
*                                                                               
         L     R1,ALINTAB                                                       
         MVI   0(R1),EOT                                                        
*                                                                               
         XC    AMITITM,AMITITM     CLEAR A(NEXT MIT ITEM)                       
         XC    MITCNT,MITCNT       CLEAR NUMEBR IN TABLE                        
         L     R1,AMITTAB                                                       
         MVI   0(R1),EOT                                                        
*                                                                               
         MVI   USRMRKT,C'0'        MAR# IS ZEROS, IF NOT ON FILE                
         MVC   USRMRKT+1(L'USRMRKT-1),USRMRKT                                   
         MVC   ADVNAME,SPACES                                                   
*                                                                               
         LA    R1,BKS              ZAP SOME ACCUMS                              
         LA    R0,NBKS                                                          
         ZAP   0(BKLQ,R1),PZERO                                                 
         LA    R1,BKLQ(R1)                                                      
         BCT   R0,*-10                                                          
*                                                                               
         MVI   ANYTAX,C'N'                                                      
         MVI   SKIPINV,C'N'                                                     
         XC    PINVNUMB,PINVNUMB   FOR NOW, ONLY SAVING THE 1ST PI              
         J     XIT                                                              
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*********************************************************************           
* INVOICE HEADER  ROUTINE                                           *           
*********************************************************************           
         USING PID,R3                                                           
PINV     NTR1  ,                   PREVIOUS INVOICE HEADER                      
         OC    PINVNUMB,PINVNUMB   PREVIOUS INVOICE NUMBER ALREADY SET?         
         BNZ   *+10                YES - ONLY KEEP THE 1ST ONE FOR NOW          
         MVC   PINVNUMB,PIINVN     PREVIOUS INVOICE NUMBER                      
         J     XIT                                                              
*                                                                               
         DROP  R3                                                               
*********************************************************************           
* PROCESS MONTHLY TOTAL RECORD FOR SPOT                             *           
*********************************************************************           
         USING MTD,R3                                                           
MTRSS    NTR1  ,                   STATION                                      
*                                                                               
         CLC   MTMOS(5),=C'TOTAL'    IGNORE STATION TOTALS                      
         JE    XIT                                                              
*                                                                               
         GOTO1 PACKIT,DMCB,MTD,ASTACNVT                                         
         ICM   R4,15,ALINITM       ADD A NEW LINE ITEM                          
         BNZ   *+8                                                              
         ICM   R4,15,ALINTAB                                                    
         USING LIND,R4                                                          
         MVC   LINMOS,MTMOS                                                     
         ZAP   LINGRS,STORDGRS     GROSS                                        
         SP    LINGRS,STPRVGRS     LESS PREVIOUS                                
         ZAP   LINNET,STORDNET     NET                                          
         SP    LINNET,STPRVNET     LESS PREVIOUS                                
         ZAP   LINTAX,STORDTAX     TAX                                          
         SP    LINTAX,STPRVTAX     LESS PREVIOUS                                
         ZAP   LINCSD,PZERO        NO CD FOR SPOT                               
         MVC   LINPUB,SPACES                                                    
         CLC   MTRT,=C'NT'         'NT' RECORD                                  
         BNE   MTRSS10                                                          
         MVC   LINSTA(L'MTDLVN),MTDLVN  THIS IS NETWORK FOR CANADA              
         B     *+10                                                             
MTRSS10  MVC   LINSTA,STATION      STATION                                      
         MVC   LINMKT,MRKTNUM      MARKET NUMBER                                
         MVC   LINMKTN,MRKTNAM     MARKET NAME                                  
         BAS   RE,MTRALIN          ADD TO LINE ITEM TOTALS                      
         J     XIT                                                              
*                                                                               
MTRSI    NTR1  ,                   INVOICE                                      
         GOTO1 PACKIT,DMCB,MTD,ASTACNVT                                         
         ICM   R4,15,AMITITM       SPOT MONTHLY INVOICE TOTAL                   
         BNZ   *+8                                                              
         ICM   R4,15,AMITTAB                                                    
         USING MITD,R4                                                          
         MVC   MITMOS,MTMOS                                                     
         MVC   MITTYP,MTTYP                                                     
         ZAP   MITGRS,STORDGRS     GROSS                                        
         SP    MITGRS,STPRVGRS     LESS PREVIOUS                                
         ZAP   MITNET,STORDNET     NET                                          
         SP    MITNET,STPRVNET     LESS PREVIOUS                                
         ZAP   MITOGRS,STORDGRS    GROSS - ORDERED                              
         ZAP   MITONET,STORDNET    NET - ORDERED                                
         BAS   RE,MTRAMIT          ADD TO MONTHLY TOTAL ACCUMS                  
         J     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*********************************************************************           
* PROCESS ID RECORD FOR PRINT                                       *           
*********************************************************************           
         USING IDD,R3                                                           
IDRPP    NTR1  ,                   ID                                           
         GOTO1 PACKIT,DMCB,IDD,AINSCNVT                                         
         ICM   R4,15,AIDPITM       ADD A NEW ID LINE ITEM                       
         BNZ   *+8                                                              
         ICM   R4,15,AIDPTAB                                                    
         USING IDPD,R4             ID RECORD FOR PRINT                          
         MVC   IDPDATE,IDDATE                                                   
         MVC   IDPDESC,IDDESC                                                   
         ZAP   IDPOGRS,INORDGRS    GET GROSS AMOUNT                             
         SP    IDPOGRS,INPRVGRS    MINUS PREVIOUS GROSS                         
         MVC   IDPPUB,PUB          MOVE PUBLICATION                             
*                                                                               
         LA    R4,IDPLNQ(R4)                                                    
         ST    R4,AIDPITM                                                       
         MVI   0(R4),EOT                                                        
         L     RF,IDPCNT                                                        
         AHI   RF,1                                                             
         ST    RF,IDPCNT                                                        
         CH    RF,=Y(MXLIN)                                                     
         BNH   *+6                                                              
         DC    H'0'                INCREASE MXLIN                               
         J     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*********************************************************************           
* PROCESS MONTHLY TOTAL RECORD FOR PRINT                            *           
*********************************************************************           
         USING MTD,R3                                                           
MTRPP    NTR1  ,                   PUB                                          
*                                                                               
         CLC   MTMOS(5),=C'TOTAL'   IGNORE PUB TOTAL RECORDS                    
         JE    XIT                                                              
*                                                                               
         GOTO1 PACKIT,DMCB,MTD,APUBCNVT                                         
         ICM   R4,15,ALINITM        ADD A NEW LINE ITEM                         
         BNZ   *+8                                                              
         ICM   R4,15,ALINTAB                                                    
         USING LIND,R4                                                          
         MVC   LINMOS,MTMOS                                                     
         ZAP   LINGRS,PBORDGRS     GROSS                                        
         SP    LINGRS,PBPRVGRS     LESS PREVIOUS                                
         ZAP   LINNET,PBORDNET     NET                                          
         SP    LINNET,PBPRVNET     LESS PREVIOUS                                
         ZAP   LINTAX,PBORDTAX     TAX                                          
         SP    LINTAX,PBPRVTAX     LESS PREVIOUS                                
         ZAP   LININS,PBORDINS     MNSERTIONS                                   
         ZAP   LINCSD,PBORDCSD     CASH DISCOUNT                                
         SP    LINCSD,PBPRVCSD     LESS PREVIOUS                                
         MVC   LINPUB,PUB                                                       
         BAS   RE,MTRALIN          ADD TO LINE ITEM TOTAL                       
         J     XIT                                                              
                                                                                
MTRPI    NTR1  ,                   INVOICE                                      
         GOTO1 PACKIT,DMCB,MTD,APUBCNVT                                         
         ICM   R4,15,AMITITM       SPOT MONTHLY INVOICE TOTAL                   
         BNZ   *+8                                                              
         ICM   R4,15,AMITTAB                                                    
         USING MITD,R4                                                          
         MVC   MITMOS,MTMOS                                                     
         MVC   MITTYP,MTTYP                                                     
         ZAP   MITGRS,PBORDGRS     GROSS                                        
         SP    MITGRS,PBPRVGRS     LESS PREVIOUS                                
         ZAP   MITNET,PBORDNET     NET                                          
         SP    MITNET,PBPRVNET     LESS PREVIOUS                                
         ZAP   MITOGRS,PBORDGRS    GROSS - ORDERED                              
         ZAP   MITONET,PBORDNET    NET - ORDERED                                
         BAS   RE,MTRAMIT          ADD TO MONTHLY TOTAL ACCUMS                  
         J     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
**********************************************************************          
* ADD ITEM TO OVERALL LINE TOTALS                                    *          
**********************************************************************          
         USING LIND,R4                                                          
MTRALIN  AP    TOTTAX,LINTAX       GET THE TOTAL TAX                            
         AP    TOTNET,LINNET                                                    
         AP    TOTCSD,LINCSD                                                    
         SP    LINNET,LINTAX       NET INCLUDED TAX                             
         CP    LINTAX,PZERO                                                     
         BE    *+8                                                              
         MVI   ANYTAX,C'Y'                                                      
         ZAP   LINCOM,PZERO                                                     
         ZAP   LINDUE,PZERO                                                     
*                                                                               
         CP    LINNET,PZERO               IF ALL ZERO - SKIP IT                 
         BNE   MTRALIN3                                                         
         CP    LINGRS,PZERO                                                     
         BNE   MTRALIN3                                                         
         CP    LINTAX,PZERO                                                     
         BNE   MTRALIN3                                                         
*                                                                               
*        FOR H9 NETPAK STILL PROCESS IF LINSTA IS NOT EMPTY                     
*        UNLESS THIS LINE IS THE SAME STATION AND MOS AS THE LAST               
*                                                                               
         CLC   AGYALPHA,=C'H9'    STARCOM?                                      
         BNE   MTRALIN2                                                         
         CLI   MEDIA,C'N'         NETWORK                                       
         BNE   MTRALIN2                                                         
*                                                                               
         CLC   LINSTA,SPACES      IS STATION PRESENT?                           
         BNH   MTRALIN2           NO - THEN SKIP                                
*                                                                               
                                                                                
         CLC   SVLINSTA,LINSTA          SAME STATION?                           
         BNE   MTRALIN3            NO- THEN PROCESS                             
         CLC   SVLINMOS,LINMOS          SAME MOS?                               
         BNE   MTRALIN3            NO- THEN PROCESS                             
*                                                                               
MTRALIN2 MVI   0(R4),EOT                  REMOVE THIS ITEM                      
         BR    RE                                                               
*                                                                               
MTRALIN3 DS    0H                                                               
         MVC   SVLINSTA,LINSTA     SAVE LINSTA AND LINMOS                       
         MVC   SVLINMOS,LINMOS                                                  
*                                                                               
         LA    R4,LINLNQ(R4)                                                    
         ST    R4,ALINITM                                                       
         MVI   0(R4),EOT                                                        
         L     RF,LINCNT                                                        
         AHI   RF,1                                                             
         ST    RF,LINCNT                                                        
         CH    RF,=Y(MXLIN)                                                     
         BNH   *+6                                                              
         DC    H'0'                INCREASE MXLIN                               
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* ADD ITEM TO OVERALL MONTH TOTALS                                   *          
**********************************************************************          
         USING MITD,R4                                                          
MTRAMIT  AP    MITOTGRS,MITGRS     ADD TO TOTAL ACCUMUS                         
         AP    MITOTNET,MITNET                                                  
         AP    MITOTOGR,MITOGRS    ORDERED                                      
         AP    MITOTONT,MITONET    ORDERED                                      
         LA    R4,MITLNQ(R4)                                                    
         ST    R4,AMITITM                                                       
         MVI   0(R4),EOT                                                        
         L     RF,MITCNT                                                        
         AHI   RF,1                                                             
         ST    RF,MITCNT                                                        
         CH    RF,=Y(MXMIT)                                                     
         BNH   *+6                                                              
         DC    H'0'                INCREASE MXMIT                               
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
*********************************************************************           
* PROCESS CT GST TAX RECORD                                         *           
*********************************************************************           
         USING CTD,R3                                                           
CTGST    NTR1  ,                   CT GST TAX RECORD                            
         GOTO1 PACKIT,DMCB,CTD,AGSTCNVT                                         
         AP    GSTTAXT,GSTTAX             TOTAL OF ALL GST'S                    
         UNPK  GSTTAXC,GSTTAX                                                   
         OI    GSTTAXC+L'GSTTAXC-1,X'F0'  POSTIVE TAX FIELD                     
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*********************************************************************           
* PROCESS CT HST TAX RECORD                                         *           
*********************************************************************           
         USING CTD,R3                                                           
CTHST    NTR1  ,                   CT HST TAX RECORD                            
         GOTO1 PACKIT,DMCB,CTD,AHSTCNVT                                         
         AP    HSTTAXT,HSTTAX             TOTAL OF ALL HST'S                    
         UNPK  HSTTAXC,HSTTAX                                                   
         OI    HSTTAXC+L'HSTTAXC-1,X'F0'  POSTIVE TAX FIELD                     
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*********************************************************************           
* PROCESS CT PST TAX RECORD                                         *           
*********************************************************************           
         USING CTD,R3                                                           
CTPST    NTR1  ,                   CT QST TAX RECORD                            
         GOTO1 PACKIT,DMCB,CTD,APSTCNVT                                         
         AP    PSTTAXT,PSTTAX             TOTAL OF ALL PST'S                    
         UNPK  PSTTAXC,PSTTAX                                                   
         OI    PSTTAXC+L'PSTTAXC-1,X'F0'  POSTIVE TAX FIELD                     
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*********************************************************************           
* PROCESS CT QST TAX RECORD                                         *           
*********************************************************************           
         USING CTD,R3                                                           
CTQST    NTR1  ,                   CT QST TAX RECORD                            
         GOTO1 PACKIT,DMCB,CTD,AQSTCNVT                                         
         AP    QSTTAXT,QSTTAX             TOTAL OF ALL QST'S                    
         UNPK  QSTTAXC,QSTTAX                                                   
         OI    QSTTAXC+L'QSTTAXC-1,X'F0'  POSTIVE TAX FIELD                     
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*********************************************************************           
* PROCESS TD TOTAL DUE RECORD                                       *           
*********************************************************************           
         USING TDD,R3                                                           
TDDUE    NTR1  ,                   CT QST TAX RECORD                            
         GOTO1 PACKIT,DMCB,TDD,ATDDCNVT                                         
         UNPK  TOTDUEC,TOTDUE                                                   
         LA    R1,TOTDUEC+L'TOTDUEC-1                                           
         GOTOR FIXNEG                                                           
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*********************************************************************           
* PROCESS AMOUNT DUE RECORD                                         *           
*********************************************************************           
         USING ADD,R3                                                           
ADR      NTR1  ,                   AMOUNT DUE RECORD                            
         GOTO1 PACKIT,DMCB,ADD,AADCNVT                                          
         MVC   BASIS,ADBAS                                                      
         SP    BASAMT,TOTTAX                                                    
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*********************************************************************           
* PROCESS USER FIELD DATA FOR ZENITH                                *           
*********************************************************************           
         USING UDD,R3                                                           
         USING MITD,R4                                                          
THUFRT   NTR1                                                                   
         L     R4,AMITTAB          GET MOS FOR BILL                             
*                                                                               
         USING THUFTABD,R1                                                      
         LA    R1,THUFTAB          USER FIELD MATCH TABLE                       
THUF10   CLI   0(R1),EOT           END OF TABLE?                                
         BE    THUFX               . YES                                        
         CLC   THUFTYPE,MITTYP     MATCH ON TYPE?                               
         BNE   THUF20              . NO                                         
         CLC   THUFCODE,UDLOC      MATCH ON USER FIELD CODE?                    
         BNE   THUFX               . NO                                         
         MVC   THNUFDAT,UDTXT      . YES, GET DATA                              
         B     THUFX                                                            
                                                                                
THUF20   LA    R1,THUFLNQ(R1)                                                   
         B     THUF10                                                           
                                                                                
THUFX    J     XIT                                                              
         DROP  R1,R3,R4                                                         
                                                                                
THUFTAB  DC    C'T',C'E1'                                                       
         DC    C'I',C'E2'                                                       
         DC    C'M',C'E1'        FOR CABLE/SYNDICATION/OTHER?                   
         DC    C'U',C'E1'        CUT-INS                                        
         DC    C'S',C'E1'        COPY SPLITS                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
**********************************************************************          
* ALLOCATE COMMISSION TO EACH LINE                                   *          
* CALCULATE LINE TOTALS                                              *          
**********************************************************************          
LINT     NTR1  ,                                                                
         ZAP   CALCDUE,PZERO                                                    
         ZAP   BIGCOM,PZERO                                                     
         OC    INVNUMB,INVNUMB     NO INVOICE                                   
         JZ    XIT                                                              
         L     R4,ALINTAB          ANY DETAILS ?                                
         CLI   0(R4),EOT                                                        
         JE    LINT7               NO, SKIP IT                                  
*        JE    XIT                 NO, SKIP IT                                  
         USING LIND,R4                                                          
*                                                                               
LINT3    CLI   SYSTM,C'S'          WHAT SYSTEM ?                                
         BNE   *+8                 DO SPOT ROUTINE                              
         BAS   RE,LINTS                                                         
         CLI   SYSTM,C'P'                                                       
         BNE   *+8                                                              
         BAS   RE,LINTP            PRINT ROUTINE                                
*                                                                               
LINT5    DS    0H                                                               
         CLC   AGYALPHA,=C'OO'     OMDUSA?                                      
         BNE   LINT5A                                                           
         CLC   CLI,=C'ELO'         AND ELI LILLY?                               
         BNE   LINT5A                                                           
         CLI   SYSTM,C'S'          SPOT OR NET?                                 
         BNE   LINT5A                                                           
         B     LINT7               CAN'T DO ALLOCATION OF COMMISSION            
*                                  FOR SPOT/NET AS THEY USE MANUAL              
*                                  BILLING AND THIS CODE CAN'T HANDLE           
*                                  ELI LILLY DOESN'T CARE ABOUT                 
*                                  THIS COMMISSION ANYWAY (I HOPE)              
*                                  SO PROCESS AS IF BASIS WAS GROSS             
*                                                                               
*                                                                               
LINT5A   CLC   AGYALPHA,=C'FR'     MTOMC?                                       
         BNE   LINT5P                                                           
         CLC   CLI,=C'SHL'         AND SHELL?                                   
         BNE   LINT5P                                                           
         CLI   SYSTM,C'S'          SPOT OR NET?                                 
         BNE   LINT5P                                                           
         B     LINT7               CAN'T DO ALLOCATION OF COMMISSION            
*                                  SO PROCESS AS IF BASIS WAS GROSS             
*                            NOTE- I MAY ENCOUNTER EXCHANGE BILLS               
*                                  WITH ZERO NET                                
*                                  AS THEY ONLY REPORT AMOUNT DUE               
*                                  I CAN SKIP ATTEMPT TO ALLOCATE               
*                                  COMMISSION                                   
*                                                                               
LINT5P   CLI   BASIS,C'G'          BASIS = GROSS                                
         BE    LINT7                                                            
         CP    CALCDUE,DUEAMT      CALCULATE VS. BILL AMOUNT DUE                
         BE    LINT7                                                            
*                                                                               
         CLC   AGYALPHA,=C'H7'     MSMC1                                        
         BNE   LINT5P3                                                          
         CLC   CLI,=C'SHL'         SHELL                                        
         BNE   LINT5P3                                                          
         CLI   BASIS,C'2'          NET-CD                                       
         BE    LINT7               SKIP COMMISSION CALCULATION                  
         B     LINT7               ALWAYS SKIP                                  
*                                  THEY NOW USE TRADE                           
*                                  CALCULATION WON'T WORK FOR TRADE             
*                                  PROBLEMS ARISE IF ONLY CD                    
*                                  IS BEING BILLED FOR AN INS.                  
LINT5P3  ZAP   DUB,DUEAMT                                                       
         SP    DUB,CALCDUE         GET THE DIFFERENCE                           
         CP    DUB,=P'15'          ALLOW 15 CENTS FOR ROUNDING                  
         BNH   *+6                                                              
         DC    H'0'                BIG ROUNDING DIFFERENCE                      
         CP    DUB,=P'-15'                                                      
         BNL   *+6                                                              
         DC    H'0'                                                             
         L     R4,ABIGLINE         GET THE LARGEST AMOUNT                       
         AP    LINCOM,DUB          ADD DIFFERENCE TO COMMISSION                 
         AP    LINDUE,DUB          AND AMOUNT DUE                               
*                                                                               
LINT7    UNPK  CDUENET,DUEAMT                                                   
         LA    R1,CDUENET+L'CDUENET-1                                           
         GOTOR FIXNEG                                                           
         MVC   CDUENETL+3(15),CDUENET                                           
         MVC   CDUENETL(3),=C'0000'      LONG $ FOR AMT SEGMENT RECS            
*                                                                               
         UNPK  CDUECOM,COMAMT                                                   
         LA    R1,CDUECOM+L'CDUECOM-1                                           
         GOTOR FIXNEG                                                           
         UNPK  CDUENETP,DUEAMT     MAKE A POSITIVE AMOUNT                       
         OI    CDUENETP+L'CDUENET-1,X'F0'                                       
*                            CDUENTLP - LONG-USED IN SOME AMT                   
         MVC   CDUENTLP(3),=C'000'                                              
         MVC   CDUENTLP+3(15),CDUENETP                                          
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
         USING LIND,R4                                                          
LINTS    ST    RE,SAVRE            ** SPOT ROUTINE **                           
LINTS3   CLI   BASIS,C'N'          BASIS = NET                                  
         BNE   LINTS5                                                           
         CP    COMAMT,PZERO                                                     
         BE    LINTS7                                                           
         CP    BASAMT,PZERO                                                     
         BNE   LINTS4                                                           
         CLC   AGYALPHA,=C'H7'     MINDSHARE?                                   
         BNE   LINTS7                                                           
*                                  (AT LEAST FOR THEIR BURGER KING              
*                                  NET CLIENTS)                                 
         CP    LINTAX,PZERO        ALSO CHECK FOR TAX                           
         BE    LINTS7                                                           
*                                                                               
LINTS4   ZAP   PL16,LINNET         SET FOR BASIS = N                            
         CLC   AGYALPHA,=C'H7'     MINDSHARE?                                   
         BNE   *+10                                                             
         AP    PL16,LINTAX         THEY INCLUDE TAX                             
*                                  (AT LEAST FOR THEIR BURGER KING              
*                                  NET CLIENTS)                                 
         MP    PL16,COMAMT         X COMMISSION                                 
         SRP   PL16,3,0                                                         
         ZAP   DUB2,BASAMT         NOTE: BASAMT HAS TAX TAKEN OUT               
         CLC   AGYALPHA,=C'H7'     MINDSHARE?                                   
         BNE   *+10                                                             
         AP    DUB2,TOTTAX         THEY INCLUDE TAX                             
*                                  (AT LEAST FOR THEIR BURGER KING              
*                                  NET CLIENTS)                                 
*                                                                               
******   DP    PL16,BASAMT         DIVIDE BY BASIS                              
         DP    PL16,DUB2           DIVIDE BY BASIS                              
         SRP   PL16(L'PL16-L'BASAMT),64-3,5   % ROUNDED TO 2DP                  
         ZAP   LINCOM,PL16(L'PL16-L'BASAMT)   SAVE COMMISSION                   
         B     LINTS7                                                           
*                                                                               
LINTS5   CLI   BASIS,C'G'          BASIS = GROSS                                
         BE    *+6                                                              
         DC    H'0'                BASIS ?                                      
         SP    LINGRS,LINTAX       TAKE TAX OUT OF GROSS                        
         ZAP   LINCOM,LINGRS       GROSS                                        
         SP    LINCOM,LINNET       LESS NET = COM                               
*                                                                               
LINTS7   ZAP   LINDUE,LINNET       NET                                          
         AP    LINDUE,LINTAX       PLUS TAX                                     
         AP    LINDUE,LINCOM       PLUS COMM = AMOUNT DUE                       
         AP    CALCDUE,LINDUE      ADD TO CALCULATED DUE                        
         ZAP   DUB,LINCOM          GET BIGGEST COMMISSION                       
         OI    DUB+7,X'0F'                                                      
         CP    DUB,BIGCOM                                                       
         BNH   *+14                                                             
         ST    R4,ABIGLINE          SAVE ADDRESS OF BIGGEST NUMBER              
         ZAP   BIGCOM,DUB           AND BIGGEST NUMBER                          
         LA    R4,LINLNQ(R4)        DO NEXT LINE ITEM                           
         CLI   0(R4),EOT                                                        
         BNE   LINTS3                                                           
         L     RE,SAVRE                                                         
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
         USING LIND,R4                                                          
LINTP    ST    RE,SAVRE            ** PRINT ROUTINE **                          
         ZAP   COMAMT,DUEAMT       COMMISSION IS AMOUNT DUE                     
         SP    COMAMT,TOTNET       LESS THE NETS                                
**N-CD                                                                          
         CLI   BASIS,C'2'          NET-CD                                       
         BNE   *+10                                                             
         AP    COMAMT,TOTCSD       ADD IT BACK IN                               
**N-CD                                                                          
LINTP5   CP    COMAMT,PZERO                                                     
         BE    LINTP7                                                           
         ZAP   PL16,LINNET         NET                                          
**N-CD                                                                          
         CLI   BASIS,C'2'          NET-CD                                       
         BNE   *+10                                                             
         SP    PL16,LINCSD                                                      
**N-CD                                                                          
         AP    PL16,LINTAX                                                      
         MP    PL16,COMAMT         X COMMISSION                                 
         SRP   PL16,3,0                                                         
*                                                                               
         DP    PL16,BASAMT         DIVIDE BY BASIS                              
         SRP   PL16(L'PL16-L'BASAMT),64-3,5   % ROUNDED TO 2DP                  
         ZAP   LINCOM,PL16(L'PL16-L'BASAMT)   SAVE COMMISSION                   
*                                                                               
LINTP7   ZAP   LINDUE,LINNET       NET                                          
*                                                                               
         CLI   BASIS,C'2'          NET-CD?                                      
         BNE   *+10                                                             
         SP    LINDUE,LINCSD                                                    
*                                                                               
         AP    LINDUE,LINTAX       PLUS TAX                                     
         AP    LINDUE,LINCOM       PLUS COMM = AMOUNT DUE                       
         AP    CALCDUE,LINDUE      ADD TO CALCULATED DUE                        
         ZAP   DUB,LINCOM          GET BIGGEST COMMISSION                       
         OI    DUB+7,X'0F'                                                      
         CP    DUB,BIGCOM                                                       
         BNH   *+14                                                             
         ST    R4,ABIGLINE          SAVE ADDRESS OF BIGGEST NUMBER              
         ZAP   BIGCOM,DUB           AND BIGGEST NUMBER                          
         LA    R4,LINLNQ(R4)        DO NEXT LINE ITEM                           
         CLI   0(R4),EOT                                                        
         BNE   LINTP5                                                           
         L     RE,SAVRE                                                         
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
*********************************************************************           
* GENERATE EDI RECORDS FROM LINE ITEM                               *           
*  NTRY R3=A(EDI MAP RECORD)                                        *           
*********************************************************************           
         USING MAPD,R3                                                          
PUTR     NTR1  ,                                                                
         MVI   SKIPREC,C'N'        DON'T SKIP RECORD                            
         XC    RTNADR,RTNADR        INITIALIZE RETURN ADDRESS                   
         XC    PREVADR,PREVADR      INITIALIZE RETURN ADDRESS                   
         TM    FCS,FCSEDI          TEST EDI FILE OPEN                           
         BO    PUTR10                                                           
*                                                                               
         GOTO1 DYNALLOC,DMCB,(0,=C'EDIMAP  '),MAPNME                            
         OPEN  (EDIMAP,(OUTPUT))                                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    FCS,FCSEDI                                                       
*                                                                               
PUTR10   L     R0,AMAP             CLEAR IO TO SPACES                           
         LA    R1,L'MAP                                                         
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,AMAP                                                          
         USING EDIHDRD,R2                                                       
         XC    EDILN(4),EDILN                                                   
         MVC   EDISET,=C'810'       SET                                         
         MVC   EDIZRO,ZEROS                                                     
         LA    R2,EDISEG-EDILN(R2)                                              
*                                                                               
PUTR20   CLI   MAPTYP,BEGQ         TEST BEGIN LOOP                              
         BNE   PUTR30                                                           
         OC    RTNADR,RTNADR       IS THIS BEGINING OF 2ND LOOP                 
         BZ    *+16                NO                                           
         MVC   PREVADR,RTNADR      YES, SAVE CURRENT IN PREVIOUS                
         MVC   PRTNROU,RTNROU                                                   
         ST    R3,RTNADR           SAVE START OF LOOP                           
         B     PUTR40                                                           
*                                                                               
PUTR30   CLI   MAPTYP,ROUQ         IS THERE A ROUTINE ?                         
         BNE   PUTR50                                                           
PUTR40   SR    R1,R1                                                            
         ICM   R1,3,MAPROUT        R1=ROUTINE NUMBER                            
         GOTO1 CONROUT             GO TO SPECIAL ROUTINE                        
         LA    R3,MAPRLNQ(R3)                                                   
         B     PUTR80                                                           
*                                                                               
PUTR50   SR    R5,R5                                                            
         ICM   R5,3,MAPFLD                                                      
         AR    R5,R2               R5=A(OUTPUT FIELD)                           
         SR    R6,R6                                                            
         ICM   R6,3,MAPLEN         R6=LENGTH OF DATA                            
*                                                                               
         CLI   MAPTYP,CONQ         IS DATA A CONSTANT ?                         
         BNE   PUTR60                                                           
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),MAPCON      MOVE CONSTANT TO OUTPUT                      
         LA    RF,MAPCON-MAPD+1(R6)                                             
         AR    R3,RF                                                            
         B     PUTR80                                                           
*                                                                               
PUTR60   CLI   MAPTYP,LWSQ         IS DATA IN LOCAL STORAGE ?                   
         BE    PUTR70                                                           
         CLI   MAPTYP,ENDQ         ARE WE ENDING OUTER LOOP                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,1(R3)                                                         
         CLI   PRTNROU,C'Y'        TEST RETURN REQUESTED                        
         JNE   PUTR100                                                          
         L     R3,PREVADR          PROCESS OUTER LOOP                           
         MVC   RTNROU,PRTNROU      RESTORE SWITCH                               
         XC    RTNADR,RTNADR                                                    
         B     PUTR10                                                           
*                                                                               
PUTR70   SR    RF,RF                                                            
         ICM   RF,3,MAPDATA        RF=DISPLACEMENT TO DATA                      
         LA    RF,LWSD(RF)                                                      
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(RF)        MOVE DATA FROM STORAGE                      
         LA    R3,MAPLNQ(R3)                                                    
*                                                                               
PUTR80   CLI   0(R3),EORQ          TEST END OF RECORD                           
         BNE   PUTR20                                                           
         L     R2,AMAP                                                          
         MVC   0(2,R2),1(R3)       SET RECORD LENGTH                            
*                                                                               
         CLI   SKIPREC,C'Y'        TEST SKIPPING THIS RECORD                    
         BE    PUTR90                                                           
         PUT   EDIMAP,(R2)                                                      
*                                                                               
PUTR90   MVI   SKIPREC,C'N'                                                     
         LA    R3,3(R3)                                                         
         CLI   0(R3),ENDQ          TEST END OF LOOP                             
         BNE   PUTR110                                                          
         LA    R3,1(R3)                                                         
         CLI   RTNROU,C'Y'         TEST RETURN REQUESTED                        
         BNE   PUTR100                                                          
         L     R3,RTNADR           PROCESS NEXT                                 
         XC    RTNADR,RTNADR                                                    
         B     PUTR10                                                           
PUTR100  XC    RTNADR,RTNADR                                                    
*                                                                               
PUTR110  CLI   0(R3),EOT           TEST END OF TABLE                            
         BNE   PUTR10                                                           
         LA    R3,1(R3)                                                         
         CLI   RTNROU,C'Y'         TEST RETURN REQUESTED                        
         JNE   PUTRX                                                            
         L     R3,RTNADR           PROCESS NEXT                                 
         B     PUTR10                                                           
PUTRX    J     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*********************************************************************           
* ROUTINE TO CONVERT CHARACTER AMOUNTS TO PACKED                    *           
*  PARM 1 -  A(TOTAL RECORD)                                        *           
*       2 -  A(CONVERSION TABLE)                                    *           
*********************************************************************           
PACKIT   NTR1  ,                                                                
         LM    R3,R4,0(R1)                                                      
*                                                                               
PACKIT3  ZAP   DUB,PZERO                                                        
         SR    R0,R0                                                            
         IC    R0,0(R4)            R0=MAXIMUM LENGTH OF DATA                    
         SR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         AR    RF,R3               RF=A(INPUT DATA)                             
         MVC   BYTE,0(RF)          SAVE THE FIRST BYTE                          
         CLI   BYTE,C'-'           TEST NEGATIVE NUMBER                         
         BNE   *+12                                                             
         SHI   R0,1                REDUCE MAXIMUM LENGTH                        
         LA    RF,1(RF)            SKIP THE SIGN                                
         LR    RE,RF               RE=A(FIRST DIGIT)                            
         SR    R1,R1                                                            
*                                                                               
PACKIT5  CLI   0(RF),C' '          TEST FOR THE END                             
         BE    PACKIT7                                                          
         AHI   R1,1                R1=NUMBER OF DIGITS                          
         LA    RF,1(RF)                                                         
         BCT   R0,PACKIT5                                                       
*                                                                               
PACKIT7  LTR   R1,R1                                                            
         BZ    PACKIT9             NO DATA - MAKE IT ZERO                       
         BCTR  R1,0                ADJUST LENGTH OF EX                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RE)                                                      
         CLI   BYTE,C'-'           TEST NEGATIVE                                
         BNE   *+10                                                             
         MP    DUB,=P'-1'                                                       
*                                                                               
PACKIT9  SR    RF,RF                                                            
         ICM   RF,3,2(R4)          RF=DISPLACEMENT TO PACKED FIELD              
         LA    RF,LWSD(RF)                                                      
         ZAP   0(BKLQ,RF),DUB      SAVE THE PACKED DATA                         
         LA    R4,L'STACNVT(R4)                                                 
         CLI   0(R4),EOT                                                        
         BNE   PACKIT3                                                          
         J     XIT                                                              
*                                                                               
EIXIT    BRAS  RE,EIOUT                                                         
XIT      XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
* CONSTANTS AND LITERAL POOL                                        *           
*********************************************************************           
                                                                                
AFLATAB  DC     A(FLATAB)          FLAT FILE TABLE                              
ASTACNVT DC     A(STACNVT)         STATION CONVERSION TABLE                     
APUBCNVT DC     A(PUBCNVT)         PUB CONVERSION TABLE                         
AADCNVT  DC     A(ADCNVT)          AMOUNT DUE CONVERSION TABLE                  
AGSTCNVT DC     A(GSTCNVT)         CTGST TAX  CONVERSION TABLE                  
AHSTCNVT DC     A(HSTCNVT)         CTHST TAX  CONVERSION TABLE                  
APSTCNVT DC     A(PSTCNVT)         CTPST TAX  CONVERSION TABLE                  
AQSTCNVT DC     A(QSTCNVT)         CTQST TAX  CONVERSION TABLE                  
ATDDCNVT DC     A(TDDCNVT)         TOTAL DUE  CONVERSION TABLE                  
AINSCNVT DC     A(INSCNVT)         ID INSERTIONS CONVERSION TABLE               
AAGYTAB  DC     A(AGYTAB)          AGENCY TABLE                                 
*                                                                               
ADIDPTAB DC     A(IDPTAB)          ID RECORD TABLE FOR PRINT                    
ADLINTAB DC     A(LINTAB)          LINE ITEM TABLE                              
ADMITTAB DC     A(MITTAB)          MONTH TABLE                                  
ADSORTAB DC     A(SORTAB)          INVOICE SORT TABLE                           
ADSORTER DC     V(SORTER)          SORTER                                       
PACKZ    DC     P'0'                                                            
*                                                                               
AOUT     DC     A(INP)                                                          
AMAP     DC     A(MAP)                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* FLAT FILE CONTROL TABLE - SEE FFRD                                 *          
**********************************************************************          
FLATAB   DS    0X                                                               
*                                  HEADER                                       
         DC    AL2(IHX-*),C'IH'                                                 
         DC    AL1(0),AL2(0,IHR-EDI)                                            
IHX      DS    0X                                                               
                                                                                
*                                  PREVIOUS INVOICE                             
         DC    AL2(PIX-*),C'PI'                                                 
         DC    AL1(0),AL2(0,PINV-EDI)                                           
PIX      DS    0X                                                               
                                                                                
*                                  ADVERTISER                                   
         DC    AL2(AVX-*),C'AV'                                                 
         DC    AL1(1+CNDSQ),AL2(SYSTM-LWSD),C'S'                                
         DC    AL1(L'AVCODE),AL2(ADVCODE-LWSD,AVCODE-AVD)                       
         DC    AL1(L'AVSNAME),AL2(ADVNAME-LWSD,AVSNAME-AVD)                     
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(1+CNDSQ),AL2(SYSTM-LWSD),C'P'                                
         DC    AL1(L'AVCODE),AL2(ADVCODE-LWSD,AVCODE-AVD)                       
         DC    AL1(L'AVPNAME),AL2(ADVNAME-LWSD,AVSNAME-AVD)                     
         DC    AL1(CONDQ)                                                       
AVX      DS    0X                                                               
                                                                                
*                                  AGENCY NAME                                  
         DC    AL2(AGX-*),C'AG'                                                 
         DC    AL1(L'AGYNAM33),AL2(AGYNAM33-LWSD,AGNAME-AGD)                    
AGX      DS    0X                                                               
                                                                                
*                                  MEDIA                                        
         DC    AL2(MEX-*),C'ME'                                                 
         DC    AL1(L'MEDIA),AL2(MEDIA-LWSD,MEMEDIA-MED)                         
MEX      DS    0X                                                               
                                                                                
*                                  BILL TO ADDRESS                              
         DC    AL2(BTX-*),C'BT'                                                 
         DC    AL1(L'BADDR1),AL2(BADDR1-LWSD,BTADDR1-BTD)                       
         DC    AL1(L'BADDR2),AL2(BADDR2-LWSD,BTADDR2-BTD)                       
         DC    AL1(L'BADDR3),AL2(BADDR3-LWSD,BTADDR3-BTD)                       
         DC    AL1(L'BADDR4),AL2(BADDR4-LWSD,BTADDR4-BTD)                       
         DC    AL1(L'BADDR5),AL2(BADDR5-LWSD,BTADDR5-BTD)                       
BTX      DS    0X                                                               
                                                                                
*                                  PRODUCT CODES                                
         DC    AL2(PRX-*),C'PR'                                                 
         DC    AL1(L'PRCODE1),AL2(PRCODE1-LWSD,PRCODE-PRD)                      
         DC    AL1(L'PRNAME1),AL2(PRNAME1-LWSD,PRNAME-PRD)                      
PRX      DS    0X                                                               
                                                                                
*                                ESTIMATE CODES                                 
         DC    AL2(ESX-*),C'ES'                                                 
         DC    AL1(L'ESCODE1),AL2(ESCODE1-LWSD,ESCODE-ESD)                      
         DC    AL1(L'ESNAME1),AL2(ESNAME1-LWSD,ESNAME-ESD)                      
                                                                                
         DC    AL1(1+CNDSQ),AL2(SYSTM-LWSD),C'P'                                
         DC    AL1(L'ESNAME2),AL2(ESNAME2-LWSD,ESPNME2-ESD)                     
         DC    AL1(CONDQ)                                                       
ESX      DS    0X                                                               
                                                                                
*                                  PRODUCT GROUP 1                              
         DC    AL2(P1X-*),C'P1'                                                 
         DC    AL1(L'PCODE1),AL2(PCODE1-LWSD,P1CODE-P1D)                        
P1X      DS    0X                                                               
                                                                                
*                                  MARKET RECORD                                
         DC    AL2(MKX-*),C'MK'                                                 
         DC    AL1(L'MRKTNUM),AL2(MRKTNUM-LWSD,MKCODE-MKD)                      
         DC    AL1(L'MRKTNAM),AL2(MRKTNAM-LWSD,MKNAMES-MKD)                     
MKX      DS    0X                                                               
                                                                                
*                                  DISTRICT RECORD                              
         DC    AL2(M2X-*),C'M2'                                                 
         DC    AL1(8+CONDQ),AL2(M2LVLD-M2D),CL8'DISTRICT'                       
         DC    AL1(L'DISTRICT),AL2(DISTRICT-LWSD,M2CODE-M2D)                    
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(L'M2NAMES),AL2(OMN1NAME-LWSD,M2NAMES-M2D)                    
         DC    AL1(CONDQ)                                                       
M2X      DS    0X                                                               
                                                                                
*                                  PUBLICATION RECORD                           
         DC    AL2(PBX-*),C'PB'                                                 
         DC    AL1(L'PUB),AL2(PUB-LWSD,PBNAME-PBD)                              
PBX      DS    0X                                                               
                                                                                
*                                ID INSERTION RECORD                            
         DC    AL2(IDX-*),C'ID'                                                 
         DC    AL1(1+CNDSQ),AL2(SYSTM-LWSD),C'P'                                
         DC    AL1(0),AL2(0,IDRPP-EDI)                                          
         DC    AL1(CONDQ)                                                       
IDX      DS    0X                                                               
                                                                                
*                                  STATION RECORD                               
         DC    AL2(STX-*),C'ST'                                                 
         DC    AL1(L'STATION),AL2(STATION-LWSD,STCODE-STD)                      
STX      DS    0X                                                               
                                                                                
*                                  MONTHLY TOTAL RECORD                         
         DC    AL2(MTX-*),C'MT'                                                 
         DC    AL1(1+CNDSQ),AL2(SYSTM-LWSD),C'S'                                
         DC    AL1(3+CONDQ),AL2(MTDLVN-MTD),CL3'STA'                            
         DC    AL1(0),AL2(0,MTRSS-EDI)                                          
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(1+CNDSQ),AL2(SYSTM-LWSD),C'S'                                
         DC    AL1(3+CONDQ),AL2(MTDLVN-MTD),CL3'INV'                            
         DC    AL1(0),AL2(0,MTRSI-EDI)                                          
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(1+CNDSQ),AL2(SYSTM-LWSD),C'P'                                
         DC    AL1(3+CONDQ),AL2(MTDLVN-MTD),CL3'PUB'                            
         DC    AL1(0),AL2(0,MTRPP-EDI)                                          
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(1+CNDSQ),AL2(SYSTM-LWSD),C'P'                                
         DC    AL1(3+CONDQ),AL2(MTDLVN-MTD),CL3'INV'                            
         DC    AL1(0),AL2(0,MTRPI-EDI)                                          
         DC    AL1(CONDQ)                                                       
MTX      DS    0X                                                               
                                                                                
*                                  MONTHLY TOTAL RECORD CANADA                  
         DC    AL2(NTX-*),C'NT'                                                 
         DC    AL1(1+CNDSQ),AL2(SYSTM-LWSD),C'S'                                
         DC    AL1(0),AL2(0,MTRSS-EDI)                                          
         DC    AL1(CONDQ)                                                       
NTX      DS    0X                                                               
                                                                                
*                                  CANADIAN TAX RECORD                          
         DC    AL2(CTX-*),C'CT'                                                 
         DC    AL1(3+CONDQ),AL2(CTLVLN-CTD),CL3'GST'                            
         DC    AL1(0),AL2(0,CTGST-EDI)                                          
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(3+CONDQ),AL2(CTLVLN-CTD),CL3'TPS'                            
         DC    AL1(0),AL2(0,CTGST-EDI)                                          
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(3+CONDQ),AL2(CTLVLN-CTD),CL3'HST'                            
         DC    AL1(0),AL2(0,CTHST-EDI)                                          
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(3+CONDQ),AL2(CTLVLN-CTD),CL3'PST'                            
         DC    AL1(0),AL2(0,CTPST-EDI)                                          
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(3+CONDQ),AL2(CTLVLN-CTD),CL3'QST'                            
         DC    AL1(0),AL2(0,CTQST-EDI)                                          
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(3+CONDQ),AL2(CTLVLN-CTD),CL3'TVQ'                            
         DC    AL1(0),AL2(0,CTQST-EDI)                                          
         DC    AL1(CONDQ)                                                       
CTX      DS    0X                                                               
                                                                                
*                                  TOTAL DUE RECORD                             
         DC    AL2(TDX-*),C'TD'                                                 
         DC    AL1(0),AL2(0,TDDUE-EDI)                                          
         DC    AL1(CONDQ)                                                       
TDX      DS    0X                                                               
                                                                                
*                                  AMOUNT DUE RECORD                            
         DC    AL2(ADX-*),C'AD'                                                 
         DC    AL1(3+CONDQ),AL2(ADLVLN-ADD),CL3'INV'                            
         DC    AL1(0),AL2(0,ADR-EDI)                                            
         DC    AL1(CONDQ)                                                       
ADX      DS    0X                                                               
                                                                                
*                                  USER DATA RECORD                             
         DC    AL2(UDX-*),C'UD'                                                 
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'P1'                              
         DC    AL1(L'REGION),AL2(REGION-LWSD,UDTXT-UDD)                         
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'P1'                              
         DC    AL1(13+CONDQ),AL2(UDDES-UDD),CL13'KBKS/MATERIAL'                 
         DC    AL1(L'MCRGN),AL2(MCRGN-LWSD,UDTXT-UDD)                           
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'P1'                              
         DC    AL1(09+CONDQ),AL2(UDDES-UDD),CL09'DEPT CODE'                     
         DC    AL1(L'PZDCODE),AL2(PZDCODE-LWSD,UDTXT-UDD)                       
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'P1'                              
         DC    AL1(12+CONDQ),AL2(UDDES-UDD),CL12'COST CNTR/GL'                  
         DC    AL1(L'M2SHLP1),AL2(M2SHLP1-LWSD,UDTXT-UDD)                       
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'P2'                              
         DC    AL1(08+CONDQ),AL2(UDDES-UDD),CL08'EXP CODE'                      
         DC    AL1(L'PZECODE),AL2(PZECODE-LWSD,UDTXT-UDD)                       
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(4+CONDQ),AL2(UDDES-UDD),CL04'SWBS'                           
         DC    AL1(L'M2SHLE1),AL2(M2SHLE1-LWSD,UDTXT-UDD)                       
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(9+CONDQ),AL2(UDDES-UDD),CL09'PROJECT #'                      
         DC    AL1(L'PZPJT#),AL2(PZPJT#-LWSD,UDTXT-UDD)                         
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(3+CONDQ),AL2(UDDES-UDD),CL3'PO#'                             
         DC    AL1(L'PO#),AL2(PO#-LWSD,UDTXT-UDD)                               
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(6+CONDQ),AL2(UDDES-UDD),CL6'D.O. #'                          
         DC    AL1(L'PO#),AL2(PO#-LWSD,UDTXT-UDD)                               
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(3+CONDQ),AL2(UDDES-UDD),CL3'DO#'                             
         DC    AL1(L'DELLPO#),AL2(DELLPO#-LWSD,UDTXT-UDD)                       
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(5+CONDQ),AL2(UDDES-UDD),CL5'P O #'                           
         DC    AL1(L'ELPO#),AL2(ELPO#-LWSD,UDTXT-UDD)                           
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(4+CONDQ),AL2(UDDES-UDD),CL4'PO #'                            
         DC    AL1(L'ELPO#),AL2(ELPO#-LWSD,UDTXT-UDD)                           
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(16+CONDQ),AL2(UDDES-UDD),CL16'PURCHASE ORDER #'              
         DC    AL1(L'SHELLPO#),AL2(SHELLPO#-LWSD,UDTXT-UDD)                     
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E2'                              
         DC    AL1(16+CONDQ),AL2(UDDES-UDD),CL16'PURCHASE ORDER #'              
         DC    AL1(L'PFZPO#),AL2(PFZPO#-LWSD,UDTXT-UDD)                         
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(9+CONDQ),AL2(UDDES-UDD),CL9'COMM ONLY'                       
         DC    AL1(L'MCPREP),AL2(MCPREP-LWSD,UDTXT-UDD)                         
         DC    AL1(L'MCTEXT),AL2(MCTEXT-LWSD,UDTXT+2-UDD)                       
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(1+CNDSQ),AL2(MEDIA-LWSD),C'S'                                
         DC    AL1(3+CNDSQ),AL2(CLI-LWSD),C'003'                                
         DC    AL1(L'THNUFDAT),AL2(THNUFDAT-LWSD,UDTXT-UDD)                     
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(1+CNDSQ),AL2(MEDIA-LWSD),C'P'                                
         DC    AL1(3+CNDSQ),AL2(CLI-LWSD),C'003'                                
         DC    AL1(L'THNUFDAT),AL2(THNUFDAT-LWSD,UDTXT-UDD)                     
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(1+CNDSQ),AL2(MEDIA-LWSD),C'N'                                
         DC    AL1(3+CNDSQ),AL2(CLI-LWSD),C'003'                                
         DC    AL1(0),AL2(0,THUFRT-EDI)                                         
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E2'                              
         DC    AL1(1+CNDSQ),AL2(MEDIA-LWSD),C'N'                                
         DC    AL1(3+CNDSQ),AL2(CLI-LWSD),C'003'                                
         DC    AL1(0),AL2(0,THUFRT-EDI)                                         
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(4+CONDQ),AL2(UDDES-UDD),CL4'ATTN'                            
         DC    AL1(L'OMNUFDAT),AL2(OMNUFDAT-LWSD,UDTXT-UDD)                     
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(14+CONDQ),AL2(UDDES-UDD),CL14'ATTENTION NAME'                
         DC    AL1(L'OMNUFDAT),AL2(OMNUFDAT-LWSD,UDTXT-UDD)                     
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(11+CONDQ),AL2(UDDES-UDD),CL11'CURRENT IO#'                   
         DC    AL1(L'MCION6),AL2(MCION6-LWSD,UDTXT-UDD)                         
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'      2006                    
         DC    AL1(07+CONDQ),AL2(UDDES-UDD),CL07'2006 IO'                       
         DC    AL1(L'MCION6),AL2(MCION6-LWSD,UDTXT-UDD)                         
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E2'      2005                    
         DC    AL1(07+CONDQ),AL2(UDDES-UDD),CL07'2005 IO'                       
         DC    AL1(L'MCION5),AL2(MCION5-LWSD,UDTXT-UDD)                         
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E2'                              
         DC    AL1(8+CONDQ),AL2(UDDES-UDD),CL8'G/L ACCT'                        
         DC    AL1(L'OMACCT),AL2(OMACCT-LWSD,UDTXT-UDD)                         
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(6+CONDQ),AL2(UDDES-UDD),CL6'MAR #-'                          
         DC    AL1(L'H9NETE1),AL2(H9NETE1-LWSD,UDTXT-UDD)                       
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E2'                              
         DC    AL1(L'EUDEF2),AL2(EUDEF2-LWSD,UDTXT-UDD)                         
         DC    AL1(CONDQ)                                                       
                                                                                
UDX      DS    0X                                                               
                                                                                
*                                  USER COMMENT RECORD                          
         DC    AL2(UCX-*),C'UC'                                                 
         DC    AL1(2+CONDQ),AL2(UCLOC-UCD),CL2'M1'                              
         DC    AL1(3+CONDQ),AL2(UCDES-UCD),CL3'MAR'                             
         DC    AL1(L'USRMRKT),AL2(USRMRKT-LWSD,UCTXT-UCD)                       
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UCLOC-UCD),CL2'D1'                              
         DC    AL1(3+CONDQ),AL2(UCDES-UCD),CL3'MAR'                             
         DC    AL1(L'USRMRKT),AL2(USRMRKT-LWSD,UCTXT-UCD)                       
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UCLOC-UCD),CL2'D1'                              
         DC    AL1(15+CONDQ),AL2(UCDES-UCD),CL15'REGION/DISTRICT'               
         DC    AL1(L'USRMRKT),AL2(USRMRKT-LWSD,UCTXT-UCD)                       
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UCLOC-UCD),CL2'E1'                              
         DC    AL1(11+CONDQ),AL2(UCDES-UCD),CL11'G/L ACCOUNT'                   
         DC    AL1(L'MCGLA),AL2(MCGLA-LWSD,UCTXT-UCD)                           
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UCLOC-UCD),CL2'M1'                              
         DC    AL1(04+CONDQ),AL2(UCDES-UCD),CL04'ATTN'                          
         DC    AL1(L'OMUCATTN),AL2(OMUCATTN-LWSD,UCTXT-UCD)                     
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UCLOC-UCD),CL2'E1'                              
         DC    AL1(1+CNDSQ),AL2(SYSTM-LWSD),C'P'                                
         DC    AL1(16+CONDQ),AL2(UCDES-UCD),CL16'COST CENTER/SITE'              
         DC    AL1(L'OMN1NAME),AL2(OMN1NAME-LWSD,UCTXT-UCD)                     
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UCLOC-UCD),CL2'E1'                              
         DC    AL1(1+CNDSQ),AL2(SYSTM-LWSD),C'P'                                
         DC    AL1(12+CONDQ),AL2(UCDES-UCD),CL12'BILL TO CODE'                  
         DC    AL1(L'OMN1NAME),AL2(OMN1NAME-LWSD,UCTXT-UCD)                     
         DC    AL1(CONDQ)                                                       
                                                                                
UCX      DS    0X                                                               
                                                                                
*                                  COMMENT RECORD                               
         DC    AL2(CMX-*),C'CM'                                                 
CMX      DS    0X                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
**********************************************************************          
* STATION 'MONTHLY TOTAL' CONVERSION TABLE                           *          
**********************************************************************          
STACNVT  DS    0XL4                                                             
         DC    AL1(L'MTSORGRS,MTSORGRS-MTD),AL2(STORDGRS-LWSD)                  
         DC    AL1(L'MTSORNET,MTSORNET-MTD),AL2(STORDNET-LWSD)                  
         DC    AL1(L'MTSORTAX,MTSORTAX-MTD),AL2(STORDTAX-LWSD)                  
         DC    AL1(L'MTSPRGRS,MTSPRGRS-MTD),AL2(STPRVGRS-LWSD)                  
         DC    AL1(L'MTSPRNET,MTSPRNET-MTD),AL2(STPRVNET-LWSD)                  
         DC    AL1(L'MTSPRTAX,MTSPRTAX-MTD),AL2(STPRVTAX-LWSD)                  
         DC    AL1(EOT)                                                         
                                                                                
**********************************************************************          
* PUB 'MONTHLY TOTAL' CONVERSION TABLE                               *          
**********************************************************************          
PUBCNVT  DS    0XL4                                                             
         DC    AL1(L'MTPORGRS,MTPORGRS-MTD),AL2(PBORDGRS-LWSD)                  
         DC    AL1(L'MTPORNET,MTPORNET-MTD),AL2(PBORDNET-LWSD)                  
         DC    AL1(L'MTPORCSD,MTPORCSD-MTD),AL2(PBORDCSD-LWSD)                  
         DC    AL1(L'MTPORTAX,MTPORTAX-MTD),AL2(PBORDTAX-LWSD)                  
         DC    AL1(L'MTPORINS,MTPORINS-MTD),AL2(PBORDINS-LWSD)                  
         DC    AL1(L'MTPPRGRS,MTPPRGRS-MTD),AL2(PBPRVGRS-LWSD)                  
         DC    AL1(L'MTPPRNET,MTPPRNET-MTD),AL2(PBPRVNET-LWSD)                  
         DC    AL1(L'MTPPRCSD,MTPPRCSD-MTD),AL2(PBPRVCSD-LWSD)                  
         DC    AL1(L'MTPPRTAX,MTPPRTAX-MTD),AL2(PBPRVTAX-LWSD)                  
         DC    AL1(EOT)                                                         
                                                                                
**********************************************************************          
* 'GST TAX'    CONVERSION TABLE                                      *          
**********************************************************************          
GSTCNVT  DS    0XL4                                                             
         DC    AL1(L'CTTAXAMT,CTTAXAMT-CTD),AL2(GSTTAX-LWSD)                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
**********************************************************************          
* 'HST TAX'    CONVERSION TABLE                                      *          
**********************************************************************          
HSTCNVT  DS    0XL4                                                             
         DC    AL1(L'CTTAXAMT,CTTAXAMT-CTD),AL2(HSTTAX-LWSD)                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
**********************************************************************          
* 'PST TAX'    CONVERSION TABLE                                      *          
**********************************************************************          
PSTCNVT  DS    0XL4                                                             
         DC    AL1(L'CTTAXAMT,CTTAXAMT-CTD),AL2(PSTTAX-LWSD)                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
**********************************************************************          
* 'QST TAX'    CONVERSION TABLE                                      *          
**********************************************************************          
QSTCNVT  DS    0XL4                                                             
         DC    AL1(L'CTTAXAMT,CTTAXAMT-CTD),AL2(QSTTAX-LWSD)                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
**********************************************************************          
* TOTAL DUE    CONVERSION TABLE                                      *          
**********************************************************************          
TDDCNVT  DS    0XL4                                                             
         DC    AL1(L'TDTOTDUE,TDTOTDUE-TDD),AL2(TOTDUE-LWSD)                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
**********************************************************************          
* 'AMOUNT DUE' CONVERSION TABLE                                      *          
**********************************************************************          
ADCNVT   DS    0XL4                                                             
         DC    AL1(L'ADBASAMT,ADBASAMT-ADD),AL2(BASAMT-LWSD)                    
         DC    AL1(L'ADCOMAMT,ADCOMAMT-ADD),AL2(COMAMT-LWSD)                    
         DC    AL1(L'ADCOMAMT,ADCOMAMT-ADD),AL2(COMAMT1-LWSD)                   
         DC    AL1(L'ADDUEAMT,ADDUEAMT-ADD),AL2(DUEAMT-LWSD)                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
**********************************************************************          
* ID RECORD INSERTIONS CONVERSION TABLE                              *          
**********************************************************************          
INSCNVT  DS    0XL4                                                             
         DC    AL1(L'IDOGROSS,IDOGROSS-IDD),AL2(INORDGRS-LWSD)                  
         DC    AL1(L'IDPGROSS,IDPGROSS-IDD),AL2(INPRVGRS-LWSD)                  
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*********************************************************************           
* AGENCY TABLE -  AGENCY/SYSTEM/CLIENT/ROUTINES/OPTIONS- SEE AGCD    *          
*********************************************************************           
AGYTAB   DS     0CL(AGCLNQ)                                                     
* MEDIAVEST - DU *                                                              
*  MEDIAVEST/ALL/COKE                                                           
                                                                                
         DC     C'DU',C' ',C'002',AL2(MCCCH-EDI),AL1(0) COKE                    
                                                                                
* MEDIAVEST - H9 *                                                              
*  MEDIAVEST/ALL/COKE/PRINT                                                     
                                                                                
         DC     C'H9',C' ',C'002',AL2(MCCCH-EDI),AL1(0) COKE                    
         DC     C'H9',C' ',C'024',AL2(MCCCH-EDI),AL1(0) COKE                    
*                                                                               
*        O24 WAS ADDED IN ERROR ON B1A FORILFE FOR CG7                          
*        THE ENTRY ABOVE IS JUST ADDED TO PROCESS                               
*        SOME "BAD" WORKER FILES WITH THAT CODE                                 
*                                                                               
* STARCOM - H9 *                                                                
*  STARCOM/BURGER KING                                                          
                                                                                
         DC     C'H9',C' ',C'BBF',AL2(H9BK-EDI),AL1(0)                          
         DC     C'H9',C' ',C'BBV',AL2(H9BK-EDI),AL1(0)                          
         DC     C'H9',C' ',C'BBI',AL2(H9BK-EDI),AL1(0)                          
         DC     C'H9',C' ',C'BBN',AL2(H9BK-EDI),AL1(0)                          
         DC     C'H9',C' ',C'BKJ',AL2(H9BK-EDI),AL1(0)                          
         DC     C'H9',C' ',C'BKC',AL2(H9BK-EDI),AL1(0)                          
         DC     C'H9',C' ',C'BKI',AL2(H9BK-EDI),AL1(0)                          
         DC     C'H9',C' ',C'BKV',AL2(H9BK-EDI),AL1(0)                          
*                                                                               
*        THE ENTRY BELOW IS NEEDED AS THE NET BILLING PROGRAM                   
*        ERRONEOULY CHANGED BBN TO 225 IN THE WORKER FILE                       
*                                                                               
         DC     C'H9',C' ',C'225',AL2(H9BK-EDI),AL1(0)                          
                                                                                
*  STARCOM/DENNY'S                                                              
                                                                                
         DC     C'H9',C' ',C'ARD',AL2(H9DN-EDI),AL1(0)                          
                                                                                
* MEDIA FIRST - M1 *                                                            
*  MEDIA FIRST/BURGER KING                                                      
                                                                                
         DC     C'M1',C' ',C'BKA',AL2(M1SBK-EDI),AL1(0)                         
         DC     C'M1',C' ',C'BKB',AL2(M1SBK-EDI),AL1(0)                         
         DC     C'M1',C' ',C'BKC',AL2(M1SBK-EDI),AL1(0)                         
         DC     C'M1',C' ',C'BKI',AL2(M1SBK-EDI),AL1(0)                         
         DC     C'M1',C' ',C'BKJ',AL2(M1SBK-EDI),AL1(0)                         
         DC     C'M1',C' ',C'BKV',AL2(M1SBK-EDI),AL1(0)                         
*                                                                               
*        THE ENTRY BELOW IS NEEDED AS THE NET BILLING PROGRAM                   
*        ERRONEOULY CHANGED BKC TO 223 IN THE WORKER FILE                       
         DC     C'M1',C' ',C'223',AL2(M1SBK-EDI),AL1(0)                         
*                                                                               
*                                                                               
* DDB NEEDHAM - NE *                                                            
*  DDB/ALL/DELL                                                                 
                                                                                
*        DC     C'NE',C' ',C'005',AL2(NEDELL-EDI),AL1(0) DELL                   
*        DC     C'NE',C' ',C'DPC',AL2(NEDELL-EDI),AL1(0) DELL                   
*        DC     C'NE',C' ',C'001',AL2(NEDELL-EDI),AL1(0) DELL                   
                                                                                
* MINDSHARE  - H7 *                                                             
*  MINDSHARE/BURGER KING                                                        
                                                                                
         DC     C'H7',C' ',C'002',AL2(H7BK-EDI),AL1(0)                          
                                                                                
* MINDSHARE  - H7 *                                                             
*  MINDSHARE/IBM - NEW 4010 FORMAT MOVED FROM 3040 FORMAT                       
*  WHEN B1A PROFILE CLIENT IS IB4                                               
         DC     C'H7',C' ',C'IB4',AL2(H7IBM-EDI),AL1(0)                         
                                                                                
* YNR - YN *                                                                    
*  YNR/ALL/MEDIA EDGE/ATT                                                       
                                                                                
         DC     C'YN',C' ',C'CDA',AL2(YNATT-EDI),AL1(AGCME)                     
         DC     C'YN',C' ',C'CGA',AL2(YNATT-EDI),AL1(AGCME)                     
         DC     C'YN',C' ',C'MML',AL2(YNATT-EDI),AL1(AGCME)                     
         DC     C'YN',C' ',C'CHA',AL2(YNATT-EDI),AL1(AGCME)                     
         DC     C'YN',C' ',C'CBT',AL2(YNATT-EDI),AL1(AGCME)                     
                                                                                
*  YNR/ALL/DIGITAL EDGE/ATT                                                     
                                                                                
         DC     C'YN',C' ',C'CDD',AL2(YNATT-EDI),AL1(AGCDE)                     
         DC     C'YN',C' ',C'COD',AL2(YNATT-EDI),AL1(AGCDE)                     
         DC     C'YN',C' ',C'ABQ',AL2(YNATT-EDI),AL1(AGCDE)                     
                                                                                
*  YNR/ALL/KANG & LEE/ATT                                                       
                                                                                
         DC     C'YN',C' ',C'ALK',AL2(YNATT-EDI),AL1(AGCKL)                     
         DC     C'YN',C' ',C'LDK',AL2(YNATT-EDI),AL1(AGCKL)                     
                                                                                
*  YNR/ALL/BRAVO/ATT                                                            
                                                                                
         DC     C'YN',C' ',C'ALS',AL2(YNATT-EDI),AL1(AGCBR)                     
         DC     C'YN',C' ',C'ICA',AL2(YNATT-EDI),AL1(AGCBR)                     
         DC     C'YN',C' ',C'LDC',AL2(YNATT-EDI),AL1(AGCBR)                     
                                                                                
*                                                                               
* MEDIACOM - DELL                                                               
                                                                                
         DC     C'M2',C' ',C'DEL',AL2(M2DELL-EDI),AL1(0) DELL                   
                                                                                
*                                                                               
* MEDIACOM - SHELL (3 AGENCIES) FR- SPOT,M2 PRINT+NET,H7 NET+SPOT               
                                                                                
         DC     C'M2',C' ',C'SHL',AL2(M2SHELL-EDI),AL1(0) SHELL                 
         DC     C'FR',C' ',C'SHL',AL2(M2SHELL-EDI),AL1(0) SHELL                 
         DC     C'H7',C' ',C'SHL',AL2(M2SHELL-EDI),AL1(0) SHELL                 
                                                                                
*                                                                               
* CARAT - PFIZER                                                                
                                                                                
         DC     C'UB',C' ',C'PFZ',AL2(UBPFZ-EDI),AL1(0)  PFIZER                 
                                                                                
* WUNDERMAN - WW *                                                              
*  WUNDERMAN/ALL/ATT                                                            
                                                                                
         DC     C'WW',C' ',C'DCN',AL2(YNATT-EDI),AL1(AGCWW)                     
         DC     C'WW',C' ',C'GCN',AL2(YNATT-EDI),AL1(AGCWW)                     
         DC     C'WW',C' ',C'WNN',AL2(YNATT-EDI),AL1(AGCWW)                     
                                                                                
* MCCANN-ERICKSON - MC *                                                        
*  MCCANN/ALL/COKE                                                              
                                                                                
         DC     C'MC',C' ',C'001',AL2(MCCCH-EDI),AL1(0) COKE                    
                                                                                
* ZENITH - MC *                                                                 
*  ZENITH/ALL/NESTLE                                                            
                                                                                
         DC     C'TH',C' ',C'003',AL2(THNES-EDI),AL1(AGCSORT) NESTLE            
*                                                                               
* OMD CANADA - HD *                                                             
*  OMD/ALL/MC DONALD                                                            
                                                                                
         DC     C'OU',C' ',C'001',AL2(OMMCDNLD-EDI),AL1(0) MCDONALD             
*                                                                               
* OMDUSA *                                                                      
*  OMD/ALL/ELI LILLY                                                            
*             NOTE**    EDI CLIENT CODE GUESSED AT                              
*                       ELO IS ACTUAL CLIENT CODE (FOR PRINT AT LEAST)          
         DC     C'OO',C' ',C'ELO',AL2(OMEL-EDI),AL1(0)  ELI LILLY               
*                                                                               
         DC     AL1(EOT)                                                        
         EJECT                                                                  
*********************************************************************           
* DCB'S                                                             *           
*********************************************************************           
TEMPEB    DCB  DDNAME=TEMPEB,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=400,                                              X        
               BLKSIZE=4000,                                           X        
               MACRF=(GM,PM),                                          X        
               EODAD=CLOSE                                                      
*                                                                               
EDIMAP   DCB   DDNAME=EDIMAP,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=400,                                              X        
               BLKSIZE=4000,                                           X        
               MACRF=PM                                                         
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,30,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=(404,,,,)'                             
*                                                                               
         DROP  RB,RA                                                            
         EJECT                                                                  
*********************************************************************           
* ROUTINE TO CONVERT DATES                                          *           
*********************************************************************           
CNVD     NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)                                                      
         CLI   0(R1),5                                                          
         BE    CNVD5                                                            
         CLI   0(R1),9                                                          
         BE    CNVD9                                                            
         DC    H'0'                UNKNOWN INPUT TYPE                           
*                                                                               
CNVD5    LA    R1,0(R2)            INPUT IS MMMDD/YY                            
         BAS   RE,GETMNTH                                                       
         MVC   WORK(2),6(R2)       YY                                           
         MVC   WORK+2(2),3(R4)     MM                                           
         MVC   WORK+4(2),3(R2)     DD                                           
         B     CNVDX                                                            
*                                                                               
CNVD9    LA    R1,0(R2)            INPUT IS MMM/YY                              
         BAS   RE,GETMNTH                                                       
         MVC   WORK(2),4(R2)       YY                                           
         MVC   WORK+2(2),3(R4)     MM                                           
         MVC   WORK+4(2),=C'01'    DD                                           
         B     CNVDX                                                            
*                                                                               
CNVDX    GOTO1 DATCON,DMCB,(0,WORK),(R3)                                        
         J     XIT                                                              
*                                                                               
GETMNTH  LA    R4,MONTAB                                                        
GETMNTH1 CLC   0(3,R1),0(R4)                                                    
         BER   RE                                                               
         LA    R4,L'MONTAB(R4)                                                  
         CLI   0(R4),EOT                                                        
         BNE   GETMNTH1                                                         
         DC    H'0'                                                             
*                                                                               
MONTAB   DS    0XL5                                                             
         DC    C'JAN01FEB02MAR03APR04MAY05JUN06'                                
         DC    C'JUL07AUG08SEP09OCT10NOV11DEC12'                                
         DC    AL1(EOT)                                                         
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*********************************************************************           
* CONVERT NEGATIVE NUMBERS                                          *           
*********************************************************************           
FIXNEG   NTR1  BASE=*,LABEL=*                                                   
         LA    RF,NEGTAB                                                        
         LA    R0,10                                                            
FIXNEG3  CLC   0(1,R1),0(RF)       MATCH NUMBER TO TABLE                        
         BE    FIXNEG5                                                          
         LA    RF,2(RF)                                                         
         BCT   R0,FIXNEG3                                                       
         OI    0(R1),X'F0'                                                      
         J     XIT                                                              
FIXNEG5  MVC   0(1,R1),1(RF)       REPLACE NEGATIVES                            
         J     XIT                                                              
*                                                                               
NEGTAB   DC   X'D097'              MINUS ZERO IS A LITTLE 'P'                   
         DC   X'D198'                                                           
         DC   X'D299'                                                           
         DC   X'D3A2'                                                           
         DC   X'D4A3'                                                           
         DC   X'D5A4'                                                           
         DC   X'D6A5'                                                           
         DC   X'D7A6'                                                           
         DC   X'D8A7'                                                           
         DC   X'D9A8'                                                           
         LTORG                                                                  
         DROP  RB                                                               
         TITLE 'STARCOM && MEDIA FIRST FOR BURGER KING'                         
*********************************************************************           
* STARCOM & MEDIA FIRST - BURGER KING                               *           
*********************************************************************           
H9BKROU  NTR1  BASE=*,LABEL=*      BURGER KING                                  
         CHI   R1,BEFRQ            BEFORE CARDS                                 
         JE    XIT                                                              
         CHI   R1,AFTRQ            AFTER CARDS                                  
         BNE   H9BKR                                                            
         CLC   USRMRKT(2),=C'CO'   SKIP CO FOR BK                               
         BNE   *+8                                                              
         MVI   SKIPINV,C'Y'                                                     
         J     XIT                                                              
                                                                                
H9BKR    SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     H9HDR                                                            
         B     H9BIG                                                            
         B     H9NTE                                                            
         B     H9DTL                                                            
         B     H9IT1                                                            
         B     H9COM                                                            
         B     H9TXI                                                            
         B     H9TXT                                                            
*                                                                               
H9HDR    MVC   AGYPROF,SPACES                                                   
         MVC   AGYPROF(2),AGYALPHA                                              
         MVC   AGYPROF+2(7),=C'-BK-NET'                                         
         CLI   MEDIA,C'N'                                                       
         JE    XIT                                                              
         MVC   AGYPROF+2(8),=C'-BK-SPOT'                                        
         CLI   SYSTM,C'S'                                                       
         JE    XIT                                                              
         MVC   AGYPROF+2(9),=C'-BK-PRINT'                                       
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
H9BIG    XC    ALINITM,ALINITM     STARCOM INVOICE HOOK                         
         L     R2,=A(H9ACCTAB)                                                  
         CLC   AGYALPHA,=C'H9'                                                  
         BE    H9BIG3                                                           
         L     R2,=A(M1ACCTAB)     MEDIA FIRST                                  
         CLC   AGYALPHA,=C'M1'                                                  
         BE    H9BIG3                                                           
         L     R2,=A(H7ACCTAB)     MINDSHARE                                    
         CLC   AGYALPHA,=C'H7'                                                  
         BE    H9BIG3                                                           
         DC    H'0'                                                             
         USING H9ACCD,R2                                                        
H9BIG3   CLC   H9CLI,ADVCODE       MATCH THE CLIENT                             
         BE    H9BIG5                                                           
         CLC   H9CLI,=C'XXX'       DEFAULT (FOR NOW)                            
         BE    H9BIG5                                                           
         LA    R2,H9ACLNQ(R2)                                                   
         B     H9BIG3                                                           
*                                                                               
H9BIG5   SR    RF,RF                                                            
         ICM   RF,7,H9AGNME        AGENCY NAME                                  
         MVC   AGYNAME,0(RF)                                                    
         MVC   AGYID,H9AGID        AGENCY ID                                    
*                                                                               
         L     R4,ALINTAB                                                       
         USING LIND,R4                                                          
         CLI   0(R4),EOT          TEST LINE DETAIL                              
         BNE   *+6                                                              
         DC    H'0'               BURGER KING REQUIRES STATION DETAIL           
         GOTOR CNVD,DMCB,(9,LINMOS),(20,ITMDCYMD)                               
         MVC   FISCAL,ITMDCYMD                                                  
         MVI   FISCALA,C' '                                                     
         CLC   ITMDCYMD+4(2),=C'06'   FISCAL YEAR START JULY                    
         BNH   H9BIG7                                                           
         GOTO1 ADDAY,DMCB,(C'Y',ITMDCYMD+2),WORK,F'1'                           
         GOTO1 DATCON,DMCB,(0,WORK),(20,WORK+6)                                 
         MVC   FISCAL,WORK+6       CCYY FOR NEXT YEAR                           
*                                                                               
H9BIG7   CLC   FISCAL,=C'2003'     SECOND JAN-JUN/03 IS 03A                     
         BNE   H9BIG9                                                           
         CLC   ITMDCYMD+4(2),=C'06'                                             
         BH    H9BIG9                                                           
         MVI   FISCALA,C'A'                                                     
         DROP  R4                                                               
*                                                                               
H9BIG9   MVC   INVTYPE,=C'DI'                                                   
         MVC   INVDESC,=C'DEBIT '                                               
         CP    DUEAMT,PZERO                                                     
         BNL   *+16                                                             
         MVC   INVTYPE,=C'CN'                                                   
         MVC   INVDESC,=C'CREDIT'                                               
*                                                                               
         MVC   W2,SPACES                                                        
         LA    R3,W2                                                            
*                                                                               
         CLI   MEDIA,C'N'         NETWORK?                                      
         BNE   H9BIG9B                                                          
         CLC   H9CLI,=C'BBN'      BROMLEY'S BBN CLIENT?                         
         BE    H9BIG9A                                                          
         CLC   H9CLI,=C'225'      ALSO BROMLEY'S BBN CLIENT?                    
         BNE   H9BIG9B                                                          
H9BIG9A  MVC   0(L'H9NETE1,R3),H9NETE1    JUST USE ENTIRE EST UDEF 1            
         B     H9BIG11                                                          
*                                                                               
H9BIG9B  DS    0H                                                               
         MVC   0(L'W2YY,R3),FISCAL+2  'W2' DATA                                 
         LA    R3,L'W2YY(R3)                                                    
         MVC   0(L'W2A,R3),FISCALA  THE LETTER 'A'                              
         CLI   0(R3),C' '           DON'T SEND IF BLANK                         
         BE    *+8                                                              
         LA    R3,L'W2A(R3)                                                     
         MVI   0(R3),C'-'                                                       
         LA    R3,L'W2DSH1(R3)                                                  
         MVC   0(L'W2MAR4,R3),USRMRKT+2 USER MARKET(4)                          
         CLC   0(L'W2MAR4,R3),SPACES                                            
         BH    *+10                                                             
         MVC   0(L'W2MAR4,R3),ZEROS                                             
         LA    R3,L'W2MAR4(R3)                                                  
         MVI   0(R3),C'-'                                                       
         LA    R3,L'W2DSH2(R3)                                                  
         MVC   0(L'W2FUND,R3),H9FUND  FUND                                      
         LA    R3,L'W2FUND(R3)                                                  
         MVI   0(R3),C'-'                                                       
         LA    R3,L'W2DSH3(R3)                                                  
*                                                                               
         MVC   0(L'W2MAR10,R3),USRMRKT+7   USER MARKET(10)                      
         CLC   0(L'W2MAR10,R3),SPACES                                           
         BH    *+10                                                             
         MVC   0(L'W2MAR10,R3),ZEROS                                            
         LA    R3,L'W2MAR10(R3)                                                 
         LA    R3,L'W2SPA(R3)                                                   
         MVC   0(L'W2MKT,R3),MRKTNUM       REAL MARKET NUMBER                   
         CLI   SYSTM,C'P'                                                       
         BNE   H9BIG11                                                          
         MVC   0(L'W2DIST,R3),DISTRICT     DISTRICT FOR PRINT                   
         OC    0(L'W2DIST,R3),W2DIST                                            
         BNZ   H9BIG11                                                          
         MVC   0(L'W2S071,R3),=C'S071'     FOR PRINT                            
*                                                                               
H9BIG11  LA    RF,H9GLNN           G/L ACC FOR NETWORK TV                       
         CLI   MEDIA,C'N'          ALL NETPAK GETS SAME GL?                     
         BE    H9BIG13             (CLC MEDIA,=C'NN')                           
         LA    RF,H9GLSX                       NETWORK RADIO                    
         CLC   MEDIA,=C'SX'                                                     
         BE    H9BIG13                                                          
         LA    RF,H9GLST                       TV                               
         CLC   MEDIA,=C'ST'                                                     
         BE    H9BIG13                                                          
         LA    RF,H9GLSR                       RADIO                            
         CLC   MEDIA,=C'SR'                                                     
         BE    H9BIG13                                                          
         LA    RF,H9GLPO                       OUTDOOR                          
         CLC   MEDIA,=C'PO'                                                     
         BE    H9BIG13                                                          
         LA    RF,H9GLPM                       MAGAZINE                         
         CLC   MEDIA,=C'PM'                                                     
         BE    H9BIG13                                                          
         CLC   MEDIA,=C'PN'                    NEWSPAPER                        
         BE    H9BIG13                                                          
         DC    H'0'                                                             
*                                                                               
H9BIG13  CLI   0(RF),C' '          TEST G/L ACCOUNT                             
         BH    *+6                                                              
         DC    H'0'                NO G/L ACCOUNT                               
         MVC   OT,SPACES                                                        
         MVC   OTZRO1,ZEROS                                                     
         MVC   OTZRO2,ZEROS                                                     
         MVC   OTGLAC,0(RF)        G/L ACC FOR TV                               
         MVC   OTFUND,H9FUND                                                    
         MVC   OTMAR4,USRMRKT+2                                                 
         CLC   OTMAR4,SPACES                                                    
         BH    *+10                                                             
         CLC   OTMAR4,ZEROS                                                     
*                                                                               
         CLI   MEDIA,C'N'         NETWORK?                                      
         BNE   H9BIG15                                                          
         CLC   H9CLI,=C'BBN'      BROMLEY'S BBN CLIENT?                         
         BE    H9BIG14                                                          
         CLC   H9CLI,=C'225'      ALSO BROMLEY'S BBN CLIENT?                    
         BNE   H9BIG15                                                          
*                                                                               
H9BIG14  MVC   OTFUND,=C'206'      NOW HARD-CODED                               
         MVC   OTMAR4,=C'025 '     NOW HARD-CODED                               
*******  MVC   OTFUND,H9NETE1+8    (FROM EST USER 1)  FUNDING SOURCE            
*******  MVC   OTMAR4,H9NETE1+3    (FROM EST USER 1)  MARKET?                   
*                                                                               
H9BIG15  DS    0H                                                               
         MVC   OTC,OT              SET 'OT' FOR COMMISSION                      
         MVC   OTC+(OTGLAC-OT)(L'OTGLAC),H9GLCOM                                
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
H9NTE    CP    DUEAMT,PZERO        TEST CREDIT AMOUNT                           
         JL    XIT                                                              
         MVI   SKIPREC,C'Y'        SKIP IF DEBIT                                
         J     XIT                                                              
*                                                                               
*                                  STARCOM DETAIL LINE HOOK                     
H9DTL    ICM   R4,15,ALINITM       R4=A(NEXT LINE ITEM)                         
         BZ    *+12                                                             
         LA    R4,LINLNQ(R4)                                                    
         B     *+8                                                              
         ICM   R4,15,ALINTAB       R4=A(FIRST IN LINE TABLE)                    
*                                                                               
         USING LIND,R4                                                          
         ST    R4,ALINITM          SAVE ADDRESS OF LINE ITEM                    
         CLC   REGION,SPACES                                                    
         BH    *+10                                                             
         MVC   REGION,ZEROS                                                     
         MVC   STATION,LINSTA      RESTORE STATION                              
         MVC   MRKTNUM,LINMKT              MARKET                               
         MVC   MRKTNAM,LINMKTN                                                  
         CLI   SYSTM,C'S'                                                       
         BE    *+10                                                             
         MVC   PUB,LINPUB           OR PUB                                      
*                                                                               
         MVC   ITMMCY(2),ITMDCYMD+4       GET MMCCYY FROM CCYYMMDD              
         MVC   ITMMCY+2(4),ITMDCYMD                                             
         MVC   YRMRKT,SPACES              BUILD  MMCCYY - MARKET NAME           
         MVC   YRMRKT(L'ITMMCY),ITMMCY    MMCCYY                                
         MVC   YRMRKT+L'ITMMCY+1(L'MRKTNAM),MRKTNAM                             
         ZAP   TAXAMT,LINTAX                                                    
*                                                                               
         UNPK  CLINDUE,LINDUE                                                   
         LA    R1,CLINDUE+L'CLINDUE-1                                           
         GOTOR FIXNEG                                                           
         MVI   QNTY1,C'0'          SET QUANTITY OF ONE                          
         MVC   QNTY1+1(L'QNTY1-1),QNTY1                                         
         MVI   QNTY1+(L'QNTY1-1),C'1'                                           
         CP    LINDUE,PZERO                                                     
         BNL   *+8                                                              
         MVI   QNTY1+(L'QNTY1-1),X'98' SET TO MINUS ONE                         
         ZAP   TAXAMT,TOTTAX                                                    
         MVI   RTNROU,C'N'                                                      
         LA    R4,LINLNQ(R4)                                                    
         CLI   0(R4),EOT                                                        
         JE    XIT                                                              
         MVI   RTNROU,C'Y'         SET RETURN FOR NEXT                          
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                  STARCOM IT1 HOOK                             
H9IT1    MVC   VNDNAME,SPACES                                                   
         MVC   VNDNAMC,SPACES                                                   
         L     R2,ALINITM          CURRENT LINE ITEM                            
         USING LIND,R2                                                          
         CLI   SYSTM,C'S'                                                       
         BNE   H9IT13                                                           
         MVC   VNDNAME(L'LINSTA),LINSTA    STATION CODE                         
         MVC   VNDNAMC,VNDNAME                                                  
         MVC   VNDNAMC+L'LINSTA+1(L'COMMSN),COMMSN                              
         B     H9IT17                                                           
*                                                                               
H9IT13   MVC   VNDNAME,LINPUB             PUB NAME                              
         MVC   VNDNAMC,VNDNAME                                                  
         LA    RF,VNDNAMC+L'VNDNAMC-L'COMMSN                                    
         BCTR  RF,0                                                             
         MVC   0(L'COMMSN+1,RF),SPACES                                          
         BCTR  RF,0                                                             
         CLI   0(RF),C' '                                                       
         BNH   *-6                                                              
         MVC   2(L'COMMSN,RF),COMMSN                                            
*                                                                               
H9IT17   DS    0H                                                               
         UNPK  CNETAMT,LINNET                                                   
         LA    R1,CNETAMT+L'CNETAMT-1                                           
         GOTOR FIXNEG                                                           
         J     XIT                                                              
*                                  COMMISSION HOOK                              
H9COM    L     R2,ALINITM          CURRENT LINE ITEM                            
         UNPK  CCOMAMT,LINCOM                                                   
         LA    R1,CCOMAMT+L'CCOMAMT-1                                           
         GOTOR FIXNEG                                                           
         CP    LINCOM,PZERO        TEST ANY COMMISSION                          
         BNE   *+8                                                              
         MVI   SKIPREC,C'Y'        SKIP ALL COMMISSION SEGMENTS                 
         J     XIT                                                              
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
*                                 TAX HOOK                                      
H9TXI    UNPK  CTAXAMT,TAXAMT     CONVERT TAX TO CHARACTER                      
         LA    R1,CTAXAMT+L'CTAXAMT-1                                           
         GOTOR FIXNEG                                                           
         UNPK  PTAXAMT,TAXAMT                                                   
         OI    PTAXAMT+L'PTAXAMT-1,X'F0'  POSTIVE TAX FIELD                     
         CP    TAXAMT,PZERO        TEST ANY TAX                                 
         BNE   *+8                                                              
         MVI   SKIPREC,C'Y'        SKIP ALL TAX SEGMENTS                        
         J     XIT                                                              
                                                                                
*                                 HOOK FOR TOTAL TAX                            
H9TXT    CP    TAXAMT,PZERO       TEST ANY TAX                                  
         BNE   *+12                                                             
         MVI   SKIPREC,C'Y'        SKIP ALL TAX SEGMENTS                        
         J     XIT                                                              
         UNPK  PTAXAMT,TAXAMT                                                   
         OI    PTAXAMT+L'PTAXAMT-1,X'F0'  POSTIVE TAX FIELD                     
         J     XIT                                                              
*                                                                               
COMMSN   DC   C'COMMISSION'                                                     
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*********************************************************************           
* CONTROL TABLE - SEE CSCD                                          *           
*********************************************************************           
* STARCOM - BK                                                                  
H9BK     DC    AL2(H9BKROU-EDI)     CONTROL ROUTINE                             
         DC    AL2(H9BKHDM-EDI)     FILE HEADER RECORD(S)                       
         DC    AL2(H9BKIVM-EDI)     INVOICE HEADER RECORDS                      
         DC    AL2(H9BKLNM-EDI)     INVOICE LINE ITEMS RECORDS                  
         DC    AL2(H9BKTOM-EDI)     INVOICE TOTAL RECORDS                       
         DC    AL1(EOT)                                                         
*                                                                               
* MEDIA FIRST - BK                   SAME AS STARCOM                            
M1SBK    DC    AL2(H9BKROU-EDI)      CONTROL ROUTINE                            
         DC    AL2(H9BKHDM-EDI)      FILE HEADER RECORD(S)                      
         DC    AL2(H9BKIVM-EDI)      INVOICE HEADER RECORDS                     
         DC    AL2(H9BKLNM-EDI)      INVOICE LINE ITEMS RECORDS                 
         DC    AL2(H9BKTOM-EDI)      INVOICE TOTAL RECORDS                      
         DC    AL1(EOT)                                                         
*                                                                               
* MINDSHARE   - BK                   SAME AS STARCOM                            
H7BK     DC    AL2(H9BKROU-EDI)      CONTROL ROUTINE                            
         DC    AL2(H9BKHDM-EDI)      FILE HEADER RECORD(S)                      
         DC    AL2(H9BKIVM-EDI)      INVOICE HEADER RECORDS                     
         DC    AL2(H9BKLNM-EDI)      INVOICE LINE ITEMS RECORDS                 
         DC    AL2(H9BKTOM-EDI)      INVOICE TOTAL RECORDS                      
         DC    AL1(EOT)                                                         
*                                                                               
H9BKHDRQ EQU   1                                                                
H9BIGQ   EQU   2                                                                
H9NTEQ   EQU   3                                                                
H9DTLQ   EQU   4                                                                
H9IT1Q   EQU   5                                                                
H9COMQ   EQU   6                                                                
H9TXIQ   EQU   7                                                                
H9TXTQ   EQU   8                                                                
         EJECT                                                                  
*********************************************************************           
* BK -  MAP TABLE(S) - SEE MAPD                                     *           
*********************************************************************           
                                                                                
H9BKHDM  DS    0X                  STARCOM/BURGER KING                          
         DC    AL1(ROUQ),AL2(H9BKHDRQ)                                          
         DC    AL1(CONQ),AL2(HDRID-HDRD,6),C'000000'                            
         DC    AL1(CONQ),AL2(HDRZRO-HDRD,5),C'00000'                            
         DC    AL1(CONQ),AL2(HDRDDS-HDRD,3),C'DDS'                              
         DC    AL1(LWSQ),AL2(HDRPROF-HDRD,L'AGYPROF,AGYPROF-LWSD)               
         DC    AL1(EORQ),AL2(EDIHDRQ)                                           
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
H9BKIVM  DS    0X                                                               
         DC    AL1(ROUQ),AL2(H9BIGQ)                                            
         DC    AL1(CONQ),AL2(BIGID-BIGD,6),C'BIG002'                            
         DC    AL1(LWSQ),AL2(BIGDATE-BIGD,L'BIGDATE,INVDCYMD-LWSD)              
         DC    AL1(LWSQ),AL2(BIGINVN-BIGD,L'INVNUMB,INVNUMB-LWSD)               
         DC    AL1(LWSQ),AL2(BIGTYPE-BIGD,L'BIGTYPE,INVTYPE-LWSD)               
         DC    AL1(EORQ),AL2(EDIHLNQ+(BIGTYPE-BIGD)+L'BIGTYPE)                  
*                                                                               
         DC    AL1(ROUQ),AL2(H9NTEQ)                                            
         DC    AL1(CONQ),AL2(NTEID-NTED,6),C'NTE003'                            
         DC    AL1(CONQ),AL2(NTEREF-NTED,L'NTEREF),C'CBH'                       
         DC    AL1(CONQ),AL2(NTEDESC-NTED,L'NTEDESC),C'CREDIT MEMO'             
         DC    AL1(EORQ),AL2(EDIHLNQ+(NTEDESC-NTED)+L'NTEDESC)                  
*                                                                               
         DC    AL1(CONQ),AL2(CURID-CURD,6),C'CUR004'                            
         DC    AL1(CONQ),AL2(CUREIC-CURD,L'CUREIC),C'BT '                       
         DC    AL1(CONQ),AL2(CURCUR-CURD,L'CURCUR),C'USD'                       
         DC    AL1(EORQ),AL2(EDIHLNQ+(CURCUR-CURD)+L'CURCUR)                    
*                                                                               
         DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF005'                            
         DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'VR '                              
         DC    AL1(CONQ),AL2(REFRI-REFD,4),C'US60'                              
         DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+4)                            
*                                                                               
         DC    AL1(CONQ),AL2(N1ID-N1D,6),C'N1 008'                              
         DC    AL1(CONQ),AL2(N1EIC-N1D,L'N1EIC),C'BT '                          
         DC    AL1(LWSQ),AL2(N1NAME-N1D,L'AGYNAME,AGYNAME-LWSD)                 
         DC    AL1(CONQ),AL2(N1ICQ-N1D,L'N1ICQ),C'92'                           
         DC    AL1(LWSQ),AL2(N1IC-N1D,L'AGYID,AGYID-LWSD)                       
         DC    AL1(EORQ),AL2(EDIHLNQ+(N1IC-N1ID)+L'N1IC)                        
*                                                                               
         DC    AL1(CONQ),AL2(DTMID-DTMD,6),C'DTM016'                            
         DC    AL1(CONQ),AL2(DTMDTQ-DTMD,L'DTMDTQ),C'404'                       
         DC    AL1(CONQ),AL2(DTMPFQ-DTMD,L'DTMPFQ),C'CY '                       
         DC    AL1(LWSQ),AL2(DTMPCCYY-DTMD,L'DTMPCCYY,FISCAL-LWSD)              
         DC    AL1(EORQ),AL2(EDIHLNQ+(DTMPCCYY-DTMD)+L'DTMPCCYY)                
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
H9BKLNM  DS    0X                           DETAIL LINE                         
         DC    AL1(BEGQ),AL2(H9DTLQ)       BEGIN LOOP                           
         DC    AL1(ROUQ),AL2(H9IT1Q)                                            
         DC    AL1(CONQ),AL2(IT1ID-IT1D,6),C'IT1035'                            
         DC    AL1(LWSQ),AL2(IT1AID-IT1D,L'IT1AID,VNDNAME-LWSD)                 
         DC    AL1(CONQ),AL2(IT1PSIQ-IT1D,L'IT1PSIQ),C'W2'                      
         DC    AL1(LWSQ),AL2(IT1PSI-IT1D,L'W2,W2-LWSD)                          
         DC    AL1(CONQ),AL2(IT1PSIQ2-IT1D,L'IT1PSIQ2),C'OT'                    
         DC    AL1(LWSQ),AL2(IT1PSI2-IT1D,L'OT,OT-LWSD)                         
         DC    AL1(EORQ),AL2(EDIHLNQ+(IT1PSI2-IT1D)+L'IT1PSI2)                  
*                                                                               
*                                           TAX ON DETAIL (IF ANY)              
         DC    AL1(ROUQ),AL2(H9TXIQ)                                            
         DC    AL1(CONQ),AL2(TXIID-TXID,6),C'TXI040'                            
         DC    AL1(CONQ),AL2(TXITTC-TXID,2),C'SU'                               
         DC    AL1(CONQ),AL2(TXINDP-TXID,1),C'2'                                
         DC    AL1(CONQ),AL2(TXIZERO-TXID,2),C'00'                              
         DC    AL1(LWSQ),AL2(TXIAMT-TXID,L'TXIAMT,PTAXAMT-LWSD)                 
         DC    AL1(EORQ),AL2(EDIHLNQ+(TXIAMT-TXID)+L'TXIAMT)                    
*                                                                               
*                                           COST ON DETAIL (ALWAYS)             
         DC    AL1(CONQ),AL2(CTPID-CTPD,6),C'CTP041'                            
         DC    AL1(CONQ),AL2(CTPNDP-CTPID,1),C'2'                               
         DC    AL1(CONQ),AL2(CTPZERO-CTPID,1),C'0'                              
         DC    AL1(LWSQ),AL2(CTPUP-CTPD,L'CTPUP,CNETAMT-LWSD)                   
         DC    AL1(CONQ),AL2(CTPQNTY-CTPD,16),C'0000000000000001'               
         DC    AL1(CONQ),AL2(CTPUMC-CTPD,2),C'EA'                               
         DC    AL1(EORQ),AL2(EDIHLNQ+(CTPUMC-CTPD)+L'CTPUMC)                    
*                                                                               
*                                           COMMISSION (IF ANY)                 
         DC    AL1(ROUQ),AL2(H9COMQ)                                            
         DC    AL1(CONQ),AL2(IT1ID-IT1D,6),C'IT1035'                            
         DC    AL1(LWSQ),AL2(IT1AID-IT1D,L'IT1AID,VNDNAMC-LWSD)                 
         DC    AL1(CONQ),AL2(IT1PSIQ-IT1D,L'IT1PSIQ),C'W2'                      
         DC    AL1(LWSQ),AL2(IT1PSI-IT1D,L'W2,W2-LWSD)                          
         DC    AL1(CONQ),AL2(IT1PSIQ2-IT1D,L'IT1PSIQ2),C'OT'                    
         DC    AL1(LWSQ),AL2(IT1PSI2-IT1D,L'OTC,OTC-LWSD)                       
         DC    AL1(EORQ),AL2(EDIHLNQ+(IT1PSI2-IT1D)+L'IT1PSI2)                  
*                                                                               
*                                           COMMISSION COST(IF ANY)             
         DC    AL1(ROUQ),AL2(H9COMQ)                                            
         DC    AL1(CONQ),AL2(CTPID-CTPD,6),C'CTP041'                            
         DC    AL1(CONQ),AL2(CTPNDP-CTPID,1),C'2'                               
         DC    AL1(CONQ),AL2(CTPZERO-CTPID,1),C'0'                              
         DC    AL1(LWSQ),AL2(CTPUP-CTPD,L'CTPUP,CCOMAMT-LWSD)                   
         DC    AL1(CONQ),AL2(CTPQNTY-CTPD,16),C'0000000000000001'               
         DC    AL1(CONQ),AL2(CTPUMC-CTPD,2),C'EA'                               
         DC    AL1(EORQ),AL2(EDIHLNQ+(CTPUMC-CTPD)+L'CTPUMC)                    
         DC    AL1(EOTQ)                                                        
         DC    AL1(ENDQ)                   END LOOP                             
*                                                                               
H9BKTOM  DS    0X                                                               
         DC    AL1(CONQ),AL2(PIDID-PIDD,6),C'PID063'                            
         DC    AL1(CONQ),AL2(PIDIDT-PIDD,L'PIDIDT),C'F'                         
         DC    AL1(LWSQ),AL2(PIDDESC-PIDD,L'REGION,REGION-LWSD)                 
         DC    AL1(EORQ),AL2(EDIHLNQ+(PIDDESC-PIDD)+L'PIDDESC)                  
*                                                                               
         DC    AL1(CONQ),AL2(TDSID-TDSD,6),C'TDS081'                            
         DC    AL1(CONQ),AL2(TDSNDP-TDSID,1),C'2'                               
         DC    AL1(LWSQ),AL2(TDSAMT-TDSD,L'TDSAMT,CDUENETP-LWSD)                
         DC    AL1(EORQ),AL2(EDIHLNQ+(TDSAMT-TDSD)+L'TDSAMT)                    
*                                                                               
         DC    AL1(ROUQ),AL2(H9TXTQ)                                            
         DC    AL1(CONQ),AL2(TXIID-TXID,6),C'TXI086'                            
         DC    AL1(CONQ),AL2(TXITTC-TXID,2),C'SU'                               
         DC    AL1(CONQ),AL2(TXINDP-TXID,1),C'2'                                
         DC    AL1(CONQ),AL2(TXIZERO-TXID,2),C'00'                              
         DC    AL1(LWSQ),AL2(TXIAMT-TXID,L'TXIAMT,PTAXAMT-LWSD)                 
         DC    AL1(EORQ),AL2(EDIHLNQ+(TXIAMT-TXID)+L'TXIAMT)                    
         DC    AL1(EOTQ,EOTQ)                                                   
         EJECT                                                                  
*********************************************************************           
* STARCOM - BURGER KING CLIENT/GL TABLE                             *           
*********************************************************************           
H9ACCTAB DS    0XL(H9ACLNQ)        STARCOM ACCOUNT CODES - SEE H9ACCD           
         DC    C'BBF',C'022'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'73689'            AGENCY ID                                    
         DC    AL3(BROM)           BROMLEY                                      
*                                                                               
         DC    C'BBV',C'020'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'73689'            AGENCY ID                                    
         DC    AL3(BROM)           BROMLEY                                      
*                                                                               
         DC    C'BBI',C'023'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76497'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'73689'            AGENCY ID                                    
         DC    AL3(BROM)           BROMLEY                                      
*                                                                               
         DC    C'BBN',C'022'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76497'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'73689'            AGENCY ID                                    
         DC    AL3(BROM)           BROMLEY                                      
*                                                                               
         DC    C'BKJ',C'021'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'33565'            AGENCY ID                                    
         DC    AL3(DARCY)          D'ARCY                                       
*                                                                               
         DC    C'BKC',C'022'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76497'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'33565'            AGENCY ID                                    
         DC    AL3(DARCY)          D'ARCY                                       
*                                                                               
         DC    C'BKI',C'023'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76497'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'33565'            AGENCY ID                                    
         DC    AL3(DARCY)          D'ARCY                                       
*                                                                               
         DC    C'BKV',C'020'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'33565'            AGENCY ID                                    
         DC    AL3(DARCY)          D'ARCY                                       
*                                                                               
*                                                                               
         DC    C'XXX',C'023'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'99999'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'99999'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76497'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'73689'            AGENCY ID                                    
         DC    AL3(BROM)           BROMLEY                                      
*                                                                               
         DC    AL1(EOT)                                                         
*                                                                               
BROM     DC    CL(L'AGYNAME)'BROMLEY COMMUNICATIONS'                            
DARCY    DC    CL(L'AGYNAME)'D''ARCY MASIUS BENTON && BOWLES, INC.'             
         EJECT                                                                  
*********************************************************************           
*  MEDIA FIRST BURGER KING CLIENT/GL TABLE                          *           
*********************************************************************           
M1ACCTAB DS    0XL(H9ACLNQ)        MEDIA FIRST ACCT CODES - SEE H9ACCD          
         DC    C'BKJ',C'021'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'58966'            AGENCY ID                                    
         DC    AL3(MEDIA1ST)       MEDIA FIRST                                  
*                                                                               
         DC    C'BKC',C'022'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76497'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'58966'            AGENCY ID                                    
         DC    AL3(MEDIA1ST)       MEDIA FIRST                                  
*                                                                               
         DC    C'BKA',C'022'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76497'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'58966'            AGENCY ID                                    
         DC    AL3(MEDIA1ST)       MEDIA FIRST                                  
*                                                                               
         DC    C'BKB',C'022'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76497'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'58966'            AGENCY ID                                    
         DC    AL3(MEDIA1ST)       MEDIA FIRST                                  
*                                                                               
         DC    C'BKI',C'023'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76497'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'58966'            AGENCY ID                                    
         DC    AL3(MEDIA1ST)       MEDIA FIRST                                  
*                                                                               
         DC    C'BKV',C'020'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'58966'            AGENCY ID                                    
         DC    AL3(MEDIA1ST)       MEDIA FIRST                                  
*                                                                               
         DC    AL1(EOT)                                                         
*                                                                               
MEDIA1ST DC    CL(L'AGYNAME)'MEDIA FIRST'                                       
         EJECT                                                                  
*********************************************************************           
*  MINDSHARE BURGER KING CLIENT/GL TABLE                            *           
*********************************************************************           
H7ACCTAB DS    0XL(H9ACLNQ)        MEDIA FIRST ACCT CODES - SEE H9ACCD          
         DC    C'BI1',C'023'       CLIENT CODE/FUNDING SOURCE                   
*                                  CHANGED FROM 023 IMDB# 2407641               
*                                  CHGED FROM 020 BACK TO 023 0118493N          
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BI2',C'023'       CLIENT CODE/FUNDING IMDB# 2135841            
*                                  CHANGED FROM 023 IMDB# 2407641               
*                                  RESTORED TO 023 9/21/07 #0144031N            
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
*                                                                               
         DC    C'BI3',C'023'       NEW CLIENT IMDB# 0147769N                    
*                                                                               
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BJ1',C'021'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BB1',C'022'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BP1',C'022'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BV0',C'020'       CLIENT CODE/TICKET# 0181553N                 
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BV1',C'020'       CLIENT CODE/FUNDING SOURCE                   
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BV2',C'020'       CLIENT CODE/FUNDING IMDB# 2135841            
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BV3',C'020'       CLIENT CODE/FUNDING IMDB# 7666351            
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BV4',C'020'       CLIENT CODE/FUNDING IMDB# 2817571            
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BV5',C'020'       CLIENT CODE/FUNDING IMDB# 2872701            
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BV6',C'020'       CLIENT CODE/FUNDING TICKET# 0109112N         
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
*  NOTE G/L FOR CLIENTS BELOW ARE SET LIKE THOSE FOR BV6                        
*       EVEN THOUGH THEY INDICATED THAT ONLY SPOT WOULD USE THE CLTS            
*       OTHER ENTRIES IN THE TABLE BELOW SHOULD CAUSE NO PROBLEMS               
*                                                                               
         DC    C'BV8',C'020'       CLIENT CODE/FUNDING TICKET# 0133745N         
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BV9',C'020'       CLIENT CODE/FUNDING TICKET# 0133745N         
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BG0',C'020'       CLIENT CODE/FUNDING TICKET# 0189892N         
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BG1',C'020'       CLIENT CODE/FUNDING TICKET# 0189892N         
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BG2',C'020'       CLIENT CODE/FUNDING TICKET# 0205126N         
         DC    C'76485'            RADIO                                        
         DC    C'76487'            NETWORK RADIO                                
         DC    C'76477'            TV                                           
         DC    C'76473'            NETWORK TV                                   
         DC    C'76533'            OUTDOOR                                      
         DC    C'76499'            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    C'BK ',C'802'       CLIENT CODE/FUNDING IMDB# 2554611            
         DC    C'76489'            RADIO                                        
*        CODES NOT KNOWN FOR THE OTHER MEDIA YET                                
         DC    C'     '            NETWORK RADIO                                
         DC    C'     '            TV                                           
         DC    C'     '            NETWORK TV                                   
         DC    C'     '            OUTDOOR                                      
         DC    C'     '            MAG/NEWS                                     
         DC    C'76523'            COMMISSION                                   
         DC    C'64581'            AGENCY ID                                    
         DC    AL3(MINDSHAR)       MINDSHARE                                    
*                                                                               
         DC    AL1(EOT)                                                         
*                                                                               
MINDSHAR DC    CL(L'AGYNAME)'MINDSHARE USA, INC.'                               
***********************************************************************         
* MINDSHARE   - IBM  NEW 4010 VERSION SPEC-27820                      *         
***********************************************************************         
H7IBMROU NTR1  BASE=*,LABEL=*                                                   
         CHI   R1,BEFRQ            BEFORE CARDS                                 
         BNE   H7IBAFTR                                                         
         J     XIT                                                              
*                                                                               
H7IBAFTR CHI   R1,AFTRQ            AFTER CARDS                                  
         BNE   H7IBMCR                                                          
         J     EIXIT                                                            
*                                                                               
H7IBMCR  SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     H7IBMHDR            FILE HEADER                                  
         B     H7IBMINV            INVOICE HEADER                               
         B     H7IBMPIN            PREVIOUS INVOICE                             
         B     H7IBMIVT            INVOICE TOTAL                                
*                                                                               
H7IBMHDR DS    0H                  FILE HEADER                                  
         MVC   AGYPROF,SPACES      CLEAR AGENCY PROFILE                         
         MVC   AGYPROF(8),=C'IBM4010-'   INIT HEADER                            
         MVC   AGYPROF+8(3),=C'NET'      DEFAULT TO NET SYSTEM                  
         CLI   MEDIA,C'N'          NET SYSTEM?                                  
         JE    XIT                 YES - DONE                                   
         MVC   AGYPROF+8(4),=C'SPOT'     SET SPOT SYSTEM                        
         CLI   SYSTM,C'S'          SPOT SYSTEM?                                 
         JE    XIT                 YES - DONE                                   
         MVC   AGYPROF+8(5),=C'PRINT'    SET PRINT SYSTEM                       
         J     XIT                                                              
*                                                                               
H7IBMINV DS    0H                  INVOICE HEADER                               
         MVC   INVTYPE,=C'DI'      DEBIT INVOICE                                
         CP    DUEAMT,PZERO        IS THIS A CREDIT INVOICE?                    
         BNL   *+10                NO                                           
         MVC   INVTYPE,=C'CN'      CREDIT INVOICE                               
         J     XIT                                                              
*                                                                               
H7IBMPIN DS    0H                  PREVIOUS INVOICE                             
         CLI   INVTYPE,C'C'        CREDIT INVOICE?                              
         BNE   *+14                NO - SKIP REF I5 SECTION                     
         OC    PINVNUMB,PINVNUMB   HAVE A PREVIOUS INVOICE NUMBER?              
         BNZ   *+8                 YES - SEND REF I5 SECTION                    
         MVI   SKIPREC,C'Y'        SKIP REF I5 SECTION                          
         J     XIT                                                              
*                                                                               
H7IBMIVT DS    0H                  INVOICE TOTAL                                
         J     XIT                                                              
*                                                                               
* MINDSHARE   - IBM                IBM - NEW 4010 VERSION                       
*                                                                               
H7IBM    DC    AL2(H7IBMROU-EDI)   CONTROL ROUTINE                              
         DC    AL2(H7IBMHDM-EDI)   FILE HEADER RECORD(S)                        
         DC    AL2(H7IBMIVM-EDI)   INVOICE HEADER RECORDS                       
         DC    AL2(0)              INVOICE LINE ITEMS RECORDS                   
         DC    AL2(H7IBMITM-EDI)   INVOICE TOTAL RECORDS                        
         DC    AL1(EOT)            END OF TABLE                                 
                                                                                
H7IBHDRQ EQU   1                   FILE HEADER                                  
H7IBINVQ EQU   2                   INVOICE HEADER                               
H7IBPINQ EQU   3                   PREVIOUS INVOICE                             
H7IBINTQ EQU   4                   INVOICE TOTAL                                
                                                                                
*********************************************************************           
* IBM -  MAP TABLE(S) - SEE MAPD                                    *           
*********************************************************************           
H7IBMHDM DS    0X                  GROUPM/IBM FILE HEADER RECORD                
         DC    AL1(ROUQ),AL2(H7IBHDRQ)                                          
         DC    AL1(CONQ),AL2(HDRID-HDRD,6),C'000000'                            
         DC    AL1(CONQ),AL2(HDRZRO-HDRD,5),C'00000'                            
         DC    AL1(CONQ),AL2(HDRDDS-HDRD,3),C'DDS'                              
         DC    AL1(LWSQ),AL2(HDRPROF-HDRD,L'AGYPROF,AGYPROF-LWSD)               
         DC    AL1(EORQ),AL2(EDIHDRQ)                                           
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
H7IBMIVM DS    0X                                                               
         DC    AL1(ROUQ),AL2(H7IBINVQ)                                          
         DC    AL1(CONQ),AL2(BIGID-BIGD,6),C'BIG002'                            
         DC    AL1(LWSQ),AL2(BIGDATE-BIGD,L'BIGDATE,INVDCYMD-LWSD)              
         DC    AL1(LWSQ),AL2(BIGINVN-BIGD,L'INVNUMB,INVNUMB-LWSD)               
         DC    AL1(LWSQ),AL2(BIGDATE1-BIGD,L'BIGDATE1,INVDCYMD-LWSD)            
         DC    AL1(LWSQ),AL2(BIGPON-BIGD,L'BIGPON,EUDEF2-LWSD)                  
         DC    AL1(LWSQ),AL2(BIGTYPE-BIGD,L'BIGTYPE,INVTYPE-LWSD)               
         DC    AL1(CONQ),AL2(BIGPURP-BIGD,L'BIGPURP),C'00'                      
         DC    AL1(EORQ),AL2(EDIHLNQ+(BIGPURP-BIGD)+L'BIGPURP)                  
*                                                                               
         DC    AL1(CONQ),AL2(CURID-CURD,6),C'CUR004'                            
         DC    AL1(CONQ),AL2(CUREIC-CURD,L'CUREIC),C'BY '                       
         DC    AL1(CONQ),AL2(CURCUR-CURD,L'CURCUR),C'USD'                       
         DC    AL1(EORQ),AL2(EDIHLNQ+(CURCUR-CURD)+L'CURCUR)                    
*                                                                               
         DC    AL1(ROUQ),AL2(H7IBPINQ)                                          
         DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF005'                            
         DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'I5 '                              
         DC    AL1(LWSQ),AL2(REFRI-REFD,L'PINVNUMB,PINVNUMB-LWSD)               
         DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+L'PINVNUMB)                   
*                                                                               
         DC    AL1(CONQ),AL2(N1ID-N1D,6),C'N1 008'                              
         DC    AL1(CONQ),AL2(N1EIC-N1D,L'N1EIC),C'RI '                          
         DC    AL1(CONQ),AL2(N1NAME-N1D,15),C'Ogilvy && Mather'                 
         DC    AL1(CONQ),AL2(N1ICQ-N1D,L'N1ICQ),C'04'                           
         DC    AL1(CONQ),AL2(N1IC-N1D,10),C'1000051301'                         
         DC    AL1(EORQ),AL2(EDIHLNQ+(N1IC-N1ID)+10)                            
*                                                                               
         DC    AL1(CONQ),AL2(N2ID-N2D,6),C'N2 009'                              
         DC    AL1(CONQ),AL2(N2NAME-N2D,12),C'Lockbox 1820'                     
         DC    AL1(EORQ),AL2(EDIHLNQ+(N2NAME-N2ID)+12)                          
*                                                                               
         DC    AL1(CONQ),AL2(N3ID-N3D,6),C'N3 010'                              
         DC    AL1(CONQ),AL2(N3ADDR1-N3D,15),C'P.O. Box 781820'                 
         DC    AL1(EORQ),AL2(EDIHLNQ+(N3ADDR1-N3D)+15)                          
*                                                                               
         DC    AL1(CONQ),AL2(N4ID-N4D,6),C'N4 011'                              
         DC    AL1(CONQ),AL2(N4CITY-N4D,12),C'Philadelphia'                     
         DC    AL1(CONQ),AL2(N4STATE-N4D,2),C'PA'                               
         DC    AL1(CONQ),AL2(N4ZIP-N4D,10),C'19178-1820'                        
         DC    AL1(CONQ),AL2(N4CNTY-N4D,2),C'US'                                
         DC    AL1(EORQ),AL2(EDIHLNQ+(N4CNTY-N4ID)+2)                           
*                                                                               
         DC    AL1(CONQ),AL2(N1ID-N1D,6),C'N1 008'                              
         DC    AL1(CONQ),AL2(N1EIC-N1D,L'N1EIC),C'BT '                          
         DC    AL1(CONQ),AL2(N1NAME-N1D,15),C'IBM Corporation'                  
         DC    AL1(CONQ),AL2(N1ICQ-N1D,L'N1ICQ),C'92'                           
         DC    AL1(CONQ),AL2(N1IC-N1D,4),C'0147'                                
         DC    AL1(EORQ),AL2(EDIHLNQ+(N1IC-N1ID)+4)                             
*                                                                               
         DC    AL1(CONQ),AL2(N3ID-N3D,6),C'N3 010'                              
         DC    AL1(CONQ),AL2(N3ADDR1-N3D,18),C'1 New Orchard Road'              
         DC    AL1(EORQ),AL2(EDIHLNQ+(N3ADDR1-N3D)+18)                          
*                                                                               
         DC    AL1(CONQ),AL2(N4ID-N4D,6),C'N4 011'                              
         DC    AL1(CONQ),AL2(N4CITY-N4D,6),C'Armonk'                            
         DC    AL1(CONQ),AL2(N4STATE-N4D,2),C'NY'                               
         DC    AL1(CONQ),AL2(N4ZIP-N4D,9),C'105041722'                          
         DC    AL1(CONQ),AL2(N4CNTY-N4D,2),C'US'                                
         DC    AL1(EORQ),AL2(EDIHLNQ+(N4CNTY-N4ID)+2)                           
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
H7IBMITM DS    0X                      INVOICE TOTALS                           
         DC    AL1(ROUQ),AL2(H7IBINTQ)                                          
         DC    AL1(CONQ),AL2(IT1ID-IT1D,6),C'IT1035'                            
         DC    AL1(CONQ),AL2(IT1AID-IT1D,3),C'001'                              
         DC    AL1(CONQ),AL2(IT1QNTY-IT1ID,1),C'0'                              
         DC    AL1(CONQ),AL2(IT1QNTY+1-IT1D,1),C'1'                             
         DC    AL1(CONQ),AL2(IT1BMC-IT1D,2),C'EA'                               
         DC    AL1(CONQ),AL2(IT1NDP-IT1ID,1),C'2'                               
         DC    AL1(CONQ),AL2(IT1ZERO-IT1ID,1),C'0'                              
         DC    AL1(LWSQ),AL2(IT1UP-IT1ID,15,CDUENETP-LWSD)                      
         DC    AL1(CONQ),AL2(IT1PSIQ-IT1D,2),C'VP'                              
         DC    AL1(LWSQ),AL2(IT1PSI-IT1D,3,ADVCODE-LWSD)                        
         DC    AL1(EORQ),AL2(EDIHLNQ+(IT1PSI-IT1D)+L'IT1PSI)                    
*                                                                               
         DC    AL1(CONQ),AL2(PAMID-PAMD,6),C'PAM042'                            
         DC    AL1(CONQ),AL2(PAMQQ-PAMID,2),C'ZZ'                               
         DC    AL1(CONQ),AL2(PAMDEC-PAMID,1),C'0'                               
         DC    AL1(CONQ),AL2(PAMQTY-PAMID,15),C'000000000000001'                
         DC    AL1(CONQ),AL2(PAMBASIS-PAMID,2),C'ZZ'                            
         DC    AL1(CONQ),AL2(PAMAQC-PAMID,1),C'1'                               
         DC    AL1(CONQ),AL2(PAMDEC2-PAMID,1),C'2'                              
         DC    AL1(LWSQ),AL2(PAMAMT-PAMID,L'PAMAMT,CDUENTLP-LWSD)               
         DC    AL1(EORQ),AL2(EDIHLNQ+(PAMAMT-PAMD)+L'PAMAMT)                    
*                                                                               
         DC    AL1(CONQ),AL2(PIDID-PIDD,6),C'PID063'                            
         DC    AL1(CONQ),AL2(PIDIDT-PIDD,L'PIDIDT),C'F'                         
         DC    AL1(LWSQ),AL2(PIDDESC-PIDD,3,ADVCODE-LWSD)                       
         DC    AL1(LWSQ),AL2(PIDDESC+4-PIDD,3,PRCODE1-LWSD)                     
         DC    AL1(LWSQ),AL2(PIDDESC+8-PIDD,3,ESCODE1-LWSD)                     
         DC    AL1(LWSQ),AL2(PIDDESC+12-PIDD,20,ESNAME1-LWSD)                   
         DC    AL1(LWSQ),AL2(PIDDESC+33-PIDD,20,ESNAME2-LWSD)                   
         DC    AL1(EORQ),AL2(EDIHLNQ+(PIDDESC-PIDD)+L'PIDDESC)                  
*                                                                               
         DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF062'                            
         DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'FJ '                              
         DC    AL1(CONQ),AL2(REFRI-REFD,2),C'01'                                
         DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+2)                            
*                                                                               
         DC    AL1(CONQ),AL2(TDSID-TDSD,6),C'TDS081'                            
         DC    AL1(CONQ),AL2(TDSNDP-TDSID,1),C'2'                               
         DC    AL1(LWSQ),AL2(TDSAMT-TDSD,L'TDSAMT,CDUENETP-LWSD)                
         DC    AL1(EORQ),AL2(EDIHLNQ+(TDSAMT-TDSD)+L'TDSAMT)                    
*                                                                               
         DC    AL1(CONQ),AL2(AMTID-AMTD,6),C'AMT084'                            
         DC    AL1(CONQ),AL2(AMTQUA-AMTID,L'AMTQUA),C'1  '                      
         DC    AL1(CONQ),AL2(AMTNDP-AMTID,1),C'2'                               
         DC    AL1(LWSQ),AL2(AMTAMT-AMTD,L'AMTAMT,CDUENTLP-LWSD)                
***      DC    AL1(LWSQ),AL2(AMTCDF-AMTD,1,INVTYPE-LWSD)                        
***      DC    AL1(EORQ),AL2(EDIHLNQ+(AMTCDF-AMTD)+L'AMTCDF)                    
         DC    AL1(EORQ),AL2(EDIHLNQ+(AMTAMT-AMTD)+L'AMTAMT)                    
*                                                                               
         DC    AL1(CONQ),AL2(AMTID-AMTD,6),C'AMT084'                            
         DC    AL1(CONQ),AL2(AMTQUA-AMTID,L'AMTQUA),C'N  '                      
         DC    AL1(CONQ),AL2(AMTNDP-AMTID,1),C'2'                               
         DC    AL1(LWSQ),AL2(AMTAMT-AMTD,L'AMTAMT,CDUENTLP-LWSD)                
***      DC    AL1(LWSQ),AL2(AMTCDF-AMTD,1,INVTYPE-LWSD)                        
***      DC    AL1(EORQ),AL2(EDIHLNQ+(AMTCDF-AMTD)+L'AMTCDF)                    
         DC    AL1(EORQ),AL2(EDIHLNQ+(AMTAMT-AMTD)+L'AMTAMT)                    
*                                                                               
         DC    AL1(CONQ),AL2(AMTID-AMTD,6),C'AMT084'                            
         DC    AL1(CONQ),AL2(AMTQUA-AMTID,L'AMTQUA),C'BAP'                      
         DC    AL1(CONQ),AL2(AMTNDP-AMTID,1),C'2'                               
         DC    AL1(LWSQ),AL2(AMTAMT-AMTD,L'AMTAMT,CDUENTLP-LWSD)                
***      DC    AL1(LWSQ),AL2(AMTCDF-AMTD,1,INVTYPE-LWSD)                        
***      DC    AL1(EORQ),AL2(EDIHLNQ+(AMTCDF-AMTD)+L'AMTCDF)                    
         DC    AL1(EORQ),AL2(EDIHLNQ+(AMTAMT-AMTD)+L'AMTAMT)                    
*                                                                               
         DC    AL1(CONQ),AL2(CTTID-CTTD,6),C'CTT089'                            
         DC    AL1(CONQ),AL2(CTTNDP-CTTD,1),C'0'                                
         DC    AL1(CONQ),AL2(CTTITM-CTTD,1),C'1'                                
         DC    AL1(EORQ),AL2(EDIHLNQ+(CTTITM-CTTD)+1)                           
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
         TITLE 'STARCOM FOR DENNY''S'                                           
*********************************************************************           
* STARCOM - DENNY'S                                                 *           
*********************************************************************           
H9DNROU  NTR1  BASE=*,LABEL=*      DENNY'S - SPOT                               
         CHI   R1,BEFRQ            BEFORE CARDS                                 
         JE    XIT                                                              
         CHI   R1,AFTRQ             AFTER CARDS                                 
         JE    XIT                                                              
*                                                                               
         SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     H9DNHDR                                                          
         B     H9DNBIG                                                          
         B     H9DNIT1                                                          
*                                                                               
H9DNHDR  MVC   AGYPROF,SPACES                                                   
         MVC   AGYPROF(8),=C'ARD-SPOT'                                          
         L     RF,=A(DARCY)                                                     
         MVC   AGYNAME,0(RF)       AGENCY IS DARCY                              
         J     XIT                                                              
*                                                                               
H9DNBIG  XC    ALINITM,ALINITM                                                  
         ZAP   ITMCNT,PZERO                                                     
         J     XIT                                                              
*                                                                               
H9DNIT1  ICM   R4,15,ALINITM       R4=A(NEXT LINE ITEM)                         
         BZ    *+12                                                             
         LA    R4,LINLNQ(R4)                                                    
         B     *+8                                                              
         ICM   R4,15,ALINTAB       R4=A(FIRST IN LINE TABLE)                    
*                                                                               
         USING LIND,R4                                                          
         ST    R4,ALINITM          SAVE ADDRESS OF LINE ITEM                    
         AP    ITMCNT,=P'1'        ITEM COUNT                                   
         OI    ITMCNT+(L'ITMCNT-1),X'0F'                                        
         UNPK  CITMCNT,ITMCNT                                                   
         UNPK  CLINDUE,LINDUE                                                   
         LA    R1,CLINDUE+L'CLINDUE-1                                           
         GOTOR FIXNEG                                                           
         GOTOR CNVD,DMCB,(9,LINMOS),(20,ITMDCYMD)                               
         MVC   ITMMCY(2),ITMDCYMD+4       GET MMCCYY FROM CCYYMMDD              
         MVC   ITMMCY+2(4),ITMDCYMD                                             
         MVC   YRMRKT,SPACES              BUILD  MMCCYY - MARKET NAME           
         MVC   YRMRKT(L'ITMMCY),ITMMCY    MMCCYY                                
         MVC   YRMRKT+L'ITMMCY+1(L'MRKTNAM),LINMKTN                             
         MVI   QNTY1,C'0'          SET QUANTITY OF ONE                          
         MVC   QNTY1+1(L'QNTY1-1),QNTY1                                         
         MVI   QNTY1+(L'QNTY1-1),C'1'                                           
         CP    LINDUE,PZERO                                                     
         BNL   *+8                                                              
         MVI   QNTY1+(L'QNTY1-1),X'98' SET TO MINUS ONE                         
                                                                                
         MVI   RTNROU,C'N'                                                      
         LA    R4,LINLNQ(R4)                                                    
         CLI   0(R4),EOT                                                        
         JE    XIT                                                              
         MVI   RTNROU,C'Y'         SET RETURN FOR NEXT                          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
* STARCOM  DENNY'S                                                              
H9DN     DC    AL2(H9DNROU-EDI)     CONTROL ROUTINE                             
         DC    AL2(H9DNHDM-EDI)     FILE HEADER RECORD(S)                       
         DC    AL2(H9DNIVM-EDI)     INVOICE HEADER RECORDS                      
         DC    AL2(H9DNLNM-EDI)     INVOICE LINE ITEMS RECORDS                  
         DC    AL2(H9DNTOM-EDI)     INVOICE TOTAL RECORDS                       
         DC    AL1(EOT)                                                         
*                                                                               
H9DNHDRQ EQU   1                                                                
H9DNBIGQ EQU   2                                                                
H9DNIT1Q EQU   3                                                                
         EJECT                                                                  
*********************************************************************           
* DENNY'S  MAP TABLE(S) - SEE MAPD                                  *           
*********************************************************************           
H9DNHDM  DS    0X                         SPOT- DENNYS                          
         DC    AL1(ROUQ),AL2(H9DNHDRQ)                                          
         DC    AL1(CONQ),AL2(HDRID-HDRD,6),C'000000'                            
         DC    AL1(CONQ),AL2(HDRZRO-HDRD,5),C'00000'                            
         DC    AL1(CONQ),AL2(HDRDDS-HDRD,3),C'DDS'                              
         DC    AL1(LWSQ),AL2(HDRPROF-HDRD,L'AGYPROF,AGYPROF-LWSD)               
         DC    AL1(EORQ),AL2(EDIHDRQ)                                           
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
H9DNIVM  DS    0X                      INVOICE HEADER                           
         DC    AL1(ROUQ),AL2(H9DNBIGQ)                                          
         DC    AL1(CONQ),AL2(BIGID-BIGD,6),C'BIG002'                            
         DC    AL1(LWSQ),AL2(BIGDATE-BIGD,L'BIGDATE,INVDCYMD-LWSD)              
         DC    AL1(LWSQ),AL2(BIGINVN-BIGD,L'INVNUMB,INVNUMB-LWSD)               
         DC    AL1(EORQ),AL2(EDIHLNQ+(BIGINVN-BIGD)+L'BIGINVN)                  
*                                                                               
         DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF005'                            
         DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'AP '                              
         DC    AL1(CONQ),AL2(REFRI-REFD,6),C'262261'                            
         DC    AL1(CONQ),AL2(REFDES-REFD,17),C'MARKETING ACCRUAL'               
         DC    AL1(EORQ),AL2(EDIHLNQ+(REFDES-REFD)+L'REFDES)                    
*                                                                               
         DC    AL1(CONQ),AL2(N1ID-N1D,6),C'N1 008'                              
         DC    AL1(CONQ),AL2(N1EIC-N1D,L'N1EIC),C'VN '                          
         DC    AL1(LWSQ),AL2(N1NAME-N1D,L'AGYNAME,AGYNAME-LWSD)                 
         DC    AL1(CONQ),AL2(N1ICQ-N1D,L'N1ICQ),C'ZZ'                           
         DC    AL1(CONQ),AL2(N1IC-N1D,5),C'43365'                               
         DC    AL1(EORQ),AL2(EDIHLNQ+(N1IC-N1ID)+L'N1IC)                        
*                                                                               
         DC    AL1(CONQ),AL2(N4ID-N4D,6),C'N4 011'                              
         DC    AL1(CONQ),AL2(N4CITY-N4D,7),C'DETROIT'                           
         DC    AL1(CONQ),AL2(N4STATE-N4D,2),C'MI'                               
         DC    AL1(EORQ),AL2(EDIHLNQ+(N4STATE-N4ID)+L'N4STATE)                  
*                                                                               
         DC    AL1(CONQ),AL2(N1ID-N1D,6),C'N1 008'                              
         DC    AL1(CONQ),AL2(N1EIC-N1D,L'N1EIC),C'ST '                          
         DC    AL1(CONQ),AL2(N1NAME-N1D,19),C'DENNY''S RESTAURANTS'             
         DC    AL1(CONQ),AL2(N1ICQ-N1D,L'N1ICQ),C'ZZ'                           
         DC    AL1(CONQ),AL2(N1IC-N1D,5),C'20000'                               
         DC    AL1(EORQ),AL2(EDIHLNQ+(N1IC-N1ID)+L'N1IC)                        
*                                                                               
         DC    AL1(CONQ),AL2(N4ID-N4D,6),C'N4 011'                              
         DC    AL1(CONQ),AL2(N4CITY-N4D,11),C'SPARTANBURG'                      
         DC    AL1(CONQ),AL2(N4STATE-N4D,2),C'SC'                               
         DC    AL1(EORQ),AL2(EDIHLNQ+(N4STATE-N4ID)+L'N4STATE)                  
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
H9DNLNM  DS    0X                      ITEM DETAIL                              
         DC    AL1(BEGQ),AL2(H9DNIT1Q)                                          
         DC    AL1(CONQ),AL2(IT1ID-IT1D,6),C'IT1035'                            
         DC    AL1(LWSQ),AL2(IT1AID-IT1D,6,CITMCNT-LWSD)                        
         DC    AL1(LWSQ),AL2(IT1QNTY-IT1D,L'IT1QNTY,QNTY1-LWSD)                 
         DC    AL1(CONQ),AL2(IT1BMC-IT1D,2),C'EA'                               
         DC    AL1(CONQ),AL2(IT1NDP-IT1ID,1),C'2'                               
         DC    AL1(CONQ),AL2(IT1ZERO-IT1ID,1),C'0'                              
         DC    AL1(LWSQ),AL2(IT1UP-IT1ID,L'IT1UP,CLINDUE-LWSD)                  
         DC    AL1(CONQ),AL2(IT1PSIQ-IT1D,2),C'VN'                              
         DC    AL1(LWSQ),AL2(IT1PSI-IT1D,6,PO#-LWSD)                            
         DC    AL1(EORQ),AL2(EDIHLNQ+(IT1PSI-IT1D)+L'IT1PSI)                    
*                                                                               
         DC    AL1(CONQ),AL2(PIDID-PIDD,6),C'PID063'                            
         DC    AL1(CONQ),AL2(PIDIDT-PIDD,L'PIDIDT),C'F'                         
         DC    AL1(LWSQ),AL2(PIDDESC-PIDD,L'YRMRKT,YRMRKT-LWSD)                 
         DC    AL1(EORQ),AL2(EDIHLNQ+(PIDDESC-PIDD)+L'PIDDESC)                  
         DC    AL1(ENDQ)               END LOOP                                 
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
H9DNTOM  DS    0X                      INVOICE TOTALS                           
         DC    AL1(CONQ),AL2(TDSID-TDSD,6),C'TDS081'                            
         DC    AL1(CONQ),AL2(TDSNDP-TDSID,1),C'2'                               
         DC    AL1(LWSQ),AL2(TDSAMT-TDSD,L'TDSAMT,CDUENET-LWSD)                 
         DC    AL1(EORQ),AL2(EDIHLNQ+(TDSAMT-TDSD)+L'TDSAMT)                    
*                                                                               
         DC    AL1(CONQ),AL2(CTTID-CTTD,6),C'CTT089'                            
         DC    AL1(CONQ),AL2(CTTNDP-CTTD,1),C'0'                                
         DC    AL1(LWSQ),AL2(CTTITM-CTTD,6,CITMCNT-LWSD)                        
         DC    AL1(EORQ),AL2(EDIHLNQ+(CTTITM-CTTD)+L'CTTITM)                    
         DC    AL1(EOTQ,EOTQ)                                                   
         TITLE 'OMD FOR ELI LILLY'                                              
*********************************************************************           
* OMDUSA - ELI LILLY                                               *            
*********************************************************************           
OMELROU  NTR1  BASE=*,LABEL=*      ELI LILLY - SPOT                             
         CHI   R1,BEFRQ            BEFORE CARDS                                 
         JE    XIT                                                              
         CHI   R1,AFTRQ             AFTER CARDS                                 
         JE    XIT                                                              
*                                                                               
         SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     OMELHDR                                                          
         B     OMELBIG                                                          
         B     OMELINV                                                          
*                                                                               
OMELHDR  MVC   AGYPROF,SPACES                                                   
         MVC   AGYPROF(2),AGYALPHA                                              
         MVC   AGYPROF+2(8),=C'-ELI-NET'                                        
         CLI   MEDIA,C'N'                                                       
         BE    OMEL05                                                           
         MVC   AGYPROF+2(9),=C'-ELI-SPOT'                                       
         CLI   SYSTM,C'S'                                                       
         BE    OMEL05                                                           
         MVC   AGYPROF+2(10),=C'-ELI-PRINT'                                     
*                                                                               
OMEL05   MVC   SVINVNUM,INVNUMB                                                 
         MVC   INVNUMB,SPACES                                                   
         LA    RE,SVINVNUM                                                      
         LA    RF,INVNUMB                                                       
         LA    R1,L'INVNUMB                                                     
OMEL10   CLI   0(RE),C'-'          IS IT A DASH                                 
         BE    *+14                SKIP IT                                      
         MVC   0(1,RF),0(RE)       MOVE FROM SVINVNUM TO INVNUMB                
         LA    RF,1(RF)            BUMP INVNUMB                                 
         LA    RE,1(RE)            BUMP SAVED INVNUMB                           
         BCT   R1,OMEL10                                                        
*                                                                               
         MVC   ELUOM,=C'PC'        UOM TYPE                                     
         CLC   ELPO#(2),=C'45'     PO STARTS WITH 45?                           
         BE    OMEL12                                                           
         MVC   ELUOM,=C'EA'       OTHERWISE SET TO DEFAULT                      
*                                                                               
OMEL12   LA    R1,ELMEDTAB        SET MEDIA NAME FROM TABLE                     
         MVC   WORK(1),SYSTM                                                    
         MVC   WORK+1(1),MEDIA+1                                                
*                                                                               
OMEL15   CLC   0(2,R1),WORK                                                     
         BE    OMEL20                                                           
         LA    R1,15(R1)                                                        
         CLI   0(R1),X'FF'       END OF TABLE                                   
         BNE   OMEL15                                                           
         DC    H'0'              UNKNOWN SYSTEM/MEDIA                           
*                                                                               
OMEL20   MVC   ELMEDN,2(R1)                                                     
*                                                                               
OMELHRX  DS    0H                                                               
         J     XIT                                                              
*                                                                               
OMELBIG  XC    ALINITM,ALINITM                                                  
         ZAP   ITMCNT,PZERO                                                     
*                                                                               
         MVC   INVTYPE,=C'DI'        DEBIT/CREDIT                               
         CP    DUEAMT,PZERO                                                     
         BNL   *+10                                                             
         MVC   INVTYPE,=C'CR'                                                   
*                                                                               
         UNPK  CLINGRS,MITOTOGR                                                 
         LA    R1,CLINGRS+L'CLINGRS-1                                           
         GOTOR FIXNEG                                                           
*                                                                               
         L     R4,AMITTAB          GET MOS FROM FIRST MITTAB ENTRY              
         USING MITD,R4                                                          
         GOTOR CNVD,DMCB,(9,MITMOS),(20,ELMOS)                                  
         J     XIT                                                              
         DROP  R4                                                               
*                                                                               
OMELINV  DS    0H                                                               
         MVI   QNTY1,C'0'          SET QUANTITY OF ONE                          
         MVC   QNTY1+1(L'QNTY1-1),QNTY1                                         
         MVI   QNTY1+(L'QNTY1-1),C'1'                                           
         CP    DUEAMT,PZERO                                                     
         BNL   *+8                                                              
         MVI   QNTY1+(L'QNTY1-1),X'98' SET TO MINUS ONE                         
         J     XIT                                                              
*                                                                               
OMELIT1  DC    H'0'                DON'T DO ANYTHING                            
*                                                                               
**ELIT1  ICM   R4,15,ALINITM       R4=A(NEXT LINE ITEM)                         
         BZ    *+12                                                             
         LA    R4,LINLNQ(R4)                                                    
         B     *+8                                                              
         ICM   R4,15,ALINTAB       R4=A(FIRST IN LINE TABLE)                    
*                                                                               
         USING LIND,R4                                                          
         ST    R4,ALINITM          SAVE ADDRESS OF LINE ITEM                    
         MVC   OVNDNAME,SPACES                                                  
         MVC   OVNDNAMC,SPACES                                                  
         CLI   SYSTM,C'S'          SPOT/NET                                     
         BNE   OMELI5                                                           
         MVC   OVNDNAME(L'LINSTA),LINSTA     STATION OR NETWORK                 
         MVC   OVNDNAMC,OVNDNAME                                                
         B     OMELI10                                                          
*                                                                               
OMELI5   MVC   OVNDNAME,LINPUB                                                  
         MVC   OVNDNAMC,OVNDNAME                                                
*                                                                               
OMELI10  DS    0H                                                               
         AP    ITMCNT,=P'1'        ITEM COUNT                                   
         OI    ITMCNT+(L'ITMCNT-1),X'0F'                                        
         UNPK  CITMCNT,ITMCNT                                                   
*****    EDIT  LINGRS,(L'CLINGRS,CLINGRS),2,ALIGN=LEFT,FLOAT=-,ZERO=NOB         
*****          LANK                                                             
*                                                                               
         UNPK  CLINGRS,LINGRS                                                   
         LA    R1,CLINGRS+L'CLINGRS-1                                           
         GOTOR FIXNEG                                                           
         MVI   QNTY1,C'0'          SET QUANTITY OF ONE                          
         MVC   QNTY1+1(L'QNTY1-1),QNTY1                                         
         MVI   QNTY1+(L'QNTY1-1),C'1'                                           
         CP    LINDUE,PZERO                                                     
         BNL   *+8                                                              
         MVI   QNTY1+(L'QNTY1-1),X'98' SET TO MINUS ONE                         
                                                                                
         MVI   RTNROU,C'N'                                                      
         LA    R4,LINLNQ(R4)                                                    
         CLI   0(R4),EOT                                                        
         JE    XIT                                                              
         MVI   RTNROU,C'Y'         SET RETURN FOR NEXT                          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
*                                                                               
ELMEDTAB DC    C'PB',CL13'MOBILE'                                               
         DC    C'PD',CL13'DIGITAL AUDIO'                                        
         DC    C'PI',CL13'INTERACTIVE'                                          
         DC    C'PL',CL13'SOCIAL'                                               
         DC    C'PM',CL13'MAGAZINES'                                            
         DC    C'PN',CL13'NEWSPAPERS'                                           
         DC    C'PO',CL13'OUTDOOR'                                              
         DC    C'PS',CL13'SUPPLEMENTS'                                          
         DC    C'PT',CL13'TRADE'                                                
         DC    C'PV',CL13'NAT. VIDEO'                                           
         DC    C'PW',CL13'LOC. VIDEO'                                           
*                                                                               
         DC    C'SN',CL13'NETWORK'                                              
         DC    C'ST',CL13'TELEVISION'                                           
         DC    C'SR',CL13'RADIO'                                                
         DC    C'SX',CL13'RADIO NETWORK'                                        
         DC    X'FFFF'              END OF TABLE                                
         DROP  RB                                                               
                                                                                
*                                                                               
* OMDUSA ELI LILLY                                                              
OMEL     DC    AL2(OMELROU-EDI)     CONTROL ROUTINE                             
         DC    AL2(OMELHDM-EDI)     FILE HEADER RECORD(S)                       
         DC    AL2(OMELIVM-EDI)     INVOICE HEADER RECORDS                      
         DC    AL2(0)               INVOICE LINE ITEMS RECORDS                  
         DC    AL2(OMELTOM-EDI)     INVOICE TOTAL RECORDS                       
         DC    AL1(EOT)                                                         
*                                                                               
OMELHDRQ EQU   1                                                                
OMELBIGQ EQU   2                                                                
OMELINVQ EQU   3                                                                
         EJECT                                                                  
*********************************************************************           
* ELI LILLY MAP TABLE(S) - SEE MAPD                               *             
*********************************************************************           
OMELHDM  DS    0X                         ELI LILLY                             
         DC    AL1(ROUQ),AL2(OMELHDRQ)                                          
         DC    AL1(CONQ),AL2(HDRID-HDRD,6),C'000000'                            
         DC    AL1(CONQ),AL2(HDRZRO-HDRD,5),C'00000'                            
         DC    AL1(CONQ),AL2(HDRDDS-HDRD,3),C'DDS'                              
         DC    AL1(LWSQ),AL2(HDRPROF-HDRD,L'AGYPROF,AGYPROF-LWSD)               
         DC    AL1(EORQ),AL2(EDIHDRQ)                                           
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
OMELIVM  DS    0X                      INVOICE HEADER                           
         DC    AL1(ROUQ),AL2(OMELBIGQ)                                          
         DC    AL1(CONQ),AL2(BIGID-BIGD,6),C'BIG002'                            
         DC    AL1(LWSQ),AL2(BIGDATE-BIGD,L'BIGDATE,INVDCYMD-LWSD)              
         DC    AL1(LWSQ),AL2(BIGINVN-BIGD,L'INVNUMB,INVNUMB-LWSD)               
         DC    AL1(LWSQ),AL2(BIGDATE1-BIGD,L'ELMOS,ELMOS-LWSD)                  
         DC    AL1(LWSQ),AL2(BIGPON-BIGD,L'ELPO#,ELPO#-LWSD)                    
         DC    AL1(LWSQ),AL2(BIGTYPE-BIGD,L'BIGTYPE,INVTYPE-LWSD)               
         DC    AL1(EORQ),AL2(EDIHLNQ+(BIGTYPE-BIGD)+L'BIGTYPE)                  
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
**ELLNM  DS    0X                      ITEM DETAIL                              
**       DC    AL1(BEGQ),AL2(OMELIT1Q)                                          
**       DC    AL1(CONQ),AL2(IT1ID-IT1D,6),C'IT1035'                            
**       DC    AL1(LWSQ),AL2(IT1AID-IT1D,5,CITMCNT+1-LWSD)                      
**       DC    AL1(LWSQ),AL2(IT1QNTY-IT1D,L'IT1QNTY,QNTY1-LWSD)                 
**       DC    AL1(LWSQ),AL2(IT1BMC-IT1D,L'ELUOM,ELUOM-LWSD)                    
**       DC    AL1(CONQ),AL2(IT1NDP-IT1ID,1),C'2'                               
**       DC    AL1(CONQ),AL2(IT1ZERO-IT1ID,1),C'0'                              
**       DC    AL1(LWSQ),AL2(IT1UP-IT1ID,L'IT1UP,CLINGRS-LWSD)                  
**       DC    AL1(CONQ),AL2(IT1PSIQ-IT1D,2),C'VP'                              
**       DC    AL1(LWSQ),AL2(IT1PSI-IT1D,L'OVNDNAMC,OVNDNAMC-LWSD)              
**       DC    AL1(EORQ),AL2(EDIHLNQ+(IT1PSI-IT1D)+L'IT1PSI)                    
**                                                                              
**                                                                              
**       DC    AL1(ENDQ)               END LOOP                                 
**       DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
OMELTOM  DS    0X                      INVOICE TOTALS                           
         DC    AL1(ROUQ),AL2(OMELINVQ)                                          
         DC    AL1(CONQ),AL2(IT1ID-IT1D,6),C'IT1035'                            
         DC    AL1(CONQ),AL2(IT1AID-IT1D,5),C'00001'                            
         DC    AL1(LWSQ),AL2(IT1QNTY-IT1D,L'IT1QNTY,QNTY1-LWSD)                 
         DC    AL1(LWSQ),AL2(IT1BMC-IT1D,L'ELUOM,ELUOM-LWSD)                    
         DC    AL1(CONQ),AL2(IT1NDP-IT1ID,1),C'2'                               
         DC    AL1(CONQ),AL2(IT1ZERO-IT1ID,1),C'0'                              
         DC    AL1(LWSQ),AL2(IT1UP-IT1ID,L'IT1UP,CLINGRS-LWSD)                  
         DC    AL1(CONQ),AL2(IT1PSIQ-IT1D,2),C'VP'                              
         DC    AL1(LWSQ),AL2(IT1PSI-IT1D,L'ELMEDN,ELMEDN-LWSD)                  
         DC    AL1(EORQ),AL2(EDIHLNQ+(IT1PSI-IT1D)+L'IT1PSI)                    
                                                                                
         DC    AL1(CONQ),AL2(PIDID-PIDD,6),C'PID063'                            
         DC    AL1(CONQ),AL2(PIDIDT-PIDD,L'PIDIDT),C'F'                         
         DC    AL1(LWSQ),AL2(PIDDESC-PIDD,L'PRCODE1,PRCODE1-LWSD)               
         DC    AL1(LWSQ),AL2(PIDDESC+4-PIDD,L'PRNAME1,PRNAME1-LWSD)             
         DC    AL1(EORQ),AL2(EDIHLNQ+(PIDDESC-PIDD)+L'PIDDESC)                  
*                                                                               
         DC    AL1(CONQ),AL2(PIDID-PIDD,6),C'PID063'                            
         DC    AL1(CONQ),AL2(PIDIDT-PIDD,L'PIDIDT),C'F'                         
         DC    AL1(LWSQ),AL2(PIDDESC-PIDD,L'ESCODE1,ESCODE1-LWSD)               
         DC    AL1(LWSQ),AL2(PIDDESC+4-PIDD,L'ESNAME1,ESNAME1-LWSD)             
         DC    AL1(LWSQ),AL2(PIDDESC+25-PIDD,L'ESNAME2,ESNAME2-LWSD)            
         DC    AL1(EORQ),AL2(EDIHLNQ+(PIDDESC-PIDD)+L'PIDDESC)                  
*                                                                               
         DC    AL1(CONQ),AL2(TDSID-TDSD,6),C'TDS081'                            
         DC    AL1(CONQ),AL2(TDSNDP-TDSID,1),C'2'                               
         DC    AL1(LWSQ),AL2(TDSAMT-TDSD,L'TDSAMT,CDUENET-LWSD)                 
         DC    AL1(EORQ),AL2(EDIHLNQ+(TDSAMT-TDSD)+L'TDSAMT)                    
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
         TITLE 'OMD FOR MC-DONALD CANADA'                                       
*********************************************************************           
* OMD-MCDONALD                                                      *           
*********************************************************************           
OMMCROU  NTR1  BASE=*,LABEL=*                                                   
         CHI   R1,BEFRQ            BEFORE CARDS                                 
         BNE   OMMCAFTR                                                         
         MVC   REGION,ZEROS                                                     
         MVC   OMUCATTN,SPACES                                                  
         J     XIT                                                              
*                                                                               
OMMCAFTR CHI   R1,AFTRQ            AFTER CARDS                                  
         BNE   OMMCR                                                            
         J     EIXIT                                                            
*                                                                               
OMMCR    SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     OMMCHDR                                                          
         B     OMMCINV                                                          
         B     OMMCGRS                                                          
         B     OMMCCOM                                                          
         B     OMMCREF                                                          
         B     OMMCIT1                                                          
         B     OMMCTAX                                                          
         B     OMMCQST                                                          
*                                                                               
                                                                                
OMMCHDR  MVC   AGYPROF,SPACES                                                   
         MVC   AGYPROF(13),=C'MCDONALD-BLNG'                                    
*                                                                               
         MVC   SVINVNUM,INVNUMB                                                 
         MVC   INVNUMB,SPACES                                                   
         LA    RE,SVINVNUM                                                      
         LA    RF,INVNUMB                                                       
         LA    R1,L'INVNUMB                                                     
OMHDR10  CLI   0(RE),C'-'          IS IT A DASH                                 
         BE    *+14                SKIP IT                                      
         MVC   0(1,RF),0(RE)       MOVE FROM SVINVNUM TO INVNUMB                
         LA    RF,1(RF)            BUMP INVNUMB                                 
         LA    RE,1(RE)            BUMP SAVED INVNUMB                           
         BCT   R1,OMHDR10                                                       
*                                                                               
         J     XIT                                                              
*                                                                               
OMMCINV  DS    0H                                                               
         ZAP   ITMCNT,PZERO                                                     
         XC    ALINITM,ALINITM     CLEAR A(NEXT LINE ITEM)                      
         XC    AIDPITM,AIDPITM     CLEAR A(NEXT ID RECORD)                      
*                                                                               
         MVC   OMATTN,OMUCATTN        SET FROM USER COMM                        
         OC    OMATTN,SPACES                                                    
         CLC   OMATTN,SPACES          SEE IF PRESENT                            
         BH    *+10                                                             
         MVC   OMATTN,OMNUFDAT        IF MISSING USE EST USER DEF               
*                                                                               
OMMCINX  J     XIT                                                              
*                                                                               
*                                  GROSS HOOK                                   
OMMCGRS  MVI   NEQNTY1,C'0'        SET QUANTITY OF ONE                          
         MVC   NEQNTY1+1(L'NEQNTY1-1),NEQNTY1                                   
         MVI   NEQNTY1+(L'NEQNTY1-1),C'1'                                       
         UNPK  CDUECOM,COMAMT1                                                  
         LA    R1,CDUECOM+L'CDUECOM-1                                           
         GOTOR FIXNEG                                                           
         AP    ITMCNT,=P'1'        ITEM COUNT                                   
         OI    ITMCNT+(L'ITMCNT-1),X'0F'                                        
         UNPK  CITMCNT,ITMCNT                                                   
*                                                                               
         CLI   SYSTM,C'P'                                                       
         BE    *+10                                                             
         MVC   ESNAME2,SPACES                                                   
         J     XIT                                                              
*                                   COMMISSION HOOK                             
OMMCCOM  DS    0H                                                               
         J     XIT                                                              
*                                                                               
OMMCREF  DS    0H                                                               
         J     XIT                                                              
*                                                                               
OMMCIT1  DS    0H                                                               
         MVC   IDATSTA,SPACES                                                   
*                                                                               
         CLI   SYSTM,C'P'                                                       
         BE    OMMCIT10                                                         
*                                                                               
         ICM   R4,15,ALINITM       R4=A(NEXT LINE ITEM)                         
         BZ    *+12                                                             
         LA    R4,LINLNQ(R4)                                                    
         B     *+8                                                              
         ICM   R4,15,ALINTAB       R4=A(FIRST IN LINE TABLE)                    
*                                                                               
         USING LIND,R4                                                          
         MVI   RTNROU,C'N'                                                      
         CLI   0(R4),EOT                                                        
         JE    XIT                                                              
*                                                                               
         ZAP   PIDCNT,=P'0'                                                     
         ST    R4,ALINITM          SAVE ADDRESS OF LINE ITEM                    
*                                                                               
         AP    ITMCNT,=P'1'        ITEM COUNT                                   
*                                                                               
         UNPK  CLINGRS,LINGRS                                                   
         LA    R1,CLINGRS+L'CLINGRS-1                                           
         GOTOR FIXNEG                                                           
         CP    LINGRS,PZERO                                                     
         BNL   *+8                                                              
         MVI   QNTY1+(L'QNTY1-1),X'98' SET TO MINUS ONE                         
*                                                                               
         MVC   IDATSTA(L'INVNUMB),INVNUMB                                       
         MVC   IDATSTA+L'INVNUMB+1(L'LINSTA),LINSTA                             
         MVC   IDATSTA+L'INVNUMB+L'LINSTA+2(L'LINMOS),LINMOS                    
*                                                                               
         MVI   RTNROU,C'N'                                                      
         LA    R4,LINLNQ(R4)                                                    
         CLI   0(R4),EOT                                                        
         JE    XIT                                                              
         MVI   RTNROU,C'Y'         SET RETURN FOR NEXT                          
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
OMMCIT10 ICM   R4,15,AIDPITM       R4=A(NEXT LINE ITEM)                         
         BZ    *+12                                                             
         LA    R4,IDPLNQ(R4)                                                    
         B     *+8                                                              
         ICM   R4,15,AIDPTAB       R4=A(FIRST IN LINE TABLE)                    
*                                                                               
         USING IDPD,R4                                                          
         MVI   RTNROU,C'N'                                                      
         CLI   0(R4),EOT                                                        
         JE    XIT                                                              
*                                                                               
         ZAP   PIDCNT,=P'0'                                                     
         ST    R4,AIDPITM          SAVE ADDRESS OF LINE ITEM                    
*                                                                               
         AP    ITMCNT,=P'1'        ITEM COUNT                                   
         OI    ITMCNT+(L'ITMCNT-1),X'0F'                                        
         UNPK  CITMCNT,ITMCNT                                                   
*                                                                               
         UNPK  CLINGRS,IDPOGRS                                                  
         LA    R1,CLINGRS+L'CLINGRS-1                                           
         GOTOR FIXNEG                                                           
         CP    IDPOGRS,PZERO                                                    
         BNL   *+8                                                              
         MVI   QNTY1+(L'QNTY1-1),X'98' SET TO MINUS ONE                         
*                                                                               
         MVC   IDATSTA(L'INVNUMB),INVNUMB                                       
         MVC   IDATSTA+L'INVNUMB+1(L'PUB),IDPPUB                                
         MVC   IDATSTA+L'INVNUMB+L'PUB+2(L'IDPDATE),IDPDATE                     
*                                                                               
         MVI   RTNROU,C'N'                                                      
         LA    R4,IDPLNQ(R4)                                                    
         CLI   0(R4),EOT                                                        
         JE    XIT                                                              
         MVI   RTNROU,C'Y'         SET RETURN FOR NEXT                          
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                  TAX HOOK                                     
OMMCTAX  DS    0H                                                               
         AP    TOTTAX,GSTTAXT             TOT=GST+HST                           
         AP    TOTTAX,HSTTAXT                                                   
         UNPK  TOTTAXC,TOTTAX                                                   
         LA    R1,TOTTAXC+L'TOTTAXC-1                                           
         GOTOR FIXNEG                                                           
*                                                                               
         CP    QSTTAX,=P'0'                IF QST = 0 SUPPRESS SEGMENT          
         JNE   OMMCT10                                                          
         MVI   SKIPREC,C'Y'                                                     
*                                                                               
OMMCT10  UNPK  QSTTAXC,QSTTAX              QST TOTAL                            
         LA    R1,QSTTAXC+L'QSTTAXC-1                                           
         GOTOR FIXNEG                                                           
*                                                                               
         J     XIT                                                              
*                                  TAX HOOK                                     
OMMCQST  DS    0H                                                               
*                                                                               
         CP    QSTTAXT,=P'0'       IF QST TOTAL = 0 SUPPRESS SEGMENT            
         JNE   OMMCQ10                                                          
         MVI   SKIPREC,C'Y'        SKIP THIS SEGMENT FROM TAPE                  
*                                                                               
OMMCQ10  UNPK  QSTTAXTC,QSTTAXT    QST TOTAL                                    
         LA    R1,QSTTAXTC+L'QSTTAXTC-1                                         
         GOTOR FIXNEG                                                           
         J     XIT                                                              
*                                                                               
                                                                                
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
* OMD-MCDONALD                                                                  
OMMCDNLD DC    AL2(OMMCROU-EDI)      CONTROL ROUTINE                            
         DC    AL2(OMMCHDM-EDI)      FILE HEADER RECORD(S)                      
         DC    AL2(OMMCIVM-EDI)      INVOICE HEADER RECORDS                     
         DC    AL2(0)                INVOICE LINE ITEMS RECORDS                 
         DC    AL2(0)                INVOICE TOTAL RECORDS                      
         DC    AL1(EOT)                                                         
*                                                                               
OMMCHDRQ EQU   1                                                                
OMMCINVQ EQU   2                                                                
OMMCGRSQ EQU   3                                                                
OMMCCOMQ EQU   4                                                                
OMMCREFQ EQU   5                                                                
OMMCIT1Q EQU   6                                                                
OMMCTAXQ EQU   7                                                                
OMMCQSTQ EQU   8                                                                
         EJECT                                                                  
*********************************************************************           
* OMD MCDONALD MAP TABLE(S) - SEE MAPD                              *           
*********************************************************************           
                                                                                
OMMCHDM  DS    0X                  OMD/MCDONALD  VERSION 4010                   
         DC    AL1(ROUQ),AL2(OMMCHDRQ)                                          
         DC    AL1(CONQ),AL2(HDRID-HDRD,6),C'000000'                            
         DC    AL1(CONQ),AL2(HDRZRO-HDRD,5),C'00000'                            
         DC    AL1(CONQ),AL2(HDRDDS-HDRD,3),C'DDS'                              
         DC    AL1(LWSQ),AL2(HDRPROF-HDRD,L'AGYPROF,AGYPROF-LWSD)               
         DC    AL1(EORQ),AL2(EDIHDRQ)                                           
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
OMMCIVM  DS    0X                                                               
         DC    AL1(ROUQ),AL2(OMMCINVQ)                                          
         DC    AL1(CONQ),AL2(BIGID-BIGD,6),C'BIG002'                            
         DC    AL1(LWSQ),AL2(BIGDATE-BIGD,L'BIGDATE,INVDCYMD-LWSD)              
         DC    AL1(LWSQ),AL2(BIGINVN-BIGD,L'INVNUMB,INVNUMB-LWSD)               
         DC    AL1(CONQ),AL2(BIGTYPE-BIGD,L'BIGTYPE),C'DI'                      
         DC    AL1(EORQ),AL2(EDIHLNQ+(BIGTYPE-BIGD)+L'BIGTYPE)                  
*                                                                               
         DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF005'                            
         DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'23 '                              
         DC    AL1(CONQ),AL2(REFRI-REFD,3),C'CAN'                               
         DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+4)                            
*                                                                               
         DC    AL1(CONQ),AL2(PERID-PERD,6),C'PER007'                            
         DC    AL1(CONQ),AL2(PERCFC-PERD,L'PERCFC),C'OC'                        
         DC    AL1(LWSQ),AL2(PERNAME-PERD,L'OMATTN,OMATTN-LWSD)                 
         DC    AL1(EORQ),AL2(EDIHLNQ+(PERNAME-PERD)+L'PERNAME)                  
*                                                                               
         DC    AL1(CONQ),AL2(N1ID-N1D,6),C'N1 008'                              
         DC    AL1(CONQ),AL2(N1EIC-N1D,L'N1EIC),C'ST '                          
         DC    AL1(LWSQ),AL2(N1NAME-N1D,L'ADVNAME,ADVNAME-LWSD)                 
         DC    AL1(CONQ),AL2(N1ICQ-N1D,L'N1ICQ),C'92'                           
         DC    AL1(LWSQ),AL2(N1IC-N1D,12,OMN1NAME-LWSD)                         
         DC    AL1(EORQ),AL2(EDIHLNQ+(N1IC-N1ID)+L'N1IC)                        
*                                                                               
         DC    AL1(CONQ),AL2(N1ID-N1D,6),C'N1 008'                              
         DC    AL1(CONQ),AL2(N1EIC-N1D,L'N1EIC),C'VN '                          
         DC    AL1(CONQ),AL2(N1NAME-N1D,10),C'OMD CANADA'                       
         DC    AL1(CONQ),AL2(N1ICQ-N1D,L'N1ICQ),C'92'                           
         DC    AL1(CONQ),AL2(N1IC-N1D,8),C'60066119'                            
         DC    AL1(EORQ),AL2(EDIHLNQ+(N1IC-N1ID)+L'N1IC)                        
*                                                                               
         DC    AL1(CONQ),AL2(DTMID-DTMD,6),C'DTM016'                            
         DC    AL1(CONQ),AL2(DTMDTQ-DTMD,L'DTMDTQ),C'035'                       
         DC    AL1(LWSQ),AL2(DTMDATE-DTMD,L'DTMDATE,INVDCYMD-LWSD)              
         DC    AL1(EORQ),AL2(EDIHLNQ+(DTMDATE-DTMD)+L'DTMDATE)                  
*                                                                               
         DC    AL1(ROUQ),AL2(OMMCGRSQ)                                          
         DC    AL1(CONQ),AL2(IT1ID-IT1D,6),C'IT1035'                            
         DC    AL1(CONQ),AL2(IT1QNTY-IT1ID,1),C'0'                              
         DC    AL1(CONQ),AL2(IT1QNTY+1-IT1D,1),C'1'                             
         DC    AL1(CONQ),AL2(IT1BMC-IT1D,2),C'EA'                               
         DC    AL1(CONQ),AL2(IT1NDP-IT1ID,1),C'0'                               
         DC    AL1(CONQ),AL2(IT1ZERO-IT1ID,1),C'0'                              
         DC    AL1(CONQ),AL2(IT1UP-IT1ID,L'IT1UP),C'0000000000000000'           
         DC    AL1(CONQ),AL2(IT1PSIQ-IT1ID,L'IT1PSIQ),C'VP'                     
         DC    AL1(CONQ),AL2(IT1PSI-IT1ID,4),C'NOTE'                            
         DC    AL1(CONQ),AL2(IT1PSIQ2-IT1ID,L'IT1PSIQ),C'BS'                    
         DC    AL1(LWSQ),AL2(IT1PSI2-IT1ID,L'OMACCT,OMACCT-LWSD)                
         DC    AL1(EORQ),AL2(EDIHLNQ+(IT1PSI2-IT1D)+L'IT1PSI2)                  
*                                                                               
         DC    AL1(CONQ),AL2(PIDID-PIDD,6),C'PID044'                            
         DC    AL1(CONQ),AL2(PIDIDT-PIDD,L'PIDIDT),C'F'                         
         DC    AL1(LWSQ),AL2(PIDDESC-PIDD,L'PRCODE1,PRCODE1-LWSD)               
         DC    AL1(LWSQ),AL2(PIDDESC+4-PIDD,L'PRNAME1,PRNAME1-LWSD)             
         DC    AL1(EORQ),AL2(EDIHLNQ+(PIDDESC-PIDD)+L'PIDDESC)                  
*                                                                               
         DC    AL1(ROUQ),AL2(OMMCGRSQ)                                          
         DC    AL1(CONQ),AL2(IT1ID-IT1D,6),C'IT1035'                            
         DC    AL1(CONQ),AL2(IT1QNTY-IT1ID,1),C'0'                              
         DC    AL1(CONQ),AL2(IT1QNTY+1-IT1D,1),C'1'                             
         DC    AL1(CONQ),AL2(IT1BMC-IT1D,2),C'EA'                               
         DC    AL1(CONQ),AL2(IT1NDP-IT1ID,1),C'0'                               
         DC    AL1(CONQ),AL2(IT1ZERO-IT1ID,1),C'0'                              
         DC    AL1(CONQ),AL2(IT1UP-IT1ID,L'IT1UP),C'0000000000000000'           
         DC    AL1(CONQ),AL2(IT1PSIQ-IT1ID,L'IT1PSIQ),C'VP'                     
         DC    AL1(CONQ),AL2(IT1PSI-IT1ID,4),C'NOTE'                            
         DC    AL1(CONQ),AL2(IT1PSIQ2-IT1ID,L'IT1PSIQ),C'BS'                    
         DC    AL1(LWSQ),AL2(IT1PSI2-IT1ID,L'OMACCT,OMACCT-LWSD)                
         DC    AL1(EORQ),AL2(EDIHLNQ+(IT1PSI2-IT1D)+L'IT1PSI2)                  
*                                                                               
         DC    AL1(CONQ),AL2(PIDID-PIDD,6),C'PID044'                            
         DC    AL1(CONQ),AL2(PIDIDT-PIDD,L'PIDIDT),C'F'                         
         DC    AL1(LWSQ),AL2(PIDDESC-PIDD,L'ESCODE1,ESCODE1-LWSD)               
         DC    AL1(LWSQ),AL2(PIDDESC+4-PIDD,L'ESNAME1,ESNAME1-LWSD)             
         DC    AL1(LWSQ),AL2(PIDDESC+25-PIDD,L'ESNAME2,ESNAME2-LWSD)            
         DC    AL1(EORQ),AL2(EDIHLNQ+(PIDDESC-PIDD)+L'PIDDESC)                  
*                                                                               
         DC    AL1(BEGQ),AL2(OMMCIT1Q)                                          
         DC    AL1(CONQ),AL2(IT1ID-IT1D,6),C'IT1035'                            
         DC    AL1(CONQ),AL2(IT1QNTY-IT1ID,1),C'0'                              
         DC    AL1(CONQ),AL2(IT1QNTY+1-IT1D,1),C'1'                             
         DC    AL1(CONQ),AL2(IT1BMC-IT1D,2),C'EA'                               
         DC    AL1(CONQ),AL2(IT1NDP-IT1ID,1),C'2'                               
         DC    AL1(CONQ),AL2(IT1ZERO-IT1ID,1),C'0'                              
         DC    AL1(LWSQ),AL2(IT1UP-IT1ID,L'CLINGRS,CLINGRS-LWSD)                
         DC    AL1(CONQ),AL2(IT1PSIQ-IT1ID,L'IT1PSIQ),C'VP'                     
         DC    AL1(LWSQ),AL2(IT1PSI-IT1ID,L'MEMEDIA,MEDIA-LWSD)                 
         DC    AL1(CONQ),AL2(IT1PSIQ2-IT1ID,2),C'BS'                            
         DC    AL1(LWSQ),AL2(IT1PSI2-IT1ID,L'OMACCT,OMACCT-LWSD)                
         DC    AL1(EORQ),AL2(EDIHLNQ+(IT1PSI2-IT1D)+L'IT1PSI2)                  
*                                                                               
         DC    AL1(CONQ),AL2(PIDID-PIDD,6),C'PID044'                            
         DC    AL1(CONQ),AL2(PIDIDT-PIDD,L'PIDIDT),C'F'                         
         DC    AL1(LWSQ),AL2(PIDDESC-PIDD,L'PIDDESC,IDATSTA-LWSD)               
         DC    AL1(EORQ),AL2(EDIHLNQ+(PIDDESC-PIDD)+L'PIDDESC)                  
*                                                                               
         DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF050'                            
         DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'P4 '                              
         DC    AL1(LWSQ),AL2(REFRI-REFD,4,REGION-LWSD)                          
         DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+4)                            
         DC    AL1(ENDQ)               END LOOP                                 
*                                                                               
         DC    AL1(ROUQ),AL2(OMMCGRSQ)                                          
         DC    AL1(CONQ),AL2(IT1ID-IT1D,6),C'IT1035'                            
         DC    AL1(CONQ),AL2(IT1QNTY-IT1ID,1),C'0'                              
         DC    AL1(CONQ),AL2(IT1QNTY+1-IT1D,1),C'1'                             
         DC    AL1(CONQ),AL2(IT1BMC-IT1D,2),C'EA'                               
         DC    AL1(CONQ),AL2(IT1NDP-IT1ID,1),C'2'                               
         DC    AL1(CONQ),AL2(IT1ZERO-IT1ID,1),C'0'                              
         DC    AL1(LWSQ),AL2(IT1UP-IT1ID,L'CDUECOM,CDUECOM-LWSD)                
         DC    AL1(CONQ),AL2(IT1PSIQ-IT1ID,L'IT1PSIQ),C'VP'                     
         DC    AL1(LWSQ),AL2(IT1PSI-IT1ID,L'MEMEDIA,MEDIA-LWSD)                 
         DC    AL1(CONQ),AL2(IT1PSIQ2-IT1ID,2),C'BS'                            
         DC    AL1(LWSQ),AL2(IT1PSI2-IT1ID,L'OMACCT,OMACCT-LWSD)                
         DC    AL1(EORQ),AL2(EDIHLNQ+(IT1PSI2-IT1D)+L'IT1PSI2)                  
*                                                                               
*                                           TAX ON DETAIL (IF ANY)              
         DC    AL1(ROUQ),AL2(OMMCTAXQ)                                          
         DC    AL1(CONQ),AL2(TXIID-TXID,6),C'TXI040'                            
         DC    AL1(CONQ),AL2(TXITTC-TXID,2),C'SP'                               
         DC    AL1(CONQ),AL2(TXINDP-TXID,1),C'2'                                
         DC    AL1(CONQ),AL2(TXIZERO-TXID,2),C'00'                              
         DC    AL1(LWSQ),AL2(TXIAMT-TXID,L'QSTTAXC,QSTTAXC-LWSD)                
         DC    AL1(CONQ),AL2(TXINDP1-TXID,1),C'2'                               
         DC    AL1(CONQ),AL2(TXIZERO1-TXID,2),C'00'                             
         DC    AL1(CONQ),AL2(TXIPCNT-TXID,L'TXIPCNT),C'00000750'                
         DC    AL1(EORQ),AL2(EDIHLNQ+(TXIJCODE-TXID)+L'TXIJCODE)                
*                                                                               
         DC    AL1(CONQ),AL2(PIDID-PIDD,6),C'PID044'                            
         DC    AL1(CONQ),AL2(PIDIDT-PIDD,L'PIDIDT),C'F'                         
         DC    AL1(LWSQ),AL2(PIDDESC-PIDD,L'PIDDESC,IDATSTA-LWSD)               
         DC    AL1(EORQ),AL2(EDIHLNQ+(PIDDESC-PIDD)+L'PIDDESC)                  
*                                                                               
         DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF050'                            
         DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'P4 '                              
         DC    AL1(LWSQ),AL2(REFRI-REFD,4,REGION-LWSD)                          
         DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+4)                            
*                                                                               
         DC    AL1(CONQ),AL2(TDSID-TDSD,6),C'TDS081'                            
         DC    AL1(CONQ),AL2(TDSNDP-TDSID,1),C'2'                               
         DC    AL1(LWSQ),AL2(TDSAMT-TDSD,L'TOTDUEC,TOTDUEC-LWSD)                
         DC    AL1(EORQ),AL2(EDIHLNQ+(TDSAMT-TDSD)+L'TDSAMT)                    
*                                                                               
         DC    AL1(CONQ),AL2(TXIID-TXID,6),C'TXI082'                            
         DC    AL1(CONQ),AL2(TXITTC-TXID,2),C'GS'                               
         DC    AL1(CONQ),AL2(TXINDP-TXID,1),C'2'                                
         DC    AL1(CONQ),AL2(TXIZERO-TXID,2),C'00'                              
         DC    AL1(LWSQ),AL2(TXIAMT-TXID,L'TOTTAXC,TOTTAXC-LWSD)                
         DC    AL1(EORQ),AL2(EDIHLNQ+(TXIAMT-TXID)+L'TXIAMT)                    
*                                                                               
         DC    AL1(ROUQ),AL2(OMMCQSTQ)                                          
         DC    AL1(CONQ),AL2(TXIID-TXID,6),C'TXI082'                            
         DC    AL1(CONQ),AL2(TXITTC-TXID,2),C'SP'                               
         DC    AL1(CONQ),AL2(TXINDP-TXID,1),C'2'                                
         DC    AL1(CONQ),AL2(TXIZERO-TXID,2),C'00'                              
         DC    AL1(LWSQ),AL2(TXIAMT-TXID,L'QSTTAXTC,QSTTAXTC-LWSD)              
         DC    AL1(CONQ),AL2(TXICOQA-TXID,L'TXICOQA),C'CD'                      
         DC    AL1(CONQ),AL2(TXIJCODE-TXID,2),C'QC'                             
         DC    AL1(EORQ),AL2(EDIHLNQ+(TXIJCODE-TXID)+L'TXIJCODE)                
*                                                                               
         DC    AL1(CONQ),AL2(CTTID-CTTD,6),C'CTT089'                            
         DC    AL1(CONQ),AL2(CTTNDP-CTTD,1),C'0'                                
         DC    AL1(LWSQ),AL2(CTTITM-CTTD,6,CITMCNT-LWSD)                        
         DC    AL1(EORQ),AL2(EDIHLNQ+(CTTITM-CTTD)+L'CTTITM)                    
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
         EJECT                                                                  
*                                                                               
         TITLE 'DDB NEEDHAM FOR DELL'                                           
*********************************************************************           
* DDB NEEDHAM NE DELL                                               *           
*********************************************************************           
NEDCROU  NTR1  BASE=*,LABEL=*                                                   
         CHI   R1,BEFRQ            BEFORE CARDS                                 
         BNE   NEDCAFTR                                                         
         J     XIT                                                              
*                                                                               
NEDCAFTR CHI   R1,AFTRQ            AFTER CARDS                                  
         BNE   NEDCR                                                            
         J     EIXIT                                                            
*                                                                               
NEDCR    SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     NEDCHDR                                                          
         B     NEDCINV                                                          
         B     NEDCGRS                                                          
         B     NEDCCOM                                                          
         B     NEDCREF                                                          
*                                                                               
                                                                                
NEDCHDR  MVC   AGYPROF,SPACES                                                   
         MVC   AGYPROF(12),=C'DELL-BILLING'                                     
*                                                                               
         MVC   SVINVNUM,INVNUMB                                                 
         MVC   INVNUMB,SPACES                                                   
         LA    RE,SVINVNUM                                                      
         LA    RF,INVNUMB                                                       
         LA    R1,L'INVNUMB                                                     
NEHDR10  CLI   0(RE),C'-'          IS IT A DASH                                 
         BE    *+14                SKIP IT                                      
         MVC   0(1,RF),0(RE)       MOVE FROM SVINVNUM TO INVNUMB                
         LA    RF,1(RF)            BUMP INVNUMB                                 
         LA    RE,1(RE)            BUMP SAVED INVNUMB                           
         BCT   R1,NEHDR10                                                       
*                                                                               
         J     XIT                                                              
*                                                                               
NEDCINV  DS    0H                                                               
NEDCINX  J     XIT                                                              
*                                                                               
*                                  GROSS HOOK                                   
NEDCGRS  MVI   NEQNTY1,C'0'        SET QUANTITY OF ONE                          
         MVC   NEQNTY1+1(L'NEQNTY1-1),NEQNTY1                                   
         MVI   NEQNTY1+(L'NEQNTY1-1),C'1'                                       
         J     XIT                                                              
*                                   COMMISSION HOOK                             
NEDCCOM  DS    0H                                                               
         J     XIT                                                              
*                                                                               
NEDCREF  DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
* DDB NEEDHAM NE - DELL                                                         
NEDELL   DC    AL2(NEDCROU-EDI)      CONTROL ROUTINE                            
         DC    AL2(NEDCHDM-EDI)      FILE HEADER RECORD(S)                      
         DC    AL2(NEDCIVM-EDI)      INVOICE HEADER RECORDS                     
         DC    AL2(0)                INVOICE LINE ITEMS RECORDS                 
         DC    AL2(0)                INVOICE TOTAL RECORDS                      
         DC    AL1(EOT)                                                         
*                                                                               
NEDCHDRQ EQU   1                                                                
NEDCINVQ EQU   2                                                                
NEDCGRSQ EQU   3                                                                
NEDCCOMQ EQU   4                                                                
NEDCREFQ EQU   5                                                                
         EJECT                                                                  
*********************************************************************           
* DDB - DELL   MAP TABLE(S) - SEE MAPD                              *           
*********************************************************************           
                                                                                
NEDCHDM  DS    0X                  DDB/DELL                                     
         DC    AL1(ROUQ),AL2(NEDCHDRQ)                                          
         DC    AL1(CONQ),AL2(HDRID-HDRD,6),C'000000'                            
         DC    AL1(CONQ),AL2(HDRZRO-HDRD,5),C'00000'                            
         DC    AL1(CONQ),AL2(HDRDDS-HDRD,3),C'DDS'                              
         DC    AL1(LWSQ),AL2(HDRPROF-HDRD,L'AGYPROF,AGYPROF-LWSD)               
         DC    AL1(EORQ),AL2(EDIHDRQ)                                           
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
NEDCIVM  DS    0X                                                               
         DC    AL1(ROUQ),AL2(NEDCINVQ)                                          
         DC    AL1(CONQ),AL2(BIGID-BIGD,6),C'BIG002'                            
         DC    AL1(LWSQ),AL2(BIGDATE-BIGD,L'BIGDATE,INVDCYMD-LWSD)              
         DC    AL1(LWSQ),AL2(BIGINVN-BIGD,L'INVNUMB,INVNUMB-LWSD)               
         DC    AL1(LWSQ),AL2(BIGDATE1-BIGD,L'BIGDATE1,INVDCYMD-LWSD)            
         DC    AL1(CONQ),AL2(BIGPON-BIGD,2),C'DO'                               
         DC    AL1(LWSQ),AL2((BIGPON+2)-BIGD,L'PO#,PO#-LWSD)                    
         DC    AL1(EORQ),AL2(EDIHLNQ+(BIGPON-BIGD)+L'BIGPON)                    
*                                                                               
         DC    AL1(CONQ),AL2(CURID-CURD,6),C'CUR004'                            
         DC    AL1(CONQ),AL2(CUREIC-CURD,L'CUREIC),C'ZZ '                       
         DC    AL1(CONQ),AL2(CURCUR-CURD,L'CURCUR),C'USD'                       
         DC    AL1(EORQ),AL2(EDIHLNQ+(CURCUR-CURD)+L'CURCUR)                    
*                                                                               
         DC    AL1(ROUQ),AL2(NEDCGRSQ)                                          
         DC    AL1(CONQ),AL2(IT1ID-IT1D,6),C'IT1035'                            
         DC    AL1(CONQ),AL2(IT1QNTY-IT1ID,1),C'2'                              
         DC    AL1(LWSQ),AL2(IT1QNTY+1-IT1D,10,CDUENET+5-LWSD)                  
         DC    AL1(CONQ),AL2(IT1BMC-IT1D,2),C'EA'                               
         DC    AL1(CONQ),AL2(IT1NDP-IT1ID,1),C'0'                               
         DC    AL1(CONQ),AL2(IT1ZERO-IT1ID,1),C'0'                              
         DC    AL1(LWSQ),AL2(IT1UP-IT1ID,L'IT1UP,NEQNTY1-LWSD)                  
         DC    AL1(EORQ),AL2(EDIHLNQ+(IT1UP-IT1D)+L'IT1UP)                      
*                                                                               
         DC    AL1(CONQ),AL2(CADID-CADD,6),C'CAD055'                            
         DC    AL1(CONQ),AL2(CADSCAC-CADD,L'CADSCAC),C'NONE'                    
         DC    AL1(CONQ),AL2(CADRIQ-CADD,L'CADRIQ),C'LI '                       
         DC    AL1(CONQ),AL2(CADRID-CADD,1),C'1'                                
*        DC    AL1(LWSQ),AL2((CADRID+1)-CADD,L'PO#,PO#-LWSD)                    
         DC    AL1(EORQ),AL2(EDIHLNQ+(CADRID-CADD)+L'CADRID)                    
*                                                                               
         DC    AL1(CONQ),AL2(TDSID-TDSD,6),C'TDS081'                            
         DC    AL1(CONQ),AL2(TDSNDP-TDSID,1),C'2'                               
         DC    AL1(LWSQ),AL2(TDSAMT-TDSD,L'TDSAMT,CDUENET-LWSD)                 
         DC    AL1(EORQ),AL2(EDIHLNQ+(TDSAMT-TDSD)+L'TDSAMT)                    
*                                                                               
         DC    AL1(CONQ),AL2(CTTID-CTTD,6),C'CTT089'                            
         DC    AL1(CONQ),AL2(CTTNDP-CTTD,1),C'0'                                
         DC    AL1(CONQ),AL2(CTTITM-CTTD,6),C'000001'                           
         DC    AL1(EORQ),AL2(EDIHLNQ+(CTTITM-CTTD)+L'CTTITM)                    
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
         EJECT                                                                  
*                                                                               
         TITLE 'Y&&R - FOR AT&&T'                                               
*********************************************************************           
* YNR - AT&T SPECIAL ROUTINES                                       *           
*********************************************************************           
YNATTROU NTR1  BASE=*,LABEL=*      YNR ATT                                      
         CHI   R1,BEFRQ            BEFORE CARDS                                 
         JE    XIT                                                              
         CHI   R1,AFTRQ            AFTER CARDS                                  
         JE    XIT                                                              
*                                                                               
         SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     YNATTHDR                                                         
*                                                                               
YNATTHDR MVC   AGYPROF,SPACES                                                   
         MVC   AGYPROF(8),=C'ATT-SPOT'                                          
         CLI   SYSTM,C'P'                                                       
         BNE   *+10                                                             
         MVC   AGYPROF(9),=C'ATT-PRINT'                                         
*                                                                               
         L     R2,AAGC                                                          
         USING AGCD,R2                                                          
         LA    R6,YNRAGTAB                                                      
         CLC   AGCOPT,0(R6)                                                     
         BE    *+18                                                             
         LA    R6,L'YNRAGTAB(R6)                                                
         CLI   0(R6),EOT                                                        
         BNE   *-18                                                             
         DC    H'0'                                                             
         MVC   PAYSITE,1(R6)       AGENCY PAY SITE                              
         J     XIT                                                              
         DROP  R2                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
YNRAGTAB DS    0XL15                                                            
         DC    AL1(AGCME),C'8CZ7310019-02 '                                     
         DC    AL1(AGCDE),C'424213        '                                     
         DC    AL1(AGCKL),C'ITP08         '                                     
         DC    AL1(AGCBR),C'GE98910003-04 '                                     
         DC    AL1(AGCWW),C'459794NEWYO-01'                                     
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*********************************************************************           
* CONTROL TABLE - SEE CSCD                                          *           
*********************************************************************           
* YNR - ALL - AT*T                                                              
YNATT    DC    AL2(YNATTROU-EDI)     HEADER ROUTINE                             
         DC    AL2(YNATTHDM-EDI)     FILE HEADER RECORD(S)                      
         DC    AL2(0)                INVOICE HEADER RECORDS                     
         DC    AL2(0)                INVOICE LINE ITEMS RECORDS                 
         DC    AL2(YNATTTOM-EDI)     INVOICE TOTAL RECORDS                      
         DC    AL1(EOT)                                                         
*                                                                               
YNATHDRQ EQU   1                                                                
         EJECT                                                                  
*********************************************************************           
* YNR - ATT MAP TABLE(S) - SEE MAPD                                 *           
*********************************************************************           
                                                                                
YNATTHDM DS    0X                                                               
         DC    AL1(ROUQ),AL2(YNATHDRQ)                                          
         DC    AL1(CONQ),AL2(HDRID-HDRD,6),C'000000'                            
         DC    AL1(CONQ),AL2(HDRZRO-HDRD,5),C'00000'                            
         DC    AL1(CONQ),AL2(HDRDDS-HDRD,3),C'DDS'                              
         DC    AL1(LWSQ),AL2(HDRPROF-HDRD,L'AGYPROF,AGYPROF-LWSD)               
         DC    AL1(EORQ),AL2(EDIHDRQ)                                           
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
YNATTTOM DS    0X                                                               
         DC    AL1(CONQ),AL2(BIGID-BIGD,6),C'BIG002'                            
         DC    AL1(LWSQ),AL2(BIGDATE-BIGD,L'BIGDATE,INVDCYMD-LWSD)              
         DC    AL1(LWSQ),AL2(BIGINVN-BIGD,L'INVNUMB,INVNUMB-LWSD)               
         DC    AL1(LWSQ),AL2(BIGPON-BIGD,L'PO#,PO#-LWSD)                        
         DC    AL1(LWSQ),AL2(BIGRN-BIGD,L'RLSN,RLSN-LWSD)                       
         DC    AL1(EORQ),AL2(EDIHLNQ+(BIGRN-BIGD)+L'RLSN)                       
*                                                                               
         DC    AL1(CONQ),AL2(N1ID-N1D,6),C'N1 008'                              
         DC    AL1(CONQ),AL2(N1EIC-N1D,L'N1EIC),C'RE '                          
         DC    AL1(LWSQ),AL2(N1NAME-N1D,L'AGYNAM33,AGYNAM33-LWSD)               
         DC    AL1(CONQ),AL2(N1ICQ-N1D,L'N1ICQ),C'92'                           
         DC    AL1(LWSQ),AL2(N1IC-N1D,L'PAYSITE,PAYSITE-LWSD)                   
         DC    AL1(EORQ),AL2(EDIHLNQ+(N1IC-N1ID)+L'N1IC)                        
*                                                                               
         DC    AL1(CONQ),AL2(IT1ID-IT1D,6),C'IT1035'                            
         DC    AL1(LWSQ),AL2(IT1AID-IT1D,L'POLN,POLN-LWSD)                      
         DC    AL1(LWSQ),AL2(IT1QNTY-IT1D,L'DUEWDEC,DUEWDEC-LWSD)               
         DC    AL1(CONQ),AL2(IT1BMC-IT1D,2),C'DO'                               
         DC    AL1(CONQ),AL2(IT1UP-IT1D,1),C'1'                                 
         DC    AL1(EORQ),AL2(EDIHLNQ+(IT1UP-IT1D)+1)                            
*                                                                               
         DC    AL1(CONQ),AL2(TDSID-TDSD,6),C'TDS081'                            
         DC    AL1(CONQ),AL2(TDSNDP-TDSID,1),C'2'                               
         DC    AL1(LWSQ),AL2(TDSAMT-TDSD,L'TDSAMT,CDUENETP-LWSD)                
         DC    AL1(EORQ),AL2(EDIHLNQ+(TDSAMT-TDSD)+L'TDSAMT)                    
         DC    AL1(EOTQ,EOTQ)                                                   
         EJECT                                                                  
         TITLE 'MEDIACOM - DELL'                                                
*********************************************************************           
* MEDIACOM M2 DELL                                                  *           
*********************************************************************           
M2DCROU  NTR1  BASE=*,LABEL=*                                                   
         CHI   R1,BEFRQ            BEFORE CARDS                                 
         BNE   M2DCAFTR                                                         
         J     XIT                                                              
*                                                                               
M2DCAFTR CHI   R1,AFTRQ            AFTER CARDS                                  
         BNE   M2DCR                                                            
         J     EIXIT                                                            
*                                                                               
M2DCR    SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     M2DCHDR                                                          
         B     M2DCINV                                                          
         B     M2DCGRS                                                          
         B     M2DCCOM                                                          
         B     M2DCREF                                                          
*                                                                               
                                                                                
M2DCHDR  MVC   AGYPROF,SPACES                                                   
         MVC   AGYPROF(09),=C'DELL-SPOT'                                        
         CLI   MEDIA,C'N'                                                       
         BNE   *+14                                                             
         MVC   AGYPROF(09),=C'DELL-NET '                                        
         B     M2HD10                                                           
         CLI   SYSTM,C'P'                                                       
         BNE   M2HD10                                                           
         MVC   AGYPROF(10),=C'DELL-PRINT'                                       
*                                                                               
M2HD10   MVC   SVINVNUM,INVNUMB                                                 
         MVC   INVNUMB,SPACES                                                   
         LA    RE,SVINVNUM                                                      
         LA    RF,INVNUMB                                                       
         LA    R1,L'INVNUMB                                                     
M2HDR10  CLI   0(RE),C'-'          IS IT A DASH                                 
         BE    *+14                SKIP IT                                      
         MVC   0(1,RF),0(RE)       MOVE FROM SVINVNUM TO INVNUMB                
         LA    RF,1(RF)            BUMP INVNUMB                                 
         LA    RE,1(RE)            BUMP SAVED INVNUMB                           
         BCT   R1,M2HDR10                                                       
*                                                                               
         J     XIT                                                              
*                                                                               
M2DCINV  DS    0H                                                               
M2DCINX  J     XIT                                                              
*                                                                               
*                                  GROSS HOOK                                   
M2DCGRS  MVI   NEQNTY1,C'0'        SET QUANTITY OF ONE                          
         MVC   NEQNTY1+1(L'NEQNTY1-1),NEQNTY1                                   
         MVI   NEQNTY1+(L'NEQNTY1-1),C'1'                                       
         J     XIT                                                              
*                                   COMMISSION HOOK                             
M2DCCOM  DS    0H                                                               
         J     XIT                                                              
*                                                                               
M2DCREF  DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
* MEDIACOM M2 DELL                                                              
M2DELL   DC    AL2(M2DCROU-EDI)      CONTROL ROUTINE                            
         DC    AL2(M2DCHDM-EDI)      FILE HEADER RECORD(S)                      
         DC    AL2(M2DCIVM-EDI)      INVOICE HEADER RECORDS                     
         DC    AL2(0)                INVOICE LINE ITEMS RECORDS                 
         DC    AL2(0)                INVOICE TOTAL RECORDS                      
         DC    AL1(EOT)                                                         
*                                                                               
M2DCHDRQ EQU   1                                                                
M2DCINVQ EQU   2                                                                
M2DCGRSQ EQU   3                                                                
M2DCCOMQ EQU   4                                                                
M2DCREFQ EQU   5                                                                
         EJECT                                                                  
*********************************************************************           
* MEDIACOM - DELL MAP TABLE(S) - SEE MAPD                           *           
*********************************************************************           
                                                                                
M2DCHDM  DS    0X                  MEDIACOM/DELL                                
         DC    AL1(ROUQ),AL2(M2DCHDRQ)                                          
         DC    AL1(CONQ),AL2(HDRID-HDRD,6),C'000000'                            
         DC    AL1(CONQ),AL2(HDRZRO-HDRD,5),C'00000'                            
         DC    AL1(CONQ),AL2(HDRDDS-HDRD,3),C'DDS'                              
         DC    AL1(LWSQ),AL2(HDRPROF-HDRD,L'AGYPROF,AGYPROF-LWSD)               
         DC    AL1(EORQ),AL2(EDIHDRQ)                                           
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
M2DCIVM  DS    0X                                                               
         DC    AL1(ROUQ),AL2(M2DCINVQ)                                          
         DC    AL1(CONQ),AL2(BIGID-BIGD,6),C'BIG002'                            
         DC    AL1(LWSQ),AL2(BIGDATE-BIGD,L'BIGDATE,INVDCYMD-LWSD)              
         DC    AL1(LWSQ),AL2(BIGINVN-BIGD,L'INVNUMB,INVNUMB-LWSD)               
         DC    AL1(LWSQ),AL2(BIGDATE1-BIGD,L'BIGDATE1,INVDCYMD-LWSD)            
******   DC    AL1(CONQ),AL2(BIGPON-BIGD,2),C'DO'                               
******   DC    AL1(LWSQ),AL2((BIGPON+2)-BIGD,L'DELLPO#,DELLPO#-LWSD)            
         DC    AL1(LWSQ),AL2((BIGPON)-BIGD,L'DELLPO#,DELLPO#-LWSD)              
         DC    AL1(EORQ),AL2(EDIHLNQ+(BIGPON-BIGD)+L'BIGPON)                    
*                                                                               
         DC    AL1(CONQ),AL2(CURID-CURD,6),C'CUR004'                            
         DC    AL1(CONQ),AL2(CUREIC-CURD,L'CUREIC),C'ZZ '                       
         DC    AL1(CONQ),AL2(CURCUR-CURD,L'CURCUR),C'USD'                       
         DC    AL1(EORQ),AL2(EDIHLNQ+(CURCUR-CURD)+L'CURCUR)                    
*                                                                               
         DC    AL1(ROUQ),AL2(M2DCGRSQ)                                          
         DC    AL1(CONQ),AL2(IT1ID-IT1D,6),C'IT1035'                            
         DC    AL1(CONQ),AL2(IT1QNTY-IT1ID,1),C'2'                              
         DC    AL1(LWSQ),AL2(IT1QNTY+1-IT1D,10,CDUENET+5-LWSD)                  
         DC    AL1(CONQ),AL2(IT1BMC-IT1D,2),C'EA'                               
         DC    AL1(CONQ),AL2(IT1NDP-IT1ID,1),C'0'                               
         DC    AL1(CONQ),AL2(IT1ZERO-IT1ID,1),C'0'                              
         DC    AL1(LWSQ),AL2(IT1UP-IT1ID,L'IT1UP,NEQNTY1-LWSD)                  
         DC    AL1(EORQ),AL2(EDIHLNQ+(IT1UP-IT1D)+L'IT1UP)                      
*                                                                               
         DC    AL1(CONQ),AL2(CADID-CADD,6),C'CAD055'                            
         DC    AL1(CONQ),AL2(CADSCAC-CADD,L'CADSCAC),C'NONE'                    
         DC    AL1(CONQ),AL2(CADRIQ-CADD,L'CADRIQ),C'LI '                       
         DC    AL1(CONQ),AL2(CADRID-CADD,1),C'1'                                
*        DC    AL1(LWSQ),AL2((CADRID+1)-CADD,L'DELLPO#,DELLPO#-LWSD)            
         DC    AL1(EORQ),AL2(EDIHLNQ+(CADRID-CADD)+L'CADRID)                    
*                                                                               
         DC    AL1(CONQ),AL2(TDSID-TDSD,6),C'TDS081'                            
         DC    AL1(CONQ),AL2(TDSNDP-TDSID,1),C'2'                               
         DC    AL1(LWSQ),AL2(TDSAMT-TDSD,L'TDSAMT,CDUENET-LWSD)                 
         DC    AL1(EORQ),AL2(EDIHLNQ+(TDSAMT-TDSD)+L'TDSAMT)                    
*                                                                               
         DC    AL1(CONQ),AL2(CTTID-CTTD,6),C'CTT089'                            
         DC    AL1(CONQ),AL2(CTTNDP-CTTD,1),C'0'                                
         DC    AL1(CONQ),AL2(CTTITM-CTTD,6),C'000001'                           
         DC    AL1(EORQ),AL2(EDIHLNQ+(CTTITM-CTTD)+L'CTTITM)                    
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
         EJECT                                                                  
         TITLE 'MEDIACOM - SHELL'                                               
*********************************************************************           
* MEDIACOM M2 SHELL                                                 *           
*********************************************************************           
M2SCROU  NTR1  BASE=*,LABEL=*                                                   
         CHI   R1,BEFRQ            BEFORE CARDS                                 
         BNE   M2SCAFTR                                                         
*                                  CLEAR PRD USER AND EST UCOMMS                
         MVC   M2SHLP1,SPACES      PRD USER 1 - COST CNTR/GL                    
         MVC   M2SHLE1,SPACES      SWBS #                                       
         J     XIT                                                              
*                                                                               
M2SCAFTR CHI   R1,AFTRQ            AFTER CARDS                                  
         BNE   M2SCR                                                            
         CLI   SYSTM,C'P'          SEE IF PRINT                                 
         BNE   M2SCA5                                                           
         CLC   PCODE1(3),=C'001'    MUST BE DIVISION 001                        
         BNE   M2SCASK              SKIP IF NOT                                 
         J     EIXIT                                                            
*                                                                               
*        SPOT OR NET                                                            
*                                                                               
M2SCA5   CLC   PCODE1(3),=C'S01'     MUST BE PRD GROUP S01                      
         BNE   M2SCASK              SKIP IF NOT                                 
         J     EIXIT                                                            
*                                                                               
M2SCASK  MVI   SKIPEIN,EISHLBQ      DIV OR PGROUP NOT LUBES                     
         MVI   SKIPINV,C'Y'                                                     
         J     EIXIT                                                            
*                                                                               
M2SCR    SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     M2SCHDR                                                          
         B     M2SCINV                                                          
         B     M2SCGRS                                                          
         B     M2SCCOM                                                          
         B     M2SCREF                                                          
*                                                                               
                                                                                
M2SCHDR  MVC   AGYPROF,SPACES                                                   
         MVC   AGYPROF(10),=C'SHELL-SPOT'                                       
         CLI   MEDIA,C'N'                                                       
         BNE   *+14                                                             
         MVC   AGYPROF(10),=C'SHELL-NET '                                       
         B     M2SHD04                                                          
         CLI   SYSTM,C'P'                                                       
         BNE   M2SHD04                                                          
         MVC   AGYPROF(11),=C'SHELL-PRINT'                                      
M2SHD04  L     RF,=A(MEDIACOM)                                                  
         MVC   AGYNAME,0(RF)       AGENCY IS MEDIACOM                           
*                                                                               
M2SHD05  LA    R1,SHMEDTAB        SET MEDIA NAME FROM TABLE                     
         MVC   WORK(1),SYSTM                                                    
         MVC   WORK+1(1),MEDIA+1                                                
*                                                                               
M2SHD06  CLC   0(2,R1),WORK                                                     
         BE    M2SHD07                                                          
         LA    R1,15(R1)                                                        
         CLI   0(R1),X'FF'       END OF TABLE                                   
         BNE   M2SHD06                                                          
         DC    H'0'              UNKNOWN SYSTEM/MEDIA                           
*                                                                               
M2SHD07  MVC   SHLMEDN,2(R1)                                                    
*                                                                               
*                                                                               
M2SHD10  MVC   SVINVNUM,INVNUMB                                                 
         MVC   INVNUMB,SPACES                                                   
         LA    RE,SVINVNUM                                                      
         LA    RF,INVNUMB                                                       
         LA    R1,L'INVNUMB                                                     
M2SHDR10 CLI   0(RE),C'-'          IS IT A DASH                                 
         BE    *+14                SKIP IT                                      
         MVC   0(1,RF),0(RE)       MOVE FROM SVINVNUM TO INVNUMB                
         LA    RF,1(RF)            BUMP INVNUMB                                 
         LA    RE,1(RE)            BUMP SAVED INVNUMB                           
         BCT   R1,M2SHDR10                                                      
*                                                                               
         J     XIT                                                              
*                                                                               
M2SCINV  DS    0H                                                               
*                                                                               
         OC    SHELLPO#,SPACES                                                  
*                                                                               
         MVI   QNTY1,C'0'          SET QUANTITY OF ONE                          
         MVC   QNTY1+1(L'QNTY1-1),QNTY1                                         
         MVI   QNTY1+(L'QNTY1-1),C'1'                                           
*                                                                               
         MVC   INVTYPE,=C'DI'        DEBIT/CREDIT                               
         CP    DUEAMT,PZERO                                                     
         BNL   *+10                                                             
         MVC   INVTYPE,=C'CR'   NOTE - ONLY ONE CHARACTER USED IN AMTD          
*                                                                               
         MVC   M2SHLDTA,SPACES      SET SHELL USER FIELDS TO SPACES             
*                                                                               
         CLC   M2SHLP1(10),SPACES    DO I HAVE PRD USER DATA?                   
         BNH   M2SCINV7             IF NOT, JUST GET SWBS                       
*                                   FROM EST USER                               
*                                                                               
         MVC   M2SHLCI,M2SHLP1      COMPANY IDENTIFER                           
         MVC   M2SHLCAR,M2SHLP1     COST ALLOCATION REFERENCE                   
*                                   FORMAT OF PRD USER 1 IS                     
*                                   CCCCNNNNNN/LLLLLLL                          
*                                   CCCC= COMPANY (4)                           
*                                   CCCCNNNNNN= COST CENTER (10)                
*                      SLASH BEFORE LLLLLLL= GL (ALWAYS 7?)                     
*                                                                               
         MVC   M2SHLGL,M2SHLP1+11   GL                                          
*                                                                               
M2SCINV7 MVC   M2SHLWBS,M2SHLE1     SWBS                                        
*                                                                               
M2SCINVM L     R4,AMITTAB          GET MOS FROM FIRST MITTAB ENTRY              
         USING MITD,R4                                                          
         GOTOR CNVD,DMCB,(9,MITMOS),(9,SHLMOS)      MMM/YY                      
*                                                                               
M2SCINVX J     XIT                                                              
         DROP  R4                                                               
*                                                                               
*                                  GROSS HOOK                                   
M2SCGRS  MVI   NEQNTY1,C'0'        SET QUANTITY OF ONE                          
         MVC   NEQNTY1+1(L'NEQNTY1-1),NEQNTY1                                   
         MVI   NEQNTY1+(L'NEQNTY1-1),C'1'                                       
*                                                                               
         J     XIT                                                              
*                                   COMMISSION HOOK                             
M2SCCOM  DS    0H                                                               
         J     XIT                                                              
*                                                                               
M2SCREF  DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
MEDIACOM DC    CL(L'AGYNAME)'MEDIACOM CORPORATION'                              
*                                                                               
SHMEDTAB DC    C'PB',CL13'MOBILE'                                               
         DC    C'PD',CL13'DIGITAL AUDIO'                                        
         DC    C'PI',CL13'INTERACTIVE'                                          
         DC    C'PL',CL13'SOCIAL'                                               
         DC    C'PM',CL13'MAGAZINES'                                            
         DC    C'PN',CL13'NEWSPAPERS'                                           
         DC    C'PO',CL13'OUTDOOR'                                              
         DC    C'PS',CL13'SUPPLEMENTS'                                          
         DC    C'PT',CL13'TRADE'                                                
         DC    C'PV',CL13'NAT. VIDEO'                                           
         DC    C'PW',CL13'LOC. VIDEO'                                           
*                                                                               
         DC    C'SN',CL13'NETWORK'                                              
         DC    C'SC',CL13'CABLE'                                                
         DC    C'SS',CL13'SYNDICATION'                                          
         DC    C'SO',CL13'NETW -OTHER'                                          
         DC    C'SD',CL13'NETW -RADIO'                                          
         DC    C'ST',CL13'TELEVISION'                                           
         DC    C'SR',CL13'RADIO'                                                
         DC    C'SX',CL13'RADIO NETWORK'                                        
         DC    X'FFFF'              END OF TABLE                                
         DROP  RB                                                               
*                                                                               
* MEDIACOM M2 SHELL                                                             
M2SHELL  DC    AL2(M2SCROU-EDI)      CONTROL ROUTINE                            
         DC    AL2(M2SCHDM-EDI)      FILE HEADER RECORD(S)                      
         DC    AL2(M2SCIVM-EDI)      INVOICE HEADER RECORDS                     
         DC    AL2(0)                INVOICE LINE ITEMS RECORDS                 
         DC    AL2(0)                INVOICE TOTAL RECORDS                      
         DC    AL1(EOT)                                                         
*                                                                               
M2SCHDRQ EQU   1                                                                
M2SCINVQ EQU   2                                                                
M2SCGRSQ EQU   3                                                                
M2SCCOMQ EQU   4                                                                
M2SCREFQ EQU   5                                                                
         EJECT                                                                  
*********************************************************************           
* MEDIACOM - SHELL MAP TABLE(S) - SEE MAPD                          *           
*********************************************************************           
                                                                                
M2SCHDM  DS    0X                  MEDIACOM/SHELL                               
         DC    AL1(ROUQ),AL2(M2SCHDRQ)                                          
         DC    AL1(CONQ),AL2(HDRID-HDRD,6),C'000000'                            
         DC    AL1(CONQ),AL2(HDRZRO-HDRD,5),C'00000'                            
         DC    AL1(CONQ),AL2(HDRDDS-HDRD,3),C'DDS'                              
         DC    AL1(LWSQ),AL2(HDRPROF-HDRD,L'AGYPROF,AGYPROF-LWSD)               
         DC    AL1(EORQ),AL2(EDIHDRQ)                                           
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
M2SCIVM  DS    0X                                                               
         DC    AL1(ROUQ),AL2(M2SCINVQ)                                          
         DC    AL1(CONQ),AL2(BIGID-BIGD,6),C'BIG002'                            
         DC    AL1(LWSQ),AL2(BIGDATE-BIGD,L'BIGDATE,INVDCYMD-LWSD)              
         DC    AL1(LWSQ),AL2(BIGINVN-BIGD,L'INVNUMB,INVNUMB-LWSD)               
         DC    AL1(LWSQ),AL2(BIGDATE1-BIGD,L'BIGDATE1,INVDCYMD-LWSD)            
******   DC    AL1(CONQ),AL2(BIGPON-BIGD,2),C'DO'                               
******   DC    AL1(LWSQ),AL2((BIGPON+2)-BIGD,L'SHELLPO#,SHELLPO#-LWSD)          
         DC    AL1(LWSQ),AL2((BIGPON)-BIGD,L'SHELLPO#,SHELLPO#-LWSD)            
         DC    AL1(LWSQ),AL2(BIGTYPE-BIGD,L'BIGTYPE,INVTYPE-LWSD)               
         DC    AL1(EORQ),AL2(EDIHLNQ+(BIGTYPE-BIGD)+L'BIGTYPE)                  
*                                                                               
         DC    AL1(CONQ),AL2(CURID-CURD,6),C'CUR004'                            
         DC    AL1(CONQ),AL2(CUREIC-CURD,L'CUREIC),C'ZZ '                       
         DC    AL1(CONQ),AL2(CURCUR-CURD,L'CURCUR),C'USD'                       
         DC    AL1(EORQ),AL2(EDIHLNQ+(CURCUR-CURD)+L'CURCUR)                    
*                                                                               
         DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF005'                            
         DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'8N '                              
         DC    AL1(LWSQ),AL2(REFRI-REFD,L'M2SHLCI,M2SHLCI-LWSD)                 
         DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+L'M2SHLCI)                    
*                                                                               
         DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF005'                            
         DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'74 '                              
         DC    AL1(LWSQ),AL2(REFRI-REFD,L'M2SHLWBS,M2SHLWBS-LWSD)               
         DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+L'M2SHLWBS)                   
*                                                                               
         DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF005'                            
         DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'CA '                              
         DC    AL1(LWSQ),AL2(REFRI-REFD,L'M2SHLCAR,M2SHLCAR-LWSD)               
         DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+L'M2SHLCAR)                   
*                                                                               
         DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF005'                            
         DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'GZ '                              
         DC    AL1(LWSQ),AL2(REFRI-REFD,L'M2SHLGL,M2SHLGL-LWSD)                 
         DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+L'M2SHLGL)                    
*                                                                               
         DC    AL1(CONQ),AL2(N1ID-N1D,6),C'N1 008'                              
         DC    AL1(CONQ),AL2(N1EIC-N1D,L'N1EIC),C'VN '                          
         DC    AL1(LWSQ),AL2(N1NAME-N1D,L'AGYNAME,AGYNAME-LWSD)                 
         DC    AL1(EORQ),AL2(EDIHLNQ+(N1NAME-N1ID)+L'AGYNAME)                   
******** DC    AL1(CONQ),AL2(N1ICQ-N1D,L'N1ICQ),C'ZZ'                           
******** DC    AL1(CONQ),AL2(N1IC-N1D,5),C'43365'                               
******** DC    AL1(EORQ),AL2(EDIHLNQ+(N1IC-N1ID)+L'N1IC)                        
*                                                                               
         DC    AL1(CONQ),AL2(N1ID-N1D,6),C'N1 008'     BILL TO                  
         DC    AL1(CONQ),AL2(N1EIC-N1D,L'N1EIC),C'BT '                          
         DC    AL1(CONQ),AL2(N1NAME-N1D,25),C'SHELL PRODUCTS US - LUBESX        
               '                                                                
         DC    AL1(EORQ),AL2(EDIHLNQ+(N1NAME-N1D)+25)                           
*                                                                               
         DC    AL1(CONQ),AL2(N3ID-N3D,6),C'N3 010'     BILL TO                  
         DC    AL1(CONQ),AL2(N3ADDR1-N3D,13),C'P.O. BOX 4484'                   
         DC    AL1(EORQ),AL2(EDIHLNQ+(N3ADDR1-N3D)+13)                          
*                                                                               
         DC    AL1(CONQ),AL2(N4ID-N4D,6),C'N4 011'                              
         DC    AL1(CONQ),AL2(N4CITY-N4D,7),C'HOUSTON'                           
         DC    AL1(CONQ),AL2(N4STATE-N4D,2),C'TX'                               
         DC    AL1(CONQ),AL2(N4ZIP-N4D,10),C'77210-4484'                        
         DC    AL1(CONQ),AL2(N4CNTY-N4D,2),C'US'                                
         DC    AL1(EORQ),AL2(EDIHLNQ+(N4CNTY-N4ID)+2)                           
*                                                                               
***                                                                             
***  TRANSLATOR SEEMS TO REQUIRE SOMETHING IN THE NEXT 2 SEGMENTS               
***  SO I'LL JUST USE ZZ AS THE CODE QUALIFIER AND 1 AS THE CODE                
***                                                                             
***      DC    AL1(CONQ),AL2(N1ICQ-N1D,2),C'ZZ'                                 
***      DC    AL1(CONQ),AL2(N1IC-N1D,1),C'1'                                   
***      DC    AL1(EORQ),AL2(EDIHLNQ+(N1IC-N1D)+L'N1IC)                         
*                                                                               
***      DC    AL1(CONQ),AL2(N3ID-N3D,6),C'N3 010'     BILL TO                  
***      DC    AL1(LWSQ),AL2(N3ADDR1-N3D,L'BADDR2,BADDR2-LWSD)                  
***      DC    AL1(LWSQ),AL2(N3ADDR2-N3D,L'BADDR3,BADDR3-LWSD)                  
***      DC    AL1(EORQ),AL2(EDIHLNQ+(N3ADDR2-N3D)+L'N3ADDR2)                   
***                                                                             
***      DC    AL1(CONQ),AL2(N3ID-N3D,6),C'N3 010'     BILL TO                  
***      DC    AL1(LWSQ),AL2(N3ADDR1-N3D,L'BADDR4,BADDR4-LWSD)                  
***      DC    AL1(LWSQ),AL2(N3ADDR2-N3D,L'BADDR5,BADDR5-LWSD)                  
***      DC    AL1(EORQ),AL2(EDIHLNQ+(N3ADDR2-N3D)+L'N3ADDR2)                   
*                                                                               
         DC    AL1(CONQ),AL2(DTMID-DTMD,6),C'DTM016'                            
         DC    AL1(CONQ),AL2(DTMDTQ-DTMD,3),C'011'                              
         DC    AL1(LWSQ),AL2(DTMDATE-DTMD,L'DTMDATE,INVDCYMD-LWSD)              
         DC    AL1(EORQ),AL2(EDIHLNQ+(DTMDATE-DTMD)+L'DTMDATE)                  
*                                                                               
         DC    AL1(ROUQ),AL2(M2SCGRSQ)                                          
         DC    AL1(CONQ),AL2(IT1ID-IT1D,6),C'IT1035'                            
         DC    AL1(CONQ),AL2(IT1AID-IT1D,1),C'1'                                
         DC    AL1(LWSQ),AL2(IT1QNTY-IT1D,L'IT1QNTY,QNTY1-LWSD)                 
*******  DC    AL1(CONQ),AL2(IT1QNTY-IT1ID,1),C'1'                              
*******  DC    AL1(LWSQ),AL2(IT1QNTY+1-IT1D,10,CDUENET+5-LWSD)                  
         DC    AL1(CONQ),AL2(IT1BMC-IT1D,2),C'EA'                               
         DC    AL1(CONQ),AL2(IT1NDP-IT1ID,1),C'2'                               
         DC    AL1(CONQ),AL2(IT1ZERO-IT1ID,1),C'0'                              
         DC    AL1(CONQ),AL2(IT1UP-IT1ID,1),C'0'                                
         DC    AL1(LWSQ),AL2(IT1UP+1-IT1ID,15,CDUENETP-LWSD)                    
         DC    AL1(CONQ),AL2(IT1PSIQ-IT1ID,2),C'CB'                             
         DC    AL1(LWSQ),AL2(IT1PSI-IT1ID,L'PRCODE1,PRCODE1-LWSD)               
         DC    AL1(LWSQ),AL2(IT1PSI+4-IT1ID,L'PRNAME1,PRNAME1-LWSD)             
         DC    AL1(EORQ),AL2(EDIHLNQ+(IT1PSI-IT1D)+L'PRCODE1+4+L'PRNAMEX        
               1)                                                               
*                                                                               
         DC    AL1(CONQ),AL2(PIDID-PIDD,6),C'PID063'                            
         DC    AL1(CONQ),AL2(PIDIDT-PIDD,L'PIDIDT),C'F'                         
         DC    AL1(LWSQ),AL2(PIDDESC-PIDD,L'SHLMEDN,SHLMEDN-LWSD)               
         DC    AL1(LWSQ),AL2(PIDDESC+L'SHLMEDN-PIDD+1,L'SHLMOS,SHLMOS-LX        
               WSD)                                                             
         DC    AL1(EORQ),AL2(EDIHLNQ+(PIDDESC-PIDD)+L'PIDDESC)                  
*                                                                               
         DC    AL1(CONQ),AL2(TDSID-TDSD,6),C'TDS081'                            
         DC    AL1(CONQ),AL2(TDSNDP-TDSID,1),C'2'                               
         DC    AL1(LWSQ),AL2(TDSAMT-TDSD,L'TDSAMT,CDUENETP-LWSD)                
         DC    AL1(EORQ),AL2(EDIHLNQ+(TDSAMT-TDSD)+L'TDSAMT)                    
*                                                                               
         DC    AL1(CONQ),AL2(AMTID-AMTD,6),C'AMT084'                            
         DC    AL1(CONQ),AL2(AMTQUA-AMTID,L'AMTQUA),C'1  '                      
         DC    AL1(CONQ),AL2(AMTNDP-AMTID,1),C'2'                               
         DC    AL1(LWSQ),AL2(AMTAMT-AMTD,L'AMTAMT,CDUENTLP-LWSD)                
         DC    AL1(EORQ),AL2(EDIHLNQ+(AMTAMT-AMTD)+L'AMTAMT)                    
*                                                                               
         DC    AL1(CONQ),AL2(CTTID-CTTD,6),C'CTT089'                            
         DC    AL1(CONQ),AL2(CTTNDP-CTTD,1),C'0'                                
         DC    AL1(CONQ),AL2(CTTITM-CTTD,6),C'000001'                           
         DC    AL1(EORQ),AL2(EDIHLNQ+(CTTITM-CTTD)+L'CTTITM)                    
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
         EJECT                                                                  
         TITLE 'CARAT - PFIZER'                                                 
*********************************************************************           
* CARAT UB  PFIZER                                                  *           
*********************************************************************           
UBDCROU  NTR1  BASE=*,LABEL=*                                                   
         CHI   R1,BEFRQ            BEFORE CARDS                                 
         BNE   UBDCAFTR                                                         
         J     XIT                                                              
*                                                                               
UBDCAFTR CHI   R1,AFTRQ            AFTER CARDS                                  
         BNE   UBDCR                                                            
         J     EIXIT                                                            
*                                                                               
UBDCR    SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     UBDCHDR                                                          
         B     UBDCINV                                                          
         B     UBDCGRS                                                          
         B     UBDCCOM                                                          
         B     UBDCREF                                                          
*                                                                               
                                                                                
UBDCHDR  MVC   AGYPROF,SPACES                                                   
         MVC   AGYPROF(10),=C'CARAT-SPOT'                                       
         CLI   MEDIA,C'N'                                                       
         BNE   *+14                                                             
         MVC   AGYPROF(10),=C'CARAT-NET '                                       
         B     UBHD10                                                           
         CLI   SYSTM,C'P'                                                       
         BNE   UBHD10                                                           
         MVC   AGYPROF(11),=C'CARAT-PRINT'                                      
*                                                                               
UBHD10   MVC   SVINVNUM,INVNUMB                                                 
         MVC   INVNUMB,SPACES                                                   
         LA    RE,SVINVNUM                                                      
         LA    RF,INVNUMB                                                       
         LA    R1,L'INVNUMB                                                     
UBHDR10  CLI   0(RE),C'-'          IS IT A DASH                                 
         BE    *+14                SKIP IT                                      
         MVC   0(1,RF),0(RE)       MOVE FROM SVINVNUM TO INVNUMB                
         LA    RF,1(RF)            BUMP INVNUMB                                 
         LA    RE,1(RE)            BUMP SAVED INVNUMB                           
         BCT   R1,UBHDR10                                                       
*                                                                               
         J     XIT                                                              
*                                                                               
UBDCINV  DS    0H                                                               
*                                                                               
         MVI   QNTY1,C'0'          SET QUANTITY OF ONE                          
         MVC   QNTY1+1(L'QNTY1-1),QNTY1                                         
         MVI   QNTY1+(L'QNTY1-1),C'1'                                           
*                                                                               
         MVC   INVTYPE,=C'DI'        DEBIT/CREDIT                               
         CP    DUEAMT,PZERO                                                     
         BNL   *+10                                                             
         MVC   INVTYPE,=C'CR'   NOTE - ONLY ONE CHARACTER USED IN AMTD          
*                                                                               
         MVC   PFZATTN,BADDR4    (IF THEY DON'T USE PRNT'S 2ND NAME)            
         LA    RE,BADDR4+6                                                      
         LA    RF,L'BADDR4-6                                                    
         CLC   BADDR4(6),=C'ATTN: '                                             
         BE    UBDCI5                                                           
         LA    RE,BADDR4+5                                                      
         LA    RF,L'BADDR4-5                                                    
         CLC   BADDR4(6),=C'ATTN '                                              
         BE    UBDCI5                                                           
         B     GETCITY                                                          
*                                                                               
UBDCI5   MVC   PFZATTN,SPACES                                                   
         EX    RF,UBDCIMV                                                       
         B     *+10                                                             
*                                                                               
UBDCIMV  MVC   PFZATTN(RF),0(RE)                                                
*                                                                               
*   OLD GETCITY EXTRACTS CITY FROM BADDR3                                       
*   COMMAS OR 2 BLANKS MUST SEPARATE CITY FORM STATE AND STATE FROM ZIP         
*                                                                               
*                                                                               
*        SHIP TO ADDRESS NOW HARD CODED HERE                                    
*                                                                               
GETCITY  DS    0H                                                               
         MVC   PFZCITY,SPACES                                                   
         MVC   PFZCITY(08),=C'NEW YORK'                                         
         MVC   PFZST,SPACES                                                     
         MVC   PFZST(2),=C'NY'                                                  
         MVC   PFZZIP,SPACES                                                    
         MVC   PFZZIP(05),=C'10017'                                             
         B     UBDCINX                                                          
*                                                                               
*        CODE BELOW COULD BE USED TO EXTRACT CITY STATE AND ZIP                 
*        FROM THE PRODUCT'S ADDRESS LINE                                        
*                                                                               
OLDCITY  DS    0H                                                               
         XC    PFZCITY,PFZCITY                                                  
         LA    R1,PFZCITY                                                       
         LA    R3,BADDR3                                                        
         LA    R4,L'BADDR3                                                      
GETC5    CLI   0(R3),C','     FIND FIRST COMMA                                  
         BE    GETCX                                                            
         CLC   0(2,R3),=C'  '  OR 2 SPACES                                      
         BE    GETCX                                                            
         MVC   0(1,R1),0(R3)                                                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,GETC5                                                         
*                                                                               
GETCX    OC    PFZCITY,SPACES                                                   
***      DC    H'0'           TEMP FOR TESTING                                  
                                                                                
         EJECT                                                                  
*                                                                               
*        GETST EXTRACTS STATE CODE FROM BADDR3                                  
*                                                                               
GETST    DS    0H                                                               
         XC    PFZST,PFZST                                                      
         LA    R1,PFZST                                                         
         LA    R3,BADDR3                                                        
         LA    R4,L'BADDR3                                                      
GETS5    CLC   0(2,R3),=C', ' FIND FIRST COMMA + SPACE                          
         BE    GETS12                                                           
         CLC   0(2,R3),=C'  ' OR 2 SPACES                                       
         BE    GETS12                                                           
         CLI   0(R3),C','     OR A COMMA                                        
         BE    GETS10                                                           
         LA    R3,1(R3)                                                         
         BCT   R4,GETS5                                                         
         B     GETSX           MEANS NO DELIMITER FOUND                         
*                                                                               
GETS10   LA    R3,1(R3)        BUMP PAST IT                                     
         B     GETS15                                                           
GETS12   LA    R3,2(R3)        BUMP PAST THEM                                   
         B     GETS15                                                           
GETS15   CLI   0(R3),C','     FIND NEXT COMMA                                   
         BE    GETS20                                                           
         CLC   0(2,R3),=C'  '  OR 2 BLANKS                                      
         BE    GETS20                                                           
         CLI   0(R3),C' '     OR SPACE                                          
         BE    GETS20                                                           
         LA    R4,2            STATE CODE MUST BE 2 CHARS                       
         MVC   0(1,R1),0(R3)                                                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,GETS15                                                        
         B     GETSX           CAN ONLY USE 2 CHARACTERS                        
*                                                                               
GETS20   DS    0H                                                               
*                                                                               
GETSX    OC    PFZST,SPACES                                                     
                                                                                
         EJECT                                                                  
*                                                                               
*        GETZIP EXTRACTS ZIP CODE FROM BADDR3                                   
*                                                                               
GETZIP   DS    0H                                                               
         XC    PFZZIP,PFZZIP                                                    
         LA    R1,PFZZIP                                                        
         LA    R3,BADDR3                                                        
         LA    R4,L'BADDR3                                                      
GETZ5    CLC   0(2,R3),=C', '  COMMA AND SPACE                                  
         BE    GETZ12                                                           
         CLC   0(2,R3),=C'  '  OR 2 SPACES                                      
         BE    GETZ12                                                           
         CLI   0(R3),C','      OR COMMA                                         
         BE    GETZ10                                                           
         LA    R3,1(R3)                                                         
         BCT   R4,GETZ5                                                         
         B     GETZX           MEANS NO DELIMITER FOUND                         
*                                                                               
GETZ10   LA    R3,1(R3)        BUMP PAST IT                                     
         SH    R4,=H'1'       DECREMENT R4                                      
         CH    R4,=H'0'       DON'T GO NEGATIVE                                 
         BH    GETZ15                                                           
         DC    H'0'           BAD DATA                                          
*                                                                               
GETZ12   LA    R3,2(R3)        BUMP PAST THEM                                   
         SH    R4,=H'2'       DECREMENT R4                                      
         CH    R4,=H'0'       DON'T GO NEGATIVE                                 
         BH    GETZ15                                                           
         DC    H'0'           BAD DATA                                          
*                                                                               
GETZ15   CLI   0(R3),C','     FIND NEXT COMMA                                   
         BE    GETZ20                                                           
         CLC   0(2,R3),=C'  '  OR 2 SPACES                                      
         BE    GETZ22                                                           
         CLI   0(R3),C' '     OR SPACE                                          
         BE    GETZ20                                                           
         LA    R3,1(R3)                                                         
         BCT   R4,GETZ15                                                        
         B     GETZX                                                            
*                                                                               
GETZ20   DS    0H                                                               
         LA    R3,1(R3)        BUMP PAST IT                                     
         SH    R4,=H'1'       DECREMENT R4                                      
         BH    GETZ25                                                           
         DC    H'0'           BAD DATA                                          
*                                                                               
GETZ22   DS    0H                                                               
         LA    R3,2(R3)        BUMP PAST THEM                                   
         SH    R4,=H'2'       DECREMENT R4                                      
         BH    *+6                                                              
         DC    H'0'           BAD DATA                                          
*                                                                               
GETZ25   MVC   0(1,R1),0(R3)                                                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,GETZ25                                                        
         B     GETZX                                                            
*                                                                               
*                                                                               
GETZX    OC    PFZZIP,SPACES                                                    
*                                                                               
UBDCINX  J     XIT                                                              
*                                                                               
*                                  GROSS HOOK                                   
UBDCGRS  MVI   NEQNTY1,C'0'        SET QUANTITY OF ONE                          
         MVC   NEQNTY1+1(L'NEQNTY1-1),NEQNTY1                                   
         MVI   NEQNTY1+(L'NEQNTY1-1),C'1'                                       
         J     XIT                                                              
*                                   COMMISSION HOOK                             
UBDCCOM  DS    0H                                                               
         J     XIT                                                              
*                                                                               
UBDCREF  DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
* CARAT UB  PFIZER                                                              
UBPFZ    DC    AL2(UBDCROU-EDI)      CONTROL ROUTINE                            
         DC    AL2(UBDCHDM-EDI)      FILE HEADER RECORD(S)                      
         DC    AL2(UBDCIVM-EDI)      INVOICE HEADER RECORDS                     
         DC    AL2(0)                INVOICE LINE ITEMS RECORDS                 
         DC    AL2(0)                INVOICE TOTAL RECORDS                      
         DC    AL1(EOT)                                                         
*                                                                               
UBDCHDRQ EQU   1                                                                
UBDCINVQ EQU   2                                                                
UBDCGRSQ EQU   3                                                                
UBDCCOMQ EQU   4                                                                
UBDCREFQ EQU   5                                                                
         EJECT                                                                  
*********************************************************************           
* CARAT - PFIZER MAP TABLE(S) - SEE MAPD                           *            
*********************************************************************           
                                                                                
UBDCHDM  DS    0X                  CARAT/PFIZER                                 
         DC    AL1(ROUQ),AL2(UBDCHDRQ)                                          
         DC    AL1(CONQ),AL2(HDRID-HDRD,6),C'000000'                            
         DC    AL1(CONQ),AL2(HDRZRO-HDRD,5),C'00000'                            
         DC    AL1(CONQ),AL2(HDRDDS-HDRD,3),C'DDS'                              
         DC    AL1(LWSQ),AL2(HDRPROF-HDRD,L'AGYPROF,AGYPROF-LWSD)               
         DC    AL1(EORQ),AL2(EDIHDRQ)                                           
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
UBDCIVM  DS    0X                                                               
         DC    AL1(ROUQ),AL2(UBDCINVQ)                                          
         DC    AL1(CONQ),AL2(BIGID-BIGD,6),C'BIG002'                            
         DC    AL1(LWSQ),AL2(BIGDATE-BIGD,L'BIGDATE,INVDCYMD-LWSD)              
         DC    AL1(LWSQ),AL2(BIGINVN-BIGD,L'INVNUMB,INVNUMB-LWSD)               
         DC    AL1(LWSQ),AL2(BIGDATE1-BIGD,L'BIGDATE1,INVDCYMD-LWSD)            
         DC    AL1(LWSQ),AL2((BIGPON)-BIGD,L'PFZPO#,PFZPO#-LWSD)                
         DC    AL1(CONQ),AL2(BIGTYPE-BIGD,L'BIGTYPE),C'DI'                      
         DC    AL1(CONQ),AL2(BIGPURP-BIGD,L'BIGPURP),C'00'                      
         DC    AL1(EORQ),AL2(EDIHLNQ+(BIGPURP-BIGD)+L'BIGPURP)                  
*                                                                               
         DC    AL1(CONQ),AL2(CURID-CURD,6),C'CUR004'                            
         DC    AL1(CONQ),AL2(CUREIC-CURD,L'CUREIC),C'SE '                       
         DC    AL1(CONQ),AL2(CURCUR-CURD,L'CURCUR),C'USD'                       
         DC    AL1(EORQ),AL2(EDIHLNQ+(CURCUR-CURD)+L'CURCUR)                    
*                                                                               
         DC    AL1(CONQ),AL2(N1ID-N1D,6),C'N1 008'                              
         DC    AL1(CONQ),AL2(N1EIC-N1D,L'N1EIC),C'RI '                          
         DC    AL1(LWSQ),AL2(N1NAME-N1D,L'PFZATTN,PFZATTN-LWSD)                 
         DC    AL1(EORQ),AL2(EDIHLNQ+(N1NAME-N1D)+L'N1NAME)                     
*                                                                               
* IF TRANSLATOR SEEMS TO REQUIRE SOMETHING IN THE NEXT 2 SEGMENTS               
*    SO I'LL JUST USE ZZ AD THE CODE QUALIFIER AND 1 AS THE CODE                
*                                                                               
***      DC    AL1(CONQ),AL2(N1CQ-N1D,2),C'ZZ'                                  
***      DC    AL1(CONQ),AL2(N1C-N1D,1),C'1'                                    
***      DC    AL1(EORQ),AL2(EDIHLNQ+(N1C-N1D)+L'NIC')                          
***                                                                             
         DC    AL1(CONQ),AL2(PERID-PERD,6),C'PER013'                            
         DC    AL1(CONQ),AL2(PERCFC-PERD,L'PERCFC),C'CN'                        
         DC    AL1(LWSQ),AL2(PERNAME-PERD,L'PFZATTN,PFZATTN-LWSD)               
         DC    AL1(EORQ),AL2(EDIHLNQ+(PERNAME-PERD)+L'PFZATTN)                  
*                                                                               
         DC    AL1(CONQ),AL2(N1ID-N1D,6),C'N1 008'     SHIP TO                  
         DC    AL1(CONQ),AL2(N1EIC-N1D,L'N1EIC),C'ST '                          
         DC    AL1(CONQ),AL2(N1NAME-N1D,11),C'PFIZER, INC'                      
         DC    AL1(EORQ),AL2(EDIHLNQ+(N1NAME-N1D)+11)                           
*                                                                               
*    TRANSLATOR SEEMS TO REQUIRE SOMETHING IN THE NEXT 2 SEGMENTS               
*    SO I'LL JUST USE ZZ AS THE CODE QUALIFIER AND 1 AS THE CODE                
*                                                                               
***      DC    AL1(CONQ),AL2(N1CQ-N1D,2),C'ZZ'                                  
***      DC    AL1(CONQ),AL2(N1C-N1D,1),C'1'                                    
***      DC    AL1(EORQ),AL2(EDIHLNQ+(N1C-N1D)+L'NIC')                          
*                                                                               
         DC    AL1(CONQ),AL2(N3ID-N3D,6),C'N3 010'     SHIP TO                  
***OLD** DC    AL1(LWSQ),AL2(N3ADDR1-N3D,L'BADDR2,BADDR2-LWSD)                  
         DC    AL1(CONQ),AL2(N3ADDR1-N3D,20),C'235 EAST 42ND STREET'            
         DC    AL1(EORQ),AL2(EDIHLNQ+(N3ADDR1-N3D)+20)                          
*                                                                               
         DC    AL1(CONQ),AL2(N4ID-N4D,6),C'N4 011'     SHIP TO                  
         DC    AL1(LWSQ),AL2(N4CITY-N4D,L'PFZCITY,PFZCITY-LWSD)                 
         DC    AL1(LWSQ),AL2(N4STATE-N4D,L'PFZST,PFZST-LWSD)                    
         DC    AL1(LWSQ),AL2(N4ZIP-N4D,L'PFZZIP,PFZZIP-LWSD)                    
         DC    AL1(EORQ),AL2(EDIHLNQ+(N4ZIP-N4D)+L'PFZZIP)                      
*                                                                               
         DC    AL1(CONQ),AL2(N1ID-N1D,6),C'N1 008'     SHIP FROM                
         DC    AL1(CONQ),AL2(N1EIC-N1D,L'N1EIC),C'SF '                          
         DC    AL1(CONQ),AL2(N1NAME-N1D,19),C'CARAT NORTH AMERICA'              
         DC    AL1(EORQ),AL2(EDIHLNQ+(N1NAME-N1D)+19)                           
*                                                                               
*    TRANSLATOR SEEMS TO REQUIRE SOMETHING IN THE NEXT 2 SEGMENTS               
*    SO I'LL JUST USE ZZ AD THE CODE QUALIFIER AND 1 AS THE CODE                
*                                                                               
***      DC    AL1(CONQ),AL2(N1CQ-N1D,2),C'ZZ'                                  
***      DC    AL1(CONQ),AL2(N1C-N1D,1),C'1'                                    
***      DC    AL1(EORQ),AL2(EDIHLNQ+(N1C-N1D)+L'NIC')                          
*                                                                               
         DC    AL1(ROUQ),AL2(UBDCGRSQ)                                          
         DC    AL1(CONQ),AL2(IT1ID-IT1D,6),C'IT1035'                            
         DC    AL1(LWSQ),AL2(IT1AID-IT1D,L'IT1AID,PFZPO#-LWSD)                  
         DC    AL1(LWSQ),AL2(IT1QNTY-IT1D,L'IT1QNTY,QNTY1-LWSD)                 
         DC    AL1(CONQ),AL2(IT1BMC-IT1D,2),C'EA'                               
         DC    AL1(CONQ),AL2(IT1NDP-IT1D,1),C'2'                                
         DC    AL1(CONQ),AL2(IT1ZERO-IT1D,1),C'0'                               
***SAV** DC    AL1(CONQ),AL2(IT1UP-IT1D,1),C'0'                                 
***SAV** DC    AL1(LWSQ),AL2(IT1UP+1-IT1D,15,CDUENET-LWSD)                      
         DC    AL1(CONQ),AL2(IT1UP-IT1D,10),C'0000000000'                       
         DC    AL1(CONQ),AL2(IT1UP+10-IT1D,6),C'000100'                         
         DC    AL1(EORQ),AL2(EDIHLNQ+(IT1UP-IT1D)+L'IT1UP)                      
*                                                                               
         DC    AL1(CONQ),AL2(CURID-CURD,6),C'CUR038'                            
         DC    AL1(CONQ),AL2(CUREIC-CURD,L'CUREIC),C'SE '                       
         DC    AL1(CONQ),AL2(CURCUR-CURD,L'CURCUR),C'USD'                       
         DC    AL1(EORQ),AL2(EDIHLNQ+(CURCUR-CURD)+L'CURCUR)                    
*                                                                               
         DC    AL1(CONQ),AL2(SACID-SACD,6),C'SAC058'                            
         DC    AL1(CONQ),AL2(SACACI-SACID,1),C'C'                               
         DC    AL1(CONQ),AL2(SACCODE-SACID,4),C'B840'                           
         DC    AL1(CONQ),AL2(SACAQ-SACID,2),C'AB'                               
         DC    AL1(CONQ),AL2(SACCHGC-SACID,7),C'DEFAULT'                        
         DC    AL1(CONQ),AL2(SACNDP-SACID,1),C'2'                               
         DC    AL1(LWSQ),AL2(SACAMT-SACID,L'SACAMT,CDUENET-LWSD)                
         DC    AL1(CONQ),AL2(SACPCTQ-SACID,1),C'Z'                              
         DC    AL1(CONQ),AL2(SACPCT-SACID,3),C'100'                             
         DC    AL1(LWSQ),AL2(SACREFID-SACID,5,PZDCODE-LWSD)                     
         DC    AL1(CONQ),AL2(SACOPTN-SACID,11),C'COST CENTER'                   
         DC    AL1(EORQ),AL2(EDIHLNQ+(SACOPTN-SACD)+L'SACOPTN)                  
*                                                                               
         DC    AL1(CONQ),AL2(SACID-SACD,6),C'SAC058'                            
         DC    AL1(CONQ),AL2(SACACI-SACID,1),C'C'                               
         DC    AL1(CONQ),AL2(SACCODE-SACID,4),C'B840'                           
         DC    AL1(CONQ),AL2(SACAQ-SACID,2),C'AB'                               
         DC    AL1(CONQ),AL2(SACCHGC-SACID,7),C'DEFAULT'                        
         DC    AL1(CONQ),AL2(SACNDP-SACID,1),C'2'                               
         DC    AL1(LWSQ),AL2(SACAMT-SACID,L'SACAMT,CDUENET-LWSD)                
         DC    AL1(CONQ),AL2(SACPCTQ-SACID,1),C'Z'                              
         DC    AL1(CONQ),AL2(SACPCT-SACID,3),C'100'                             
         DC    AL1(LWSQ),AL2(SACREFID-SACID,4,PZECODE-LWSD)                     
         DC    AL1(CONQ),AL2(SACOPTN-SACID,10),C'GL ACCOUNT'                    
         DC    AL1(EORQ),AL2(EDIHLNQ+(SACOPTN-SACD)+L'SACOPTN)                  
*                                                                               
         DC    AL1(CONQ),AL2(SACID-SACD,6),C'SAC058'                            
         DC    AL1(CONQ),AL2(SACACI-SACID,1),C'C'                               
         DC    AL1(CONQ),AL2(SACCODE-SACID,4),C'B840'                           
         DC    AL1(CONQ),AL2(SACAQ-SACID,2),C'AB'                               
         DC    AL1(CONQ),AL2(SACCHGC-SACID,7),C'DEFAULT'                        
         DC    AL1(CONQ),AL2(SACNDP-SACID,1),C'2'                               
         DC    AL1(LWSQ),AL2(SACAMT-SACID,L'SACAMT,CDUENET-LWSD)                
         DC    AL1(CONQ),AL2(SACPCTQ-SACID,1),C'Z'                              
         DC    AL1(CONQ),AL2(SACPCT-SACID,3),C'100'                             
         DC    AL1(CONQ),AL2(SACREFID-SACID,3),C'140'                           
         DC    AL1(CONQ),AL2(SACOPTN-SACID,07),C'COMPANY'                       
         DC    AL1(EORQ),AL2(EDIHLNQ+(SACOPTN-SACD)+L'SACOPTN)                  
*                                                                               
         DC    AL1(CONQ),AL2(SACID-SACD,6),C'SAC058'                            
         DC    AL1(CONQ),AL2(SACACI-SACID,1),C'C'                               
         DC    AL1(CONQ),AL2(SACCODE-SACID,4),C'B840'                           
         DC    AL1(CONQ),AL2(SACAQ-SACID,2),C'AB'                               
         DC    AL1(CONQ),AL2(SACCHGC-SACID,7),C'DEFAULT'                        
         DC    AL1(CONQ),AL2(SACNDP-SACID,1),C'2'                               
         DC    AL1(LWSQ),AL2(SACAMT-SACID,L'SACAMT,CDUENET-LWSD)                
         DC    AL1(CONQ),AL2(SACPCTQ-SACID,1),C'Z'                              
         DC    AL1(CONQ),AL2(SACPCT-SACID,3),C'100'                             
         DC    AL1(LWSQ),AL2(SACREFID-SACID,L'PZPJT#,PZPJT#-LWSD)               
         DC    AL1(CONQ),AL2(SACOPTN-SACID,14),C'PROJECT NUMBER'                
         DC    AL1(EORQ),AL2(EDIHLNQ+(SACOPTN-SACD)+L'SACOPTN)                  
*                                                                               
         DC    AL1(CONQ),AL2(TDSID-TDSD,6),C'TDS081'                            
         DC    AL1(CONQ),AL2(TDSNDP-TDSID,1),C'2'                               
         DC    AL1(LWSQ),AL2(TDSAMT-TDSD,L'TDSAMT,CDUENET-LWSD)                 
         DC    AL1(EORQ),AL2(EDIHLNQ+(TDSAMT-TDSD)+L'TDSAMT)                    
*                                                                               
         DC    AL1(CONQ),AL2(AMTID-AMTD,6),C'AMT084'                            
         DC    AL1(CONQ),AL2(AMTQUA-AMTID,L'AMTQUA),C'1  '                      
         DC    AL1(CONQ),AL2(AMTNDP-AMTID,1),C'2'                               
         DC    AL1(LWSQ),AL2(AMTAMT-AMTD,L'AMTAMT,CDUENETL-LWSD)                
         DC    AL1(LWSQ),AL2(AMTCDF-AMTD,1,INVTYPE-LWSD)                        
         DC    AL1(EORQ),AL2(EDIHLNQ+(AMTCDF-AMTD)+L'AMTCDF)                    
*                                                                               
         DC    AL1(CONQ),AL2(AMTID-AMTD,6),C'AMT084'                            
         DC    AL1(CONQ),AL2(AMTQUA-AMTID,L'AMTQUA),C'N  '                      
         DC    AL1(CONQ),AL2(AMTNDP-AMTID,1),C'2'                               
         DC    AL1(LWSQ),AL2(AMTAMT-AMTD,L'AMTAMT,CDUENETL-LWSD)                
         DC    AL1(LWSQ),AL2(AMTCDF-AMTD,1,INVTYPE-LWSD)                        
         DC    AL1(EORQ),AL2(EDIHLNQ+(AMTCDF-AMTD)+L'AMTCDF)                    
*                                                                               
         DC    AL1(CONQ),AL2(AMTID-AMTD,6),C'AMT084'                            
         DC    AL1(CONQ),AL2(AMTQUA-AMTID,L'AMTQUA),C'BAP'                      
         DC    AL1(CONQ),AL2(AMTNDP-AMTID,1),C'2'                               
         DC    AL1(LWSQ),AL2(AMTAMT-AMTD,L'AMTAMT,CDUENETL-LWSD)                
         DC    AL1(LWSQ),AL2(AMTCDF-AMTD,1,INVTYPE-LWSD)                        
         DC    AL1(EORQ),AL2(EDIHLNQ+(AMTCDF-AMTD)+L'AMTCDF)                    
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
         TITLE 'MCCANN FOR COKE'                                                
*********************************************************************           
* INMC - COKE                                                       *           
*********************************************************************           
MCCCROU  NTR1  BASE=*,LABEL=*                                                   
         CHI   R1,BEFRQ            BEFORE CARDS                                 
         BNE   MCCCAFTR                                                         
         MVC   MCGLA,SPACES                                                     
         MVC   MCTEXT,SPACES                                                    
         MVC   MCRGN,SPACES                                                     
         MVC   MCION,SPACES                                                     
         MVC   MCION6,SPACES                                                    
         MVC   MCION5,SPACES                                                    
         MVC   MCPREP,=C'00'                                                    
         J     XIT                                                              
*                                                                               
MCCCAFTR CHI   R1,AFTRQ            AFTER CARDS                                  
         BNE   MCCCR                                                            
*                                                                               
         CLC   ITMDCY,=C'2005'     SEE IF AFTER 2005                            
         BH    MCCCAF5             THEY WON'T HAVE A KBKS CODE                  
*                                                                               
         CLC   MCRGN,SPACES        ANY REGION ? (KBKS MATERIAL CODE)            
         BNH   MCCCAFT2            NO, SKIP INVOICE                             
MCCCAF5  CP    DUEAMT,PZERO        TEST ZERO INVOICE                            
         JNE   XIT                                                              
         CLC   MCPREP,=C'00'                                                    
         JNE   XIT                                                              
         B     *+8                                                              
MCCCAFT2 MVI   SKIPEIN,EIREGQ                                                   
         MVI   SKIPINV,C'Y'        SKIP INVOICE                                 
         J     EIXIT                                                            
*                                                                               
MCCCR    SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     MCCCHDR                                                          
         B     MCCCINV                                                          
         B     MCCCGRS         NET WITH COST CENTER AND KBKS #                  
         B     MCCCGRR         REF050 FOR THEM                                  
         B     MCCCGRR         REF050 FOR THEM                                  
         B     MCCCGRR         REF050 FOR THEM                                  
         B     MCCC06          NET MINUS COST CENTER + KBKS #                   
         B     MCCC06R         REF050 FOR THEM                                  
         B     MCCC06R         REF050 FOR THEM                                  
         B     MCCCCOM                                                          
         B     MCCCREF                                                          
*                                                                               
                                                                                
MCCCHDR  MVC   AGYPROF,SPACES                                                   
         CLC   AGYALPHA,=C'DU'                                                  
         BNE   MCCCHD10                                                         
         MVC   AGYPROF(13),=C'MEDIAVEST-NET'                                    
         J     XIT                                                              
MCCCHD10 CLC   AGYALPHA,=C'H9'                                                  
         BNE   MCCCHD20                                                         
         MVC   AGYPROF(15),=C'MEDIAVEST-PRINT'                                  
         J     XIT                                                              
MCCCHD20 MVC   AGYPROF(14),=C'MCCANN-NETWORK'                                   
         J     XIT                                                              
*                                                                               
MCCCINV  MVC   MCTYPE,=C'DI'      DEBIT/CREDIT                                  
         CP    DUEAMT,PZERO                                                     
*                                                                               
*        NOW CHECK SIGN TO TOTAL NET INSTEAD OF FROM AD REC                     
*        I WAS INSTRUCTED TO LEAVE THIS AS CHECKING DUEAMT                      
*        INSTEAD OF PROPERLY CHECKING THE SIGN OF THE TOTAL NET                 
*                                                                               
******   CP    MITOTNET,PZERO                                                   
         BNL   *+10                                                             
         MVC   MCTYPE,=C'CR'                                                    
*                                                                               
         MVC   MCPCODE,=C'0101'     PRODUCT CODE                                
*                                                                               
         CLI   SYSTM,C'P'                                                       
         BNE   MCCCIN03                                                         
*                                                                               
         ZAP   DUB,BASAMT          AS PER SKAY GET FROM AD RECD                 
         OI    DUB+L'DUB-1,X'0F'   GET POSITIVE GROSS                           
         UNPK  MCCGRS,DUB                                                       
*                                                                               
         ZAP   DUB,COMAMT1         GET POSITIVE COMMISSION                      
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MCCCOM,DUB          AS PER SKAY GET FROM AD RECD                 
*                                                                               
         ZAP   DUB,DUEAMT          AS PER SKAY GET FROM AD RECD                 
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MCCNET,DUB          NET                                          
*                                                                               
         ZAP   DUB,MITOTNET        GET POSITIVE NET - REAL                      
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MCCRNET,DUB                                                      
*                                                                               
*                                                                               
         L     R4,ALINTAB                                                       
         USING LIND,R4                                                          
         CLI   0(R4),EOT          TEST LINE DETAIL                              
         BE    MCCCIN02                                                         
         GOTOR CNVD,DMCB,(9,LINMOS),(20,ITMDCYMD)                               
         B     MCCCIN04                                                         
*                                                                               
MCCCIN02 L     R4,AMITTAB          GET MOS FOR BILL  IMDB#2264971               
         USING MITD,R4                                                          
         GOTOR CNVD,DMCB,(9,MITMOS),(20,ITMDCYMD)                               
         B     MCCCIN04                                                         
*                                                                               
MCCCIN03 ZAP   DUB,MITOTGRS        GET POSITIVE GROSS                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MCCGRS,DUB                                                       
*                                                                               
         ZAP   PL16,DUB            POSITIVE GROSS                               
         MP    PL16,=P'15'         * 15%                                        
         SRP   PL16,64-2,5                                                      
         OI    PL16+L'PL16-1,X'0F'                                              
         UNPK  MCCCOM,PL16         POSITIVE COMMISSION                          
*                                                                               
         SP    DUB,PL16            GET POSITIVE NET - CALCULATED                
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MCCNET,DUB                                                       
*                                                                               
         ZAP   DUB,MITOTNET        GET POSITIVE NET - REAL                      
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MCCRNET,DUB                                                      
*                                                                               
*                                                                               
         L     R4,AMITTAB          GET MOS FOR BILL                             
         USING MITD,R4                                                          
         GOTOR CNVD,DMCB,(9,MITMOS),(20,ITMDCYMD)                               
*                                                                               
MCCCIN04 MVI   MCPRIOR,C'N'        SET NOT FOR PRIOR                            
         CLC   ITMDCYMD(L'CCYY),INVDCYMD   ITEM VS. BILL YEAR                   
         BNL   *+8                                                              
         MVI   MCPRIOR,C'Y'        SET BILL IS FOR PRIOR YEAR                   
*                                                                               
         MVC   MCN1IC(10),=C'1040855   '                                        
         MVC   MCREFC(10),=C'0101003800'                                        
*                                                                               
         CLC   CLI,=C'024'         CHECK FOR TEMP BAD CODE                      
         BE    MCCCIN05            (ALSO MEDIAVEST FORMAT)                      
         CLC   CLI,=C'002'         IS IT MEDIAVEST  FORMAT                      
         BNE   MCCCINX                                                          
MCCCIN05 GOTO1 DATCON,DMCB,(5,0),(20,INVDCYMD) TODAY'S DATE                     
         MVC   MCN1IC(10),=C'0001001835'                                        
*        CLI   SYSTM,C'P'          IS IT A PRINT EB RUN IMDB#2223621            
*        BNE   MCCCIN06                                                         
*        CLC   PRCODE1,=C'LIT'                                                  
*        BE    MCCCIN20                                                         
*                                                                               
MCCCIN06 LA    R1,PRCODTB                                                       
MCCCIN08 CLI   0(R1),EOT          ARE WE AT THE END OF TABLE                    
         BE    MCCCIN20           NO MATCHING PR CODES FOUND EXIT               
         CLC   PRCODE1,0(R1)                                                    
         BNE   MCCCIN09                                                         
         MVC   MCREFC(10),=C'0101003900'                                        
         B     MCCCIN20                                                         
MCCCIN09 LA    R1,L'PRCODTB(R1)                                                 
         B     MCCCIN08                                                         
*                                                                               
MCCCIN20 MVC   MCION,MCION5           SET PROPER INTERNAL ORDER NUMBER          
         CLC   ITMDCY,=C'2005'        SEE IF MOS AFTER 2005                     
         BNH   MCCCINX                NO - EXIT                                 
         MVC   MCREFC,SPACES          SET COST CENTER AND KBKS                  
         MVC   MCRGN,SPACES           TO SPACES                                 
         MVC   MCION,MCION6           USE ESTIMATE USER2 (2006) I/O #           
*                                                                               
MCCCINX  J     XIT                                                              
*                                                                               
*                                  GROSS (NOW NET) HOOK                         
*                                  MUST HAVE YEAR 2005 OR EARLIER               
MCCCGRS  DS    0H                                                               
         CLC   ITMDCY,=C'2005'                                                  
         BNH   MCCCGR10                                                         
         MVI   SKIPREC,C'Y'     SKIP RECORD WITH COST CENTER AND KBKS #         
         J     XIT                                                              
*                                                                               
MCCCGR10 DS    0H                                                               
         MVI   QNTY1,C'0'          SET QUANTITY OF ONE                          
         MVC   QNTY1+1(L'QNTY1-1),QNTY1                                         
         MVI   QNTY1+(L'QNTY1-1),C'1'                                           
         CLI   MCPRIOR,C'Y'          TEST PRIOR YEAR                            
         BNE   *+14                                                             
         MVC   MCGLA,=C'0215201000'  USE SPECIAL ACCOUNT                        
         J     XIT                                                              
*                                                                               
*        GO SET GL FROM TABLE                                                   
*                                                                               
         GOTOR MCCGLA                                                           
         J     XIT                                                              
*                                                                               
*****    CLC   MCGLA,SPACES          ACCOUNT INPUT ?                            
*****    JNE   XIT                   YES, USE IT                                
*****    MVC   MCGLA,=C'0891400000'                                             
*****    J     XIT                                                              
*                                                                               
MCCCGRR  DS    0H                                                               
         CLC   ITMDCY,=C'2005'                                                  
         BNH   MCCCGR10                                                         
         MVI   SKIPREC,C'Y'     SKIP RECORD WITH COST CENTER AND KBKS #         
         J     XIT                                                              
*                                                                               
MCCCGRRX J     XIT                                                              
*                                                                               
*                                  NET MINUS COST CENTER + KBKS#  HOOK          
*                                  MUST HAVE YEAR AFTER 2005                    
MCCC06   DS    0H                                                               
         CLC   ITMDCY(4),=C'2005'                                               
         BH    MCCC06B                                                          
         MVI   SKIPREC,C'Y'                                                     
         J     XIT                                                              
*                                                                               
MCCC06B  DS    0H                                                               
         MVI   QNTY1,C'0'          SET QUANTITY OF ONE                          
         MVC   QNTY1+1(L'QNTY1-1),QNTY1                                         
         MVI   QNTY1+(L'QNTY1-1),C'1'                                           
         CLI   MCPRIOR,C'Y'          TEST PRIOR YEAR                            
         BNE   *+14                                                             
         MVC   MCGLA,=C'0215201000'  USE SPECIAL ACCOUNT                        
         J     XIT                                                              
*                                                                               
*        GO SET GL FROM TABLE                                                   
*                                                                               
         GOTOR MCCGLA                                                           
         J     XIT                                                              
*                                                                               
*****    CLC   MCGLA,SPACES          ACCOUNT INPUT ?                            
*****    JNE   XIT                   YES, USE IT                                
*****    MVC   MCGLA,=C'0891400000'                                             
*****    J     XIT                                                              
*                                                                               
MCCC06R  DS    0H                                                               
         CLC   ITMDCY(4),=C'2005'                                               
         BH    MCCC06RX                                                         
         MVI   SKIPREC,C'Y'                                                     
         J     XIT                                                              
*                                                                               
MCCC06RX J     XIT                                                              
*                                                                               
*                                   COMMISSION HOOK                             
MCCCCOM  MVI   QNTY1,C'0'          SET QUANTITY OF MINUS ONE                    
         MVC   QNTY1+1(L'QNTY1-1),QNTY1                                         
         MVI   QNTY1+(L'QNTY1-1),X'98'                                          
         CLI   MCPRIOR,C'Y'         TEST PRIOR YEAR                             
         BNE   *+14                                                             
         MVC   MCGLA,=C'0215201011' USE SPECIAL ACCOUNT                         
         J     XIT                                                              
         MVC   MCGLA,=C'0891203000' SET G/L ACCOUNT                             
         J     XIT                                                              
*                                                                               
MCCCREF  CLC   MCTEXT,SPACES         TEST CODE                                  
         JH    XIT                                                              
         MVI   SKIPREC,C'Y'          NONE, SKIP SEGMENT                         
         J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*********************************************************************           
* ROUTINE TO SET GL CODE FOR COKE                                   *           
*********************************************************************           
MCCGLA   NTR1  BASE=*,LABEL=*                                                   
         LA    R5,COKEGLT                                                       
         MVC   WORK(2),AGYALPHA                                                 
         MVC   WORK+2(2),MEDIA  (SYSTEM+MEDIA)                                  
         MVI   WORK+4,C' '                                                      
MCCGLA5  CLI   0(R5),X'FF'      END OF TABLE?                                   
         BNE   *+6                                                              
         DC    H'0'             FATAL ERROR                                     
         CLC   0(5,R5),WORK                                                     
         BE    MCCGLA7                                                          
         LA    R5,15(R5)                                                        
         B     MCCGLA5                                                          
*                                                                               
MCCGLA7  MVC   MCGLA,5(R5)                                                      
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
*   TABLE OF COKE GL CODES                                                      
*               AGY   SYS  MED  S-MED GL CODE                                   
*                                                                               
COKEGLT  DC    C'DU',C'N',C'N',C' ',C'0891400000' NETWORK                       
         DC    C'DU',C'N',C'C',C' ',C'0891400000' NETWORK - CABLE               
         DC    C'DU',C'N',C'S',C' ',C'0891400000' NETWORK - SYNDICATION         
         DC    C'DU',C'N',C'D',C' ',C'0891402000' NETWORK - RADIO NET           
         DC    C'DU',C'N',C'O',C' ',C'0891402000' NETWORK - OTHER-RADIO         
         DC    C'H9',C'P',C'M',C' ',C'0891404000' PRINT - MAGAZINES             
         DC    C'H9',C'P',C'N',C' ',C'0891404000' PRINT - NEWSPAPERS            
         DC    C'H9',C'P',C'T',C' ',C'0891404000' PRINT - TRADE                 
         DC    C'H9',C'P',C'S',C' ',C'0891404000' PRINT - SUPPLEMENTS           
         DC    C'H9',C'P',C'I',C' ',C'0891412000' PRINT - INTERACTIVE           
         DC    C'H9',C'P',C'O',C' ',C'0891410000' PRINT - OUTDOOR               
         DC    X'FFFF'                                                          
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
* MCCANN - ALL - COKE                                                           
MCCCH    DC    AL2(MCCCROU-EDI)      CONTROL ROUTINE                            
         DC    AL2(MCCCHDM-EDI)      FILE HEADER RECORD(S)                      
         DC    AL2(MCCCIVM-EDI)      INVOICE HEADER RECORDS                     
         DC    AL2(0)                INVOICE LINE ITEMS RECORDS                 
         DC    AL2(0)                INVOICE TOTAL RECORDS                      
         DC    AL1(EOT)                                                         
*                                                                               
MCCCHDRQ EQU   1                                                                
MCCCINVQ EQU   2                                                                
MCCCGRSQ EQU   3           NET WITH COST CENTER AND KBKS #                      
MCCCGR1Q EQU   4           REF050 FOR THEM                                      
MCCCGR2Q EQU   5           REF050 FOR THEM                                      
MCCCGR3Q EQU   6           REF050 FOR THEM                                      
MCCC06Q  EQU   7           FOR 2006 AND LATER BILLS                             
*                          NET WITHOUT COST CENTER AND KBKS #                   
MCCC061Q EQU   8           REF050 FOR THEM                                      
MCCC062Q EQU   9           REF050 FOR THEM                                      
MCCCCOMQ EQU   10          NO LONGER USED? (WAS 4)                              
MCCCREFQ EQU   11          MCTEXT                                               
                                                                                
* MEDIAVEST PR CODE TABLE                                                       
                                                                                
PRCODTB  DS    0CL3                                                             
         DC    CL3'BJB'                                                         
         DC    CL3'HIK'                                                         
*        DC    CL3'LIT'                                                         
*        DC    CL3'MML'                                                         
         DC    CL3'MOJ'                                                         
         DC    CL3'ODW'                                                         
         DC    CL3'SOJ'                                                         
         DC    CL3'TEY'                                                         
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*********************************************************************           
* INMC - COKE  MAP TABLE(S) - SEE MAPD                              *           
*********************************************************************           
                                                                                
MCCCHDM  DS    0X                  MCCANN/COKE                                  
         DC    AL1(ROUQ),AL2(MCCCHDRQ)                                          
         DC    AL1(CONQ),AL2(HDRID-HDRD,6),C'000000'                            
         DC    AL1(CONQ),AL2(HDRZRO-HDRD,5),C'00000'                            
         DC    AL1(CONQ),AL2(HDRDDS-HDRD,3),C'DDS'                              
         DC    AL1(LWSQ),AL2(HDRPROF-HDRD,L'AGYPROF,AGYPROF-LWSD)               
         DC    AL1(EORQ),AL2(EDIHDRQ)                                           
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
MCCCIVM  DS    0X                                                               
         DC    AL1(ROUQ),AL2(MCCCINVQ)                                          
         DC    AL1(CONQ),AL2(BIGID-BIGD,6),C'BIG002'                            
         DC    AL1(LWSQ),AL2(BIGDATE-BIGD,L'BIGDATE,INVDCYMD-LWSD)              
         DC    AL1(LWSQ),AL2(BIGINVN-BIGD,L'INVNUMB,INVNUMB-LWSD)               
         DC    AL1(LWSQ),AL2(BIGTYPE-BIGD,L'BIGTYPE,MCTYPE-LWSD)                
         DC    AL1(LWSQ),AL2(BIGPURP-BIGD,L'BIGPURP,MCPREP-LWSD)                
         DC    AL1(EORQ),AL2(EDIHLNQ+(BIGPURP-BIGD)+L'BIGPURP)                  
*                                                                               
         DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF005'                            
         DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'23 '                              
         DC    AL1(LWSQ),AL2(REFRI-REFD,L'MCPCODE,MCPCODE-LWSD)                 
         DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+4)                            
*                                                                               
         DC    AL1(CONQ),AL2(N1ID-N1D,6),C'N1 008'                              
         DC    AL1(CONQ),AL2(N1EIC-N1D,L'N1EIC),C'SF '                          
         DC    AL1(CONQ),AL2(N1ICQ-N1D,L'N1ICQ),C'ZZ'                           
         DC    AL1(LWSQ),AL2(N1IC-N1D,10,MCN1IC-LWSD)                           
         DC    AL1(EORQ),AL2(EDIHLNQ+(N1IC-N1ID)+10)                            
*                                                                               
         DC    AL1(ROUQ),AL2(MCCCGRSQ)                                          
         DC    AL1(CONQ),AL2(IT1ID-IT1D,6),C'IT1035'                            
         DC    AL1(CONQ),AL2(IT1AID-IT1D,1),C'1'                                
         DC    AL1(LWSQ),AL2(IT1QNTY-IT1D,L'IT1QNTY,QNTY1-LWSD)                 
         DC    AL1(CONQ),AL2(IT1BMC-IT1D,2),C'EA'                               
         DC    AL1(CONQ),AL2(IT1NDP-IT1ID,1),C'2'                               
         DC    AL1(CONQ),AL2(IT1ZERO-IT1ID,1),C'0'                              
*                                                                               
*   WAS GROSS - NOW REAL (NOT CALCULATED) NET - TICKET # 2492081                
*                                                                               
***      DC    AL1(LWSQ),AL2(IT1UP-IT1ID,L'MCCGRS,MCCGRS-LWSD)                  
         DC    AL1(LWSQ),AL2(IT1UP-IT1ID,L'MCCRNET,MCCRNET-LWSD)                
         DC    AL1(CONQ),AL2(IT1PSIQ-IT1D,2),C'BC'                              
         DC    AL1(LWSQ),AL2(IT1PSI-IT1D,L'MCRGN,MCRGN-LWSD)                    
         DC    AL1(EORQ),AL2(EDIHLNQ+(IT1PSI-IT1D)+L'MCRGN)                     
*                                                                               
         DC    AL1(ROUQ),AL2(MCCCGR1Q)                                          
         DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF050'                            
         DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'DP '                              
         DC    AL1(LWSQ),AL2(REFRI-REFD,10,MCREFC-LWSD)                         
         DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+10)                           
*                                                                               
         DC    AL1(ROUQ),AL2(MCCCGR2Q)                                          
         DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF050'                            
         DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'OQ '                              
         DC    AL1(LWSQ),AL2(REFRI-REFD,L'MCION,MCION-LWSD)                     
         DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+L'MCION)                      
*                                                                               
         DC    AL1(ROUQ),AL2(MCCCGR3Q)                                          
         DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF050'                            
         DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'12 '                              
         DC    AL1(LWSQ),AL2(REFRI-REFD,L'MCGLA,MCGLA-LWSD)                     
         DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+L'MCGLA)                      
*                                                                               
         DC    AL1(ROUQ),AL2(MCCCREFQ)                                          
         DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF050'                            
         DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'EX '                              
         DC    AL1(LWSQ),AL2(REFRI-REFD,L'MCTEXT,MCTEXT-LWSD)                   
         DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+L'MCTEXT)                     
*                                                                               
*        BELOW RECORD SAME AS ABOVE EXCEPT BC DATA NOT                          
*        IN IT1035 AND REF050 FOR DP EXCLUDED                                   
*        FOR 2006 (AND LATER DATA)                                              
*                                                                               
         DC    AL1(ROUQ),AL2(MCCC06Q)                                           
         DC    AL1(CONQ),AL2(IT1ID-IT1D,6),C'IT1035'                            
         DC    AL1(CONQ),AL2(IT1AID-IT1D,1),C'1'                                
         DC    AL1(LWSQ),AL2(IT1QNTY-IT1D,L'IT1QNTY,QNTY1-LWSD)                 
         DC    AL1(CONQ),AL2(IT1BMC-IT1D,2),C'EA'                               
         DC    AL1(CONQ),AL2(IT1NDP-IT1ID,1),C'2'                               
         DC    AL1(CONQ),AL2(IT1ZERO-IT1ID,1),C'0'                              
*                                                                               
*   WAS GROSS - NOW REAL (NOT CALCULATED) NET - TICKET # 2492081                
*                                                                               
***      DC    AL1(LWSQ),AL2(IT1UP-IT1ID,L'MCCGRS,MCCGRS-LWSD)                  
         DC    AL1(LWSQ),AL2(IT1UP-IT1ID,L'MCCRNET,MCCRNET-LWSD)                
         DC    AL1(EORQ),AL2(EDIHLNQ+(IT1UP-IT1D)+L'IT1UP)                      
*                                                                               
         DC    AL1(ROUQ),AL2(MCCC061Q)                                          
         DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF050'                            
         DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'OQ '                              
         DC    AL1(LWSQ),AL2(REFRI-REFD,L'MCION,MCION-LWSD)                     
         DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+L'MCION)                      
*                                                                               
         DC    AL1(ROUQ),AL2(MCCC062Q)                                          
         DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF050'                            
         DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'12 '                              
         DC    AL1(LWSQ),AL2(REFRI-REFD,L'MCGLA,MCGLA-LWSD)                     
         DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+L'MCGLA)                      
*                                                                               
         DC    AL1(ROUQ),AL2(MCCCREFQ)                                          
         DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF050'                            
         DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'EX '                              
         DC    AL1(LWSQ),AL2(REFRI-REFD,L'MCTEXT,MCTEXT-LWSD)                   
         DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+L'MCTEXT)                     
*        COMMISSION LOOP NO-OPED (TICKET # 2492081)                             
*                                                                               
***      DC    AL1(ROUQ),AL2(MCCCCOMQ)                                          
***      DC    AL1(CONQ),AL2(IT1ID-IT1D,6),C'IT1035'                            
***      DC    AL1(CONQ),AL2(IT1AID-IT1D,1),C'2'                                
***      DC    AL1(LWSQ),AL2(IT1QNTY-IT1D,L'IT1QNTY,QNTY1-LWSD)                 
***      DC    AL1(CONQ),AL2(IT1BMC-IT1D,2),C'EA'                               
***      DC    AL1(CONQ),AL2(IT1NDP-IT1ID,1),C'2'                               
***      DC    AL1(CONQ),AL2(IT1ZERO-IT1ID,1),C'0'                              
***      DC    AL1(LWSQ),AL2(IT1UP-IT1ID,L'MCCCOM,MCCCOM-LWSD)                  
***      DC    AL1(CONQ),AL2(IT1PSIQ-IT1D,2),C'BC'                              
***      DC    AL1(LWSQ),AL2(IT1PSI-IT1D,L'MCRGN,MCRGN-LWSD)                    
***      DC    AL1(EORQ),AL2(EDIHLNQ+(IT1PSI-IT1D)+L'MCRGN)                     
***                                                                             
***      DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF050'                            
***      DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'DP '                              
***      DC    AL1(LWSQ),AL2(REFRI-REFD,10,MCREFC-LWSD)                         
***      DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+10)                           
***                                                                             
***      DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF050'                            
***      DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'OQ '                              
***      DC    AL1(LWSQ),AL2(REFRI-REFD,L'MCION,MCION-LWSD)                     
***      DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+L'MCION)                      
***                                                                             
***      DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF050'                            
***      DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'12 '                              
***      DC    AL1(LWSQ),AL2(REFRI-REFD,L'MCGLA,MCGLA-LWSD)                     
***      DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+L'MCGLA)                      
***                                                                             
***      DC    AL1(ROUQ),AL2(MCCCREFQ)                                          
***      DC    AL1(CONQ),AL2(REFID-REFD,6),C'REF050'                            
***      DC    AL1(CONQ),AL2(REFRIQ-REFD,3),C'EX '                              
***      DC    AL1(LWSQ),AL2(REFRI-REFD,L'MCTEXT,MCTEXT-LWSD)                   
***      DC    AL1(EORQ),AL2(EDIHLNQ+(REFRI-REFD)+L'MCTEXT)                     
***                                                                             
         DC    AL1(CONQ),AL2(TDSID-TDSD,6),C'TDS081'                            
         DC    AL1(CONQ),AL2(TDSNDP-TDSID,1),C'2'                               
*                                                                               
*   NOW REAL (NOT CALCULATED) NET - TICKET # 2492081                            
*                                                                               
***      DC    AL1(LWSQ),AL2(TDSAMT-TDSD,L'TDSAMT,MCCNET-LWSD)                  
         DC    AL1(LWSQ),AL2(TDSAMT-TDSD,L'TDSAMT,MCCRNET-LWSD)                 
         DC    AL1(EORQ),AL2(EDIHLNQ+(TDSAMT-TDSD)+L'TDSAMT)                    
         DC    AL1(EOTQ,EOTQ)                                                   
         EJECT                                                                  
*                                                                               
         TITLE 'ZENITH FOR NESTLE'                                              
*********************************************************************           
* ZENITH- NESTLE                                                    *           
*********************************************************************           
THNEROU  NTR1  BASE=*,LABEL=*                                                   
         CHI   R1,BEFRQ            BEFORE CARDS                                 
         BNE   THNEAFTR                                                         
         MVC   THNUFDAT,SPACES     USER FIELD                                   
         J     XIT                                                              
*                                                                               
*                                                                               
THNEAFTR CHI   R1,AFTRQ            AFTER CARDS                                  
         BNE   THNER                                                            
                                                                                
         CP    DUEAMT,PZERO        TEST ZERO INVOICE                            
         BNE   *+12                                                             
         MVI   SKIPEIN,EIZEROQ                                                  
         B     THNESKIP                                                         
                                                                                
         CLC   THNUFDAT,SPACES     USER FIELD DATA?                             
         BNE   *+12                                                             
         MVI   SKIPEIN,EIMUFQ                                                   
         B     THNESKIP                                                         
                                                                                
         J     EIXIT                                                            
THNESKIP MVI   SKIPINV,C'Y'        SKIP INVOICE                                 
         J     EIXIT                                                            
*                                                                               
THNER    SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     THNEHDR                                                          
         B     THNEINV                                                          
*                                                                               
THNEHDR  MVC   AGYPROF,SPACES                                                   
         MVC   AGYPROF(15),=C'ZENITH-NES-SPOT'                                  
         CLI   MEDIA,C'N'                                                       
         BNE   *+14                                                             
         MVC   AGYPROF(15),=C'ZENITH-NES-NET '                                  
         B     THNEHD10                                                         
         CLI   SYSTM,C'P'                                                       
         BNE   THNEHD10                                                         
         MVC   AGYPROF(15),=C'ZENITH-NES-PRNT'                                  
*                                                                               
THNEHD10 MVC   SVINVNUM,INVNUMB                                                 
         MVC   INVNUMB,SPACES                                                   
         LA    RE,SVINVNUM                                                      
         LA    RF,INVNUMB                                                       
         LA    R1,L'INVNUMB                                                     
THHDR10  CLI   0(RE),C'-'          IS IT A DASH                                 
         BE    *+14                SKIP IT                                      
         MVC   0(1,RF),0(RE)       MOVE FROM SVINVNUM TO INVNUMB                
         LA    RF,1(RF)            BUMP INVNUMB                                 
         LA    RE,1(RE)            BUMP SAVED INVNUMB                           
         BCT   R1,THHDR10                                                       
*                                                                               
         J     XIT                                                              
*                                                                               
THNEINV  MVC   INVTYPE,=C'DI'        DEBIT/CREDIT                               
         CP    DUEAMT,PZERO                                                     
         BNL   *+10                                                             
         MVC   INVTYPE,=C'CR'                                                   
*                                                                               
         ZAP   DUB,DUEAMT          GET POSITIVE DUE AMOUNT                      
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  MCCNET,DUB                                                       
*                                                                               
         OC    MCNALL,MCNALL       ADD UP DUE AMOUNTS FOR GRAND TOTAL           
         BNZ   *+10                                                             
         ZAP   MCNALL,PZERO                                                     
         AP    MCNALL,DUEAMT                                                    
         EDIT  MCNALL,MCCGRS,0,ZERO=NOBLANK,FILL=0                              
*                                                                               
         MVC   MCNALTYP,=C'DI'        DEBIT/CREDIT                              
         CP    MCNALL,PZERO                                                     
         BNL   *+10                                                             
         MVC   MCNALTYP,=C'CR'                                                  
*                                                                               
         J     XIT                                                              
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
* ZENITH - ALL - NESTLE              IMDB# 2245741                              
THNES    DC    AL2(THNEROU-EDI)      CONTROL ROUTINE                            
         DC    AL2(THNEHDM-EDI)      FILE HEADER RECORD(S)                      
         DC    AL2(THNEIVM-EDI)      INVOICE HEADER RECORDS                     
         DC    AL2(0)                INVOICE LINE ITEMS RECORDS                 
         DC    AL2(0)                INVOICE TOTAL RECORDS                      
         DC    AL2(0)                ALL IVOICES PROCESSED                      
         DC    AL1(EOT)                                                         
*                                                                               
THNEHDRQ EQU   1                                                                
THNEINVQ EQU   2                                                                
         EJECT                                                                  
*********************************************************************           
* INMC - NESTLE MAP TABLE(S) - SEE MAPD CHANGED FROM 3050 TO 4010   *           
*********************************************************************           
                                                                                
THNEHDM  DS    0X                  MCCANN/NESTLE IS NOW ZENITH/NESTLE           
         DC    AL1(ROUQ),AL2(THNEHDRQ)                                          
         DC    AL1(CONQ),AL2(HDRID-HDRD,6),C'000000'                            
         DC    AL1(CONQ),AL2(HDRZRO-HDRD,5),C'00000'                            
         DC    AL1(CONQ),AL2(HDRDDS-HDRD,3),C'DDS'                              
         DC    AL1(LWSQ),AL2(HDRPROF-HDRD,L'AGYPROF,AGYPROF-LWSD)               
         DC    AL1(EORQ),AL2(EDIHDRQ)                                           
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
THNEIVM  DS    0X                                                               
         DC    AL1(ROUQ),AL2(THNEINVQ)                                          
         DC    AL1(CONQ),AL2(BIGID-BIGD,6),C'BIG002'                            
         DC    AL1(LWSQ),AL2(BIGDATE-BIGD,L'BIGDATE,INVDCYMD-LWSD)              
         DC    AL1(LWSQ),AL2(BIGINVN-BIGD,L'INVNUMB,INVNUMB-LWSD)               
         DC    AL1(LWSQ),AL2(BIGPON-BIGD,L'THNUFDAT,THNUFDAT-LWSD)              
         DC    AL1(LWSQ),AL2(BIGTYPE-BIGD,L'BIGTYPE,INVTYPE-LWSD)               
         DC    AL1(EORQ),AL2(EDIHLNQ+(BIGTYPE-BIGD)+L'BIGTYPE)                  
                                                                                
         DC    AL1(CONQ),AL2(N1ID-N1D,6),C'N1 008'                              
         DC    AL1(CONQ),AL2(N1EIC-N1D,2),C'RI'                                 
         DC    AL1(CONQ),AL2(N1NAME-N1D,10),C'0000184175'                       
         DC    AL1(CONQ),AL2(N1ICQ-N1D,1),C'1'                                  
         DC    AL1(CONQ),AL2(N1IC-N1D,9),C'879432557'                           
         DC    AL1(EORQ),AL2(EDIHLNQ+(N1IC-N1ID)+9)                             
                                                                                
         DC    AL1(CONQ),AL2(N1ID-N1D,6),C'N1 008'                              
         DC    AL1(CONQ),AL2(N1EIC-N1D,2),C'BT'                                 
         DC    AL1(CONQ),AL2(N1NAME-N1D,10),C'NESTLE USA'                       
         DC    AL1(CONQ),AL2(N1ICQ-N1D,1),C'1'                                  
         DC    AL1(CONQ),AL2(N1IC-N1D,9),C'008256224'                           
         DC    AL1(EORQ),AL2(EDIHLNQ+(N1IC-N1ID)+9)                             
                                                                                
         DC    AL1(CONQ),AL2(IT1ID-IT1D,6),C'IT1035'                            
         DC    AL1(CONQ),AL2(IT1QNTY-IT1D,11),C'00000000001'                    
         DC    AL1(CONQ),AL2(IT1BMC-IT1D,2),C'EA'                               
         DC    AL1(CONQ),AL2(IT1NDP-IT1D,1),C'2'                                
         DC    AL1(CONQ),AL2(IT1ZERO-IT1D,1),C'0'                               
         DC    AL1(LWSQ),AL2(IT1UP-IT1D,L'MCCNET,MCCNET-LWSD)                   
         DC    AL1(CONQ),AL2(IT1PSIQ-IT1D,2),C'IN'                              
         DC    AL1(CONQ),AL2(IT1PSI-IT1D,5),C'00010'                            
         DC    AL1(EORQ),AL2(EDIHLNQ+(IT1PSI-IT1D)+5)                           
                                                                                
         DC    AL1(CONQ),AL2(TDSID-TDSD,6),C'TDS081'                            
         DC    AL1(CONQ),AL2(TDSNDP-TDSD,1),C'2'                                
         DC    AL1(LWSQ),AL2(TDSAMT-TDSD,L'MCCNET,MCCNET-LWSD)                  
         DC    AL1(EORQ),AL2(EDIHLNQ+(TDSAMT-TDSD)+L'TDSAMT)                    
                                                                                
         DC    AL1(CONQ),AL2(CTTID-CTTD,6),C'CTT089'                            
         DC    AL1(CONQ),AL2(CTTNDP-CTTD,1),C'0'                                
         DC    AL1(CONQ),AL2(CTTITM-CTTD,6),C'000001'                           
         DC    AL1(EORQ),AL2(EDIHLNQ+(CTTITM-CTTD)+L'CTTITM)                    
         DC    AL1(EOTQ,EOTQ)                                                   
         EJECT                                                                  
*                                                                               
         TITLE 'TRANSMISSION SUMMARY REPORT'                                    
*********************************************************************           
* TRANSMISSION ERROR SUMMARY                                                    
*********************************************************************           
         USING PLIND,R2                                                         
EIOUT    NTR1  BASE=*,LABEL=*                                                   
         L     R2,APLINE                                                        
         LTR   R2,R2                                                            
         JZ    XIT                                                              
         CLI   SKIPINV,C'Y'           SKIPPED?                                  
         JNE   XIT                    . NO                                      
*                                                                               
         TM    FCS,FCSEIO                                                       
         BO    EIO5                                                             
         MVC   2(25,R2),=C'EDI INVOICE OMISSIONS (*)'                           
         MVC   134(25,R2),=C'--------------------------'                        
         GOTO1 AREPORT                                                          
         MVC   PLINUM(9),=C'INVOICE #'                                          
         MVC   PLIDAT(4),=C'DATE'                                               
         GOTO1 AREPORT                                                          
         MVC   PLINUM,=C'--------------------------'                            
         MVC   PLIDAT,=C'--------------------------'                            
         GOTO1 AREPORT                                                          
         OI    FCS,FCSEIO                                                       
         XC    SKIPCNT,SKIPCNT                                                  
         ZAP   SKIPAMT,PZERO                                                    
*                                                                               
EIO5     CLI   SKIPEIN,0              ERROR INFO NUMBER?                        
         BE    EIO8                   . NO                                      
         MVI   PLISTAR,C'*'                                                     
         AP    SKIPAMT,DUEAMT                                                   
         SR    R1,R1                                                            
         ICM   R1,3,SKIPCNT                                                     
         AHI   R1,1                                                             
         STCM  R1,3,SKIPCNT                                                     
*                                                                               
EIO8     MVC   PLINUM,INVNUMB         INVOICE NUMBER                            
         MVC   PLIDAT,INVDATE         INVOICE DATE                              
*                                                                               
         LA    R1,EITAB               ERROR INFO TABLE                          
EIO10    CLI   0(R1),EOT              END OF TABLE?                             
         BE    EIO20                  . YES                                     
         CLC   SKIPEIN,0(R1)          MATCH ON INFO NUMBER?                     
         BE    EIO20                  . YES                                     
         ZIC   RF,1(R1)               LENGTH OF TEXT                            
         LA    R1,2(RF,R1)            BUMP TO NEXT                              
         B     EIO10                                                            
*                                                                               
EIO20    ZIC   RF,1(R1)               LENGTH OF TEXT                            
         CHI   RF,L'PLIDES            DOES IT FIT?                              
         BNH   *+8                    . YES                                     
         LHI   RF,L'PLIDES            . NO, TAKE MAX                            
         SHI   RF,1                   DECREMENT                                 
         BM    EIOX                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PLIDES(0),2(R1)                                                  
         GOTO1 AREPORT                                                          
EIOX     J     XIT                                                              
         DROP  R2                                                               
*                                                                               
EIZEROQ  EQU   1            . ZERO INVOICE                                      
EIMUFQ   EQU   2            . MISSING USER FIELD DATA                           
EIREGQ   EQU   3            . MISSING REGION DATA                               
EISHLBQ  EQU   4            . SHELL - NOT IN DIVISION 001 (PRINT)               
*                                     OR PGRP S01 (SPOT/NET)                    
*                                     LUBES                                     
*                                                                               
EITAB    DS    0H                      ERROR INFO TABLE                         
**NOP    DC    AL1(0),AL1(16),C'INVOICE INCLUDED'                               
         DC    AL1(EIZEROQ),AL1(12),C'ZERO INVOICE'                             
         DC    AL1(EIMUFQ),AL1(23),C'MISSING USER FIELD DATA'                   
         DC    AL1(EIREGQ),AL1(19),C'MISSING REGION DATA'                       
         DC    AL1(EISHLBQ),AL1(26),C'NOT DIV 001 OR PRD GRP S01'               
         DC    AL1(EOT),AL1(13),C'UNKNOWN ERROR'                                
*                                                                               
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* TRANSMISSION ERROR SUMMARY TOTAL                                              
*********************************************************************           
         USING PLIND,R2                                                         
EIOTOT   NTR1  BASE=*,LABEL=*                                                   
         L     R2,APLINE                                                        
         LTR   R2,R2                                                            
         JZ    XIT                                                              
         OC    SKIPCNT,SKIPCNT                                                  
         JZ    XIT                                                              
*                                                                               
         GOTO1 AREPORT                                                          
         MVC   2(18,R2),=C'INVOICES OMITTED ='                                  
         LA    R2,20(R2)                                                        
         EDIT  SKIPCNT,(3,(R2)),ZERO=NOBLANK                                    
         LA    R2,4(R2)                                                         
         MVC   0(5,R2),=C'FOR $'                                                
         LA    R2,6(R2)                                                         
         EDIT  SKIPAMT,(16,(R2)),2,COMMAS=YES,FLOAT=-,ZERO=NOBLANK,ALIG+        
               N=LEFT                                                           
         GOTO1 AREPORT                                                          
         J     XIT                                                              
         DROP  R2,RB                                                            
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* STORAGE AND BUFFERS                                               *           
*********************************************************************           
                                                                                
MXLIN    EQU   6000                                                             
MXMIT    EQU   12                                                               
MXSRT    EQU   400                                                              
MXSLIN   EQU   400                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'*IDPTAB*'       ID RECORD TABLE FOR PRINT                    
IDPTAB   DS    (MXLIN)XL(IDPLNQ)                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*LINTAB*'       LINE ITEM TABLE                              
LINTAB   DS    (MXLIN)XL(LINLNQ)                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*MITTAB*'       MONTHLY INVOICE TOTALS                       
MITTAB   DS    (MXLIN)XL(MITLNQ)                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*SORTAB*'       INVOICE SORT TABLE                           
SORTAB   DS    (MXSRT)XL(MXSLIN)                                                
*                                                                               
INP      DS    CL500                                                            
MAP      DS    CL500                                                            
*                                                                               
         EJECT                                                                  
*********************************************************************           
* LOCAL WORKING STORAGE                                             *           
*********************************************************************           
                                                                                
LWSD     DSECT                                                                  
PARMS    DS    0F                                                               
AEDINME  DS    AL4                                                              
COMFAC   DS    AL4                                                              
DYNALLOC DS    AL4                                                              
ADMASTC  DS    AL4                                                              
APLINE   DS    AL4                                                              
AREPORT  DS    AL4                                                              
PARMSLNQ EQU   *-PARMS                                                          
*                                                                               
FCS      DS     X'00'              FILE CONTROL SWITCH                          
FCSTEMP  EQU    X'80'              TEMP (INPUT) FILE IS OPEN                    
FCSEDI   EQU    X'40'              EDI  (OUTPUT) FILE IS OPEN                   
FCSPROC  EQU    X'20'              IN PROCESS  ROUTINE                          
FCSSORT  EQU    X'10'              IN SORT ROUTINE                              
FCSEIO   EQU    X'08'              EI OUTPUT USED                               
FCSSORE  EQU    X'01'              SORT END - COMPLETED                         
*                                                                               
CONROUT  DS    A                   A(CONTROL ROUTINE)                           
BEFRQ    EQU   240                 BEFORE READING INPUT CARDS                   
AFTRQ    EQU   241                 AFTER READING INPUT CARDS                    
*                                                                               
AIDPTAB  DS    A                   A(ID RECORD DETAIL)                          
ALINTAB  DS    A                   A(LINE ITEM DETAIL)                          
AMITTAB  DS    A                   A(MONTHLY INVOICE TOTAL TABLE)               
ASORTAB  DS    A                   A(INVOICE SORT TABLE)                        
VSORTER  DS    V                   A(SORTER)                                    
*                                                                               
MAPNME   DS    0CL20               (I.E. SPTTAPE.SP0EMXX)                       
MAPTAPE  DS    CL7                 SPTTAPE                                      
         DS    CL1                 .                                            
MAPSYS   DS    CL3                 SP0                                          
MAPPGM   DS    CL2                 EB                                           
MAPAGY   DS    CL2                 XX                                           
         DS    CL5                 N/D                                          
*                                                                               
SYSTM    DS    CL1                 SYSTEM                                       
CLI      DS    CL3                 CLIENT                                       
MEDIA    DS    CL2                 MEDIA                                        
PCODE1   DS    CL5                 P1CODE                                       
PRCODE1  DS    CL(L'PRCODE)        PRCODE                                       
PRNAME1  DS    CL(L'PRNAME)        PRNAME                                       
ESCODE1  DS    CL(L'ESCODE)        ESCODE                                       
ESNAME1  DS    CL(L'ESNAME)        ESNAME                                       
ESNAME2  DS    CL(L'ESPNME2)       ESNAME                                       
*                                                                               
BTADR    DS    0CL180              BILL TO ADDRESS                              
BADDR1   DS    CL36                                                             
BADDR2   DS    CL36                                                             
BADDR3   DS    CL36                                                             
BADDR4   DS    CL36                                                             
BADDR5   DS    CL36                                                             
*                                                                               
AAGC     DS    A                   A(AGENCY CLIENT TABLE)                       
AAGYSTAB DS    A                   A(AGENCY SYSTEM TABLE)                       
*                                                                               
BKLQ     EQU   8                                                                
BKS      DS    0D                                                               
STORDGRS DS    PL(BKLQ)            STATAON ORDERED  - GRS                       
STORDNET DS    PL(BKLQ)                             - NET                       
STORDTAX DS    PL(BKLQ)                             - TAX                       
STPRVGRS DS    PL(BKLQ)            STATION PREVIOUS - GRS                       
STPRVNET DS    PL(BKLQ)                             - NET                       
STPRVTAX DS    PL(BKLQ)                             - TAX                       
*                                                                               
PBORDGRS DS    PL(BKLQ)            PUB      ORDERED - GRS                       
PBORDNET DS    PL(BKLQ)                             - NET                       
PBORDCSD DS    PL(BKLQ)                             - CD                        
PBORDTAX DS    PL(BKLQ)                             - TAX                       
PBORDINS DS    PL(BKLQ)                             - INSERTIONS                
PBPRVGRS DS    PL(BKLQ)            PUB     PREVIOUS - GRS                       
PBPRVNET DS    PL(BKLQ)                             - NET                       
PBPRVCSD DS    PL(BKLQ)                             - CD                        
PBPRVTAX DS    PL(BKLQ)                             - TAX                       
*                                                                               
INORDGRS DS    PL(BKLQ)            ID       ORDERED - GRS                       
INPRVGRS DS    PL(BKLQ)            ID PREV  ORDERED - GRS                       
*                                                                               
BASAMT   DS    PL(BKLQ)            BASE AMOUNT                                  
COMAMT   DS    PL(BKLQ)            COMMISSION                                   
COMAMT1  DS    PL(BKLQ)            COMMISSION FROM AS IN ADCOMAMT               
DUEAMT   DS    PL(BKLQ)            DUE                                          
TAXAMT   DS    PL(BKLQ)            TAX                                          
*                                                                               
TOTTAX   DS    PL(BKLQ)                                                         
TOTNET   DS    PL(BKLQ)                                                         
TOTCSD   DS    PL(BKLQ)                                                         
*                                                                               
CALCDUE  DS    PL(BKLQ)            CALCULATED AMOUNT DUE                        
BIGCOM   DS    PL(BKLQ)            BIGGEST COMMISSION                           
*                                                                               
MITOTGRS DS    PL(BKLQ)            MEDIA INVOICE TOTAL GROSS                    
MITOTNET DS    PL(BKLQ)            MEDIA INVOICE TOTAL NET                      
MITOTOGR DS    PL(BKLQ)            MEDIA INVOICE TOTAL GROSS - ORDERED          
MITOTONT DS    PL(BKLQ)            MEDIA INVOICE TOTAL NET - ORDERED            
*                                                                               
GSTTAX   DS    PL(BKLQ)            GST TAX AMOUNT PACKED                        
HSTTAX   DS    PL(BKLQ)            HST TAX AMOUNT PACKED                        
PSTTAX   DS    PL(BKLQ)            PST TAX AMOUNT PACKED                        
QSTTAX   DS    PL(BKLQ)            QST TAX AMOUNT PACKED                        
*                                                                               
GSTTAXT  DS    PL(BKLQ)            GST TAX AMOUNT PACKED                        
HSTTAXT  DS    PL(BKLQ)            HST TAX AMOUNT PACKED                        
PSTTAXT  DS    PL(BKLQ)            PST TAX AMOUNT PACKED                        
QSTTAXT  DS    PL(BKLQ)            QST TAX AMOUNT PACKED                        
*                                                                               
TOTDUE   DS    PL(BKLQ)            TOTAL DUE AMOUNT PACKED                      
NBKS     EQU   (*-BKS)/BKLQ                                                     
*                                                                               
DATCON   DS    A                                                                
ADDAY    DS    A                                                                
*                                                                               
SPACES   DS    CL80                                                             
ZEROS    DS    CL20                                                             
PZERO    DS    PL1                                                              
*                                                                               
SAVRE    DS    F                                                                
DUB      DS    D                                                                
DUB2     DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    CL64                                                             
BYTE     DS    XL1                                                              
HALF     DS    H                                                                
PL16     DS    PL16                                                             
PL8      DS    PL8                                                              
SVRE     DS    F                                                                
RTNROU   DS    C                   RETURN TO ROUTINE 'Y' OR 'N'                 
PRTNROU  DS    C                   RETURN TO ROUTINE 'Y' OR 'N'                 
RTNADR   DS    F                   A(START OF MAP DATA)                         
PREVADR  DS    F                   A(START OF MAP DATA)                         
*                                                                               
SKIPREC  DS    CL1                                                              
SKIPINV  DS    CL1                                                              
SKIPEIN  DS    XL1                                                              
SKIPCNT  DS    XL2                                                              
SKIPAMT  DS    PL8                                                              
PIDCNT   DS    PL8                 TEMPORARY COUNTER                            
INSCNT   DS    PL8                 TEMPORARY INSERTIONS COUNTER                 
*                                                                               
ANYTAX   DS    CL1                                                              
PASSCON  DS    CL1                                                              
*                                                                               
AIDPITM  DS    A                   A(CURRENT ID RECORD DETAIL)                  
IDPCNT   DS    F                   NUMBER IN LINE ITEM TABLE                    
ALINITM  DS    A                   A(CURRENT LINE ITEM DETAIL)                  
LINCNT   DS    F                   NUMBER IN LINE ITEM TABLE                    
AMITITM  DS    A                   A(CURRENT MONTHLY INVOICE TOTAL)             
MITCNT   DS    F                   NUMBER IN MONTHY INVOICE TABLE               
*                                                                               
SVLINSTA DS    CL(L'LINSTA)        LAST STATION AND MOS PROCESSED               
SVLINMOS DS    CL(L'LINMOS)                                                     
*                                                                               
AGYALPHA DS    CL2                 AGENCY APHHA                                 
AGYNAME  DS    CL40                AGENCY NAME                                  
AGYNAM33 DS    CL33                AGENCY NAME - FROM BILLING                   
PAYSITE  DS    CL14                PAY SITE                                     
POLN     DS    CL10                PURCHASE ORDER LINE NUMBER                   
DUEWDEC  DS    CL11                AMOUNT DUE WITH A DECIMAL                    
RLSN     DS    CL2                 RELEASE NUMBER                               
*                                                                               
AGYID    DS    CL5                                                              
AGYPROF  DS    CL15                                                             
ADVCODE  DS    CL(L'AVCODE)                                                     
ADVNAME  DS    CL(L'AVSNAME)                                                    
STATION  DS    CL(L'STCODE)                                                     
PUB      DS    CL(L'PBNAME)                                                     
MRKTNUM  DS    CL(L'MKCODE)                                                     
MRKTNAM  DS    CL(L'MKNAMES)                                                    
DISTRICT DS    CL(L'M2CODE)                                                     
VNDNAME  DS    CL(L'IT1AID)                                                     
VNDNAMC  DS    CL(L'IT1AID)                                                     
IDATSTA  DS    CL(L'PIDDESC)      INSERTION DATE OR STATION                     
*                                                                               
OVNDNAME DS    CL(L'IT1PSI)       LONGER FOR OMDUSA - ELI-LILLY                 
OVNDNAMC DS    CL(L'IT1PSI)                                                     
*                                                                               
INVNUMB  DS    CL(L'IHINVN)                                                     
INVDATE  DS    CL(L'IHINVD)                                                     
INVTYPE  DS    CL(L'BIGTYPE)                                                    
INVDESC  DS    CL(L'NTEDESC)                                                    
REGION   DS    CL5                                                              
USRMRKT  DS    CL17                XX1234-1234567890                            
*                                                                               
H9NETE1  DS    CL23                NETWORK ESTIMATE USER1 (MAR #-)              
*                                  USED IN H9DATA W2 FIELD                      
*                                                                               
*                                  FORMAT IS:                                   
*                                  FISCAL YEAR (2)                              
*                                  -                                            
*                                  NETWORK MARKET (4)                           
*                                  -                                            
*                                  FUNDING SOURCE (3)                           
*                                  -                                            
*                                  10 DIGIT CODE                                
*                                                                               
PO#      DS    CL6                 MAKE BIGGER?                                 
DELLPO#  DS    CL22                MAX OF BIGPON (MEDIACOM'S 32 MAX)            
*                                          BUT I CAN ONLY USE 22)               
SHELLPO# DS    CL22                MAX OF BIGPON (MEDIACOM'S 32 MAX)            
*                                                                               
SHLMOS   DS    CL8                 SHELL MOS                                    
*                                                                               
M2SHLP1  DS    CL32                M2 SHELL PRODUCT USER 1 FIELD                
M2SHLE1  DS    CL32                M2 SHELL ESTIMATE USER 1 FIELD               
EUDEF2   DS    CL32                ESTIMATE UDEF 2 (FOR EVERYONE)               
***                                                                             
***      DON'T KNOW WHERE THEY WILL BE SUPPLYING THIS                           
***      ESTIMATE USER?                                                         
***                                                                             
PFZPO#   DS    CL22                MAX OF BIGPON (CARAT'S 16 MAX)               
*                                                                               
PZDCODE  DS    CL5                 DEPT CODE - PFIZER                           
PZECODE  DS    CL4                 EXP CODE - PFIZER                            
PZPJT#   DS    CL32                PROJECT NUMBER                               
*                                                                               
ELMOS    DS    CL8                 ELI LILLY MOS                                
ELPO#    DS    CL22                ELI LILLY'S PO#                              
*                                  EST USER1 P O # (PRINT AND NET)              
*                                  EST USER1 PO # (SPOT)                        
*                                  22 EVEN THOUGH THEY HAVE A MAX OF 32         
*                                  BIG3PON IS ONLY 22 CHARACTERS LONG           
*                                                                               
ELUOM    DS    CL2                 ELI LILLY UNIT OF MEASURE                    
*                                  SET T0 PC IS ELPO# STARTS WITH 45            
*                                  SET TO EA IF ELPO3 STARTS WITH 49            
ELMEDN   DS    CL13                SET FROM ELMEDTAB                            
*                                                                               
SHLMEDN  DS    CL13                SHELL MEDIA NAME                             
*                                                                               
YRMRKT   DS    CL(L'ITMMCY+L'MRKTNAM+1)                                         
*                                                                               
BASIS    DS    CL1                                                              
*                                                                               
ABIGLINE DS    A                   A(LINE WITH BIGGEST COMMISSION)              
*                                                                               
ITMCNT   DS    PL4                 ITEM COUNT                                   
CITMCNT  DS    CL6                                                              
*                                                                               
INVDCYMD DS    CL8                 INVOICE DATE CCYYMMDD                        
         ORG   INVDCYMD                                                         
CCYY     DS    CL4                 CCYY                                         
MMDD     DS    CL4                 MMDD                                         
*                                                                               
FISCAL   DS    CL4                 FISCAL -  CCYY                               
FISCALA  DS    CL1                 APPEND AN 'A' FOR SECOND HALF                
*                                                                               
ITMDCYMD DS    0CL8                FIRST LINE ITEM DATE CCYYMMDD                
ITMDCY   DS    CL4                 FIRST LINE ITEM DATE CCYY                    
ITMDMON  DS    CL2                 FIRST LINE ITEM DATE MM                      
ITMDDAY  DS    CL2                 FIRST LINE ITEM DATE DD                      
ITMMCY   DS    CL6                 MMCCYY - FROM ABOVE                          
QNTY1    DS    CL(L'IT1QNTY)       QUANTITY OF ONE                              
NEQNTY1  DS    CL(L'IT1UP)         QUANTITY OF ONE                              
*                                                                               
CNETAMT  DS    CL(L'CTPUP)         CHARACTER NET AMOUNT                         
CCOMAMT  DS    CL(L'CTPUP)         CHARACTER COMMISSION AMOUNT                  
CDUENET  DS    CL(L'TDSAMT)        CHARACTER DUE NET                            
CDUENETP DS    CL(L'TDSAMT)        CHARACTER DUE NET - POSITIVE                 
CDUENETL DS    CL(L'AMTAMT)        CHARACTER DUE NET - LONG (18)                
CDUENTLP DS    CL(L'AMTAMT)        CHARACTER DUE NET - LONG POSITIVE            
CDUECOM  DS    CL(L'SACAMT)        CHARACTER DUE COMMISSION                     
CTAXAMT  DS    CL(L'TXIAMT)        CHARACTER TAX AMOUNT                         
PTAXAMT  DS    CL(L'TXIAMT)        POSITIVE  TAX AMOUNT                         
GSTTAXC  DS    CL(L'TXIAMT)        CST TAX AMOUNT CHARACTER                     
QSTTAXC  DS    CL(L'TXIAMT)        QST TAX AMOUNT CHARACTER                     
HSTTAXC  DS    CL(L'TXIAMT)        HST TAX AMOUNT CHARACTER                     
PSTTAXC  DS    CL(L'TXIAMT)        PST TAX AMOUNT CHARACTER                     
TOTTAXC  DS    CL(L'TXIAMT)        TOTAL TAX AMOUNT CHARACTER                   
QSTTAXTC DS    CL(L'TXIAMT)        QSTTAX TOTAL AMOUNT CHARACTER                
TOTDUEC  DS    CL(L'TDSAMT)        TOTAL DUE AMOUNT CHARACTER                   
*                                                                               
CLINDUE  DS    CL(L'IT1UP)         CHARACTER LINE ITEM UNIT PRICE               
CLINGRS  DS    CL(L'IT1UP)         CHARACTER LINE ITEM GROSS PRICE              
SVINVNUM DS    CL(L'IHINVN)        SAVED INVOICE NUMBER                         
PINVNUMB DS    CL(L'PIINVN)        PREVIOUS INVOICE NUMBER                      
         EJECT                                                                  
AGYDATA  DS    0X                  SPECIAL AGENCY DATA                          
*                                                                               
OMDATA   DS    0X                  SPECIAL OMD DATA  MCDONALD                   
OMACCT   DS    CL10                OM UD E2 ACCOUNT #                           
OMNUFDAT DS    CL(L'UDTXT)         ATTENTION NAME/USER FIELD DATA               
OMUCATTN DS    CL(L'UCTXT)         ATTENTION NAME/USER COMM DATA                
OMATTN   DS    CL(L'UCTXT)         ATTENTION (FROM EITHER OF ABOVE)             
OMN1NAME DS    CL(L'M2NAMES)                                                    
OMGRS    DS    CL(L'IT1UP)         MCCANN GROSS(CHARACTER)                      
*                                                                               
         ORG   AGYDATA                                                          
PFZDATA  DS    0X                  CARAT - PFIZER                               
PFZATTN  DS    CL(L'BADDR4)        ATTENTION FOR BT ADDRESS                     
*                                  BELOW GOTTEN FOR BT ADDRESS LINE3            
PFZCITY  DS    CL20                CITY                                         
PFZST    DS    CL2                 STATE                                        
PFZZIP   DS    CL10                ZIP                                          
*                                                                               
         ORG   AGYDATA                                                          
H9DATA   DS    0X                  STARCOM - SPECIAL DATA                       
W2       DS    0CL(W2LNQ)                                                       
W2YY     DS    CL2                 YEAR                                         
W2A      DS    CL1                 "A" (OPTIONAL)                               
W2DSH1   DS    CL1                 -                                            
W2MAR4   DS    CL4                 FOUR BYTES FROM USER MARKET CODE             
W2DSH2   DS    CL1                 -                                            
W2FUND   DS    CL3                 FUND                                         
W2DSH3   DS    CL1                 -                                            
W2MAR10  DS    CL10                LAST 10 OF USER MARKET CODE                  
W2LNQ    EQU   *-W2YY                                                           
W2SPA    DS    CL1                 BLANK                                        
W2MKT    DS    CL4                 BMS (SPOT MARKET NUMBER)                     
         ORG   W2MKT                                                            
W2DIST   DS    CL4                 DISTRICT FOR PRINT)                          
         ORG   W2MKT                                                            
W2S071   DS    CL4                 S071 (FOR PRINT)                             
***W2LNQ    EQU   *-W2YY                                                        
*                                                                               
OT       DS    0CL(OTLNQ)                                                       
OTZRO1   DS    CL5                 '00000'                                      
OTGLAC   DS    CL5                 G/L ACCOUNT                                  
OTZRO2   DS    CL3                 '000'                                        
OTFUND   DS    CL3                 FUND                                         
OTMAR4   DS    CL4                 FOUR BYTES FROM USER MARKET CODE             
OTLNQ    EQU   *-OTZRO1                                                         
*                                                                               
OTC      DS    CL(L'OT)            FOR THE COMMISSION POSTING                   
         EJECT                                                                  
*                                                                               
         ORG   AGYDATA                                                          
MCDATA   DS    0X                  SPECIAL MCCANN DATA                          
MCCGRS   DS    CL(L'TDSAMT)        MCCANN GROSS(CHARACTER)                      
MCCCOM   DS    CL(L'TDSAMT)        MCCANN COMMISSION(CHARACTER)                 
MCCNET   DS    CL(L'TDSAMT)        MCCANN GROSS - COMM(CHARACTER)               
MCCRNET  DS    CL(L'TDSAMT)        MCCANN NET - REAL (CHARACTER)                
MCNUFDAT DS    CL15                USER FIELD DATA    - NESTLE                  
         ORG    MCNUFDAT                                                        
THNUFDAT DS    CL15                USER FIELD DATA ZENITH TO NESTLE             
MCNALL   DS    PL8                 MCCANN TOTAL OF ALL INVOICES                 
MCNALTYP DS    CL2                 MCCANN TOTAL TYPE - DI/CR                    
         ORG   MCNUFDAT                                                         
MCTYPE   DS    CL2                 DI=(DEBIT) CR=(CREDIT)                       
MCPREP   DS    CL2                 PURPOSE-PREPAID CODE                         
MCRGN    DS    CL11                REGION                                       
MCTEXT   DS    CL4                 TEXT                                         
MCPCODE  DS    CL4                 P1CODE V2=0201, ELSE=0101                    
MCION    DS    CL9                 INTERNAL ORDER NUMBER (TO BE SENT)           
MCION6   DS    CL9                 INTERNAL ORDER NUMBER EST USER1 2005         
MCION5   DS    CL9                 INTERNAL ORDER NUMBER EST USER2 2006         
MCGLA    DS    CL10                GENERAL LEDGER ACCOUNT                       
MCPRIOR  DS    CL1                 BILL FOR PRIOR YEAR 'Y' OR 'N'               
MCN1IC   DS    CL10                N1 IDENTIFICATION CODE FOR COKE              
MCREFC   DS    CL10                REF IDENTIFICATION CODE FOR COKE             
         EJECT                                                                  
         ORG   AGYDATA                                                          
M2SHLDTA DS    0CL56         M2 SHELL DATA                                      
M2SHLCI  DS    CL4           COMPANY IDENITIFER                                 
M2SHLCAR DS    CL10          COST ALLOCATION REFERENCE                          
M2SHLGL  DS    CL10          G/L ACCOUNT (MIGHT NEVER BE MORE THAN 7)           
*                            THIS ALLOWS FOR A BIGGER GL CODE                   
M2SHLWBS DS    CL32          WBS ELEMENT                                        
*                                                                               
         DS    CL5           SPARE                                              
         ORG                                                                    
*                                                                               
LWSX     EQU   *                                                                
         EJECT                                                                  
*********************************************************************           
* DSECT TO COVER THE FLAT FILE RECORD TABLE                         *           
*********************************************************************           
FFRD     DSECT                                                                  
FFRLN    DS    XL2                 LENGTH OF ENTRY                              
FFRID    DS    CL2                 RECORD ID                                    
FFRDATA  DS    0XL5                                                             
FFRDLN   DS    XL1                 DATA LENGTH                                  
FFRDDST  DS    AL2                 DISP. TO DESTINATION                         
FFRDSRC  DS    AL2                 DISP. TO SOURCE                              
         ORG   FFRDATA                                                          
FFRROUID DS    XL3                 BINARY ZEROS IDENTIFIES A ROUTINE            
FFRROU   DS    XL2                 DISP. TO ROUTINE                             
         ORG   FFRDATA                                                          
FFRCDLN  DS    XL1                 CONDITIONAL DATA LENGTH                      
CONDQ    EQU   X'80'               CONDITIONAL                                  
CNDSQ    EQU   X'C0'               CONDITIONAL (CHECK LOCAL STORAGE)            
FFRCDDSP DS    AL2                 DISP. TO CONDITIONAL DATA                    
FFRCDDAT DS    0C                  CONDITIONAL DATA                             
         ORG                                                                    
         EJECT                                                                  
*********************************************************************           
* DSECT TO THE AGENCY/CLIENT TABLE                                  *           
*********************************************************************           
                                                                                
AGCD     DSECT                                                                  
AGCAGY   DS    CL2                 AGENCY ALPHA                                 
AGCSYS   DS    CL1                 SYSTEM                                       
AGCCLI   DS    CL3                 CLIENT                                       
AGCCSC   DS    AL2                 CLIENT/ SYSTEM                               
AGCOPT   DS    XL1                 OPTIONS                                      
AGCME    EQU   1                    MEDIA EDGE                                  
AGCDE    EQU   2                    DIGITAL EDGE                                
AGCKL    EQU   3                    KANG & LEEE                                 
AGCBR    EQU   4                    BRAVO                                       
AGCWW    EQU   5                    WUNDERMAN                                   
AGCSORT  EQU   X'80'               . SORT USED                                  
AGCLNQ   EQU   *-AGCD                                                           
                                                                                
*********************************************************************           
* DSECT FOR CLIENT/SYSTEM CONTROL TABLE                             *           
*********************************************************************           
                                                                                
CSCD     DSECT                                                                  
CSCROUT  DS    AL2                 ROUTINE                                      
CSCMAPS  DS    0XL2                MAP FOR THE:                                 
CSCMHDR  DS    AL2                 HEADER RECORD(S)                             
CSCMIVH  DS    AL2                 INVOICE HEADER RECORD(S)                     
CSCMLIN  DS    AL2                 LINE DETAIL RECORD(S)                        
CSCMTOT  DS    AL2                 INVOICE TOTAL RECORD(S)                      
CSCNMAP  EQU   (*-CSCMAPS)/L'CSCMAPS                                            
CSCMXTR  DS    AL2                 TRANSMISSION TOTAL RECORD                    
         EJECT                                                                  
*********************************************************************           
* DSECT TO COVER MAP TABLE ENTRY                                    *           
*********************************************************************           
                                                                                
MAPD     DSECT                                                                  
MAPTYP   DS    AL1                 TYPE OF ENTRY TO FOLLOW                      
ROUQ     EQU   1                   ROUTINE                                      
LWSQ     EQU   2                   FROM LOCAL SAVED DATA                        
CONQ     EQU   3                   CONSTANT                                     
BEGQ     EQU   4                   BEGIN LOOP                                   
ENDQ     EQU   5                   END LOOP                                     
MAPROUT  DS    XL2                 DISPLACEMENT TO ROUTINE                      
MAPRLNQ  EQU   *-MAPD                                                           
         ORG   MAPROUT                                                          
MAPFLD   DS    AL2                 DISPLACEMENT TO OUTPUT FIELD                 
MAPLEN   DS    AL2                 LENGTH OF OUTPUT FIELD (OR ZERO)             
MAPDATA  DS    XL2                 DISPLACEMENT TO DATA                         
MAPLNQ   EQU   *-MAPD                                                           
         ORG   MAPDATA                                                          
MAPCON   DS    0C                  CONSTANT                                     
         ORG                                                                    
*                                                                               
EORQ     EQU   X'00'               END OF RECORD                                
EOTQ     EQU   X'FF'               END OF TABLE                                 
ALL      EQU   X'FF'                                                            
EOT      EQU   X'FF'                                                            
*                                                                               
         EJECT                                                                  
*********************************************************************           
* DSECT TO COVER ID RECD DETAIL  - ID:INSERTION DATE                *           
*********************************************************************           
IDPD     DSECT                                                                  
IDPDATE  DS    CL8                 MOS MMM/YY                                   
IDPDESC  DS    CL25                DESCRIPTION                                  
IDPOGRS  DS    PL(BKLQ)            ORDERED GROSS                                
IDPONET  DS    PL(BKLQ)            ORDERED NET                                  
IDPOCSD  DS    PL(BKLQ)            ORDERED CD                                   
IDPOTAX  DS    PL(BKLQ)            ORDERED TAX                                  
IDPPGRS  DS    PL(BKLQ)            PREVIOUS GROSS                               
IDPPNET  DS    PL(BKLQ)            PREVIOUS NET                                 
IDPPCSD  DS    PL(BKLQ)            PREVIOUS CASH DISCOUNT                       
IDPPTAX  DS    PL(BKLQ)            PREVIOUS TAX                                 
IDPPUB   DS    CL60                PUB                                          
IDPLNQ   EQU   *-IDPD                                                           
                                                                                
*********************************************************************           
* DSECT TO COVER LINE ITEM DETAIL - MT:STA:MMM/YY OR MT:PUB:MMM/YY  *           
*********************************************************************           
LIND     DSECT                                                                  
LINMOS   DS    CL8                 MOS MMM/YY                                   
LINGRS   DS    PL(BKLQ)            GROSS                                        
LINNET   DS    PL(BKLQ)            NET                                          
LINCSD   DS    PL(BKLQ)            CD                                           
LINTAX   DS    PL(BKLQ)            TAX                                          
LININS   DS    PL(BKLQ)            INSERTIONS                                   
LINCOM   DS    PL(BKLQ)            COMMISSION                                   
LINDUE   DS    PL(BKLQ)            DUE                                          
LINSTA   DS    CL9                 STATION                                      
LINMKT   DS    CL4                 MARKET                                       
LINMKTN  DS    CL24                MARKET NAME                                  
         ORG   LINSTA                                                           
LINPUB   DS    CL60                PUB                                          
LINLNQ   EQU   *-LIND                                                           
                                                                                
*********************************************************************           
* DSECT TO COVER MONTHLY INVOICE TOTAL - MT:INV:MMM/YY                          
*********************************************************************           
                                                                                
MITD     DSECT                                                                  
MITMOS   DS    CL8                 MOS MMM/YY                                   
MITGRS   DS    PL(BKLQ)            GROSS                                        
MITNET   DS    PL(BKLQ)            NET                                          
MITOGRS  DS    PL(BKLQ)            GROSS - ORDERED                              
MITONET  DS    PL(BKLQ)            NET - ORDERED                                
MITTYP   DS    CL1                 TYPE                                         
MITLNQ   EQU   *-MITD                                                           
         EJECT                                                                  
*********************************************************************           
* DSECT TO COVER STARCOM CLIENT ACCOUNT CODES                                   
*********************************************************************           
                                                                                
H9ACCD   DSECT                                                                  
H9CLI    DS    CL3                 CLIENT                                       
H9FUND   DS    CL3                 FUND                                         
H9GLSR   DS    CL5                 G/L ACCOUNT RADIO                            
H9GLSX   DS    CL5                             NETWORK RADIO                    
H9GLST   DS    CL5                             TV                               
H9GLNN   DS    CL5                             NETWORK TV                       
H9GLPO   DS    CL5                             OUTDOOR                          
H9GLPM   DS    CL5                             MAGAZINE/NEWSPAPER               
H9GLCOM  DS    CL5                             COMMISSION                       
H9AGID   DS    CL5                 AGENCY ID                                    
H9AGNME  DS    AL3                 AGENCY NAME                                  
H9ACLNQ  EQU   *-H9ACCD                                                         
         EJECT                                                                  
***********************************************************************         
* DSECT FOR EDI HEADER LINE                                           *         
***********************************************************************         
                                                                                
EDIHDRD  DSECT                                                                  
EDILN    DS    XL2                 LENGTH                                       
         DS    XL2                                                              
EDISET   DS    CL3                 SET (810)                                    
EDISPA   DS    CL3                 SPACES                                       
EDIHLNQ  EQU   *-EDIHDRD                                                        
EDISEG   DS    CL3                 SEGEMENT                                     
EDISEQ   DS    CL3                 SEQUENCE                                     
EDIZRO   DS    CL5                 SPARE (00000)                                
EDILNQ   EQU   *-EDILN                                                          
EDIDATA  DS    0C                                                               
*                                                                               
         ORG   EDILN                                                            
EDIHDLN  DS    XL2                 RECORD LENGTH                                
         DS    XL2                 NULLS                                        
EDIHDSID DS    XL6                 RECORD LENGTH                                
EDIHDZ   DS    XL11                MUST BE CHARACTER ZEROS                      
EDIHDDN  DS    CL22                DOCUMENT NAME                                
EDIHDGID DS    CL2                 FUNCTIONAL GROUP ID                          
EDIHDDDS DS    CL15                =C'DDS'                                      
EDIHDPID DS    CL15                PARTNER'S PROFILE ID (EXPORT)                
*                                  PARTNER'S APPLICATION CODE (IMPORT)          
EDIHDRQ  EQU   *-EDIHDRD                                                        
         EJECT                                                                  
* DSECT FOR MEDIA BILLING 'FILE HEADER' RECORD                                  
FHD      DSECT                                                                  
FHRT     DS    CL1                 RECORD TYPE - 'H'                            
FHCLI    DS    CL3                 CLIENT CODE FROM B1A PROFILE                 
FHSYS    DS    CL1                 SYSTEM                                       
FHAGY    DS    CL2                 AGENCY                                       
         DS    CL9                 N/D                                          
FHSTD    DS    CL1                 STANDARD - X                                 
FHVRSN   DS    CL4                 VERSION - 4010                               
FHDOCT   DS    CL3                 DOCUMNET TYPE - 810                          
FHPROD   DS    CL1                 P=PRODUCTION, T=TEST                         
         EJECT                                                                  
* DSECT FOR MEDIA BILLING 'INVOICE HEADER' RECORD                               
IHD      DSECT                                                                  
IHRT     DS    CL2                 RECORD TYPE - 'IH'                           
IHINVN   DS    CL10                INVOICE NUMBER                               
IHINVD   DS    CL8                 INVOICE DATE MMMDD/YY                        
IHDUED   DS    CL8                 DUE DATE MMMDD/YY                            
IHINVT   DS    CL1                 INVOICE TYPE                                 
IHINVTN  EQU   C'N'                 NORMAL                                      
IHINVTM  EQU   C'M'                 MANUAL                                      
IHINVTR  EQU   C'R'                 REVERSAL                                    
IHSHIPD  DS    CL8                 SHIPPED DATE                                 
IHLNQ    EQU   *-IHD                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'AGENCY' RECORD                                       
AGD      DSECT                                                                  
AGRT     DS    CL2                 RECORD TYPE - 'AG'                           
AGNAME   DS    CL33                AGENCY NAME                                  
AGADDR   DS    CL33                AGENCY ADDRESS                               
AGLNQ    EQU   *-AGD                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'MEDIA' RECORD                                        
MED      DSECT                                                                  
MERT     DS    CL2                 RECORD TYPE - 'ME'                           
MEMEDIA  DS    CL2                 SYSTEM CODE                                  
MEPMQ    EQU   C'PM'                PRINT MAGAZINE                              
MEPNQ    EQU   C'PN'                      NEWSPAPER                             
MESTQ    EQU   C'ST'                SPOT TELEVISION                             
MESRQ    EQU   C'SR'                     RADIO                                  
MENCQ    EQU   C'NC'               NETWORK CABLE                                
MECSN    EQU   C'SN'               SPOT NETWORK FOR OMD CANADA                  
MEDESC   DS    CL10                MEDIA DESCRIPTION                            
MELNQ    EQU   *-MED                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'ADVERTISER' RECORD                                   
AVD      DSECT                                                                  
AVRT     DS    CL2                 RECORD TYPE - 'AV'                           
AVCODE   DS    CL3                 ADVERTISER CODE                              
AVSPOT   DS    0C                  SPOT                                         
AVSNAME  DS    CL24                NAME                                         
AVSINFN  DS    CL8                 INTERFACE NUMBER                             
         ORG   AVSPOT                                                           
AVPRNT   DS    0C                  PRINT                                        
AVPNAME  DS    CL20                NAME                                         
AVPINFN  DS    CL8                 INTERFACE NUMBER                             
         ORG                                                                    
AVLNQ    EQU   *-AVD                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'BILL TO' RECORD                                      
BTD      DSECT                                                                  
BTRT     DS    CL2                 RECORD TYPE - 'BT'                           
BTADDR1  DS    CL36                ADDRESS LINE 1                               
BTADDR2  DS    CL36                             2                               
BTADDR3  DS    CL36                             3                               
BTADDR4  DS    CL36                             4                               
BTADDR5  DS    CL36                             5                               
BTLNQ    EQU   *-BTD                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'PRODUCT GROUP - 1' RECORD                            
P1D      DSECT                                                                  
P1RT     DS    CL2                 RECORD TYPE - 'P1'                           
P1CODE   DS    CL5                 CODE                                         
P1LVLD   DS    CL12                LEVEL DESC                                   
P1NAME   DS    CL24                NAME                                         
P1LNQ    EQU   *-P1D                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'PRODUCT GROUP - 2' RECORD                            
P2D      DSECT                                                                  
P2RT     DS    CL2                 RECORD TYPE - 'P2'                           
P2CODE   DS    CL5                 CODE                                         
P2LVLD   DS    CL12                LEVEL DESC                                   
P2NAME   DS    CL24                NAME                                         
P2LNQ    EQU   *-P2D                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'PRODUCT GROUP - 3' RECORD                            
P3D      DSECT                                                                  
P3RT     DS    CL2                 RECORD TYPE - 'P3'                           
P3CODE   DS    CL5                 CODE                                         
P3LVLD   DS    CL12                  LEVEL DESC                                 
P3NAME   DS    CL24                NAME                                         
P3LNQ    EQU   *-P3D                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'PRODUCT' RECORD                                      
PRD      DSECT                                                                  
PRRT     DS    CL2                 RECORD TYPE - 'PR'                           
PRCODE   DS    CL3                 CODE                                         
PRNAME   DS    CL24                NAME                                         
PRINFN   DS    CL24                INTERFACE NUMBER                             
PRLNQ    EQU   *-PRD                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'ESTIMATE RECORD                                      
ESD      DSECT                                                                  
ESRT     DS    CL2                 RECORD TYPE - 'ES'                           
ESCODE   DS    CL3                 CODE                                         
ESNAME   DS    CL20                NAME                                         
ESPNME2  DS    CL20                NAME 2 (PRINT)                               
ESLNQ    EQU   *-ESD                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'MARKET GROUP 1' RECORD                               
M1D      DSECT                                                                  
M1RT     DS    CL2                 RECORD TYPE - 'M1'                           
M1CODE   DS    CL5                 CODE                                         
M1LVLD   DS    CL12                LEVEL DESC                                   
M1NAMES  DS    CL24                NAME - SPOT                                  
         ORG   M1NAMES                                                          
M1NAMEP  DS    CL20                NAME - PRINT                                 
         ORG                                                                    
M1LNQ    EQU   *-M1D                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'MARKET GROUP 2' RECORD                               
M2D      DSECT                                                                  
M2RT     DS    CL2                 RECORD TYPE - 'M2'                           
M2CODE   DS    CL5                 CODE                                         
M2LVLD   DS    CL12                LEVEL DESC                                   
M2NAMES  DS    CL24                NAME - SPOT                                  
         ORG   M2NAMES                                                          
M2NAMEP  DS    CL20                NAME - PRINT                                 
         ORG                                                                    
M2LNQ    EQU   *-M2D                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'MARKET GROUP 3' RECORD                               
M3D      DSECT                                                                  
M3RT     DS    CL2                 RECORD TYPE - 'M3'                           
M3CODE   DS    CL5                 CODE                                         
M3LVLD   DS    CL12                LEVEL DESC                                   
M3NAMES  DS    CL24                NAME - SPOT                                  
         ORG   M3NAMES                                                          
M3NAMEP  DS    CL20                NAME - PRINT                                 
         ORG                                                                    
M3LNQ    EQU   *-M3D                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'MARKET' RECORD                                       
MKD      DSECT                                                                  
MKRT     DS    CL2                 RECORD TYPE - 'MK'                           
MKCODE   DS    CL4                 CODE                                         
MKNAMES  DS    CL24                NAME - SPOT                                  
         ORG   MKNAMES                                                          
MKNAMEP  DS    CL20                NAME - PRINT                                 
         ORG                                                                    
MKLNQ    EQU   *-MKD                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'STATION' RECORD                                      
STD      DSECT                                                                  
STRT     DS    CL2                 RECORD TYPE - 'ST'                           
STCODE   DS    CL9                 CODE                                         
STCITY   DS    CL24                CITY                                         
STAFFLL  DS    CL9                 AFFILIATE                                    
STLNQ    EQU   *-STD                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'PUBLICATION' RECORD                                  
PBD      DSECT                                                                  
PBRT     DS    CL2                 RECORD TYPE - 'PB'                           
PBNAME   DS    CL60                NAME                                         
PBNAME2  DS    CL20                NAME  - PART 2                               
PBLNQ    EQU   *-PBD                                                            
*                                                                               
* DSECT FOR MEDIA BILLING 'INSERTION DATE' RECORD                               
IDD      DSECT                                                                  
IDRT     DS    CL2                 RECORD TYPE - 'ID'                           
IDDATE   DS    CL11                INSERTION DATE                               
IDDESC   DS    CL25                DESCRIPTION                                  
IDOGROSS DS    CL11                ORDERED GROSS                                
IDONET   DS    CL11                ORDERED NET                                  
IDOCDISC DS    CL11                ORDERED CASH DISCOUNT                        
IDOTAX   DS    CL11                ORDERED TAX                                  
IDPGROSS DS    CL11                PREVIOUS GROSS                               
IDPNET   DS    CL11                PREVIOUS NET                                 
IDPCDISC DS    CL11                PREVIOUS CASH DISCOUNT                       
IDPTAX   DS    CL11                PREVIOUS TAX                                 
IDLNQ    EQU   *-IDD                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'COMMENT' RECORD                                      
CMD      DSECT                                                                  
CMRT     DS    CL2                 RECORD TYPE - 'CM'                           
CMLVLN   DS    CL4                 COMMENT LEVEL NAME                           
CMLPRDQ  EQU   C'PRD'                                                           
CMLESTQ  EQU   C'EST'                                                           
CMTXT    DS    CL80                COMMENT TEXT                                 
CMLNQ    EQU   *-CMD                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'USER-DEFINED DATA' RECORD                            
UDD      DSECT                                                                  
UDRT     DS    CL2                 RECORD TYPE - 'UD'                           
UDLOC    DS    CL2                 DATA LOCATION                                
UDLP1Q   EQU   C'P1'                                                            
UDLP2Q   EQU   C'P2'                                                            
UDLE1Q   EQU   C'E1'                                                            
UDDES    DS    CL20                DATA DESCRIPTION                             
UDTXT    DS    CL32                DATA TEXT                                    
UDLNQ    EQU   *-UDD                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'USER-COMMENT DATA' RECORD                            
UCD      DSECT                                                                  
UCRT     DS    CL2                 RECORD TYPE - 'UC'                           
UCLOC    DS    CL2                 DATA LOCATION                                
UCDES    DS    CL20                DATA DESCRIPTION                             
UCTXT    DS    CL32                DATA TEXT                                    
UCLNQ    EQU   *-UCD                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'MONTHLY TOTAL' RECORD                                
MTD      DSECT                                                                  
MTRT     DS    CL2                 RECORD TYPE - 'MT'                           
MTDLVN   DS    CL4                 DETAIL LEVEL NAME                            
MTMOS    DS    CL8                 MONTH OF SERVICE -MMM/YY                     
MTTYP    DS    CL1                 TYPE                                         
MTPRINV  DS    CL10                PREVIOUS INV NUMBER                          
MTSORGRS DS    CL11                SPOT - ORDERED GROSS                         
MTSORNET DS    CL11                               NET                           
MTSORTAX DS    CL11                               TAX                           
MTSORSPT DS    CL11                               SPOTS                         
MTSPRGRS DS    CL11                       PREVIOUS GROSS                        
MTSPRNET DS    CL11                                NET                          
MTSPRTAX DS    CL11                                TAX                          
MTSPRSPT DS    CL11                                SPOTS                        
         ORG   MTPRINV                                                          
MTCOST   DS    CL1                 COST TYPE - NETWORK ONLY                     
MTNPRINV DS    CL10                PREVIOUS INV NUMBER                          
MTNORGRS DS    CL11                ORDERED GROSS                                
MTNORNET DS    CL11                ORDERED NET                                  
MTNORTAX DS    CL11                ORDERED TAX                                  
MTNORSPT DS    CL11                ORDERED SPOTS                                
MTNPRGRS DS    CL11                PREVIOUS GROSS                               
MTNPRNET DS    CL11                PREVIOUS NET                                 
MTNPRTAX DS    CL11                PREVIOUS TAX                                 
MTNPRSPT DS    CL11                PREVIOUS SPOTS                               
         ORG   MTTYP                                                            
MTPPRINV DS    CL10                PREVIOUS INV NUMBER                          
MTPORGRS DS    CL11                PRINT - ORDERED GROSS                        
MTPORNET DS    CL11                                NET                          
MTPORCSD DS    CL11                                CASH DISCOUNT                
MTPORTAX DS    CL11                                TAX                          
MTPORINS DS    CL11                                INSERTIONS                   
MTPPRGRS DS    CL11                        PREVIOUS GROSS                       
MTPPRNET DS    CL11                                 NET                         
MTPPRCSD DS    CL11                                 CASH DISCOUNT               
MTPPRTAX DS    CL11                                 TAX                         
MTPPRSPT DS    CL11                                 INSERTIONS                  
         ORG                                                                    
MTLNQ    EQU   *-MTD                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'GST/QST/HST/PST TAX' RECORD                          
CTD      DSECT                                                                  
CTRT     DS    CL2                 RECORD TYPE - 'CT'                           
CTLVLN   DS    CL7                 DETAIL LEVEL NAME                            
CTACCT   DS    CL16                ACCOUNT                                      
CTPCNT   DS    CL11                PERCENT                                      
CTBASIS  DS    CL11                BASIS                                        
CTTAXAMT DS    CL11                TAX AMOUNT                                   
CTLNQ    EQU   *-CTD                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'TAX DUE' RECORD                                      
TDD      DSECT                                                                  
TDRT     DS    CL2                 RECORD TYPE - 'TD'                           
TDTOTDUE DS    CL11                TOTAL DUE                                    
TDLNQ    EQU   *-TDD                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'AMOUNT DUE' RECORD                                   
ADD      DSECT                                                                  
ADRT     DS    CL2                 RECORD TYPE - 'AD'                           
ADLVLN   DS    CL4                 DETAIL LEVEL NAME                            
ADBAS    DS    CL1                 BILL BASIS                                   
ADBGRSQ  EQU   C'G'                GROSS                                        
ADBNETQ  EQU   C'N'                NET                                          
ADBMIXQ  EQU   C'M'                MIXED                                        
ADABAS   DS    CL1                 ADJUSTED BASIS                               
ADADJPCT DS    CL7                 ADJUSTMENT %                                 
ADBASAMT DS    CL11                BASIS AMOUNT                                 
ADCOMAMT DS    CL11                COMMISSION                                   
ADDUEAMT DS    CL11                AMOUNT DUE AT LEVEL                          
ADLNQ    EQU   *-ADD                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'PREVIOUS INVOICE' RECORD                             
PID      DSECT                                                                  
PIRT     DS    CL2                 RECORD TYPE - 'PI'                           
PIINVN   DS    CL10                INVOICE NUMBER                               
PIGRS    DS    CL11                GROSS                                        
PINET    DS    CL11                NET                                          
PILNQ    EQU   *-PID                                                            
                                                                                
* DSECT FOR MEDIA BILLING 'REMITTANCE ADDRESS' RECORD                           
RAD      DSECT                                                                  
RART     DS    CL2                 RECORD TYPE - 'RA'                           
RANAME   DS    CL20                NAME                                         
RAADDR   DS    CL24                ADDRESS                                      
RACITY   DS    CL24                CITY                                         
RASTATE  DS    CL3                 STATE                                        
RAPOSTAL DS    CL10                POSTAL CODE                                  
RALNQ    EQU   *-RAD                                                            
         EJECT                                                                  
**********************************************************************          
* DSECTS FOR EDI 4010 SEGMENTS                                       *          
**********************************************************************          
                                                                                
* DSECT FOR HDR RECORD                                                          
HDRD     DSECT                                                                  
HDRID    DS    CL3                 ID                                           
HDRSEQ   DS    CL3                 SEQUENCE                                     
HDRZRO   DS    CL5                 ZEROS                                        
         DS    CL24                N/D                                          
HDRDDS   DS    CL15                =C'DDS'                                      
HDRPROF  DS    CL15                USER PROFILE                                 
                                                                                
* DSECT FOR BIG SEGMENT                                                         
BIGD     DSECT                                                                  
BIGID    DS    CL3                 ID                                           
BIGSEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
BIGDATE  DS    CL8                 DATE                                         
BIGINVN  DS    CL22                INVOICE NUMBER                               
BIGDATE1 DS    CL8                 DATE                                         
BIGPON   DS    CL22                PURCHASE ORDER NUMBER                        
BIGRN    DS    CL30                RELEASE NUMBER                               
         DS    CL8                 CHANGE ORDER SEQUENCE NUMBER                 
BIGTYPE  DS    CL2                 TRANSACTION TYPE CODE                        
BIGPURP  DS    CL2                 PURPOSE CODE                                 
                                                                                
* DSECT FOR NTE SEGMENT                                                         
NTED     DSECT                     NOTE/SPECIAL INSTRUCTION                     
NTEID    DS    CL3                 ID                                           
NTESEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
NTEREF   DS    CL3                 NOTE REFERENCE CODE                          
NTEDESC  DS    CL11                DESCRIPTION - 'CREDIT MEMO'                  
                                                                                
* DSECT FOR CUR SEGMENT                                                         
CURD     DSECT                     CURRENCY                                     
CURID    DS    CL3                 ID                                           
CURSEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
CUREIC   DS    CL3                 ENTITY IDENTFIER CODE                        
CURCUR   DS    CL3                 CURRENCY CODE                                
                                                                                
* DSECT FOR REF SEGMENT                                                         
REFD     DSECT                     REFERENCE IDENTIFICATION                     
REFID    DS    CL3                 ID                                           
REFSEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
REFRIQ   DS    CL3                 REFERENCE IDENTIFICATION QUALIFIER           
REFRI    DS    CL30                REFERENCE IDENTIFICATION                     
REFDES   DS    CL80                REFERENCE DESCRIPTION                        
REFIDQ   DS    CL3                 REFERENCE ID QUALIFIER                       
REFIDEN  DS    CL30                REFERENCE ID                                 
                                                                                
* DSECT FOR PER SEGMENT                                                         
PERD     DSECT                     REFERENCE IDENTIFICATION                     
PERID    DS    CL3                 ID                                           
PERSEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
PERCFC   DS    CL2                 CONTACT FUNCTION CODE                        
PERNAME  DS    CL60                CONTACT NAME                                 
                                                                                
* DSECT FOR N1  SEGMENT                                                         
N1D      DSECT                     NAME                                         
N1ID     DS    CL3                 ID                                           
N1SEQ    DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
N1EIC    DS    CL3                 ENTITY IDENTIFIER CODE                       
N1NAME   DS    CL60                NAME                                         
N1ICQ    DS    CL2                 IDENTIFICATION CODE QUALIFIER                
N1IC     DS    CL80                IDENTIFICATION CODE                          
                                                                                
* DSECT FOR N2  SEGMENT                                                         
N2D      DSECT                     ADDITIONAL NAME INFORMATION                  
N2ID     DS    CL3                 ID                                           
N2SEQ    DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
N2NAME   DS    CL60                NAME                                         
N2NAME2  DS    CL60                NAME                                         
                                                                                
* DSECT FOR N3  SEGMENT                                                         
N3D      DSECT                     ADDRESS INFORMATION                          
N3ID     DS    CL3                 ID                                           
N3SEQ    DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
N3ADDR1  DS    CL55                ADDRESS LINE 1                               
N3ADDR2  DS    CL55                ADDRESS LINE 2                               
                                                                                
* DSECT FOR N4  SEGMENT                                                         
N4D      DSECT                     GEOGRAPHIC LOCATION                          
N4ID     DS    CL3                 ID                                           
N4SEQ    DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
N4CITY   DS    CL30                CITY NAME                                    
N4STATE  DS    CL2                 STATE                                        
N4ZIP    DS    CL15                ZIP                                          
N4CNTY   DS    CL3                 COUNTRY                                      
N4LOCQ   DS    CL2                 LOCATION QUALIFER                            
N4LOCID  DS    CL30                LOCATION IDENTIFER                           
                                                                                
* DSECT FOR ITD  SEGMENT                                                        
ITDD     DSECT                     ADDITIONAL NAME INFORMATION                  
ITDID    DS    CL3                 ID                                           
ITDSEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
ITD1     DS    CL2                 TERMS BASIS DATE CODE                        
ITD2     DS    CL1                 NUMBER OF DECIMAL PLACES                     
ITD3     DS    CL6                 TERMS DISCOUNT PERCENT                       
ITD4     DS    CL8                 TERMS DISCOUNT DUE DATE                      
ITD5     DS    CL1                 NUMBER OF DECIMAL PLACES                     
ITD6     DS    CL3                 TERMS DISCOUNT DAYS DUE                      
ITD7     DS    CL8                 TERMS NET DUE DATE                           
ITD8     DS    CL1                 NUMBER OF DECIMAL PLACES                     
ITD9     DS    CL3                 TERMS NET DAYS                               
ITD10    DS    CL1                 NUMBER OF DECIMAL PLACES                     
ITD11    DS    CL10                TERMS DISCOUNT AMOUNT                        
ITD12    DS    CL8                 TERMS DEFERRED DUE DATE                      
ITD13    DS    CL1                 NUMBER OF DECIMAL PLACES                     
ITD14    DS    CL10                DEFERRED AMOUNT DUE                          
ITD15    DS    CL1                 NUMBER OF DECIMAL PLACES                     
ITD16    DS    CL5                 PERCENT OF INVOICE PAYABLE                   
ITD17    DS    CL80                DESCRIPTION                                  
ITD18    DS    CL1                 NUMBER OF DECIMAL PLACES                     
ITD19    DS    CL2                 DAY OF MONTH                                 
ITD20    DS    CL2                 PAYMENT METHOD CODE                          
ITD21    DS    CL1                 NUMBER OF DECIMAL PLACES                     
ITD22    DS    CL10                PERCENT                                      
                                                                                
* DSECT FOR DTM SEGMENT                                                         
DTMD     DSECT                     DATE/TIME REFERENCE                          
DTMID    DS    CL3                 ID                                           
DTMSEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
DTMDTQ   DS    CL3                 DATE/TIME QUALIFIER                          
DTMDATE  DS    CL8                 DATE                                         
DTMTIME  DS    CL8                 TIME                                         
         DS    CL2                 TIME CODE                                    
DTMPFQ   DS    CL3                 DATE TIME PERIOD FORMAT QUALIFIER            
DTMPCCYY DS    CL4                 DATE TIME PERIOD - CCYY                      
                                                                                
* DSECT FOR IT1 SEGMENT                                                         
IT1D     DSECT                     BASELINE ITEM DATA (INVOICE)                 
IT1ID    DS    CL3                 ID                                           
IT1SEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
IT1AID   DS    CL20                ASSIGNED IDENTIFICATION                      
IT1QNTY  DS    CL11                QUANTITY INVOICED                            
IT1BMC   DS    CL2                 UNIT OR BASIS FOR MEASUREMENT CODE           
IT1NDP   DS    CL1                 NUMBER OF DECIMAL PLACES                     
IT1ZERO  DS    CL1                 FILLER ZERO                                  
IT1UP    DS    CL16                UNIT PRICE                                   
         DS    CL2                 BASIS OF UNIT PRICE CODE                     
IT1PSIQ  DS    CL2                 PRODUCT/SERVICE ID QUALIFIER                 
IT1PSI   DS    CL48                PRODUCT/SERVICE ID                           
IT1PSIQ2 DS    CL2                 PRODUCT/SERVICE ID QUALIFIER                 
IT1PSI2  DS    CL48                PRODUCT/SERVICE ID                           
IT1PSIQ3 DS    CL2                 PRODUCT/SERVICE ID QUALIFIER                 
IT1PSI3  DS    CL48                PRODUCT/SERVICE ID                           
*                                                                               
* DSECT FOR PAM SEGMENT                                                         
PAMD     DSECT                     PERIOD AMOUNT (INVOICE)                      
PAMID    DS    CL3                 ID                                           
PAMSEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
PAMQQ    DS    CL2                 QUANTITY QUALIFIER                           
PAMDEC   DS    CL1                 NUMBER OF DECIMAL PLACES                     
PAMQTY   DS    CL15                QUANTITY                                     
PAMBASIS DS    CL2                 UNIT OR BASIS FOR MEASUREMENT CODE           
         DS    CL143               A WHOLE LOTTA FIELDS IN BETWEEN              
PAMAQC   DS    CL3                 AMOUNT QUALIFIER CODE                        
PAMDEC2  DS    CL1                 NUMBER OF DECIMAL PLACES                     
PAMAMT   DS    CL18                MONETARY AMOUNT                              
*                                                                               
* DSECT FOR TXI SEGMENT                                                         
TXID     DSECT                     TAX INFORMATION                              
TXIID    DS    CL3                 ID                                           
TXISEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
TXITTC   DS    CL2                 TAX TYPE CODE                                
TXINDP   DS    CL1                 NUMBER OF DECIMAL PLACES                     
TXIZERO  DS    CL2                 FILLER ZEROS                                 
TXIAMT   DS    CL16                AMOUNT                                       
TXINDP1  DS    CL1                                                              
TXIZERO1 DS    CL2                                                              
TXIPCNT  DS    CL8                 PERCENT                                      
TXICOQA  DS    CL2                 TAX JURISDICTION CODE QUALIFIER              
TXIJCODE DS    CL10                TAX JURISDICTION CODE                        
                                                                                
* DSECT FOR CTP SEGMENT                                                         
CTPD     DSECT                     PRICING INFORMATION                          
CTPID    DS    CL3                 ID                                           
CTPSEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
         DS    CL2                 CLASS OF TRADE CODE                          
         DS    CL3                 PRICE IDENTIFIER CODE                        
CTPNDP   DS    CL1                 NUMBER OF DECIMAL PLACES                     
CTPZERO  DS    CL1                 FILLER ZERO                                  
CTPUP    DS    CL16                UNIT PRICE                                   
CTPQNTY  DS    CL16                QUANTITY                                     
CTPUMC   DS    CL2                 UNIT OR BASIS FOR MEASUREMENT CODE           
                                                                                
* DSECT FOR PID SEGMENT                                                         
PIDD     DSECT                     PRODUCT/ITEM DESCRIPTION                     
PIDID    DS    CL3                 ID                                           
PIDSEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
PIDIDT   DS    CL1                 ITEM DESCRIPTION TYPE                        
         DS    CL3                 PRODUCT/PROCESS CHARACTERISTIC CODE          
         DS    CL2                 AGENCY QUALIFIER CODE                        
PIDDCOD  DS    CL12                PRODUCT DESCRIPTION CODE                     
PIDDESC  DS    CL80                DESCRIPTION                                  
                                                                                
* DSECT FOR CAD SEGMENT                                                         
CADD     DSECT                     CARRIER DETAIL                               
CADID    DS    CL3                 ID                                           
CADSEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
CADTMT   DS    CL2                 TRANSPORTATION METHOD/TYPE CODE              
         DS    CL4                 EQUIPMENT INITIAL                            
         DS    CL10                EQUIPMENT NUMBER                             
CADSCAC  DS    CL4                 STANDARD CARRIER ALPHA CODE                  
         DS    CL35                ROUTING                                      
         DS    CL2                 SHIPMENT/ORDER STATUS CODE                   
CADRIQ   DS    CL3                 REFERENCE IDENTIFICATION QUALIFIER           
CADRID   DS    CL30                REFERENCE IDENTIFICATION                     
CADSLCD  DS    CL2                 SERVICE LEVEL CODE                           
                                                                                
* DSECT FOR TDS SEGMENT                                                         
TDSD     DSECT                     TOTAL MONETARY VALUE SUMMARY                 
TDSID    DS    CL3                 ID                                           
TDSSEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
TDSNDP   DS    CL1                 NUMBER OF DECIMAL PLACES                     
TDSAMT   DS    CL15                AMOUNT                                       
                                                                                
* DSECT FOR AMT SEGMENT                                                         
AMTD     DSECT                     TOTAL MONETARY VALUE SUMMARY                 
AMTID    DS    CL3                 ID                                           
AMTSEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
AMTQUA   DS    CL3                 AMOUNT QUALIFER CODE                         
AMTNDP   DS    CL1                 NUMBER OF DECIMAL PLACES                     
AMTAMT   DS    CL18                AMOUNT                                       
AMTCDF   DS    CL1                 CREDIT/DEBIT FLAG (C OR D)                   
                                                                                
* DSECT FOR SAC SEGMENT                                                         
SACD     DSECT                     SERVICE                                      
SACID    DS    CL3                 ID                                           
SACSEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
SACACI   DS    CL1                 ALLOWANCE OR CHARGE INDICATOR                
SACCODE  DS    CL4                 SERV. PROMO. ALLOW. OR CHARGE CODE           
SACAQ    DS    CL2                 AGENCY QUALIFIER CODE                        
SACCHGC  DS    CL10                AGENCY SERV. PROMO.  ALLOW. OR C C           
SACNDP   DS    CL1                 NUMBER OF DECIMAL PLACES                     
SACAMT   DS    CL16                AMOUNT                                       
SACPCTQ  DS    CL1                 PCT. QUALIFER                                
SACPNDP  DS    CL1                 NUMBER OF DECIMALS                           
SACPCT   DS    CL7                 PCT.                                         
         DS    CL1                 NUMBER OF DECIMALS                           
         DS    CL10                RATE                                         
         DS    CL2                 UNIT OR BASIS FOR MEASUREMENT CODE           
         DS    CL1                 NUMBER OF DECIMALS                           
         DS    CL16                QUANTITY                                     
         DS    CL1                 NUMBER OF DECIMAL PLACES                     
         DS    CL16                QUANTITY                                     
         DS    CL2                 METHOD OF HANDLING CODE                      
SACREFID DS    CL30                REFERENCE IDENTIFICATION                     
SACOPTN  DS    CL20                OPTION NUMBER                                
SACDESC  DS    CL80                DESCRIPTION                                  
         DS    CL3                 LANGUAGE CODE                                
*                                                                               
* DSECT FOR CTT SEGMENT                                                         
CTTD     DSECT                     SERVICE                                      
CTTID    DS    CL3                 ID                                           
CTTSEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
CTTNDP   DS    CL1                 NUMBER OF DP                                 
CTTITM   DS    CL6                 NUMBER OF LINE ITEMS                         
         EJECT                                                                  
*                                                                               
**********************************************************************          
* DSECTS FOR EDI 3050 SEGMENTS                                       *          
**********************************************************************          
                                                                                
* DSECT FOR BIG SEGMENT                                                         
BIG3D    DSECT                                                                  
BIG3ID   DS    CL3                 ID                                           
BIG3SEQ  DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
BIG3DATE DS    CL6                 DATE                                         
BIG3INVN DS    CL22                INVOICE NUMBER                               
         DS    CL6                 DATE                                         
BIG3PON  DS    CL22                PURCHASE ORDER NUMBER                        
BIG3RN   DS    CL30                RELEASE NUMBER                               
         DS    CL8                 CHANGE ORDER SEQUENCE NUMBER                 
BIG3TYPE DS    CL2                 TRANSACTION TYPE CODE                        
                                                                                
* DSECT FOR REF SEGMENT                                                         
REF3D    DSECT                     REFERENCE IDENTIFICATION                     
REF3ID   DS    CL3                 ID                                           
REF3SEQ  DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
REF3RIQ  DS    CL3                 REFERENCE IDENTIFICATION QUALIFIER           
REF3RI   DS    CL30                REFERENCE IDENTIFICATION                     
REF3DES  DS    CL80                REFERENCE DESCRIPTION                        
REF3IDQ  DS    CL3                 REFERENCE ID QUALIFIER                       
REF3IDEN DS    CL30                REFERENCE ID                                 
                                                                                
* DSECT FOR N1  SEGMENT                                                         
N13D     DSECT                     NAME                                         
N13ID    DS    CL3                 ID                                           
N13SEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
N13EIC   DS    CL2                 ENTITY IDENTIFIER CODE                       
N13NAME  DS    CL35                NAME                                         
N13ICQ   DS    CL2                 IDENTIFICATION CODE QUALIFIER                
N13IC    DS    CL20                IDENTIFICATION CODE                          
                                                                                
* DSECT FOR N4  SEGMENT                                                         
N43D     DSECT                     GEOGRAPHIC LOCATION                          
N43ID    DS    CL3                 ID                                           
N43SEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
N43CITY  DS    CL30                CITY NAME                                    
N43STATE DS    CL2                 STATE                                        
N43POST  DS    CL11                POSTAL CODE                                  
N43CNTRY DS    CL3                 COUNTRY CODE                                 
                                                                                
* DSECT FOR IT1 SEGMENT                                                         
IT13D    DSECT                     BASELINE ITEM DATA (INVOICE)                 
IT13ID   DS    CL3                 ID                                           
IT13SEQ  DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
IT13AID  DS    CL11                ASSIGNED IDENTIFICATION                      
IT13NDP  DS    CL1                 NUMBER OF DECIMAL PLACES                     
IT13QNTY DS    CL10                QUANTITY INVOICED                            
IT13BMC  DS    CL2                 UNIT OR BASIS FOR MEASUREMENT CODE           
IT13NDPU DS    CL1                 NUMBER OF DECIMAL PLACES                     
IT13ZERO DS    CL2                 FILLER ZERO                                  
IT13UP   DS    CL15                UNIT PRICE                                   
         DS    CL2                 BASIS OF UNIT PRICE CODE                     
IT13PSQ  DS    CL2                 PRODUCT/SERVICE ID QUALIFIER                 
IT13PSI  DS    CL40                PRODUCT/SERVICE ID                           
IT13PSQ2 DS    CL2                 PRODUCT/SERVICE ID QUALIFIER                 
IT13PSI2 DS    CL40                PRODUCT/SERVICE ID                           
IT13PSQ3 DS    CL2                 PRODUCT/SERVICE ID QUALIFIER                 
IT13PSI3 DS    CL40                PRODUCT/SERVICE ID                           
                                                                                
* DSECT FOR TDS SEGMENT                                                         
TDS3D    DSECT                     TOTAL MONETARY VALUE SUMMARY                 
TDS3ID   DS    CL3                 ID                                           
TDS3SEQ  DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
TDS3NDP  DS    CL1                 NUMBER OF DECIMAL PLACES                     
TDS3AMT  DS    CL15                AMOUNT                                       
                                                                                
* DSECT FOR CTT SEGMENT                                                         
CTT3D    DSECT                     TRANSACTION TOTALS                           
CTT3ID   DS    CL3                 ID                                           
CTT3SEQ  DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
CTT3NDP  DS    CL1                 NUMBER OF DECIMAL PLACES                     
CTT3NOL  DS    CL6                 NUMBER OF LINE ITEMS                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
* DSECT FOR MCCANN - NESTLE - USER FIELD TABLE                                  
***********************************************************************         
*MCUFTABD DSECT                                                                 
*MCUFBMON DS    CL2                 START MONTH                                 
*MCUFEMON DS    CL2                 END MONTH                                   
*MCUFTYPE DS    CL1                 MT TYPE                                     
*MCUFCODE DS    CL2                 USER FIELD CODE                             
*MCUFLNQ  EQU   *-MCUFTABD                                                      
***********************************************************************         
* DSECT FOR ZENITH - NESTLE - USER FIELD TABLE                                  
***********************************************************************         
THUFTABD DSECT                                                                  
THUFTYPE DS    CL1                 MT TYPE                                      
THUFCODE DS    CL2                 USER FIELD CODE                              
THUFLNQ  EQU   *-THUFTABD                                                       
***********************************************************************         
* DSECT FOR ERROR REPORT PRINT LINE                                             
***********************************************************************         
PLIND    DSECT                                                                  
PLISTAR  DS    CL1                                                              
         DS    CL1                                                              
PLINUM   DS    CL(L'IHINVN)        INVOICE NUMBER                               
         DS    CL1                                                              
PLIDAT   DS    CL(L'IHINVD)        INVOICE DATE                                 
         DS    CL2                 ERROR FLAG                                   
PLIDES   DS    CL32                ERROR DESCRIPTION                            
***********************************************************************         
* DSECT FOR INVOICE SORT RECORD                                                 
***********************************************************************         
SORTRECD DSECT                                                                  
SORTLEN  DS    0AL4                                                             
SORTLN   DS    AL2                                                              
         DS    AL2                                                              
SORTKEY  DS    0CL26                                                            
SORTDEF  DS    CL14                DEFINED BASED ON SORT                        
SORTINV  DS    CL10                INVOICE NUMBER                               
SORTSEQ  DS    AL2                 SEQUENCE NUMBER                              
SORTLKQ  EQU   *-SORTRECD                                                       
***********************************************************************         
*                                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'100DDEDIMAP  03/03/20'                                      
         END                                                                    
