*          DATA SET DDXMLMAP   AT LEVEL 062 AS OF 12/04/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE T00ABFA                                                                  
*INCLUDE SORTER                                                                 
T00ABF   TITLE 'MEDIA INVOICE XML MAPPING'                                      
***********************************************************************         
* THIS OVERLAY IS USED BY SPOT,PRINT AND NET FOR XML TRANSMISSION     *         
* --------------------------------------------------------------------*         
* USER    JIRA         DATE                  CHANGE LOG               *         
* ---- ------------- -------- --------------------------------------- *         
* AKAT SPEC-51919    12/04/20 CHANGES FOR CHEVRON FOR AGENCY H7       *         
* AKAT SPEC-22922    04/19/18 CHANGE TEST FOR ID MVCMON TO MVCTO      *         
* AKAT SPEC-7747     02/17/17 CHANGES FOR CHEVRON FOR AGENCY H7       *         
* AKAT SPEC-52       07/20/16 CHANGES FOR CHEVRON FOR AGENCY H7       *         
* AKAT DSCUSTREQ-100 04/25/16 FIX <AmountDue> FIELD FOR MONDELEZ      *         
* AKAT DSCUSTREQ-100 04/13/16 REPORT TAX CORRECTLY FOR MONDELEZ       *         
* AKAT MOXSYS-113    01/08/16 NEW EDI FILE FOR AGY UB MONDELEZ        *         
*                                                                     *         
* PERSON LEVEL IMDB#   DESCRIPTION                                              
* ------ ----- -----   -----------                                              
*                                                                               
*  BPLA                CHANGES FOR NEW KRAFT ID- FOR CLT KRS                    
*                                                                               
*  BPLA                ALLOW ANOTHER DIVISION (007) FOR CHEVRON                 
*                                                                               
*  BPLA    53          CREATE A PRTTAPE FOR KRAFT - NOT A SFTPDISK              
*                                                                               
*                                                                               
*  BPLA                CHANGES FOR WARNER BROS. FOR OMD (OO)                    
*                      THE ABOVE ARE NO-OPED                                    
*                      CHANGES FOR KRAFT MEDIAVEST TORONTO (O0)                 
*                                                                               
*  BPLA                CHEVRON - CHG TO WAY NET-CD FORMULA                      
*                      IS HANDLED                                               
*                                                                               
*  BPLA   051          UPPER CASE V AND CHEVRON LUBRICATION DIV.                
*                                                                               
*  BPLA   046          CHANGE TO CHEVRON FILE NAME FOR H7                       
*                                                                               
*  BPLA   045          PROCESS CHEVRON CORPORATE DIV (005)                      
*                      AND PRD GRP (X201)                                       
*                                                                               
*  BPLA   044          FOR CHEVRON REPORT OUTDOOR MKT                           
*                      FROM 2ND BUY COMMENT - SENT IN MISC.                     
*                      FIELD FOR INSERTION DATA                                 
*                                                                               
*  BPLA   043          CHANGE TO CHEVRON DESCRIPTION FIELD                      
*                      TO INCLUDE INFO FROM THE SUPPLIER PART ID                
*                      SPECIAL CODE TO HANDLE CHEVRON SPOT TRADE                
*                      BILLS - REPORT A CALCULATED NET                          
*                                                                               
*  BPLA   041-042      FIX FOR USING NET-CD FORMULAS (WARNER BROS.)  *          
*                                                                               
*  BPLA   037-040      CHEVRON TRADE BILL CHG, PASS CLIENT BACK TO EX,          
*                      FIX CHEVRON E1 CHECK                                     
*                                                                               
*  BPLA   036          Change DataObjects and only put schemaLocation           
*                      in first invoice for YN - Chevron XML files              
*                      These changes to match the ones done in                  
*                      PPREPAE02 as per Paul Holmes's instructions              
*                                                                               
*  BPLA                SWITCH SEQUENCE OF FIELDTICKETINFORMATION                
*                      AND JOBLOCATIONINFORMATION FOR CHEVRON XML               
*                      ALSO CHECK FOR HISPANIC RETAIL DIVISION                  
*                                                                               
*  BPLA                CHEVRON XML WITH PRINT INSERTION DETAILS                 
*                      AND PUBS AND PRD GROUPS, MKTS, STAS FOR SPOT             
*                                                                               
*  BPLA                CHANGES FOR WB FLIGHT XML'S                              
*                      RETURNS DATASET NAME IN SECOND PARAMETER                 
*                      CHANGES FOR MQ STYLE DATASETS AND MQ RECORD              
*                                                                               
*  BPLA   24   0117196N  SUBTRACT PREV. BILLED NET FROM NET OR ORACLE*          
*                                                                               
*  BPLA   25             WB FLIGHT - SJR TEST                                   
*                                                                               
*  BPLA   30             FILE NAMING CHANGE 4-CHARACTER AGY ID                  
*                                                                               
*  BPLA                  YNRO - Chevron                                         
*                        COPY WB FLIGHT FOR AGENCY FR                           
*                        (GROUP M CONSOLIDATION)                                
*                                                                               
*  BPLA                  CHANGE TO HANDLE CHEVRON TRADE BILLS                   
*                                                                               
**********************************************************************          
MEDIM    CSECT                                                                  
         PRINT NOGEN                                                            
XML      NMOD1 (LWSX-LWSD),*XMLM*,CLEAR=YES,R8,RA                               
         LR    R9,RC                                                            
         USING LWSD,R9                                                          
         MVC   PARMS(PARMSLNQ),0(R1)                                            
         ST    R1,SAVR1                                                         
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
         GOTO1 DATCON,DMCB,(5,0),(20,CTODAY) TODAY'S DATE YYYYMMDD              
         MVC   WBTODAY(2),CTODAY+4    MM                                        
         MVI   WBTODAY+2,C'/'                                                   
         MVC   WBTODAY+3(2),CTODAY+6  DD                                        
         MVI   WBTODAY+5,C'/'                                                   
         MVC   WBTODAY+6(4),CTODAY    YYYY    MM/DD/YYYY                        
*                                                                               
         MVC   AIDPTAB,ADIDPTAB    ID RECORD DETAIL                             
         MVC   ALINTAB,ADLINTAB    LINE ITEM DETAIL                             
         MVC   AMITTAB,ADMITTAB    MONTHLY INVOICE TOTAL TABLE                  
         MVC   ASORTAB,ADSORTAB    INVOICE SORT TABLE                           
         MVC   VSORTER,ADSORTER    SORTER                                       
*                                                                               
*        GET TIME OF DAY                                                        
         TIME                                                                   
*                                                                               
*        R0 NOW HAS TIME HHMMSSHS  (PWOS)                                       
*                                                                               
         ST    R0,FULL                                                          
         SRL   R0,4                                                             
         ST    R0,MYFULL                                                        
         XC    DUB,DUB                                                          
         MVC   DUB+5(3),MYFULL                                                  
         OI    DUB+7,X'0F'                                                      
         CVB   R6,DUB                                                           
         EDIT  (R6),(5,TIMEOFD),2,FILL=0                                        
         MVI   TIMEOFD+5,C'.'                                                   
         UNPK  WORK(3),FULL+2(2)                                                
         MVC   TIMEOFD+6(2),WORK     HH.MM.SS                                   
*                                                                               
         MVI   HDRSENT,C'N'                                                     
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
EDIM3    L     R2,AEDINME          XML FILE NAME (SPTTAPE.SP0EXXX)              
*                                  OR (PRTTAPE.PP0EXXX)                         
         MVC   MAPNME,0(R2)                                                     
         MVC   MAPPGM,=C'XM'       OUTPUT IS SPTTAPE.SP0XMXX                    
         MVI   MAPSUFX,C'1'        DEFAULT                                      
*                                  OR (PRTTAPE.PP0XMXX)                         
         L     RF,ADMASTC                                                       
         CLC   MCUSERID-MASTD(6,RF),=C'MVCTOK'   KRAFT GROCERY ID               
         BE    EDIM3G                                                           
***      CLC   MCUSERID-MASTD(6,RF),=C'MVCMON'   KRAFT SNACKS ID                
         CLC   MCUSERID-MASTD(5,RF),=C'MVCTO'    KRAFT SNACKS ID                
         BE    EDIM3S                                                           
         CLC   MCUSERID-MASTD(5,RF),=C'STWTO'    KRAFT GROCERY KRS              
         BE    EDIM3K                                                           
         B     EDIM3X    IF NOT THESE LEAVE SUFFIX ALONE                        
*                                                                               
EDIM3G   MVI   MAPSUFX,C'2'        SET SUFFIX - GROCERY                         
         B     EDIM3X                                                           
*                                                                               
EDIM3S   MVI   MAPSUFX,C'3'        SET SUFFIX - SNACKS                          
         B     EDIM3X                                                           
*                                                                               
EDIM3K   MVI   MAPSUFX,C'4'        SET SUFFIX - GROCERY KRS                     
         B     EDIM3X                                                           
*                                                                               
*        NOTE  THE FIELDS ABOVE ONLY USED IF AGCFOPT IS P                       
*                                                                               
*                                                                               
EDIM3X   MVC   DSNAME,SPACES                                                    
         MVC   DSNAME+0(4),=C'BIL.'                                             
         L     R2,AEDINME                                                       
         MVC   DSNAME+4(3),0(R2)    GET PRT, SPT, OR NET FROM EDINME            
         MVI   DSNAME+7,C'.'                                                    
*                                                                               
         L     RF,ADMASTC                                                       
         L     RF,MCAEXTRA-MASTD(RF)                                            
         MVC   DSNAME+8(4),MCAGYCOD-MCEXTRA(RF)                                 
*                                                                               
         MVC   DSNAME+12(2),=C'.D'                                              
         MVC   DSNAME+14(6),CTODAY+2    YYMMDD                                  
         MVC   DSNAME+20(2),=C'.T'                                              
         MVC   DSNAME+22(2),TIMEOFD         WITHOUT .'S                         
         MVC   DSNAME+24(2),TIMEOFD+3                                           
         MVC   DSNAME+26(2),TIMEOFD+6                                           
         MVC   MQMAPNM(14),=C'SFTPDISK.PROD.'                                   
         CLI   PARMS,C'T'                                                       
         BNE   *+10                                                             
         MVC   MQMAPNM+9(4),=C'TEST'                                            
*                                                                               
         MVI   FILESW,C' '         CLEAR - WILL BE SET TO Y IF I                
*                                  ACTUALLY PROCESS A BILL                      
*                                                                               
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
         CLC   FHDOCT,=C'810'      REALLY XML                                   
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
EDIM13   DS    0H                                                               
         MVC   FILEOPT,AGCFOPT     SAVE FILE OPTION                             
*                                                                               
         CLC   FHAGY(2),=C'H7'         MINDSHARE?                               
         BNE   EDIM13B                                                          
         CLC   FHCLI(3),=C'CHV'        AND CHEVRON                              
         BNE   EDIM13B                                                          
         MVC   DSNAME+8(4),=C'MINC'    ALTER AGY CODE TO MINC                   
*                                                                               
EDIM13B  TM    FCS,FCSSORE         SORT ENDED?                                  
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
         TM    FCS,FCSEDI          TEST XML OPEN                                
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
EDIM22   TM    FCS,FCSEDI          TEST XML OPEN                                
         BNO   EDIM24                                                           
         CLOSE XMLMAP                                                           
*                                                                               
         NI    FCS,ALL-(FCSEDI)                                                 
                                                                                
EDIM24   TM    FCS,FCSEIO          OUTPUT SUMMARY?                              
         BZ    *+8                 . NO                                         
         BRAS  RE,EIOTOT                                                        
*                                                                               
         L     R1,SAVR1                                                         
         LA    RF,MQMAPNM                                                       
         ST    RF,4(R1)          RETURN FILE NAME TO EX PROGRAM                 
*                                IN SECOND PARAMETER                            
         MVC   0(1,R1),FILESW    SEND BACK INDICATOR IF FILE PRODUCED           
*                                Y=YES                                          
         MVC   1(3,R1),CLI       RETURN INTERNAL CLIENT CODE TO EX              
*                                (FROM FHCLI)                                   
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
         DROP  R4                                                               
*                                                                               
SI25     XC    0(SORTLKQ,R2),0(R2)                                              
*                                                                               
         USING IHD,R4                                                           
SI30     CLC   IHRT,=C'IH'                                                      
         BNE   SI40                                                             
         MVC   INVNUMB(L'IHINVN),IHINVN                                         
         DROP  R4                                                               
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
         DROP  R2                                                               
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
         DROP  R3,R5                                                            
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
         MVC   INVDDATE,IHDUED     INVOICE DUE DATE                             
         GOTOR CNVD,DMCB,(5,INVDDATE),(20,DUEDCYMD)                             
*                                                                               
         MVC   DUEDMDY(2),DUEDCYMD+4   MM/DD/YYYY                               
         MVI   DUEDMDY+2,C'/'                                                   
         MVC   DUEDMDY+3(2),DUEDCYMD+6                                          
         MVI   DUEDMDY+5,C'/'                                                   
         MVC   DUEDMDY+6(4),DUEDCYMD                                            
*                                                                               
         XC    AIDPITM,AIDPITM     CLEAR A(NEXT ID ITEM)                        
         XC    IDPCNT,IDPCNT       CLEAR NUMEBR IN TABLE                        
         L     R1,AIDPTAB                                                       
         MVI   0(R1),EOT                                                        
*                                                                               
         XC    ALINITM,ALINITM     CLEAR A(NEXT LINE ITEM)                      
         XC    LINCNT,LINCNT       CLEAR NUMEBR IN TABLE                        
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
         MVI   INTGONLY,C'N'       INTEGRATION ONLY SWITCH                      
         XC    PINVNUMB,PINVNUMB   FOR NOW, ONLY SAVING THE 1ST PI              
         J     XIT                                                              
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*********************************************************************           
* PREVIOUS INVOICE ROUTINE                                          *           
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
         DROP  R4                                                               
*                                                                               
MTRSI    NTR1  ,                   INVOICE                                      
*                                                                               
         CLC   MTMOS(5),=C'TOTAL'   IGNORE INV TOTAL RECORDS                    
         JE    XIT                                                              
*                                                                               
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
         ZAP   IDPOCSD,INORDCSD    GET ORDERED CD                               
         SP    IDPOCSD,INPRVCSD    MINUS PREVIOUS CD                            
*                                                                               
*        IDPOGRS IS NOW BILLED GROSS                                            
*        IDPONET IS JUST ORDERED NET-I MAY NEED TO ALTER TO BILLED NET?         
*        ORACLE NOW SAYS YES THEY DO.                                           
*                                                                               
         ZAP   IDPONET,INORDNET    ORDERED NET                                  
         SP    IDPONET,INPRVNET    MINUS PREVIOUSLY BILLED NET                  
*                                                                               
         MVC   IDPPUB,PUB          MOVE PUBLICATION                             
         MVC   IDPREF,IDREF        REFERENCE NUMBER                             
         MVC   IDPMISC,IDMISC      MISC. INS. DATA                              
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
         ZAP   LININS,PBORDINS     INSERTIONS                                   
         ZAP   LINCSD,PBORDCSD     CASH DISCOUNT                                
         SP    LINCSD,PBPRVCSD     LESS PREVIOUS                                
         MVC   LINPUB,PUB                                                       
         BAS   RE,MTRALIN          ADD TO LINE ITEM TOTAL                       
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
MTRPI    NTR1  ,                   INVOICE                                      
*                                                                               
         CLC   MTMOS(5),=C'TOTAL'   IGNORE INV TOTAL RECORDS                    
         JE    XIT                                                              
*                                                                               
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
         CLC   CLI,=C'CHV'                SEE IF CHEVRON                        
         BE    MTRALIN3                   SINCE I DON'T CHECK SPOTS/INS         
*                                                                               
         MVI   0(R4),EOT                  REMOVE THIS ITEM                      
         BR    RE                                                               
*                                                                               
MTRALIN3 LA    R4,LINLNQ(R4)                                                    
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
         MVC   BASIS,ADBAS          SAVE BASIS                                  
         MVC   ABASIS,ADABAS        SAVE ADJ BASIS                              
         MVC   BASISPCT,ADADJPCT    SAVE BFORM %                                
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
         JE    LINT6                                                            
         USING LIND,R4                                                          
*                                                                               
LINT3    MVI   TRADESW,C'N'        SET OFF SPOT TRADE INDICATOR                 
         CLI   SYSTM,C'S'          WHAT SYSTEM ?                                
         BNE   LINT4               DO SPOT ROUTINE                              
         CLI   MEDIA,C'S'          MUST REALLY BE SPOTPAK                       
         BNE   LINT3X              NO TRADE FOR NETWORK                         
         CLC   CLI,=C'CHV'         CHEVRON?                                     
         BNE   LINT3X                                                           
         CLI   BASIS,C'N'          NET BASIS                                    
         BNE   LINT3X                                                           
         CLI   ABASIS,C'G'         "OF" BASIS GROSS?                            
         BNE   LINT3X                                                           
         CLC   BASISPCT(6),=C'850000' FORMULA N + 85% OF GROSS = TRADE          
         BNE   LINT3X                                                           
         MVI   TRADESW,C'Y'        SET ON TRADE INDICATOR                       
LINT3X   BAS   RE,LINTS                                                         
         B     LINT5                                                            
*                                                                               
LINT4    CLI   SYSTM,C'P'                                                       
         BNE   *+8                                                              
         BAS   RE,LINTP            PRINT ROUTINE                                
*                                                                               
LINT5    CLI   BASIS,C'G'          BASIS = GROSS                                
         BE    LINT12              * NOTE WBF PROCESSING WILL                   
*                                  DUMP IF GROSS FORMULA ENCOUNTERED            
*                                  THEY MUST USE NET                            
*                                  FOR NET FORMULAS                             
*                                                                               
         CLC   CLI,=C'CHV'         SEE IF CHEVRON                               
         BE    LINT5B                                                           
         CLC   CLI,=C'WBF'         SEE IF WARNER BROS.                          
         BNE   LINT5C                                                           
LINT5B   CP    CALCDUE,PZERO       AND A ZERO NET                               
         BNE   LINT5C                                                           
         B     LINT6             IF SO SKIP CHECK BELOW                         
*                                AS IT'S MOST LIKELY A CNET TRADE BILL          
*                                                                               
LINT5C   CP    CALCDUE,DUEAMT      CALCULATE VS. BILL AMOUNT DUE                
         BNE   LINT5D                                                           
         B     LINT6                                                            
*                                                                               
LINT5D   ZAP   DUB,DUEAMT                                                       
         SP    DUB,CALCDUE         GET THE DIFFERENCE                           
         CLC   CLI,=C'CHV'          SEE IF CHEVRON                              
         BNE   LINT5D2                                                          
         CLI   BASIS,C'2'          AND NET-CD BASIS                             
         BNE   LINT5D2                                                          
         OC    ABIGLINE,ABIGLINE   BE SURE I HAVE AN ADDRESS                    
         BNZ   LINT5D4             ADD DIFFERENCE TO IT                         
         B     LINT6            ROUNDING CHECK WON'T WORK SOMETIMES             
*                               JUST ADD TO LARGEST AMOUNT                      
*                                                                               
*                                                                               
LINT5D2  CP    DUB,=P'15'          ALLOW 15 CENTS FOR ROUNDING                  
         BNH   *+6                                                              
         DC    H'0'                BIG ROUNDING DIFFERENCE                      
         CP    DUB,=P'-15'                                                      
         BNL   *+6                                                              
         DC    H'0'                                                             
LINT5D4  L     R4,ABIGLINE         GET THE LARGEST AMOUNT                       
         AP    LINCOM,DUB          ADD DIFFERENCE TO COMMISSION                 
         AP    LINDUE,DUB          AND AMOUNT DUE                               
         CLI   TRADESW,C'Y'        CHEVRON TRADE BILL?                          
         BNE   LINT6                                                            
         ZAP   LINNET,LINCOM       SET NET TO COMMISSION                        
         ZAP   LINDUE,LINNET       SET DUE TO THE SAME                          
*                                                                               
LINT6    DS    0H                                                               
         CLC   CLI,=C'CHV'         CHEVRON PROCESSING?                          
         BE    LINT7                                                            
         CLC   CLI,=C'KFT'         OR KRAFT PROCESSING                          
         BE    LINT7                                                            
         CLC   CLI,=C'MDN'         OR MONDELEZ PROCESSING                       
         BE    LINT7                                                            
         CLC   CLI,=C'MDZ'         OR MONDELEZ PROCESSING                       
         BE    LINT7                                                            
         CLC   CLI,=C'WBF'         WBF PROCESING?                               
         BNE   LINT12              IF NOT,DO WHAT I DID FOR ORACLE              
LINT7    CLI   BASIS,C'N'          NET                                          
         BE    LINT9                                                            
         CLI   BASIS,C'2'          NET - CD                                     
         BE    *+6                                                              
         DC    H'0'                WBF MUST USE NET FORMULA                     
*                                                                               
*        NOW DO WBF FORMATIING                                                  
*                                                                               
LINT9    DS    0H                                                               
*                                  ASSUMES THEIR BASIS IS NET                   
*                                  OR NET - CD                                  
         ZAP   DUB2,BASAMT         NOTE: BASAMT HAS TAX TAKEN OUT               
         AP    DUB2,TOTTAX         THEY INCLUDE TAX                             
         EDIT  (P8,DUB2),CDUENET,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-              
*                                                                               
         CLC   CLI,=C'CHV'      CHEVRON?                                        
         BE    LINT11                                                           
*                                                                               
         CLC   CLI,=C'KFT'      KRAFT?                                          
         BE    LINT10K                                                          
         CLC   CLI,=C'MDN'      MONDELEZ?                                       
         BE    LINT10M          YES                                             
         CLC   CLI,=C'MDZ'      MONDELEZ?                                       
         BE    LINT10M          YES                                             
*                                                                               
*        SET DEFAULT INTEGRATION DATA                                           
*                                                                               
         MVC   WBFDITG,SPACES                                                   
         MVC   WBFDITG(21),=C'0</IntegrationAmount>'                            
         CLI   INTGONLY,C'Y'      INTEGRATION ONLY?                             
         BNE   LINT9A             IF SO, SAME AS NET                            
*                                                                               
         MVC   WBFDITG,SPACES                                                   
         MVC   WBFDITG(L'CDUENET),CDUENET                                       
         LA    R1,WBFDITG+L'WBFDITG-1                                           
LINT90   CLI   0(R1),C' '                                                       
         BH    LINT90C                                                          
         BCT   R1,LINT90           SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
LINT90C  MVC   1(20,R1),=C'</IntegrationAmount>'                                
*                                                                               
LINT9A   MVC   WBFDNET,SPACES                                                   
         MVC   WBFDNET(L'CDUENET),CDUENET                                       
         LA    R1,WBFDNET+L'WBFDNET-1                                           
LINT9B   CLI   0(R1),C' '                                                       
         BH    LINT9C                                                           
         BCT   R1,LINT9B           SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
LINT9C   MVC   1(12,R1),=C'</NetAmount>'                                        
*                                                                               
LINT10   DS    0H                                                               
         EDIT  COMAMT,CDUECOM,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                 
*                                                                               
         MVC   WBFDCOM,SPACES                                                   
         MVC   WBFDCOM(L'CDUECOM),CDUECOM                                       
         LA    R1,WBFDCOM+L'WBFDCOM-1                                           
LINT10B  CLI   0(R1),C' '                                                       
         BH    LINT10C                                                          
         BCT   R1,LINT10B          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
LINT10C  MVC   1(19,R1),=C'</CommissionAmount>'                                 
*                                                                               
         EDIT  DUEAMT,CDUEAMT,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                 
*                                                                               
         MVC   WBFDDUE,SPACES                                                   
         MVC   WBFDDUE(L'CDUEAMT),CDUEAMT                                       
         LA    R1,WBFDDUE+L'WBFDDUE-1                                           
LINT10D  CLI   0(R1),C' '                                                       
         BH    LINT10E                                                          
         BCT   R1,LINT10D          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
LINT10E  MVC   1(26,R1),=C'</InvoiceItemControlTotal>'                          
         B     LINT11W       SKIP TO MORE WBF CODE                              
*                                                                               
LINT10K  DS    0H            KRAFT CODE                                         
*                                                                               
         EDIT  DUEAMT,CDUEAMT,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                 
*                                                                               
         MVI   MVKDDUE-1,X'FF'    TO PREVENT GOING PAST START                   
         MVC   MVKDDUE,SPACES                                                   
         MVC   MVKDDUE(L'CDUEAMT),CDUEAMT                                       
         LA    R1,MVKDDUE+L'MVKDDUE-1                                           
LINT10K2 CLI   0(R1),C' '                                                       
         BH    LINT10K4                                                         
         BCT   R1,LINT10K2         SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
LINT10K4 MVC   1(12,R1),=C'</InvAmount>'                                        
*                                                                               
         EDIT  TOTDUE,CDUEAMT,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                 
*                                                                               
         MVI   MVKTDUE-1,X'FF'    TO PREVENT GOING PAST START                   
         MVC   MVKTDUE,SPACES                                                   
         MVC   MVKTDUE(L'CDUEAMT),CDUEAMT                                       
         LA    R1,MVKTDUE+L'MVKTDUE-1                                           
LINT10K6 CLI   0(R1),C' '                                                       
         BH    LINT10K8                                                         
         BCT   R1,LINT10K6         SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
LINT10K8 MVC   1(12,R1),=C'</AmountDue>'                                        
*                                                                               
         SP    TOTDUE,DUEAMT     TOTAL TAX = TOTDUE - DUEAMT                    
         EDIT  TOTDUE,CDUEAMT,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                 
*                                                                               
         MVI   MVKTTAX-1,X'FF'    TO PREVENT GOING PAST START                   
         MVC   MVKTTAX,SPACES                                                   
         MVC   MVKTTAX(L'CDUEAMT),CDUEAMT                                       
         LA    R1,MVKTTAX+L'MVKTTAX-1                                           
LINT10K9 CLI   0(R1),C' '                                                       
         BH    LINT10KA                                                         
         BCT   R1,LINT10K9         SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
LINT10KA MVC   1(10,R1),=C'</TAX-HST>'                                          
*                                                                               
*        EBCANUMBER FROM EST USER 2 (CHECK ANNO - NOW PO#)                      
*                                                                               
         MVI   MVKTE2-1,X'FF'     TO PREVENT GOING PAST START                   
         MVC   MVKTE2,SPACES                                                    
         MVC   MVKTE2(L'MVEBCAN),MVEBCAN                                        
         LA    R1,MVKTE2+L'MVKTE2-1                                             
LINT10KC CLI   0(R1),C' '                                                       
         BH    LINT10KD                                                         
         BCT   R1,LINT10KC         SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
LINT10KD MVC   1(13,R1),=C'</eBCANumber>'                                       
         B     LINT12A                                                          
*                                                                               
LINT10M  DS    0H            MONDELEZ CODE                                      
*                                                                               
         EDIT  DUEAMT,CDUEAMT,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                 
*                                                                               
         MVI   UBMZDDUE-1,X'FF'    TO PREVENT GOING PAST START                  
         MVC   UBMZDDUE,SPACES                                                  
         MVC   UBMZDDUE(L'CDUEAMT),CDUEAMT                                      
         LA    R1,UBMZDDUE+L'UBMZDDUE-1                                         
LINT10M2 CLI   0(R1),C' '                                                       
         BH    LINT10M4                                                         
         BCT   R1,LINT10M2         SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
LINT10M4 MVC   1(12,R1),=C'</InvAmount>'                                        
*                                                                               
         EDIT  TAXAMT,CDUEAMT,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                 
*                                                                               
         MVI   UBMZTAX-1,X'FF'    TO PREVENT GOING PAST START                   
         MVC   UBMZTAX,SPACES                                                   
         MVC   UBMZTAX(L'CDUEAMT),CDUEAMT                                       
         LA    R1,UBMZTAX+L'UBMZTAX-1                                           
LINT10M6 CLI   0(R1),C' '                                                       
         BH    LINT10MA                                                         
         BCT   R1,LINT10M6         SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
LINT10MA MVC   1(10,R1),=C'</TAX-HST>'                                          
*                                                                               
***      EDIT  TOTDUE,CDUEAMT,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                 
         EDIT  DUEAMT,CDUEAMT,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                 
*                                                                               
         MVI   UBMZDUE-1,X'FF'    TO PREVENT GOING PAST START                   
         MVC   UBMZDUE,SPACES                                                   
         MVC   UBMZDUE(L'CDUEAMT),CDUEAMT                                       
         LA    R1,UBMZDUE+L'UBMZDUE-1                                           
LINT10M8 CLI   0(R1),C' '                                                       
         BH    LINT10MB                                                         
         BCT   R1,LINT10M8         SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
LINT10MB MVC   1(12,R1),=C'</AmountDue>'                                        
*                                                                               
*        REQNUMBER FROM UCOMM E1                                                
*                                                                               
         MVI   UBMZE1-1,X'FF'     TO PREVENT GOING PAST START                   
         MVC   UBMZE1,SPACES                                                    
         MVC   UBMZE1(L'MDZUE1),MDZUE1                                          
         LA    R1,UBMZE1+L'UBMZE1-1                                             
LINT10MC CLI   0(R1),C' '                                                       
         BH    LINT10MD                                                         
         BCT   R1,LINT10MC         SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
LINT10MD MVC   1(12,R1),=C'</ReqNumber>'                                        
*                                                                               
*        EBCANUMBER FROM UCOMM E2                                               
*                                                                               
         MVI   UBMZE2-1,X'FF'     TO PREVENT GOING PAST START                   
         MVC   UBMZE2,SPACES                                                    
         MVC   UBMZE2(L'MDZUE2),MDZUE2                                          
         LA    R1,UBMZE2+L'UBMZE2-1                                             
LINT10ME CLI   0(R1),C' '                                                       
         BH    LINT10MF                                                         
         BCT   R1,LINT10ME         SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
LINT10MF MVC   1(13,R1),=C'</eBCANumber>'                                       
         B     LINT12A                                                          
*                                                                               
LINT11   DS    0H            THIS CODE FOR CHEVRON                              
         EDIT  DUEAMT,CDUEAMT,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                 
*                                                                               
         MVC   CHVDDUE,SPACES                                                   
         MVC   CHVDDUE(L'CDUEAMT),CDUEAMT                                       
         LA    R1,CHVDDUE+L'CHVDDUE-1                                           
LINT11D  CLI   0(R1),C' '                                                       
         BH    LINT11E                                                          
         BCT   R1,LINT11D          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
LINT11E  MVC   1(22,R1),=C'</pidx:MonetaryAmount>'                              
         B     LINT12A                                                          
*                                                                               
LINT11W  OC    WBFTOT,WBFTOT   ANYTHING THERE YET?                              
         BNZ   *+10                                                             
         ZAP   WBFTOT,PZERO                                                     
         AP    WBFTOT,DUEAMT   FOR WB FILE TOTALS                               
         B     LINT12A                                                          
*                                                                               
LINT12   DS    0H                                                               
         CLC   CLI,=C'WBF'        GROSS FORMULA FOR WBF?                        
         BNE   *+6                                                              
         DC    H'0'               DUMP - MUST BE NET                            
*                                                                               
LINT12A  DS    0H                                                               
         EDIT  DUEAMT,CDUENET,0,ALIGN=LEFT,ZERO=NOBLANK                         
*                                                                               
         MVC   ORCDNET,SPACES                                                   
         MVC   ORCDNET(L'CDUENET),CDUENET                                       
         LA    R1,ORCDNET+L'ORCDNET-1                                           
LINT12B  CLI   0(R1),C' '                                                       
         BH    LINT12C                                                          
         BCT   R1,LINT12B          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
LINT12C  MVC   1(8,R1),=C'</VALUE>'                                             
*                                                                               
***T7    UNPK  CDUENET,DUEAMT                                                   
*                                                                               
         MVI   INVDRCR,C'D'                                                     
         MVI   INVSIGN,C'+'                                                     
         CP    DUEAMT,PZERO                                                     
         BNL   *+12                                                             
         MVI   INVDRCR,C'C'                                                     
         MVI   INVSIGN,C'-'                                                     
*                                                                               
***      LA    R1,CDUENET+L'CDUENET-1                                           
***      GOTOR FIXNEG                                                           
*                                                                               
         EDIT  COMAMT,CDUECOM,0,ALIGN=LEFT,ZERO=NOBLANK                         
*                                                                               
         MVC   ORCDCOM,SPACES                                                   
         MVC   ORCDCOM(L'CDUECOM),CDUECOM                                       
         LA    R1,ORCDCOM+L'ORCDCOM-1                                           
LINT12E  CLI   0(R1),C' '                                                       
         BH    LINT12F                                                          
         BCT   R1,LINT12E          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
LINT12F  MVC   1(8,R1),=C'</VALUE>'                                             
*                                                                               
*                                                                               
***      UNPK  CDUECOM,COMAMT                                                   
***      OI    CDUECOM+L'CDUECOM-1,X'F0'     MAKE POSTIVE                       
*                                                                               
         MVI   COMSIGN,C'+'                                                     
         MVI   COMDRCR,C'D'                                                     
         CP    COMAMT,PZERO                                                     
         BNL   *+12                                                             
         MVI   COMDRCR,C'C'                                                     
         MVI   COMSIGN,C'-'                                                     
*                                                                               
*                                                                               
         EDIT  DUEAMT,CDUENETP,0,ALIGN=LEFT,ZERO=NOBLANK                        
*                                                                               
***      UNPK  CDUENETP,DUEAMT     MAKE A POSITIVE AMOUNT                       
***      OI    CDUENETP+L'CDUENET-1,X'F0'                                       
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
         USING LIND,R4                                                          
LINTS    ST    RE,SAVRE            ** SPOT ROUTINE **                           
LINTS3   CLI   BASIS,C'N'          BASIS = NET                                  
         BNE   LINTS5                                                           
*                                                                               
         CLI   TRADESW,C'Y'        CHEVRON SPOT TRADE BILLING?                  
         BNE   *+10                                                             
         ZAP   LINNET,LINGRS       SET LINNET TO LINGRS                         
*                                  SO I CAN CALCULATE COMMISSION                
*                                                                               
         CP    COMAMT,PZERO                                                     
         BE    LINTS7                                                           
*                                                                               
         CLI   TRADESW,C'Y'        CHEVRON SPOT TRADE BILLING?                  
         BNE   LINTS3X             DON'T CHECK BASAMT - IT WILL BE 0            
*                                                                               
*   NET DOWN VALUE IN LINNET (NOW GROSS) AND SAVE RESULT IN LINCOM              
*                                                                               
         ZAP   PL16,LINNET                                                      
         MP    PL16,=P'8500'                                                    
         DP    PL16,=P'10000'                                                   
         CP    PL16+13(3),=P'5000' REMAINDER                                    
         BL    LINTS3D                                                          
         AP    PL16(13),=P'1'      ROUND UP                                     
         B     LINTS3W                                                          
*                                                                               
LINTS3D  CP    PL16+13(3),=P'-5000' REMAINDER                                   
         BH    LINTS3W                                                          
         SP    PL16(13),=P'1'      ROUND DOWN                                   
*                                                                               
LINTS3W  ZAP   LINCOM,PL16+5(8)                                                 
         B     LINTS7                                                           
*                                                                               
                                                                                
LINTS3X  CP    BASAMT,PZERO                                                     
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
*                                                                               
         CLI   TRADESW,C'Y'        CHEVRON SPOT TRADE BILLING?                  
         BNE   LINTS8                                                           
         ZAP   LINDUE,LINCOM       SET DUE TO COMMISSION                        
         ZAP   LINNET,LINCOM       SET NET TO COMMISSION                        
*                                                                               
LINTS8   AP    CALCDUE,LINDUE      ADD TO CALCULATED DUE                        
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
*                                                                               
         CLI   BASIS,C'2'          NET-CD?                                      
         BNE   *+10                                                             
         AP    COMAMT,TOTCSD       ADD IT BACK IN                               
*                                                                               
*  DON'T WORRY ABOUT LINITEMS FOR NOW - WBF DOESN'T REPORT THEM                 
*                                                                               
LINTP5   CP    COMAMT,PZERO                                                     
         BE    LINTP7                                                           
         ZAP   PL16,LINNET         NET                                          
*                                                                               
         CLI   BASIS,C'2'          NET-CD?                                      
         BNE   *+10                                                             
         SP    PL16,LINCSD                                                      
*                                                                               
         AP    PL16,LINTAX                                                      
         MP    PL16,COMAMT         X COMMISSION                                 
         SRP   PL16,3,0                                                         
*                                                                               
         ZAP   DUB,TOTNET                                                       
         CLI   BASIS,C'2'          NET-CD?                                      
         BNE   *+10                                                             
         SP    DUB,TOTCSD          TAKE IT OUT                                  
*                                                                               
*******  DP    PL16,TOTNET         DIVIDE BY BASIS                              
         ZAP   LINCOM,PZERO                                                     
         CP    DUB,PZERO           AVOID DIVIDE BY ZERO                         
         BE    LINTP7                                                           
*                                                                               
         DP    PL16,DUB            DIVIDE BY BASIS                              
         SRP   PL16(L'PL16-L'TOTNET),64-3,5   % ROUNDED TO 2DP                  
         ZAP   LINCOM,PL16(L'PL16-L'TOTNET)   SAVE COMMISSION                   
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
* GENERATE XML RECORDS FROM LINE ITEM                               *           
*  NTRY R3=A(XML MAP RECORD)                                        *           
*********************************************************************           
         USING MAPD,R3                                                          
PUTR     NTR1  ,                                                                
         XC    RTNADR,RTNADR        INITIALIZE RETURN ADDRESS                   
         XC    PREVADR,PREVADR      INITIALIZE RETURN ADDRESS                   
*                                                                               
         MVI   CHVFLAGS,0          INIT CHEVRON FLAGS                           
         CLC   AGYALPHA,=C'H7'     GROUPM?                                      
         BNE   PUTR05              NO                                           
         CLC   CLI,=C'CHV'         CHEVRON?                                     
         BNE   PUTR05              NO                                           
         OI    CHVFLAGS,CHVACTV    PROCESSING CHEVRON FOR H7                    
*                                                                               
PUTR05   TM    FCS,FCSEDI          TEST XML FILE OPEN                           
         BO    PUTR10                                                           
*                                                                               
*        IF FILEOPT IS P - DIFFERENT CALL                                       
*                                                                               
         CLI   FILEOPT,C'P'                                                     
         BNE   PUTR7                                                            
*                                                                               
         GOTO1 DYNALLOC,DMCB,(0,=C'XMLMAP  '),MAPNME                            
         OPEN  (XMLMAP,(OUTPUT))                                                
         B     PUTR7X                                                           
*                                                                               
PUTR7    MVI   BYTE,X'45'         X'04' = BIG NAMES                             
         MVC   DUB,=X'000005000001'                                             
         GOTO1 DYNALLOC,DMCB,(X'80',=C'XMLMAP  '),(BYTE,DUB),          X        
               (X'80',MQMAPNM)                                                  
*****    GOTO1 DYNALLOC,DMCB,(0,=C'XMLMAP  '),MQMAPNM                           
         OPEN  (XMLMAP,(OUTPUT))                                                
*                                                                               
PUTR7X   LTR   RF,RF                                                            
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
PUTR15   L     R2,AMAP                                                          
         XC    0(4,R2),0(R2)                                                    
         LA    R2,4(R2)            PAST RECORD LENGTH BYTES                     
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
PUTR80   CLI   0(R3),EORQ          END OF RECORD                                
         BNE   PUTR20                                                           
         L     R2,AMAP                                                          
         MVC   0(2,R2),1(R3)       SET RECORD LENGTH                            
*                                                                               
         CLC   =C'</pidx:InvoiceDetails>',4(R2) END OF INVOICE DETAILS?         
         BNE   *+8                 NO                                           
         NI    CHVFLAGS,X'FF'-CHVDTL  TURN OFF PROCESSED DETAIL FLAG            
         CLI   SKIPSET,C'Y'        TEST SKIPPING THIS RECORD SET                
         BE    PUTR90                                                           
         TM    CHVFLAGS,CHVDTL     ALREADY PROCESSED FIRST DETAIL?              
         BNZ   PUTR90              YES                                          
         PUT   XMLMAP,(R2)                                                      
*                                                                               
         TM    CHVFLAGS,CHVACTV    PROCESSING CHEVRON FOR H7?                   
         BZ    PUTR90              NO                                           
         CLC   =C'</pidx:InvoiceLineItem>',4(R2) END OF INVOICE DETAIL?         
         BNE   *+8                 NO                                           
         OI    CHVFLAGS,CHVDTL     YES - SET PROCESSED DETAIL FLAG              
         CLC   =C'<pidx:Comment>',4(R2) COMMENT XML TAG?                        
         BNE   PUTR90              NO                                           
*                                                                               
         BAS   RE,PUTRCHV          LOOP THROUGH DETAILS                         
         XC    ALINITM,ALINITM     CLEAR A(NEXT ID ITEM FOR SPOT/NET)           
         XC    AIDPITM,AIDPITM     CLEAR A(NEXT ID ITEM FOR PRINT)              
         XC    IDPCNT,IDPCNT       CLEAR NUMBER IN TABLE                        
         ZAP   ITMCNT,PZERO        CLEAR ITEM COUNT                             
*                                                                               
PUTR90   DS    0H                  SET SKIPSET TO N FOR NEXT SET                
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
         DROP  R3                                                               
         EJECT                                                                  
*********************************************************************           
* GO THROUGH DETAILS TO EXTRACT DATA FOR INVOICE PROPERTIES         *           
*  NTRY R3=A(XML MAP RECORD)                                        *           
*********************************************************************           
PUTRCHV  NTR1  ,                                                                
*                                                                               
         LAY   R3,YNCVLNM2                                                      
         USING MAPD,R3                                                          
         ST    R3,RTNADR           SAVE START OF LOOP                           
         MVC   SVRTNROU,RTNROU     SAVE RTNROU                                  
*                                                                               
PUTRC10  L     R0,AMAP             CLEAR IO TO SPACES                           
         LA    R1,L'MAP                                                         
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,AMAP                                                          
         XC    0(4,R2),0(R2)                                                    
         LA    R2,4(R2)            PAST RECORD LENGTH BYTES                     
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,3,MAPROUT        R1=ROUTINE NUMBER                            
         GOTO1 CONROUT             GO TO SPECIAL ROUTINE                        
*                                                                               
         L     R2,AMAP                                                          
*                                                                               
         LA    R1,CHVDESC2+L'CHVDESC2-1                                         
*                                                                               
         CLC   =C'</pidx:LineItemIdentifier>',0(R1)                             
         BE    *+8                                                              
         BCT   R1,*-10                                                          
*                                                                               
         LA    RF,CHVDESC2                                                      
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
*                                                                               
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R2),0(RF)                                                    
*                                                                               
         LA    R2,6(R1,R2)         SET MONETARY AMOUNT HERE                     
         MVI   0(R2),C'$'          START WITH $                                 
*                                                                               
         LA    R1,CHVLDUE+L'CHVLDUE-1                                           
*                                                                               
         CLC   =C'</pidx:MonetaryAmount>',0(R1)                                 
         BE    *+8                                                              
         BCT   R1,*-10                                                          
*                                                                               
         LA    RF,CHVLDUE                                                       
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
*                                                                               
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R2),0(RF)       MONETARY AMOUNT                              
*                                                                               
         L     R1,AMAP             START OF BUFFER                              
         LR    RF,R1               SAVE START OF BUFFER                         
         LA    R1,L'MAP-1(R1)      END OF BUFFER                                
*                                                                               
         CLI   0(R1),C' '          SPACE?                                       
         BH    *+8                 NO                                           
         BCT   R1,*-8              YES - SCAN BACKWARD FOR NON-SPACE            
*                                                                               
         SR    R1,RF               LENGTH OF ENTRY                              
         AHI   R1,1                PLUS 1                                       
         STCM  R1,3,0(RF)          SET RECORD LENGTH                            
*                                                                               
         CLI   SKIPSET,C'Y'        SKIPPING THIS RECORD SET                     
         BE    PUTRC90             YES                                          
         L     R2,AMAP             POINT TO A(BUFFER)                           
         PUT   XMLMAP,(R2)         NO - PUT RECORD TO DATASET                   
*                                                                               
PUTRC90  CLI   RTNROU,C'Y'         RETURN REQUESTED?                            
         JNE   PUTRCX              YES - DONE                                   
         L     R3,RTNADR           RESET TO BEGINNING OF TABLE                  
         B     PUTRC10             PROCESS NEXT INVOICE DETAIL                  
*                                                                               
PUTRCX   MVC   RTNROU,SVRTNROU     RESET RTNROU                                 
         J     XIT                 EXIT                                         
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
         DC    AL1(0),AL2(0,IHR-XML)                                            
IHX      DS    0X                                                               
                                                                                
*                                  PREVIOUS INVOICE                             
         DC    AL2(PIX-*),C'PI'                                                 
         DC    AL1(0),AL2(0,PINV-XML)                                           
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
         DC    AL1(L'MEDDESC),AL2(MEDDESC-LWSD,MEDESC-MED)                      
MEX      DS    0X                                                               
                                                                                
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
                                                                                
*                                  PRODUCT GROUP 1  (DIVISION FOR PRT)          
         DC    AL2(P1X-*),C'P1'                                                 
         DC    AL1(L'PCODE1),AL2(PCODE1-LWSD,P1CODE-P1D)                        
         DC    AL1(L'P1NAME),AL2(PNAME1-LWSD,P1NAME-P1D)                        
P1X      DS    0X                                                               
                                                                                
*                                  PRODUCT GROUP 2                              
         DC    AL2(P2X-*),C'P2'                                                 
         DC    AL1(L'PCODE2),AL2(PCODE2-LWSD,P2CODE-P2D)                        
         DC    AL1(L'P2NAME),AL2(PNAME2-LWSD,P2NAME-P2D)                        
P2X      DS    0X                                                               
                                                                                
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
         DC    AL1(0),AL2(0,IDRPP-XML)                                          
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
         DC    AL1(0),AL2(0,MTRSS-XML)                                          
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(1+CNDSQ),AL2(SYSTM-LWSD),C'S'                                
         DC    AL1(3+CONDQ),AL2(MTDLVN-MTD),CL3'INV'                            
         DC    AL1(0),AL2(0,MTRSI-XML)                                          
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(1+CNDSQ),AL2(SYSTM-LWSD),C'P'                                
         DC    AL1(3+CONDQ),AL2(MTDLVN-MTD),CL3'PUB'                            
         DC    AL1(0),AL2(0,MTRPP-XML)                                          
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(1+CNDSQ),AL2(SYSTM-LWSD),C'P'                                
         DC    AL1(3+CONDQ),AL2(MTDLVN-MTD),CL3'INV'                            
         DC    AL1(0),AL2(0,MTRPI-XML)                                          
         DC    AL1(CONDQ)                                                       
MTX      DS    0X                                                               
                                                                                
*                                  MONTHLY TOTAL RECORD CANADA                  
         DC    AL2(NTX-*),C'NT'                                                 
         DC    AL1(1+CNDSQ),AL2(SYSTM-LWSD),C'S'                                
         DC    AL1(0),AL2(0,MTRSS-XML)                                          
         DC    AL1(CONDQ)                                                       
NTX      DS    0X                                                               
                                                                                
*                                  CANADIAN TAX RECORD                          
         DC    AL2(CTX-*),C'CT'                                                 
         DC    AL1(3+CONDQ),AL2(CTLVLN-CTD),CL3'GST'                            
         DC    AL1(0),AL2(0,CTGST-XML)                                          
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(3+CONDQ),AL2(CTLVLN-CTD),CL3'TPS'                            
         DC    AL1(0),AL2(0,CTGST-XML)                                          
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(3+CONDQ),AL2(CTLVLN-CTD),CL3'HST'                            
         DC    AL1(0),AL2(0,CTHST-XML)                                          
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(3+CONDQ),AL2(CTLVLN-CTD),CL3'PST'                            
         DC    AL1(0),AL2(0,CTPST-XML)                                          
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(3+CONDQ),AL2(CTLVLN-CTD),CL3'QST'                            
         DC    AL1(0),AL2(0,CTQST-XML)                                          
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(3+CONDQ),AL2(CTLVLN-CTD),CL3'TVQ'                            
         DC    AL1(0),AL2(0,CTQST-XML)                                          
         DC    AL1(CONDQ)                                                       
CTX      DS    0X                                                               
                                                                                
*                                  TOTAL DUE RECORD                             
         DC    AL2(TDX-*),C'TD'                                                 
         DC    AL1(0),AL2(0,TDDUE-XML)                                          
         DC    AL1(CONDQ)                                                       
TDX      DS    0X                                                               
                                                                                
*                                  AMOUNT DUE RECORD                            
         DC    AL2(ADX-*),C'AD'                                                 
         DC    AL1(3+CONDQ),AL2(ADLVLN-ADD),CL3'INV'                            
         DC    AL1(0),AL2(0,ADR-XML)                                            
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
         DC    AL1(10+CONDQ),AL2(UDDES-UDD),CL10'PLANT CODE'                    
         DC    AL1(32),AL2(CHVP1-LWSD,UDTXT-UDD)                                
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(15+CONDQ),AL2(UDDES-UDD),CL15'SERVICE ORDER #'               
         DC    AL1(25),AL2(CHVE1-LWSD,UDTXT-UDD)                                
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(3+CONDQ),AL2(UDDES-UDD),CL3'PO#'                             
         DC    AL1(L'PO#),AL2(PO#-LWSD,UDTXT-UDD)                               
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(5+CONDQ),AL2(UDDES-UDD),CL5'P.O.#'                           
         DC    AL1(L'PO#),AL2(PO#-LWSD,UDTXT-UDD)                               
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(6+CONDQ),AL2(UDDES-UDD),CL6'P.O. #'                          
         DC    AL1(L'PO#),AL2(PO#-LWSD,UDTXT-UDD)                               
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
         DC    AL1(0),AL2(0,THUFRT-XML)                                         
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E2'                              
         DC    AL1(1+CNDSQ),AL2(MEDIA-LWSD),C'N'                                
         DC    AL1(3+CNDSQ),AL2(CLI-LWSD),C'003'                                
         DC    AL1(0),AL2(0,THUFRT-XML)                                         
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(4+CONDQ),AL2(UDDES-UDD),CL4'ATTN'                            
         DC    AL1(L'OMNUFDAT),AL2(OMNUFDAT-LWSD,UDTXT-UDD)                     
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'      2006                    
         DC    AL1(14+CONDQ),AL2(UDDES-UDD),CL14'INTERNAL ORDER'                
         DC    AL1(L'MCION6),AL2(MCION6-LWSD,UDTXT-UDD)                         
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E1'                              
         DC    AL1(6+CONDQ),AL2(UDDES-UDD),CL6'MAR #-'                          
         DC    AL1(L'H9NETE1),AL2(H9NETE1-LWSD,UDTXT-UDD)                       
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E2'      2005                    
         DC    AL1(14+CONDQ),AL2(UDDES-UDD),CL14'INTERNAL ORDER'                
         DC    AL1(L'MCION5),AL2(MCION5-LWSD,UDTXT-UDD)                         
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E2'                              
         DC    AL1(8+CONDQ),AL2(UDDES-UDD),CL8'G/L ACCT'                        
         DC    AL1(L'OMACCT),AL2(OMACCT-LWSD,UDTXT-UDD)                         
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UDLOC-UDD),CL2'E2'                              
         DC    AL1(5+CONDQ),AL2(UDDES-UDD),CL5'EBCA#'  eBCANumber               
         DC    AL1(L'MVEBCAN),AL2(MVEBCAN-LWSD,UDTXT-UDD)                       
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
                                                                                
         DC    AL1(2+CONDQ),AL2(UCLOC-UCD),CL2'E1'                              
         DC    AL1(09+CONDQ),AL2(UCDES-UCD),CL09'WB FLIGHT'                     
         DC    AL1(L'WBFLT),AL2(WBFLT-LWSD,UCTXT-UCD)                           
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
                                                                                
         DC    AL1(2+CONDQ),AL2(UCLOC-UCD),CL2'E1'                              
         DC    AL1(19+CONDQ),AL2(UCDES-UCD),CL19'SERV ENTRY APPROVER'           
         DC    AL1(L'CHVUCE1),AL2(CHVUCE1-LWSD,UCTXT-UCD)                       
         DC    AL1(CONDQ)                                                       
                                                                                
*        THE NEXT ONE IS FOR WB FLIGHT FOR NETPAK                               
*        IT'S NOT REALLY A UCOMM BUT IS PASSED AS SUCH                          
*                                                                               
         DC    AL1(2+CONDQ),AL2(UCLOC-UCD),CL2'WB'                              
         DC    AL1(09+CONDQ),AL2(UCDES-UCD),CL09'WB FLIGHT'                     
         DC    AL1(L'WBFLT),AL2(WBFLT-LWSD,UCTXT-UCD)                           
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UCLOC-UCD),CL2'E1'                              
         DC    AL1(15+CONDQ),AL2(UCDES-UCD),CL15'AUTHORIZATION #'               
         DC    AL1(L'MDZUE1),AL2(MDZUE1-LWSD,UCTXT-UCD)                         
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(2+CONDQ),AL2(UCLOC-UCD),CL2'E2'                              
         DC    AL1(06+CONDQ),AL2(UCDES-UCD),CL06'EBCA #'                        
         DC    AL1(L'MDZUE2),AL2(MDZUE2-LWSD,UCTXT-UCD)                         
         DC    AL1(CONDQ)                                                       
***                                                                             
* THIS TABLE ENTRY IS BECAUSE CARAT GOT THE UCOMM FIELD 2 WRONG                 
***                                                                             
         DC    AL1(2+CONDQ),AL2(UCLOC-UCD),CL2'E2'                              
         DC    AL1(05+CONDQ),AL2(UCDES-UCD),CL05'EBCA#'                         
         DC    AL1(L'MDZUE2),AL2(MDZUE2-LWSD,UCTXT-UCD)                         
         DC    AL1(CONDQ)                                                       
                                                                                
UCX      DS    0X                                                               
*                               BILLING GENERATED COMMENT RECORD                
         DC    AL2(CMX-*),C'CM'                                                 
         DC    AL1(3+CONDQ),AL2(CMLVLN-CMD),CL3'INV'                            
         DC    AL1(24+CONDQ),AL2(CMTXT-CMD),CL24'** THIS INVOICE INCLUDX        
               ES'                                                              
         DC    AL1(L'CMTXT),AL2(BILLCM-LWSD,CMTXT-CMD)                          
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(3+CONDQ),AL2(CMLVLN-CMD),CL3'PER'                            
         DC    AL1(24+CONDQ),AL2(CMTXT-CMD),CL24'** THIS INVOICE INCLUDX        
               ES'                                                              
         DC    AL1(L'CMTXT),AL2(BILLCM-LWSD,CMTXT-CMD)                          
         DC    AL1(CONDQ)                                                       
                                                                                
         DC    AL1(4+CONDQ),AL2(CMLVLN-CMD),CL4'TYPE'                           
         DC    AL1(24+CONDQ),AL2(CMTXT-CMD),CL24'** THIS INVOICE INCLUDX        
               ES'                                                              
         DC    AL1(L'CMTXT),AL2(BILLCM-LWSD,CMTXT-CMD)                          
         DC    AL1(CONDQ)                                                       
                                                                                
                                                                                
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
         DC    AL1(L'IDONET,IDONET-IDD),AL2(INORDNET-LWSD)                      
         DC    AL1(L'IDPNET,IDPNET-IDD),AL2(INPRVNET-LWSD)                      
         DC    AL1(L'IDOCDISC,IDOCDISC-IDD),AL2(INORDCSD-LWSD)                  
         DC    AL1(L'IDPCDISC,IDPCDISC-IDD),AL2(INPRVCSD-LWSD)                  
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*********************************************************************           
* AGENCY TABLE -  AGENCY/SYSTEM/CLIENT/ROUTINES/OPTIONS- SEE AGCD    *          
*********************************************************************           
AGYTAB   DS     0CL(AGCLNQ)                                                     
                                                                                
*  STARCOM/ORACLE (H9)                                                          
*                           ACTUAL EDI CLIENT CODE UNKNOWN                      
         DC     C'H9',C' ',C'024',AL2(H9ORC-XML),AL1(0),C' '  ORACLE            
*                                                                               
*  SJR/WB FILGHT (SJ)       TEST                                                
*                           ACTUAL EDI CLIENT CODE UNKNOWN                      
         DC     C'SJ',C' ',C'WBF',AL2(SJWBF-XML),AL1(0),C' '  WB FLIGHT         
*                                                                               
*  MEDIAVEST KRAFT                                                              
*                           ACTUAL EDI CLIENT CODE UNKNOWN                      
         DC     C'O0',C' ',C'KFT',AL2(MVKFT-XML),AL1(0),C'P'  KRAFT             
*                                                                               
*  CARLA MONDELEZ                                                               
*                           ACTUAL EDI CLIENT CODES MDZ AND MDN                 
         DC     C'UB',C' ',C'MDZ',AL2(UBMDZ-XML),AL1(0),C' '  MONDELEZ          
         DC     C'UB',C' ',C'MDN',AL2(UBMDZ-XML),AL1(0),C' '  MONDELEZ          
*                                                                               
* DDSB/WB FILGHT (SJ)       TEST                                                
         DC     C'*B',C' ',C'WBF',AL2(SJWBF-XML),AL1(0),C' '  WB FLIGHT         
*                                                                               
* METEST/WB FILGHT (W+)       TEST                                              
         DC     C'W+',C' ',C'WBF',AL2(MEWBF-XML),AL1(0),C' '  WB FLIGHT         
*                                                                               
* M2/WB FILGHT (M2)         MEDIACOM                                            
         DC     C'M2',C' ',C'WBF',AL2(MEWBF-XML),AL1(0),C' '  WB FLIGHT         
*                                                                               
* FR/WB FILGHT (FR)        FDMJW                                                
         DC     C'FR',C' ',C'WBF',AL2(MEWBF-XML),AL1(0),C' '  WB FLIGHT         
*                                                                               
* H7/WB FILGHT (FR)        MINDSHARE                                            
         DC     C'H7',C' ',C'WBF',AL2(MEWBF-XML),AL1(0),C' '  WB FLIGHT         
*                                                                               
**OO/WB FILGHT (OO)        OMD                                                  
****     DC     C'OO',C' ',C'WBF',AL2(MEWBF-XML),AL1(0),C' '  WB FLIGHT         
****                                                                            
* YN/CHEVRON (YN)           YOUNG & RUBICAM                                     
         DC     C'YN',C' ',C'CHV',AL2(YNCHV-XML),AL1(0),C' '  CHEVRON           
*                                                                               
* FR/CHEVRON (FR)           YOUNG & RUBICAM                                     
         DC     C'FR',C' ',C'CHV',AL2(YNCHV-XML),AL1(0),C' '  CHEVRON           
*                                                                               
* H7/CHEVRON (H7)           MINDSHARE                                           
         DC     C'H7',C' ',C'CHV',AL2(YNCHV-XML),AL1(0),C' '  CHEVRON           
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
               MACRF=(GM,PM),                                          X        
               EODAD=CLOSE                                                      
*                                                                               
XMLMAP   DCB   DDNAME=XMLMAP,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=400,                                              X        
               BLKSIZE=4000,                                           X        
               MACRF=PM                                                         
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,30,A),FORMAT=BI'                             
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=(404,,,,)'                             
*                                                                               
         DROP  RB,R8,RA                                                         
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
         TITLE 'STARCOM - ORACLE'                                               
*********************************************************************           
* STARCOM-ORACLE                                                    *           
*********************************************************************           
H9ORROU  NTR1  BASE=*,LABEL=*                                                   
         CHI   R1,BEFRQ            BEFORE CARDS                                 
         BNE   H9ORAFTR                                                         
         MVC   REGION,ZEROS                                                     
         J     XIT                                                              
*                                                                               
H9ORAFTR CHI   R1,AFTRQ            AFTER CARDS                                  
         BNE   H9ORR                                                            
         J     EIXIT                                                            
*                                                                               
H9ORR    SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     H9ORHDR                                                          
         B     H9ORINV                                                          
         B     H9ORGRS                                                          
         B     H9ORDTL                                                          
         B     H9ORIT1                                                          
         B     H9ORFEND                                                         
*                                                                               
                                                                                
H9ORHDR  DS    0H                                                               
         MVI   SKIPSET,C'N'                                                     
         MVC   AGYPROF,SPACES                                                   
         MVC   AGYPROF(13),=C'ORACLE-BLNG'                                      
         MVC   ORCINVND,SPACES                                                  
         MVC   ORCINVNR,SPACES                                                  
         MVC   ORCINVND(L'INVNUMB),INVNUMB                                      
         MVC   ORCINVNR(L'INVNUMB),INVNUMB                                      
*                                                                               
         LA    R1,ORCINVND+L'ORCINVND-1                                         
H9ORH2   CLI   0(R1),C' '                                                       
         BH    H9ORH2X                                                          
         BCT   R1,H9ORH2           SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
H9ORH2X  MVC   1(13,R1),=C'</DOCUMENTID>'                                       
*                                                                               
         LA    R1,ORCINVNR+L'ORCINVNR-1                                         
H9ORH3   CLI   0(R1),C' '                                                       
         BH    H9ORH3X                                                          
         BCT   R1,H9ORH3           SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
H9ORH3X  MVC   1(14,R1),=C'</REFERENCEID>'                                      
*                                                                               
*                                                                               
*        CODE TO STRIP OFF DASHES - NO-OPED                                     
*                                                                               
***      MVC   SVINVNUM,INVNUMB                                                 
***      MVC   INVNUMB,SPACES                                                   
***      LA    RE,SVINVNUM                                                      
***      LA    RF,INVNUMB                                                       
***      LA    R1,L'INVNUMB                                                     
***DR10  CLI   0(RE),C'-'          IS IT A DASH                                 
***      BE    *+14                SKIP IT                                      
***      MVC   0(1,RF),0(RE)       MOVE FROM SVINVNUM TO INVNUMB                
***      LA    RF,1(RF)            BUMP INVNUMB                                 
***      LA    RE,1(RE)            BUMP SAVED INVNUMB                           
***      BCT   R1,H9HDR10                                                       
*                                                                               
         CLI   HDRSENT,C'Y'        HAVE I SENT A HEADER?                        
         BE    H9ORHDR5                                                         
         MVI   HDRSENT,C'Y'                                                     
         J     XIT                                                              
*                                                                               
H9ORHDR5 MVI   SKIPSET,C'Y'        ONLY SEND ONE HEADER                         
         J     XIT                                                              
*                                                                               
H9ORINV  DS    0H                                                               
         MVI   SKIPSET,C'N'                                                     
         MVC   SVPO#,PO#                                                        
         MVC   PO#,SPACES                                                       
         LA    RE,SVPO#                                                         
         LA    RF,PO#                                                           
         LA    R1,L'PO#                                                         
H9ORI10  CLI   0(RE),C'-'          IS IT A DASH                                 
         BE    *+14                SKIP IT                                      
         MVC   0(1,RF),0(RE)       MOVE FROM SVPO# TO PO#                       
         LA    RF,1(RF)            BUMP PO#                                     
         LA    RE,1(RE)            BUMP SAVED PO#                               
         BCT   R1,H9ORI10                                                       
*                                                                               
         MVC   ORCPO#,SPACES                                                    
         MVC   ORCPO#(L'PO#),PO#                                                
*                                                                               
         LA    R1,ORCPO#+L'ORCPO#-1                                             
H9ORI2   CLI   0(R1),C' '                                                       
         BH    H9ORI2X                                                          
         BCT   R1,H9ORI2           SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
H9ORI2X  MVC   1(13,R1),=C'</DOCUMENTID>'                                       
*                                                                               
*                                  NOW BUILD DESCRIPTION LINE                   
         MVC   ORCDESC,SPACES                                                   
         MVC   ORCDESC+132(33),SPACES    TO 165                                 
         ZAP   ITMCNT,PZERO                                                     
         XC    ALINITM,ALINITM     CLEAR A(NEXT LINE ITEM)                      
         XC    AIDPITM,AIDPITM     CLEAR A(NEXT ID RECORD)                      
*                                                                               
         MVI   INVSIGN,C'+'                                                     
         MVI   INVDRCR,C'D'        SET SIGN FOR INVOICE                         
         CP    DUEAMT,PZERO                                                     
         BNL   *+12                                                             
         MVI   INVDRCR,C'C'                                                     
         MVI   INVSIGN,C'-'                                                     
*                                                                               
         MVC   ORCDESC(10),=C'FOR MEDIA:'                                       
         LA    R5,ORCDESC+10                                                    
         MVC   0(10,R5),MEDDESC                                                 
         LA    R5,10(R5)                                                        
         CLI   0(R5),C' '   FIND FIRST NON-SPACE                                
         BH    *+8                                                              
         BCT   R5,*-8                                                           
*                                                                               
         MVI   1(R5),C';'                                                       
         LA    R5,3(R5)                                                         
         MVC   0(8,R5),=C'PRODUCT:'                                             
         LA    R5,8(R5)                                                         
*                                                                               
         LA    RE,L'PRNAME1                                                     
         LA    R1,PRNAME1                                                       
H9ORI4   CLI   0(R1),C'&&'                                                      
         BNE   *+8                                                              
         MVI   0(R1),C'+'   CHANGE & TO + (XML CAN'T HANDLE &)                  
         LA    R1,1(R1)                                                         
         BCT   RE,H9ORI4                                                        
*                                                                               
         MVC   0(L'PRNAME1,R5),PRNAME1                                          
         LA    R5,L'PRNAME1(R5)                                                 
         CLI   0(R5),C' '   FIND FIRST NON-SPACE                                
         BH    *+8                                                              
         BCT   R5,*-8                                                           
*                                                                               
         MVI   1(R5),C';'                                                       
         LA    R5,3(R5)                                                         
         MVC   0(9,R5),=C'ESTIMATE:'                                            
         LA    R5,9(R5)                                                         
*                                                                               
         LA    RE,L'ESNAME1                                                     
         LA    R1,ESNAME1                                                       
H9ORI6   CLI   0(R1),C'&&'                                                      
         BNE   *+8                                                              
         MVI   0(R1),C'+'   CHANGE & TO + (XML CAN'T HANDLE &)                  
         LA    R1,1(R1)                                                         
         BCT   RE,H9ORI6                                                        
*                                                                               
         MVC   0(L'ESNAME1,R5),ESNAME1                                          
         LA    R5,L'ESNAME1(R5)                                                 
         CLI   0(R5),C' '   FIND FIRST NON-SPACE                                
         BH    *+8                                                              
         BCT   R5,*-8                                                           
*                                                                               
         MVI   1(R5),C';'                                                       
         LA    R5,3(R5)                                                         
         MVC   0(17,R5),=C'MONTH OF SERVICE:'                                   
         LA    R5,17(R5)                                                        
         ICM   RE,15,ALINTAB                                                    
         USING LIND,RE                                                          
         MVC   0(6,R5),LINMOS     USE MONTH OF FIRST LINTAB ENTRY               
         DROP  RE                                                               
         MVI   6(R5),C';'                                                       
         LA    R5,8(R5)                                                         
         MVC   0(17,R5),=C'PURCHASE ORDER #:'                                   
         LA    R5,17(R5)                                                        
         MVC   0(L'PO#,R5),PO#                                                  
         LA    R5,L'PO#(R5)                                                     
         CLI   0(R5),C' '   FIND FIRST NON-SPACE                                
         BH    *+8                                                              
         BCT   R5,*-8                                                           
*                                                                               
         MVC   1(12,R5),=C'</DESCRIPTN>'                                        
*                                                                               
H9ORINX  J     XIT                                                              
*                                                                               
*                                  GROSS HOOK                                   
*        WHY DO I NEED THIS?                                                    
*                                                                               
H9ORGRS  MVI   NEQNTY1,C'0'        SET QUANTITY OF ONE                          
         MVC   NEQNTY1+1(L'NEQNTY1-1),NEQNTY1                                   
         MVI   NEQNTY1+(L'NEQNTY1-1),C'1'                                       
*                                                                               
         EDIT  COMAMT1,CDUECOM,0,ALIGN=LEFT,ZERO=NOBLANK                        
***      UNPK  CDUECOM,COMAMT1                                                  
***      LA    R1,CDUECOM+L'CDUECOM-1                                           
***      GOTOR FIXNEG                                                           
*                                                                               
         AP    ITMCNT,=P'1'        ITEM COUNT                                   
         OI    ITMCNT+(L'ITMCNT-1),X'0F'                                        
         UNPK  CITMCNT,ITMCNT                                                   
*                                                                               
         CLI   SYSTM,C'P'                                                       
         BE    *+10                                                             
         MVC   ESNAME2,SPACES                                                   
         J     XIT                                                              
*                                                                               
*                                                                               
*                                  DETAIL LINE HOOK                             
H9ORDTL  DS    0H                                                               
         MVC   IDATSTA,SPACES                                                   
         MVI   ORCREF-1,X'FF'     TO PREVENT GOING PAST BEGINNING               
         MVC   ORCREF,SPACES                                                    
         MVI   ORCPUB-1,X'FF'     TO PREVENT GOING PAST BEGINNING               
         MVC   ORCPUB,SPACES                                                    
*                                                                               
         CLI   SYSTM,C'P'         PRINT?                                        
         BE    H9ORDTLP                                                         
*                                                                               
         ICM   R4,15,ALINITM       R4=A(NEXT LINE ITEM)                         
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
         EDIT  LINDUE,CLINDUE,0,ALIGN=LEFT,ZERO=NOBLANK                         
*                                                                               
***      UNPK  CLINDUE,LINDUE                                                   
***      OI    CLINDUE+L'CLINDUE-1,X'F0'  MAKE POSITIVE                         
*                                                                               
         MVI   LINSIGN,C'+'                                                     
         MVI   LINDRCR,C'D'                                                     
         CP    LINDUE,PZERO                                                     
         BNL   *+12                                                             
         MVI   LINDRCR,C'C'                                                     
         MVI   LINSIGN,C'-'                                                     
*                                                                               
         MVI   QNTY1,C'0'          SET QUANTITY OF ONE                          
         MVC   QNTY1+1(L'QNTY1-1),QNTY1                                         
         MVI   QNTY1+(L'QNTY1-1),C'1'                                           
         CP    LINDUE,PZERO                                                     
         BNL   *+8                                                              
         MVI   QNTY1+(L'QNTY1-1),X'98' SET TO MINUS ONE                         
         ZAP   TAXAMT,TOTTAX                                                    
*                                                                               
         MVC   ORCLDUE,SPACES                                                   
         MVC   ORCLDUE(L'CLINDUE),CLINDUE                                       
         LA    R1,ORCLDUE+L'ORCLDUE-1                                           
H9ORD3   CLI   0(R1),C' '                                                       
         BH    H9ORD5                                                           
         BCT   R1,H9ORD3           SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
H9ORD5   MVC   1(8,R1),=C'</VALUE>'                                             
*                                                                               
         MVI   RTNROU,C'N'                                                      
         LA    R4,LINLNQ(R4)                                                    
         CLI   0(R4),EOT                                                        
         JE    XIT                                                              
         MVI   RTNROU,C'Y'         SET RETURN FOR NEXT                          
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
H9ORDTLP DS    0H                  PRINT USES INSERTION TABLE                   
*                                                                               
         ICM   R4,15,AIDPITM       R4=A(NEXT LINE ITEM)                         
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
*                                                                               
         EDIT  IDPONET,CLINDUE,0,ALIGN=LEFT,ZERO=NOBLANK                        
*                                                                               
         MVC   ORCLDUE,SPACES                                                   
         MVC   ORCLDUE(L'CLINDUE),CLINDUE                                       
         LA    R1,ORCLDUE+L'ORCLDUE-1                                           
H9ORD3P  CLI   0(R1),C' '                                                       
         BH    H9ORD5P                                                          
         BCT   R1,H9ORD3P          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
H9ORD5P  MVC   1(8,R1),=C'</VALUE>'                                             
*                                                                               
         MVI   LINSIGN,C'+'                                                     
         MVI   LINDRCR,C'D'                                                     
         CP    IDPONET,PZERO                                                    
         BNL   *+12                                                             
         MVI   LINDRCR,C'C'                                                     
         MVI   LINSIGN,C'-'                                                     
*                                                                               
         MVC   ORCREF(L'IDPREF),IDPREF   REFERENCE NUMBER                       
         LA    R1,ORCREF+L'ORCREF-1                                             
H9ORD7P  CLI   0(R1),C' '                                                       
         BH    H9ORD8P                                                          
         BCT   R1,H9ORD7P          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
H9ORD8P  MVC   1(10,R1),=C'</LINENUM>'                                          
*                                                                               
*                                                                               
         MVI   RTNROU,C'N'                                                      
         LA    R4,IDPLNQ(R4)                                                    
         CLI   0(R4),EOT                                                        
         JE    XIT                                                              
         MVI   RTNROU,C'Y'         SET RETURN FOR NEXT                          
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                  STARCOM HOOK                                 
H9ORIT1  MVC   VNDNAME,SPACES                                                   
         MVC   VNDNAMC,SPACES                                                   
         MVI   ORCREF-1,X'FF'     TO PREVENT GOING PAST BEGINNING               
         MVC   ORCREF,SPACES        NONE FOR NON-PRINT?                         
         MVI   ORCPUB-1,X'FF'     TO PREVENT GOING PAST BEGINNING               
         MVC   ORCPUB,SPACES                                                    
         L     R2,ALINITM          CURRENT LINE ITEM                            
         USING LIND,R2                                                          
         CLI   SYSTM,C'S'                                                       
         BNE   H9ORIT20                                                         
         MVC   VNDNAME(L'LINSTA),LINSTA    STATION CODE                         
         MVC   VNDNAMC,VNDNAME                                                  
*                                                                               
         MVC   ORCPUB(L'LINSTA),LINSTA   STATION                                
         LA    R1,ORCPUB+L'ORCPUB-1                                             
H9ORIT2  CLI   0(R1),C' '                                                       
         BH    H9ORIT2X                                                         
         BCT   R1,H9ORIT2          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
H9ORIT2X MVC   1(12,R1),=C'</DESCRIPTN>'                                        
*                                                                               
*                                                                               
H9ORIT17 DS    0H                                                               
         EDIT  LINNET,CLINDUE,0,ALIGN=LEFT,ZERO=NOBLANK                         
*                                                                               
         B     H9ORITX                                                          
         DROP  R2                                                               
*                                                                               
H9ORIT20 DS    0H                                                               
         L     R2,AIDPITM            CURRENT INSERTION DATA                     
         USING IDPD,R2                                                          
         MVC   VNDNAME,IDPPUB             PUB NAME                              
         MVC   VNDNAMC,VNDNAME                                                  
         LA    RE,L'VNDNAMC                                                     
         LA    R1,VNDNAMC                                                       
H9ORI1P  CLI   0(R1),C'&&'                                                      
         BNE   *+8                                                              
         MVI   0(R1),C'+'  CHANGE & TO + SINCE XML CAN'T HANDLE &               
         LA    R1,1(R1)     BUMP TO NEXT POSITION                               
         BCT   RE,H9ORI1P   CHECK FOR MORE                                      
*                                                                               
         MVC   ORCPUB(L'VNDNAMC),VNDNAMC  PUB NAME AND CODE                     
         LA    R1,ORCPUB+L'ORCPUB-1                                             
H9ORI2P  CLI   0(R1),C' '                                                       
         BH    H9ORI2PX                                                         
         BCT   R1,H9ORI2P          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
H9ORI2PX MVC   1(12,R1),=C'</DESCRIPTN>'                                        
*                                                                               
*                                   ORDERED NET                                 
*                                   NOW LESS PREV. BILLED NET                   
*                                                                               
         EDIT  IDPONET,CLINDUE,0,ALIGN=LEFT,ZERO=NOBLANK                        
*                                                                               
         MVI   LINSIGN,C'+'                                                     
         MVI   LINDRCR,C'D'                                                     
         CP    IDPONET,PZERO                                                    
         BNL   *+12                                                             
         MVI   LINDRCR,C'C'                                                     
         MVI   LINSIGN,C'-'                                                     
*                                                                               
         MVC   ORCREF(L'IDPREF),IDPREF   reference number                       
         LA    R1,ORCREF+L'ORCREF-1                                             
H9ORI7P  CLI   0(R1),C' '                                                       
         BH    H9ORI8P                                                          
         BCT   R1,H9ORI7P          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
H9ORI8P  MVC   1(10,R1),=C'</LINENUM>'                                          
*                                                                               
*                                                                               
         B     H9ORITX                                                          
         DROP  R2                                                               
*                                                                               
***      UNPK  CNETAMT,LINNET                                                   
***      LA    R1,CNETAMT+L'CNETAMT-1                                           
***      GOTOR FIXNEG                                                           
H9ORITX  MVC   ORCLDUE,SPACES                                                   
         MVC   ORCLDUE(L'CLINDUE),CLINDUE                                       
         LA    R1,ORCLDUE+L'ORCLDUE-1                                           
H9ORI3P  CLI   0(R1),C' '                                                       
         BH    H9ORI5P                                                          
         BCT   R1,H9ORI3P          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
H9ORI5P  MVC   1(8,R1),=C'</VALUE>'                                             
*                                                                               
         J     XIT                                                              
*                                                                               
H9ORFEND DS    0H                                                               
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
* STARCOM ORACLE                                                                
H9ORC    DC    AL2(H9ORROU-XML)      CONTROL ROUTINE                            
         DC    AL2(H9ORHDM-XML)      FILE HEADER RECORD(S)                      
         DC    AL2(H9ORIVM-XML)      INVOICE HEADER RECORDS                     
         DC    AL2(H9ORLNM-XML)      INVOICE LINE ITEMS RECORDS                 
         DC    AL2(H9ORTOM-XML)      INVOICE TOTAL RECORDS                      
         DC    AL2(H9OREND-XML)      END OF FILE                                
         DC    AL1(EOT)                                                         
*                                                                               
H9ORHDRQ EQU   1                                                                
H9ORINVQ EQU   2                                                                
H9ORGRSQ EQU   3                                                                
H9ORDTLQ EQU   4                                                                
H9ORIT1Q EQU   5                                                                
H9ORENDQ EQU   6                                                                
         EJECT                                                                  
*********************************************************************           
* STARCOM ORACLE XML MAP TABLE(S) - SEE MAPD                        *           
*********************************************************************           
                                                                                
H9ORHDM  DS    0X                  STARCOM/ORACLE VERSION 4010                  
***      DC    AL1(ROUQ),AL2(H9ORHDRQ)                                          
***      DC    AL1(CONQ),AL2(HDRID-HDRD,6),C'000000'                            
***      DC    AL1(CONQ),AL2(HDRZRO-HDRD,5),C'00000'                            
***      DC    AL1(CONQ),AL2(HDRDDS-HDRD,3),C'DDS'                              
***      DC    AL1(LWSQ),AL2(HDRPROF-HDRD,L'AGYPROF,AGYPROF-LWSD)               
***      DC    AL1(EORQ),AL2(75)     (SAME AS OLD EDIHDRQ)                      
***      DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
         DC    AL1(ROUQ),AL2(H9ORHDRQ)                                          
         DC    AL1(CONQ),AL2(0,55),C'<?xml version="1.0" encoding="UTF-X        
               8" standalone="no" ?>'                                           
         DC    AL1(EORQ),AL2(4+55)                                              
         DC    AL1(CONQ),AL2(0,21),C'<PROCESS_INVOICE_002>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
         DC    AL1(CONQ),AL2(0,12),C'<CNTROLAREA>'                              
         DC    AL1(EORQ),AL2(4+12)                                              
         DC    AL1(CONQ),AL2(0,5),C'<BSR>'                                      
         DC    AL1(EORQ),AL2(4+5)                                               
         DC    AL1(CONQ),AL2(0,24),C'<VERB value="PROCESS" />'                  
         DC    AL1(EORQ),AL2(4+24)                                              
         DC    AL1(CONQ),AL2(0,24),C'<NOUN value="INVOICE" />'                  
         DC    AL1(EORQ),AL2(4+24)                                              
         DC    AL1(CONQ),AL2(0,24),C'<REVISION value="002" />'                  
         DC    AL1(EORQ),AL2(4+24)                                              
         DC    AL1(CONQ),AL2(0,6),C'</BSR>'                                     
         DC    AL1(EORQ),AL2(4+6)                                               
         DC    AL1(CONQ),AL2(0,8),C'<SENDER>'                                   
         DC    AL1(EORQ),AL2(4+8)                                               
         DC    AL1(CONQ),AL2(0,37),C'<LOGICALID>WWW.ORACLE.COM</LOGICALX        
               ID>'                                                             
         DC    AL1(EORQ),AL2(4+37)                                              
         DC    AL1(CONQ),AL2(0,30),C'<COMPONENT>INVOICE</COMPONENT>'            
         DC    AL1(EORQ),AL2(4+30)                                              
         DC    AL1(CONQ),AL2(0,20),C'<TASK>PROCESS</TASK>'                      
         DC    AL1(EORQ),AL2(4+20)                                              
         DC    AL1(CONQ),AL2(0,13),C'<REFERENCEID>'                             
         DC    AL1(LWSQ),AL2(13,L'ORCINVNR,ORCINVNR-LWSD)                       
         DC    AL1(EORQ),AL2(4+13+L'ORCINVNR)                                   
         DC    AL1(CONQ),AL2(0,30),C'<CONFIRMATION>0</CONFIRMATION>'            
         DC    AL1(EORQ),AL2(4+30)                                              
         DC    AL1(CONQ),AL2(0,24),C'<LANGUAGE>ENG</LANGUAGE>'                  
         DC    AL1(EORQ),AL2(4+24)                                              
         DC    AL1(CONQ),AL2(0,11),C'<CODEPAGE/>'                               
         DC    AL1(EORQ),AL2(4+11)                                              
         DC    AL1(CONQ),AL2(0,09),C'<AUTHID/>'                                 
         DC    AL1(EORQ),AL2(4+09)                                              
         DC    AL1(CONQ),AL2(0,09),C'</SENDER>'                                 
         DC    AL1(EORQ),AL2(4+09)                                              
** END OF SENDER                                                                
         DC    AL1(CONQ),AL2(0,40),C'<DATETIME qualifier="CREATION" typX        
               e="T">'                                                          
         DC    AL1(EORQ),AL2(4+40)                                              
         DC    AL1(CONQ),AL2(0,06),C'<YEAR>'                                    
         DC    AL1(LWSQ),AL2(6,04,TYYYY-LWSD)                                   
         DC    AL1(CONQ),AL2(10,07),C'</YEAR>'                                  
         DC    AL1(EORQ),AL2(4+17)                                              
         DC    AL1(CONQ),AL2(0,07),C'<MONTH>'                                   
         DC    AL1(LWSQ),AL2(7,02,TMM-LWSD)                                     
         DC    AL1(CONQ),AL2(09,08),C'</MONTH>'                                 
         DC    AL1(EORQ),AL2(4+17)                                              
         DC    AL1(CONQ),AL2(0,05),C'<DAY>'                                     
         DC    AL1(LWSQ),AL2(5,02,TDAY-LWSD)                                    
         DC    AL1(CONQ),AL2(07,06),C'</DAY>'                                   
         DC    AL1(EORQ),AL2(4+13)                                              
         DC    AL1(CONQ),AL2(0,15),C'<HOUR>12</HOUR>'                           
         DC    AL1(EORQ),AL2(4+15)                                              
         DC    AL1(CONQ),AL2(0,19),C'<MINUTE>00</MINUTE>'                       
         DC    AL1(EORQ),AL2(4+19)                                              
         DC    AL1(CONQ),AL2(0,19),C'<SECOND>00</SECOND>'                       
         DC    AL1(EORQ),AL2(4+19)                                              
         DC    AL1(CONQ),AL2(0,25),C'<SUBSECOND>00</SUBSECOND>'                 
         DC    AL1(EORQ),AL2(4+25)                                              
         DC    AL1(CONQ),AL2(0,26),C'<TIMEZONE>-0500</TIMEZONE>'                
         DC    AL1(EORQ),AL2(4+26)                                              
         DC    AL1(CONQ),AL2(0,11),C'</DATETIME>'                               
         DC    AL1(EORQ),AL2(4+11)                                              
**  END OF DATE TIME                                                            
         DC    AL1(CONQ),AL2(0,13),C'</CNTROLAREA>'                             
         DC    AL1(EORQ),AL2(4+13)                                              
**                                                                              
**  END OF CONTROL AREA                                                         
**                                                                              
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
H9ORIVM  DS    0X                                                               
         DC    AL1(ROUQ),AL2(H9ORINVQ)                                          
         DC    AL1(CONQ),AL2(0,10),C'<DATAAREA>'                                
         DC    AL1(EORQ),AL2(4+10)                                              
         DC    AL1(CONQ),AL2(0,17),C'<PROCESS_INVOICE>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
         DC    AL1(CONQ),AL2(0,11),C'<INVHEADER>'                               
         DC    AL1(EORQ),AL2(4+11)                                              
         DC    AL1(CONQ),AL2(0,38),C'<AMOUNT qualifier="DOCUMENT" type=X        
               "T">'                                                            
         DC    AL1(EORQ),AL2(4+38)                                              
         DC    AL1(CONQ),AL2(0,07),C'<VALUE>'                                   
         DC    AL1(LWSQ),AL2(07,L'ORCDNET,ORCDNET-LWSD)                         
         DC    AL1(EORQ),AL2(4+07+L'ORCDNET)                                    
         DC    AL1(CONQ),AL2(0,22),C'<NUMOFDEC>2</NUMOFDEC>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
         DC    AL1(CONQ),AL2(0,06),C'<SIGN>'                                    
         DC    AL1(LWSQ),AL2(06,L'INVSIGN,INVSIGN-LWSD)                         
         DC    AL1(CONQ),AL2(06+L'INVSIGN,07),C'</SIGN>'                        
         DC    AL1(EORQ),AL2(4+13+L'INVSIGN)                                    
         DC    AL1(CONQ),AL2(0,24),C'<CURRENCY>USD</CURRENCY>'                  
         DC    AL1(EORQ),AL2(4+24)                                              
*                                                                               
*        D OR C IF DEBIT OR CREDIT                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,06),C'<DRCR>'                                    
         DC    AL1(LWSQ),AL2(06,L'INVDRCR,INVDRCR-LWSD)                         
         DC    AL1(CONQ),AL2(06+L'INVDRCR,07),C'</DRCR>'                        
         DC    AL1(EORQ),AL2(4+13+L'INVDRCR)                                    
         DC    AL1(CONQ),AL2(0,09),C'</AMOUNT>'                                 
         DC    AL1(EORQ),AL2(4+09)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,40),C'<DATETIME qualifier="CREATION" typX        
               e="T">'                                                          
         DC    AL1(EORQ),AL2(4+40)                                              
         DC    AL1(CONQ),AL2(0,06),C'<YEAR>'                                    
         DC    AL1(LWSQ),AL2(6,04,CCYY-LWSD)                                    
         DC    AL1(CONQ),AL2(10,07),C'</YEAR>'                                  
         DC    AL1(EORQ),AL2(4+17)                                              
         DC    AL1(CONQ),AL2(0,07),C'<MONTH>'                                   
         DC    AL1(LWSQ),AL2(7,02,MM-LWSD)                                      
         DC    AL1(CONQ),AL2(09,08),C'</MONTH>'                                 
         DC    AL1(EORQ),AL2(4+17)                                              
         DC    AL1(CONQ),AL2(0,05),C'<DAY>'                                     
         DC    AL1(LWSQ),AL2(5,02,DD-LWSD)                                      
         DC    AL1(CONQ),AL2(07,06),C'</DAY>'                                   
         DC    AL1(EORQ),AL2(4+13)                                              
         DC    AL1(CONQ),AL2(0,15),C'<HOUR>12</HOUR>'                           
         DC    AL1(EORQ),AL2(4+15)                                              
         DC    AL1(CONQ),AL2(0,19),C'<MINUTE>00</MINUTE>'                       
         DC    AL1(EORQ),AL2(4+19)                                              
         DC    AL1(CONQ),AL2(0,19),C'<SECOND>00</SECOND>'                       
         DC    AL1(EORQ),AL2(4+19)                                              
         DC    AL1(CONQ),AL2(0,25),C'<SUBSECOND>00</SUBSECOND>'                 
         DC    AL1(EORQ),AL2(4+25)                                              
         DC    AL1(CONQ),AL2(0,26),C'<TIMEZONE>-0500</TIMEZONE>'                
         DC    AL1(EORQ),AL2(4+26)                                              
         DC    AL1(CONQ),AL2(0,11),C'</DATETIME>'                               
         DC    AL1(EORQ),AL2(4+11)                                              
         DC    AL1(CONQ),AL2(0,12),C'<DOCUMENTID>'                              
         DC    AL1(LWSQ),AL2(12,L'ORCINVND,ORCINVND-LWSD)                       
         DC    AL1(EORQ),AL2(4+12+L'ORCINVND)                                   
         DC    AL1(CONQ),AL2(0,11),C'<DESCRIPTN>'                               
         DC    AL1(LWSQ),AL2(11,L'ORCDESC,ORCDESC-LWSD)                         
         DC    AL1(EORQ),AL2(4+11+L'ORCDESC)                                    
         DC    AL1(CONQ),AL2(0,09),C'<PARTNER>'                                 
         DC    AL1(EORQ),AL2(4+09)                                              
         DC    AL1(CONQ),AL2(0,30),C'<NAME index="1">STARCOM</NAME>'            
         DC    AL1(EORQ),AL2(4+30)                                              
         DC    AL1(CONQ),AL2(0,10),C'</PARTNER>'                                
         DC    AL1(EORQ),AL2(4+10)                                              
         DC    AL1(CONQ),AL2(0,12),C'</INVHEADER>'                              
         DC    AL1(EORQ),AL2(4+12)                                              
*                                                                               
*        COMMISSION (BILL FORMULA ADJUSTMENT)                                   
*                                                                               
         DC    AL1(CONQ),AL2(0,11),C'<INVCHARGE>'                               
         DC    AL1(EORQ),AL2(4+11)                                              
         DC    AL1(CONQ),AL2(0,38),C'<AMOUNT qualifier="EXTENDED" type=X        
               "T">'                                                            
         DC    AL1(EORQ),AL2(4+38)                                              
         DC    AL1(CONQ),AL2(0,07),C'<VALUE>'                                   
         DC    AL1(LWSQ),AL2(07,L'ORCDCOM,ORCDCOM-LWSD)                         
         DC    AL1(EORQ),AL2(4+07+L'ORCDCOM)                                    
         DC    AL1(CONQ),AL2(0,22),C'<NUMOFDEC>2</NUMOFDEC>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
         DC    AL1(CONQ),AL2(0,06),C'<SIGN>'                                    
         DC    AL1(LWSQ),AL2(06,L'COMSIGN,COMSIGN-LWSD)                         
         DC    AL1(CONQ),AL2(06+L'COMSIGN,07),C'</SIGN>'                        
         DC    AL1(EORQ),AL2(4+13+L'COMSIGN)                                    
         DC    AL1(CONQ),AL2(0,24),C'<CURRENCY>USD</CURRENCY>'                  
         DC    AL1(EORQ),AL2(4+24)                                              
*                                                                               
*        D OR C IF DEBIT OR CREDIT                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,06),C'<DRCR>'                                    
         DC    AL1(LWSQ),AL2(06,L'COMDRCR,COMDRCR-LWSD)                         
         DC    AL1(CONQ),AL2(06+L'COMDRCR,07),C'</DRCR>'                        
         DC    AL1(EORQ),AL2(4+13+L'COMDRCR)                                    
         DC    AL1(CONQ),AL2(0,09),C'</AMOUNT>'                                 
         DC    AL1(EORQ),AL2(4+09)                                              
         DC    AL1(CONQ),AL2(0,35),C'<CHARGETYPE>COMMISSION</CHARGETYPEX        
               >'                                                               
         DC    AL1(EORQ),AL2(4+35)                                              
         DC    AL1(CONQ),AL2(0,12),C'</INVCHARGE>'                              
         DC    AL1(EORQ),AL2(4+12)                                              
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
*        PUB INFORMATION FOLLOWS - WILL BE A LOOP?                              
*                                                                               
H9ORLNM  DS    0X                    DETAIL LINE                                
         DC    AL1(BEGQ),AL2(H9ORDTLQ)    BEGIN LOOP                            
         DC    AL1(ROUQ),AL2(H9ORIT1Q)                                          
         DC    AL1(CONQ),AL2(0,09),C'<INVLINE>'                                 
         DC    AL1(EORQ),AL2(4+09)                                              
         DC    AL1(CONQ),AL2(0,38),C'<AMOUNT qualifier="EXTENDED" type=X        
               "T">'                                                            
         DC    AL1(EORQ),AL2(4+38)                                              
         DC    AL1(CONQ),AL2(0,07),C'<VALUE>'                                   
         DC    AL1(LWSQ),AL2(07,L'ORCLDUE,ORCLDUE-LWSD)                         
         DC    AL1(EORQ),AL2(4+07+L'ORCLDUE)                                    
         DC    AL1(CONQ),AL2(0,22),C'<NUMOFDEC>2</NUMOFDEC>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
         DC    AL1(CONQ),AL2(0,06),C'<SIGN>'                                    
         DC    AL1(LWSQ),AL2(06,L'LINSIGN,LINSIGN-LWSD)                         
         DC    AL1(CONQ),AL2(06+L'LINSIGN,07),C'</SIGN>'                        
         DC    AL1(EORQ),AL2(4+13+L'LINSIGN)                                    
         DC    AL1(CONQ),AL2(0,24),C'<CURRENCY>USD</CURRENCY>'                  
         DC    AL1(EORQ),AL2(4+24)                                              
*                                                                               
*        D OR C IF DEBIT OR CREDIT                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,06),C'<DRCR>'                                    
         DC    AL1(LWSQ),AL2(06,L'LINDRCR,LINDRCR-LWSD)                         
         DC    AL1(CONQ),AL2(06+L'LINDRCR,07),C'</DRCR>'                        
         DC    AL1(EORQ),AL2(4+13+L'LINDRCR)                                    
         DC    AL1(CONQ),AL2(0,09),C'</AMOUNT>'                                 
         DC    AL1(EORQ),AL2(4+09)                                              
         DC    AL1(CONQ),AL2(0,09),C'<LINENUM>'                                 
         DC    AL1(LWSQ),AL2(09,L'ORCREF,ORCREF-LWSD)                           
         DC    AL1(EORQ),AL2(4+09+L'ORCREF)                                     
*****    DC    AL1(CONQ),AL2(0,20),C'<LINENUM>1</LINENUM>'                      
*****    DC    AL1(EORQ),AL2(4+20)                                              
         DC    AL1(CONQ),AL2(0,11),C'<DESCRIPTN>'                               
         DC    AL1(LWSQ),AL2(11,L'ORCPUB,ORCPUB-LWSD)                           
         DC    AL1(EORQ),AL2(4+11+L'ORCPUB)                                     
         DC    AL1(CONQ),AL2(0,12),C'<DOCUMNTREF>'                              
         DC    AL1(EORQ),AL2(4+12)                                              
         DC    AL1(CONQ),AL2(0,32),C'<DOCTYPE>PurchaseOrder</DOCTYPE>'          
         DC    AL1(EORQ),AL2(4+32)                                              
         DC    AL1(CONQ),AL2(0,12),C'<DOCUMENTID>'                              
         DC    AL1(LWSQ),AL2(12,L'ORCPO#,ORCPO#-LWSD)                           
         DC    AL1(EORQ),AL2(4+12+L'ORCPO#)                                     
         DC    AL1(CONQ),AL2(0,12),C'<PARTNRID />'                              
         DC    AL1(EORQ),AL2(4+12)                                              
         DC    AL1(CONQ),AL2(0,14),C'<PARTNRTYPE />'                            
         DC    AL1(EORQ),AL2(4+14)                                              
         DC    AL1(CONQ),AL2(0,09),C'<LINENUM>'                                 
         DC    AL1(LWSQ),AL2(09,L'ORCREF,ORCREF-LWSD)                           
         DC    AL1(EORQ),AL2(4+09+L'ORCREF)                                     
*****    DC    AL1(CONQ),AL2(0,20),C'<LINENUM>1</LINENUM>'                      
*****    DC    AL1(EORQ),AL2(4+20)                                              
         DC    AL1(CONQ),AL2(0,14),C'<SCHLINENUM />'                            
         DC    AL1(EORQ),AL2(4+14)                                              
         DC    AL1(CONQ),AL2(0,13),C'</DOCUMNTREF>'                             
         DC    AL1(EORQ),AL2(4+13)                                              
         DC    AL1(CONQ),AL2(0,10),C'</INVLINE>'                                
         DC    AL1(EORQ),AL2(4+10)                                              
         DC    AL1(EOTQ)                                                        
         DC    AL1(ENDQ)           END OF LOOP                                  
*                                                                               
*                                                                               
H9ORTOM  DS    0X                  SENT AT END OF INVOICE                       
         DC    AL1(CONQ),AL2(0,18),C'</PROCESS_INVOICE>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
         DC    AL1(CONQ),AL2(0,11),C'</DATAAREA>'                               
         DC    AL1(EORQ),AL2(4+11)                                              
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
H9OREND  DS    0X                                                               
         DC    AL1(ROUQ),AL2(H9ORENDQ)                                          
         DC    AL1(CONQ),AL2(0,22),C'</PROCESS_INVOICE_002>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
                                                                                
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
         EJECT                                                                  
         TITLE 'SJR - WB FLIGHT'                                                
*********************************************************************           
* SJR - WB FILGHT TEST                                           *              
*********************************************************************           
SJWBROU  NTR1  BASE=*,LABEL=*                                                   
         CHI   R1,BEFRQ            BEFORE CARDS                                 
         BNE   SJWBAFTR                                                         
         MVC   WBFLT,SPACES                                                     
         MVC   BILLCM,SPACES                                                    
         J     XIT                                                              
*                                                                               
SJWBAFTR CHI   R1,AFTRQ            AFTER CARDS                                  
         BNE   SJWBR                                                            
*                                                                               
         CLC   WBFLT,SPACES      CHECK FOR WB FLIGHT PRESENT                    
         BH    SJWBA3                                                           
*                                SHOULD ONLY HAPPEN FOR NON-THEATRICAL          
         MVI   SKIPINV,C'Y'      SET TO SKIP INVOICE -                          
         MVI   SKIPEIN,EIWBFQ                                                   
         B     SJWBA5                                                           
*                                                                               
SJWBA3   MVI   FILESW,C'Y'       TELL EB THAT I PROCESSED A BILL                
*                                                                               
SJWBA5   CLC   BILLCM+25(11),=C'INTEGRATION'   CHECK BILLING COMMENT            
         BNE   *+8                                                              
         MVI   INTGONLY,C'Y'     INTEGRATION ONLY                               
         J     EIXIT                                                            
*                                                                               
SJWBR    SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
*        NOTE: THE NUMBER OF BRANCHES BELOW MUST MATCH                          
*              THE NUMBER OF ENTRIES IN SJWBF (THE TABLE                        
*              OF BRANCH ADDRESS FOR THE MAP TABLES).                           
*              THIS IS TRUE FOR ALL INTERFACES.                                 
*                                                                               
         B     SJWBHDR                                                          
         B     SJWBINV                                                          
         B     SJWBGRS                                                          
         B     SJWBDTL                                                          
         B     SJWBIT1                                                          
         B     SJWBFEND                                                         
*                                                                               
                                                                                
SJWBHDR  DS    0H                                                               
         MVI   SKIPSET,C'N'                                                     
         MVC   AGYPROF,SPACES                                                   
         MVC   AGYPROF(13),=CL13'WBF-BLNG'                                      
         CLI   HDRSENT,C'Y'        HAVE I SENT A HEADER?                        
         BE    SJWBHDR5                                                         
         MVI   HDRSENT,C'Y'                                                     
         J     XIT                                                              
*                                                                               
SJWBHDR5 MVI   SKIPSET,C'Y'        ONLY SEND ONE HEADER                         
         J     XIT                                                              
*                                                                               
SJWBINV  DS    0H                                                               
*                                                                               
SJWBINX  J     XIT                                                              
*                                                                               
*                                  GROSS HOOK                                   
*        WHY DO I NEED THIS?                                                    
*                                                                               
SJWBGRS  DS    0H                 DO NOTHING                                    
         J     XIT                                                              
*                                                                               
*                                                                               
*                                  DETAIL LINE HOOK                             
SJWBDTL  DS    0H                                                               
         J     XIT               DO NOTHING                                     
         EJECT                                                                  
*                                                                               
SJWBIT1  DS    0H                                                               
         MVI   SKIPSET,C'N'                                                     
         ZAP   ITMCNT,PZERO                                                     
         XC    ALINITM,ALINITM     CLEAR A(NEXT LINE ITEM)                      
         XC    AIDPITM,AIDPITM     CLEAR A(NEXT ID RECORD)                      
*                                                                               
**       EDIT  (P8,WBFTOT),CDUENET,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-            
**                                                                              
**       MVC   WBFDTOT,SPACES                                                   
**       MVC   WBFDTOT(L'CDUENET),CDUENET                                       
**       LA    R1,WBFDTOT+L'WBFDTOT-1                                           
**WBIT9B CLI   0(R1),C' '                                                       
**       BH    SJWBIT9C                                                         
**       BCT   R1,SJWBIT9B         SCAN BACKWARD FOR NON-SPACE                  
**                                                                              
**WBIT9C MVC   1(13,R1),=C'</BatchTotal>'                                       
*                                                                               
*                                DO NOTHING                                     
         J     XIT                                                              
*                                                                               
SJWBFEND DS    0H                                                               
         EDIT  (P8,WBFTOT),CDUENET,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-            
                                                                                
         MVC   WBFDTOT,SPACES                                                   
         MVC   WBFDTOT(L'CDUENET),CDUENET                                       
         LA    R1,WBFDTOT+L'WBFDTOT-1                                           
SJWBFE9B CLI   0(R1),C' '                                                       
         BH    SJWBFE9C                                                         
         BCT   R1,SJWBFE9B         SCAN BACKWARD FOR NON-SPACE                  
                                                                                
SJWBFE9C MVC   1(13,R1),=C'</BatchTotal>'                                       
*                                                                               
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* SJR WB FLIGHT                                                                 
SJWBF    DC    AL2(SJWBROU-XML)      CONTROL ROUTINE                            
         DC    AL2(SJWBHDM-XML)      FILE HEADER RECORD(S)                      
         DC    AL2(0)                INVOICE HEADER RECORDS                     
         DC    AL2(0)                INVOICE LINE ITEM RECORDS                  
         DC    AL2(SJWBIVM-XML)      INVOICE TOTAL RECORDS                      
         DC    AL2(SJWBEND-XML)      FILE TOTAL RECORDS                         
         DC    AL1(EOT)                                                         
*                                                                               
SJWBHDRQ EQU   1                                                                
SJWBINVQ EQU   5                                                                
SJWBENDQ EQU   6                                                                
         EJECT                                                                  
*********************************************************************           
* SJR WARNER BROS.  XML MAP TABLE(S) - SEE MAPD                                 
*********************************************************************           
                                                                                
SJWBHDM  DS    0X                  SJR/WB VERSION 4010                          
*                                                                               
         DC    AL1(ROUQ),AL2(SJWBHDRQ)                                          
**OLD**  DC    AL1(CONQ),AL2(0,56),C'<?xml version="1.0" encoding="UTF-         
**OLD**        8" stanalone="yes" ?>                                            
**OLD**  DC    AL1(EORQ),AL2(4+56)                                              
**OLD**  DC    AL1(CONQ),AL2(0,62),C'<Request xmlns:xsi="http://www.w3.         
**OLD**        org/2001/XMLSchema-instance>'                                    
**OLD**  DC    AL1(EORQ),AL2(4+62)                                              
**OLD**  DC    AL1(CONQ),AL2(0,13),C'<DataObjects>'                             
**OLD**  DC    AL1(EORQ),AL2(4+13)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,39),C'<?xml version="1.0" encoding="UTF-X        
               8" ?>'                                                           
         DC    AL1(EORQ),AL2(4+39)                                              
         DC    AL1(CONQ),AL2(0,44),C'<DataObjects xmlns="http://www.medX        
               iacom.com"'                                                      
         DC    AL1(EORQ),AL2(4+44)                                              
         DC    AL1(CONQ),AL2(0,53),C'xmlns:xsi="http://www.w3.org/2001/X        
               XMLSchema-instance"'                                             
         DC    AL1(EORQ),AL2(4+53)                                              
         DC    AL1(CONQ),AL2(0,71),C'xsi:schemaLocation="http://www.medX        
               iacom.com wb_invoice_schema_1.0.xsd">'                           
         DC    AL1(EORQ),AL2(4+71)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,11),C'<Timestamp>'                               
         DC    AL1(LWSQ),AL2(11,10,WBTODAY-LWSD)                                
         DC    AL1(LWSQ),AL2(22,8,TIMEOFD-LWSD)                                 
         DC    AL1(CONQ),AL2(30,12),C'</Timestamp>'                             
         DC    AL1(EORQ),AL2(4+11+10+1+8+12)                                    
**                                                                              
         DC    AL1(CONQ),AL2(0,20),C'<Sender>DDS</Sender>'                      
         DC    AL1(EORQ),AL2(4+20)                                              
         DC    AL1(EOTQ,EOTQ)                                                   
**                                                                              
**  END OF HEADER DATA                                                          
**                                                                              
SJWBIVM  DS    0X                      INVOICE DATA                             
         DC    AL1(ROUQ),AL2(SJWBINVQ)                                          
         DC    AL1(CONQ),AL2(0,13),C'<InvoiceItem>'                             
         DC    AL1(EORQ),AL2(4+13)                                              
         DC    AL1(CONQ),AL2(0,25),C'<InvoiceItemControlTotal>'                 
         DC    AL1(LWSQ),AL2(25,L'WBFDDUE,WBFDDUE-LWSD)                         
         DC    AL1(EORQ),AL2(4+25+L'WBFDDUE)                                    
         DC    AL1(CONQ),AL2(0,12),C'<WBFlightID>'                              
**TEST** DC    AL1(CONQ),AL2(12,10),C'1234567890'                               
         DC    AL1(LWSQ),AL2(12,L'WBFLT,WBFLT-LWSD)                             
         DC    AL1(CONQ),AL2(12+L'WBFLT,13),C'</WBFlightID>'                    
         DC    AL1(EORQ),AL2(4+12+L'WBFLT+13)                                   
         DC    AL1(CONQ),AL2(0,11),C'<InvoiceNo>'                               
         DC    AL1(LWSQ),AL2(11,L'INVNUMB,INVNUMB-LWSD)                         
         DC    AL1(CONQ),AL2(11+L'INVNUMB,12),C'</InvoiceNo>'                   
         DC    AL1(EORQ),AL2(4+11+L'INVNUMB+12)                                 
         DC    AL1(CONQ),AL2(0,09),C'<DueDate>'                                 
         DC    AL1(LWSQ),AL2(09,L'DUEDMDY,DUEDMDY-LWSD)                         
         DC    AL1(CONQ),AL2(09+L'DUEDMDY,10),C'</DueDate>'                     
         DC    AL1(EORQ),AL2(4+09+L'DUEDMDY+10)                                 
         DC    AL1(CONQ),AL2(0,11),C'<NetAmount>'                               
         DC    AL1(LWSQ),AL2(11,L'WBFDNET,WBFDNET-LWSD)                         
         DC    AL1(EORQ),AL2(4+11+L'WBFDNET)                                    
         DC    AL1(CONQ),AL2(0,18),C'<CommissionAmount>'                        
         DC    AL1(LWSQ),AL2(18,L'WBFDCOM,WBFDCOM-LWSD)                         
         DC    AL1(EORQ),AL2(4+18+L'WBFDCOM)                                    
         DC    AL1(CONQ),AL2(0,19),C'<IntegrationAmount>'                       
         DC    AL1(LWSQ),AL2(19,L'WBFDITG,WBFDITG-LWSD)                         
         DC    AL1(EORQ),AL2(4+19+L'WBFDITG)                                    
**OLD    DC    AL1(CONQ),AL2(0,40),C'<IntegrationAmount>0</IntegrationA         
**OLD          mount>'                                                          
**OLD    DC    AL1(EORQ),AL2(4+40)                                              
         DC    AL1(CONQ),AL2(0,08),C'<Client>'                                  
         DC    AL1(LWSQ),AL2(08,L'ADVCODE,ADVCODE-LWSD)                         
         DC    AL1(CONQ),AL2(08+L'ADVCODE,09),C'</Client>'                      
         DC    AL1(EORQ),AL2(4+08+L'ADVCODE+09)                                 
         DC    AL1(CONQ),AL2(0,09),C'<Product>'                                 
         DC    AL1(LWSQ),AL2(09,L'PRCODE1,PRCODE1-LWSD)                         
         DC    AL1(CONQ),AL2(09+L'PRCODE1,10),C'</Product>'                     
         DC    AL1(EORQ),AL2(4+09+L'PRCODE1+10)                                 
         DC    AL1(CONQ),AL2(0,07),C'<Media>'                                   
         DC    AL1(LWSQ),AL2(07,L'MEDIA,MEDIA-LWSD)                             
         DC    AL1(CONQ),AL2(07+L'MEDIA,08),C'</Media>'                         
         DC    AL1(EORQ),AL2(4+07+L'MEDIA+08)                                   
*                                                                               
         DC    AL1(CONQ),AL2(0,14),C'</InvoiceItem>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
**                                                                              
**  END OF INVOICE DATA                                                         
**                                                                              
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
SJWBEND  DS    0X                                                               
         DC    AL1(ROUQ),AL2(SJWBENDQ)                                          
         DC    AL1(CONQ),AL2(0,12),C'<BatchTotal>'                              
         DC    AL1(LWSQ),AL2(12,L'WBFDTOT,WBFDTOT-LWSD)                         
         DC    AL1(EORQ),AL2(4+12+L'WBFDTOT)                                    
         DC    AL1(CONQ),AL2(0,14),C'</DataObjects>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
                                                                                
***OLD** DC    AL1(CONQ),AL2(0,10),C'</Request>'                                
***OLD** DC    AL1(EORQ),AL2(4+10)                                              
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
         EJECT                                                                  
         TITLE 'KRAFT - MEDIAVEST TORONTO  (O0)'                                
*********************************************************************           
* KRAFT                                                          *              
*********************************************************************           
MVKRROU  NTR1  BASE=*,LABEL=*                                                   
         CHI   R1,BEFRQ            BEFORE CARDS                                 
         BNE   MVKRAFTR                                                         
         MVC   MVKESTN,SPACES                                                   
         J     XIT                                                              
*                                                                               
MVKRAFTR CHI   R1,AFTRQ            AFTER CARDS                                  
         BNE   MVKRR                                                            
*                                                                               
MVKRA3   MVI   FILESW,C'Y'       TELL EB THAT I PROCESSED A BILL                
*                                                                               
MVKRA5   DS    0H                                                               
         MVI   MVKESTN-1,X'FF'     JUST IN CASE NO DATA                         
         LA    R1,MVKESTN                                                       
         ST    R1,SAVER1                                                        
*                                                                               
         LA    RE,L'ESNAME1                                                     
         LA    R1,ESNAME1                                                       
MVKVAFT2 CLI   0(R1),C'&&'                                                      
         BNE   *+8                                                              
         MVI   0(R1),C'+'   CHANGE & TO + (XML CAN'T HANDLE &)                  
         LA    R1,1(R1)                                                         
         BCT   RE,MVKVAFT2                                                      
*                                                                               
         L     R1,SAVER1                                                        
         MVC   0(L'ESNAME1,R1),ESNAME1                                          
*                                                                               
         CLI   SYSTM,C'P'     PRINT MAY HAVE A SECOND ESTIMATE NAME             
         BNE   MVKVAFT6                                                         
*                                                                               
         LA    RE,L'ESNAME2                                                     
         LA    R1,ESNAME2                                                       
MVKVAFT3 CLI   0(R1),C'&&'                                                      
         BNE   *+8                                                              
         MVI   0(R1),C'+'   CHANGE & TO + (XML CAN'T HANDLE &)                  
         LA    R1,1(R1)                                                         
         BCT   RE,MVKVAFT3                                                      
*                                                                               
         LA    R1,MVKESTN+30                                                    
         CLI   0(R1),C' '     SCAN BACKWARD FOR FIRST NON-SPACE                 
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVC   2(L'ESNAME2,R1),ESNAME2                                          
*                                                                               
MVKVAFT6 DS    0H            NOW APPEND THE ENDING PIDX                         
*                                                                               
         LA    R1,MVKESTN+L'MVKESTN-1                                           
         CLI   0(R1),C' '     SCAN BACKWARD FOR FIRST NON-SPACE                 
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVC   1(10,R1),=C'</EstName>'                                          
*                                                                               
                                                                                
         J     EIXIT                                                            
*                                                                               
MVKRR    SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
*        NOTE: THE NUMBER OF BRANCHES BELOW MUST MATCH                          
*              THE NUMBER OF ENTRIES IN MVKFT (THE TABLE                        
*              OF BRANCH ADDRESS FOR THE MAP TABLES).                           
*              THIS IS TRUE FOR ALL INTERFACES.                                 
*                                                                               
         B     MVKRHDR                                                          
         B     MVKRINV                                                          
         B     MVKRGRS                                                          
         B     MVKRDTL                                                          
         B     MVKRIT1                                                          
         B     MVKFTEND                                                         
*                                                                               
                                                                                
MVKRHDR  DS    0H                                                               
         MVI   SKIPSET,C'N'                                                     
         MVC   AGYPROF,SPACES                                                   
         MVC   AGYPROF(13),=CL13'KRAFT BLNG'                                    
         CLI   HDRSENT,C'Y'        HAVE I SENT A HEADER?                        
         BE    MVKRHDR5                                                         
         MVI   HDRSENT,C'Y'                                                     
         J     XIT                                                              
*                                                                               
MVKRHDR5 MVI   SKIPSET,C'Y'        ONLY SEND ONE HEADER                         
         J     XIT                                                              
*                                                                               
MVKRINV  DS    0H                                                               
*                                                                               
MVKRINX  J     XIT                                                              
*                                                                               
*                                  GROSS HOOK                                   
*        WHY DO I NEED THIS?                                                    
*                                                                               
MVKRGRS  DS    0H                 DO NOTHING                                    
         J     XIT                                                              
*                                                                               
*                                                                               
*                                  DETAIL LINE HOOK                             
MVKRDTL  DS    0H                                                               
         J     XIT               DO NOTHING                                     
         EJECT                                                                  
*                                                                               
MVKRIT1  DS    0H                                                               
         MVI   SKIPSET,C'N'                                                     
         ZAP   ITMCNT,PZERO                                                     
         XC    ALINITM,ALINITM     CLEAR A(NEXT LINE ITEM)                      
         XC    AIDPITM,AIDPITM     CLEAR A(NEXT ID RECORD)                      
*                                DO NOTHING                                     
         J     XIT                                                              
*                                                                               
MVKFTEND DS    0H                DO NOTHING                                     
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* SJR WB FLIGHT                                                                 
MVKFT    DC    AL2(MVKRROU-XML)      CONTROL ROUTINE                            
         DC    AL2(MVKRHDM-XML)      FILE HEADER RECORD(S)                      
         DC    AL2(0)                INVOICE HEADER RECORDS                     
         DC    AL2(0)                INVOICE LINE ITEM RECORDS                  
         DC    AL2(MVKRIVM-XML)      INVOICE TOTAL RECORDS                      
         DC    AL2(MVKREND-XML)      FILE TOTAL RECORDS                         
         DC    AL1(EOT)                                                         
*                                                                               
MVKRHDRQ EQU   1                                                                
MVKRINVQ EQU   5                                                                
MVKRENDQ EQU   6                                                                
         EJECT                                                                  
*********************************************************************           
* MEDIAVEST KRAFT  XML MAP TABLE(S) - SEE MAPD                                  
*********************************************************************           
                                                                                
MVKRHDM  DS    0X                  VERSION 4010                                 
*                                                                               
         DC    AL1(ROUQ),AL2(MVKRHDRQ)                                          
         DC    AL1(CONQ),AL2(0,39),C'<?xml version="1.0" encoding="UTF-X        
               8" ?>'                                                           
         DC    AL1(EORQ),AL2(4+39)                                              
         DC    AL1(CONQ),AL2(0,15),C'<MediaBillings>'                           
         DC    AL1(EORQ),AL2(4+15)                                              
         DC    AL1(EOTQ,EOTQ)                                                   
**                                                                              
**  END OF HEADER DATA                                                          
**                                                                              
MVKRIVM  DS    0X                      INVOICE DATA                             
         DC    AL1(ROUQ),AL2(MVKRINVQ)                                          
         DC    AL1(CONQ),AL2(0,14),C'<MediaBilling>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
*                                                                               
*        Media here is my suggestion                                            
*        turns out they don't want it                                           
*                                                                               
****     DC    AL1(CONQ),AL2(0,07),C'<Media>'                                   
****     DC    AL1(LWSQ),AL2(07,L'MEDIA,MEDIA-LWSD)                             
****     DC    AL1(CONQ),AL2(07+L'MEDIA,08),C'</Media>'                         
****     DC    AL1(EORQ),AL2(4+07+L'MEDIA+08)                                   
*                                                                               
         DC    AL1(CONQ),AL2(0,08),C'<Client>'                                  
         DC    AL1(LWSQ),AL2(08,L'ADVCODE,ADVCODE-LWSD)                         
         DC    AL1(CONQ),AL2(08+L'ADVCODE,09),C'</Client>'                      
         DC    AL1(EORQ),AL2(4+08+L'ADVCODE+09)                                 
         DC    AL1(CONQ),AL2(0,09),C'<Product>'                                 
         DC    AL1(LWSQ),AL2(09,L'PRCODE1,PRCODE1-LWSD)                         
         DC    AL1(CONQ),AL2(09+L'PRCODE1,10),C'</Product>'                     
         DC    AL1(EORQ),AL2(4+09+L'PRCODE1+10)                                 
*                                                                               
         DC    AL1(CONQ),AL2(0,10),C'<Estimate>'                                
         DC    AL1(LWSQ),AL2(10,L'ESCODE1,ESCODE1-LWSD)                         
         DC    AL1(CONQ),AL2(10+L'ESCODE1,11),C'</Estimate>'                    
         DC    AL1(EORQ),AL2(4+10+L'ESCODE1+11)                                 
*                                                                               
         DC    AL1(CONQ),AL2(0,09),C'<EstName>'                                 
         DC    AL1(LWSQ),AL2(09,L'MVKESTN,MVKESTN-LWSD)                         
         DC    AL1(EORQ),AL2(4+09+L'MVKESTN)                                    
*                                                                               
*        ESTIMATE NAME HERE                                                     
*                                                                               
         DC    AL1(CONQ),AL2(0,09),C'<Invoice>'                                 
         DC    AL1(LWSQ),AL2(09,L'INVNUMB,INVNUMB-LWSD)                         
         DC    AL1(CONQ),AL2(09+L'INVNUMB,10),C'</Invoice>'                     
         DC    AL1(EORQ),AL2(4+09+L'INVNUMB+10)                                 
         DC    AL1(CONQ),AL2(0,09),C'<InvDate>'                                 
         DC    AL1(LWSQ),AL2(09,2,MM-LWSD)                                      
         DC    AL1(CONQ),AL2(11,1),C'/'                                         
         DC    AL1(LWSQ),AL2(12,2,DD-LWSD)                                      
         DC    AL1(CONQ),AL2(14,1),C'/'                                         
         DC    AL1(LWSQ),AL2(15,4,CCYY-LWSD)                                    
         DC    AL1(CONQ),AL2(19,10),C'</InvDate>'                               
         DC    AL1(EORQ),AL2(4+09+2+1+2+1+4+10)                                 
*                                                                               
         DC    AL1(CONQ),AL2(0,11),C'<InvAmount>'                               
         DC    AL1(LWSQ),AL2(11,L'MVKDDUE,MVKDDUE-LWSD)                         
         DC    AL1(EORQ),AL2(4+11+L'MVKDDUE)                                    
*                                                                               
         DC    AL1(CONQ),AL2(0,09),C'<TAX-HST>'                                 
         DC    AL1(LWSQ),AL2(09,L'MVKTTAX,MVKTTAX-LWSD)                         
         DC    AL1(EORQ),AL2(4+09+L'MVKTTAX)                                    
*                                                                               
         DC    AL1(CONQ),AL2(0,11),C'<AmountDue>'                               
         DC    AL1(LWSQ),AL2(11,L'MVKTDUE,MVKTDUE-LWSD)                         
         DC    AL1(EORQ),AL2(4+11+L'MVKTDUE)                                    
*                                                                               
*        SEND AN EMPTY ReqNumber FIELD                                          
*        REQUESTED BY HEATHER GIBSON APR17/2012                                 
*                                                                               
         DC    AL1(CONQ),AL2(0,23),C'<ReqNumber></ReqNumber>'                   
         DC    AL1(EORQ),AL2(4+23)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,12),C'<eBCANumber>'                              
         DC    AL1(LWSQ),AL2(12,L'MVKTE2,MVKTE2-LWSD)                           
         DC    AL1(EORQ),AL2(4+12+L'MVKTE2)                                     
                                                                                
         DC    AL1(CONQ),AL2(0,10),C'<Currency>'                                
         DC    AL1(CONQ),AL2(10,3),C'CAD'                                       
         DC    AL1(CONQ),AL2(13,11),C'</Currency>'                              
         DC    AL1(EORQ),AL2(4+10+3+11)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,15),C'</MediaBilling>'                           
         DC    AL1(EORQ),AL2(4+15)                                              
**                                                                              
**  END OF INVOICE DATA                                                         
**                                                                              
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
MVKREND  DS    0X                                                               
         DC    AL1(ROUQ),AL2(MVKRENDQ)                                          
         DC    AL1(CONQ),AL2(0,16),C'</MediaBillings>'                          
         DC    AL1(EORQ),AL2(4+16)                                              
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
         EJECT                                                                  
*                                                                               
         TITLE 'MONDELEZ - CARAT (UB)'                                          
***********************************************************************         
*                              MONDELEZ                               *         
***********************************************************************         
UBMZROU  NTR1  BASE=*,LABEL=*                                                   
         CHI   R1,BEFRQ            BEFORE CARDS                                 
         BNE   UBMZAFTR                                                         
         MVC   UBMZESTN,SPACES                                                  
         J     XIT                                                              
*                                                                               
UBMZAFTR CHI   R1,AFTRQ            AFTER CARDS                                  
         BNE   UBMZR                                                            
*                                                                               
UBMZA3   MVI   FILESW,C'Y'         TELL EB THAT I PROCESSED A BILL              
*                                                                               
UBMZA5   MVI   UBMZESTN-1,X'FF'    JUST IN CASE NO DATA                         
         LA    R1,UBMZESTN                                                      
         ST    R1,SAVER1                                                        
*                                                                               
         LA    RE,L'ESNAME1                                                     
         LA    R1,ESNAME1                                                       
UBMVAFT2 CLI   0(R1),C'&&'                                                      
         BNE   *+8                                                              
         MVI   0(R1),C'+'   CHANGE & TO + (XML CAN'T HANDLE &)                  
         LA    R1,1(R1)                                                         
         BCT   RE,UBMVAFT2                                                      
*                                                                               
         L     R1,SAVER1                                                        
         MVC   0(L'ESNAME1,R1),ESNAME1                                          
*                                                                               
         CLI   SYSTM,C'P'     PRINT MAY HAVE A SECOND ESTIMATE NAME             
         BNE   UBMVAFT6                                                         
*                                                                               
         LA    RE,L'ESNAME2                                                     
         LA    R1,ESNAME2                                                       
UBMVAFT3 CLI   0(R1),C'&&'                                                      
         BNE   *+8                                                              
         MVI   0(R1),C'+'   CHANGE & TO + (XML CAN'T HANDLE &)                  
         LA    R1,1(R1)                                                         
         BCT   RE,UBMVAFT3                                                      
*                                                                               
         LA    R1,UBMZESTN+30                                                   
         CLI   0(R1),C' '     SCAN BACKWARD FOR FIRST NON-SPACE                 
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVC   2(L'ESNAME2,R1),ESNAME2                                          
*                                                                               
UBMVAFT6 DS    0H            NOW APPEND THE ENDING PIDX                         
*                                                                               
         LA    R1,MVKESTN+L'MVKESTN-1                                           
         CLI   0(R1),C' '     SCAN BACKWARD FOR FIRST NON-SPACE                 
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVC   1(10,R1),=C'</EstName>'                                          
*                                                                               
         J     EIXIT                                                            
*                                                                               
UBMZR    SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
*        NOTE: THE NUMBER OF BRANCHES BELOW MUST MATCH                          
*              THE NUMBER OF ENTRIES IN UBMDZ (THE TABLE                        
*              OF BRANCH ADDRESS FOR THE MAP TABLES).                           
*              THIS IS TRUE FOR ALL INTERFACES.                                 
*                                                                               
         B     UBMZHDR                                                          
         B     UBMZINV                                                          
         B     UBMZGRS                                                          
         B     UBMZDTL                                                          
         B     UBMZIT1                                                          
         B     UBMDZEND                                                         
*                                                                               
                                                                                
UBMZHDR  DS    0H                 HEADER ROUTINE                                
         MVI   SKIPSET,C'N'       DO NOT SKIP THIS RECORD SET                   
         MVC   AGYPROF,SPACES     INIT TO SPACES                                
         MVC   AGYPROF(13),=CL13'MONDELEZ BLNG'                                 
         CLI   HDRSENT,C'Y'       HAVE I SENT A HEADER?                         
         BE    UBMZHDR5           YES                                           
         MVI   HDRSENT,C'Y'       INDICATE HEADER HAS BEEN SENT                 
         J     XIT                EXIT                                          
*                                                                               
UBMZHDR5 MVI   SKIPSET,C'Y'       ONLY SEND ONE HEADER                          
         J     XIT                EXIT                                          
*                                                                               
UBMZINV  DS    0H                 INVOICE HOOK                                  
         J     XIT                DO NOTHING                                    
*                                                                               
UBMZGRS  DS    0H                 GROSS HOOK                                    
         J     XIT                DO NOTHING                                    
*                                                                               
UBMZDTL  DS    0H                 DETAIL LINE HOOK                              
         J     XIT                DO NOTHING                                    
*                                                                               
UBMZIT1  MVI   SKIPSET,C'N'       DO NOT SKIP THIS LINE                         
         ZAP   ITMCNT,PZERO       INIT ITMCNT                                   
         XC    ALINITM,ALINITM    CLEAR A(NEXT LINE ITEM)                       
         XC    AIDPITM,AIDPITM    CLEAR A(NEXT ID RECORD)                       
         J     XIT                EXIT                                          
*                                                                               
UBMDZEND DS    0H                 END ROUTINE                                   
         J     XIT                DO NOTHING                                    
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* Carat Mondelez                                                                
UBMDZ    DC    AL2(UBMZROU-XML)      CONTROL ROUTINE                            
         DC    AL2(UBMZHDM-XML)      FILE HEADER RECORD(S)                      
         DC    AL2(0)                INVOICE HEADER RECORDS                     
         DC    AL2(0)                INVOICE LINE ITEM RECORDS                  
         DC    AL2(UBMZIVM-XML)      INVOICE TOTAL RECORDS                      
         DC    AL2(UBMZEND-XML)      FILE TOTAL RECORDS                         
         DC    AL1(EOT)                                                         
*                                                                               
UBMZHDRQ EQU   1                                                                
UBMZINVQ EQU   5                                                                
UBMZENDQ EQU   6                                                                
                                                                                
***********************************************************************         
*            CARAT MONDELEZ  XML MAP TABLE(S) - SEE MAPD              *         
***********************************************************************         
UBMZHDM  DS    0X                  VERSION 4010                                 
*                                                                               
         DC    AL1(ROUQ),AL2(UBMZHDRQ)                                          
         DC    AL1(CONQ),AL2(0,39),C'<?xml version="1.0" encoding="UTF-X        
               8" ?>'                                                           
         DC    AL1(EORQ),AL2(4+39)                                              
         DC    AL1(CONQ),AL2(0,15),C'<MediaBillings>'                           
         DC    AL1(EORQ),AL2(4+15)                                              
         DC    AL1(EOTQ,EOTQ)                                                   
**                                                                              
**  END OF HEADER DATA                                                          
**                                                                              
UBMZIVM  DS    0X                      INVOICE DATA                             
         DC    AL1(ROUQ),AL2(UBMZINVQ)                                          
         DC    AL1(CONQ),AL2(0,14),C'<MediaBilling>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,08),C'<Client>'                                  
         DC    AL1(LWSQ),AL2(08,L'ADVCODE,ADVCODE-LWSD)                         
         DC    AL1(CONQ),AL2(08+L'ADVCODE,09),C'</Client>'                      
         DC    AL1(EORQ),AL2(4+08+L'ADVCODE+09)                                 
         DC    AL1(CONQ),AL2(0,09),C'<Product>'                                 
         DC    AL1(LWSQ),AL2(09,L'PRCODE1,PRCODE1-LWSD)                         
         DC    AL1(CONQ),AL2(09+L'PRCODE1,10),C'</Product>'                     
         DC    AL1(EORQ),AL2(4+09+L'PRCODE1+10)                                 
*                                                                               
         DC    AL1(CONQ),AL2(0,10),C'<Estimate>'                                
         DC    AL1(LWSQ),AL2(10,L'ESCODE1,ESCODE1-LWSD)                         
         DC    AL1(CONQ),AL2(10+L'ESCODE1,11),C'</Estimate>'                    
         DC    AL1(EORQ),AL2(4+10+L'ESCODE1+11)                                 
*                                                                               
         DC    AL1(CONQ),AL2(0,09),C'<EstName>'                                 
         DC    AL1(LWSQ),AL2(09,L'UBMZESTN,UBMZESTN-LWSD)                       
         DC    AL1(EORQ),AL2(4+09+L'UBMZESTN)                                   
*                                                                               
         DC    AL1(CONQ),AL2(0,09),C'<Invoice>'                                 
         DC    AL1(LWSQ),AL2(09,L'INVNUMB,INVNUMB-LWSD)                         
         DC    AL1(CONQ),AL2(09+L'INVNUMB,10),C'</Invoice>'                     
         DC    AL1(EORQ),AL2(4+09+L'INVNUMB+10)                                 
*                                                                               
         DC    AL1(CONQ),AL2(0,09),C'<InvDate>'                                 
         DC    AL1(LWSQ),AL2(09,2,MM-LWSD)                                      
         DC    AL1(CONQ),AL2(11,1),C'/'                                         
         DC    AL1(LWSQ),AL2(12,2,DD-LWSD)                                      
         DC    AL1(CONQ),AL2(14,1),C'/'                                         
         DC    AL1(LWSQ),AL2(15,4,CCYY-LWSD)                                    
         DC    AL1(CONQ),AL2(19,10),C'</InvDate>'                               
         DC    AL1(EORQ),AL2(4+09+2+1+2+1+4+10)                                 
*                                                                               
         DC    AL1(CONQ),AL2(0,11),C'<InvAmount>'                               
         DC    AL1(LWSQ),AL2(11,L'UBMZDDUE,UBMZDDUE-LWSD)                       
         DC    AL1(EORQ),AL2(4+11+L'UBMZDDUE)                                   
*                                                                               
         DC    AL1(CONQ),AL2(0,09),C'<TAX-HST>'                                 
         DC    AL1(LWSQ),AL2(09,L'UBMZTAX,UBMZTAX-LWSD)                         
         DC    AL1(EORQ),AL2(4+09+L'UBMZTAX)                                    
*                                                                               
         DC    AL1(CONQ),AL2(0,11),C'<AmountDue>'                               
         DC    AL1(LWSQ),AL2(11,L'UBMZDUE,UBMZDUE-LWSD)                         
         DC    AL1(EORQ),AL2(4+11+L'UBMZDUE)                                    
*                                                                               
         DC    AL1(CONQ),AL2(0,11),C'<ReqNumber>'                               
         DC    AL1(LWSQ),AL2(11,L'UBMZE1,UBMZE1-LWSD)                           
         DC    AL1(EORQ),AL2(4+11+L'UBMZE1)                                     
*                                                                               
         DC    AL1(CONQ),AL2(0,12),C'<eBCANumber>'                              
         DC    AL1(LWSQ),AL2(12,L'UBMZE2,UBMZE2-LWSD)                           
         DC    AL1(EORQ),AL2(4+12+L'UBMZE2)                                     
                                                                                
         DC    AL1(CONQ),AL2(0,10),C'<Currency>'                                
         DC    AL1(CONQ),AL2(10,3),C'USD'                                       
         DC    AL1(CONQ),AL2(13,11),C'</Currency>'                              
         DC    AL1(EORQ),AL2(4+10+3+11)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,15),C'</MediaBilling>'                           
         DC    AL1(EORQ),AL2(4+15)                                              
**                                                                              
**  END OF INVOICE DATA                                                         
**                                                                              
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
UBMZEND  DS    0X                                                               
         DC    AL1(ROUQ),AL2(UBMZENDQ)                                          
         DC    AL1(CONQ),AL2(0,16),C'</MediaBillings>'                          
         DC    AL1(EORQ),AL2(4+16)                                              
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
         EJECT                                                                  
*                                                                               
         TITLE 'METEST - WB FLIGHT'                                             
*********************************************************************           
* METEST - WB FILGHT TEST                                        *              
*********************************************************************           
MEWBROU  NTR1  BASE=*,LABEL=*                                                   
         CHI   R1,BEFRQ            BEFORE CARDS                                 
         BNE   MEWBAFTR                                                         
         MVC   WBFLT,SPACES                                                     
         MVC   BILLCM,SPACES                                                    
         J     XIT                                                              
*                                                                               
MEWBAFTR CHI   R1,AFTRQ            AFTER CARDS                                  
         BNE   MEWBR                                                            
*                                                                               
         CLC   WBFLT,SPACES      CHECK FOR WB FLIGHT PRESENT                    
         BH    MEWBA3                                                           
*                                SHOULD ONLY HAPPEN FOR NON-THEATRICAL          
         MVI   SKIPINV,C'Y'      SET TO SKIP INVOICE -                          
         MVI   SKIPEIN,EIWBFQ                                                   
         B     MEWBA5                                                           
*                                                                               
MEWBA3   MVI   FILESW,C'Y'                                                      
*                                                                               
MEWBA5   CLC   BILLCM+25(11),=C'INTEGRATION'   CHECK BILLING COMMENT            
         BNE   *+8                                                              
         MVI   INTGONLY,C'Y'     INTEGRATION ONLY                               
         J     EIXIT                                                            
*                                                                               
MEWBR    SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
*        NOTE: THE NUMBER OF BRANCHES BELOW MUST MATCH                          
*              THE NUMBER OF ENTRIES IN MEWBF (THE TABLE                        
*              OF BRANCH ADDRESS FOR THE MAP TABLES).                           
*              THIS IS TRUE FOR ALL INTERFACES.                                 
*                                                                               
         B     MEWBHDR                                                          
         B     MEWBINV                                                          
         B     MEWBGRS                                                          
         B     MEWBDTL                                                          
         B     MEWBIT1                                                          
         B     MEWBFEND                                                         
*                                                                               
                                                                                
MEWBHDR  DS    0H                                                               
         MVI   SKIPSET,C'N'                                                     
         MVC   AGYPROF,SPACES                                                   
         MVC   AGYPROF(13),=CL13'WBF-BLNG'                                      
         CLI   HDRSENT,C'Y'        HAVE I SENT A HEADER?                        
         BE    MEWBHDR5                                                         
         MVI   HDRSENT,C'Y'                                                     
         J     XIT                                                              
*                                                                               
MEWBHDR5 MVI   SKIPSET,C'Y'        ONLY SEND ONE HEADER                         
         J     XIT                                                              
*                                                                               
MEWBINV  DS    0H                                                               
*                                                                               
MEWBINX  J     XIT                                                              
*                                                                               
*                                  GROSS HOOK                                   
*        WHY DO I NEED THIS?                                                    
*                                                                               
MEWBGRS  DS    0H                 DO NOTHING                                    
         J     XIT                                                              
*                                                                               
*                                                                               
*                                  DETAIL LINE HOOK                             
MEWBDTL  DS    0H                                                               
         J     XIT               DO NOTHING                                     
         EJECT                                                                  
*                                                                               
MEWBIT1  DS    0H                                                               
         MVI   SKIPSET,C'N'                                                     
         ZAP   ITMCNT,PZERO                                                     
         XC    ALINITM,ALINITM     CLEAR A(NEXT LINE ITEM)                      
         XC    AIDPITM,AIDPITM     CLEAR A(NEXT ID RECORD)                      
*                                                                               
**       EDIT  (P8,WBFTOT),CDUENET,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-            
**                                                                              
**       MVC   WBFDTOT,SPACES                                                   
**       MVC   WBFDTOT(L'CDUENET),CDUENET                                       
**       LA    R1,WBFDTOT+L'WBFDTOT-1                                           
**WBIT9B CLI   0(R1),C' '                                                       
**       BH    MEWBIT9C                                                         
**       BCT   R1,MEWBIT9B         SCAN BACKWARD FOR NON-SPACE                  
**                                                                              
**WBIT9C MVC   1(13,R1),=C'</BatchTotal>'                                       
*                                                                               
*                                DO NOTHING                                     
         J     XIT                                                              
*                                                                               
MEWBFEND DS    0H                                                               
         EDIT  (P8,WBFTOT),CDUENET,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-            
                                                                                
         MVC   WBFDTOT,SPACES                                                   
         MVC   WBFDTOT(L'CDUENET),CDUENET                                       
         LA    R1,WBFDTOT+L'WBFDTOT-1                                           
MEWBFE9B CLI   0(R1),C' '                                                       
         BH    MEWBFE9C                                                         
         BCT   R1,MEWBFE9B         SCAN BACKWARD FOR NON-SPACE                  
                                                                                
MEWBFE9C MVC   1(13,R1),=C'</BatchTotal>'                                       
*                                                                               
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* MER WB FLIGHT                                                                 
MEWBF    DC    AL2(MEWBROU-XML)      CONTROL ROUTINE                            
         DC    AL2(MEWBHDM-XML)      FILE HEADER RECORD(S)                      
         DC    AL2(0)                INVOICE HEADER RECORDS                     
         DC    AL2(0)                INVOICE LINE ITEM RECORDS                  
         DC    AL2(MEWBIVM-XML)      INVOICE TOTAL RECORDS                      
         DC    AL2(MEWBEND-XML)      FILE TOTAL RECORDS                         
         DC    AL1(EOT)                                                         
*                                                                               
MEWBHDRQ EQU   1                                                                
MEWBINVQ EQU   5                                                                
MEWBENDQ EQU   6                                                                
         EJECT                                                                  
*********************************************************************           
* MER WARNER BROS.  XML MAP TABLE(S) - SEE MAPD                                 
*********************************************************************           
                                                                                
MEWBHDM  DS    0X                  METEST/WB VERSION 4010                       
*                                                                               
         DC    AL1(ROUQ),AL2(MEWBHDRQ)                                          
**OLD**  DC    AL1(CONQ),AL2(0,56),C'<?xml version="1.0" encoding="UTF-         
**OLD**        8" stanalone="yes" ?>                                            
**OLD**  DC    AL1(EORQ),AL2(4+56)                                              
**OLD**  DC    AL1(CONQ),AL2(0,62),C'<Request xmlns:xsi="http://www.w3.         
**OLD**        org/2001/XMLSchema-instance>'                                    
**OLD**  DC    AL1(EORQ),AL2(4+62)                                              
**OLD**  DC    AL1(CONQ),AL2(0,13),C'<DataObjects>'                             
**OLD**  DC    AL1(EORQ),AL2(4+13)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,39),C'<?xml version="1.0" encoding="UTF-X        
               8" ?>'                                                           
         DC    AL1(EORQ),AL2(4+39)                                              
         DC    AL1(CONQ),AL2(0,44),C'<DataObjects xmlns="http://www.medX        
               iacom.com"'                                                      
         DC    AL1(EORQ),AL2(4+44)                                              
         DC    AL1(CONQ),AL2(0,53),C'xmlns:xsi="http://www.w3.org/2001/X        
               XMLSchema-instance"'                                             
         DC    AL1(EORQ),AL2(4+53)                                              
         DC    AL1(CONQ),AL2(0,71),C'xsi:schemaLocation="http://www.medX        
               iacom.com wb_invoice_schema_1.0.xsd">'                           
         DC    AL1(EORQ),AL2(4+71)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,11),C'<Timestamp>'                               
         DC    AL1(LWSQ),AL2(11,10,WBTODAY-LWSD)                                
         DC    AL1(LWSQ),AL2(22,8,TIMEOFD-LWSD)                                 
         DC    AL1(CONQ),AL2(30,12),C'</Timestamp>'                             
         DC    AL1(EORQ),AL2(4+11+10+1+8+12)                                    
**                                                                              
         DC    AL1(CONQ),AL2(0,20),C'<Sender>DDS</Sender>'                      
         DC    AL1(EORQ),AL2(4+20)                                              
         DC    AL1(EOTQ,EOTQ)                                                   
**                                                                              
**  END OF HEADER DATA                                                          
**                                                                              
MEWBIVM  DS    0X                      INVOICE DATA                             
         DC    AL1(ROUQ),AL2(MEWBINVQ)                                          
         DC    AL1(CONQ),AL2(0,13),C'<InvoiceItem>'                             
         DC    AL1(EORQ),AL2(4+13)                                              
         DC    AL1(CONQ),AL2(0,25),C'<InvoiceItemControlTotal>'                 
         DC    AL1(LWSQ),AL2(25,L'WBFDDUE,WBFDDUE-LWSD)                         
         DC    AL1(EORQ),AL2(4+25+L'WBFDDUE)                                    
         DC    AL1(CONQ),AL2(0,12),C'<WBFlightID>'                              
**TEST** DC    AL1(CONQ),AL2(12,10),C'1234567890'                               
         DC    AL1(LWSQ),AL2(12,L'WBFLT,WBFLT-LWSD)                             
         DC    AL1(CONQ),AL2(12+L'WBFLT,13),C'</WBFlightID>'                    
         DC    AL1(EORQ),AL2(4+12+L'WBFLT+13)                                   
         DC    AL1(CONQ),AL2(0,11),C'<InvoiceNo>'                               
         DC    AL1(LWSQ),AL2(11,L'INVNUMB,INVNUMB-LWSD)                         
         DC    AL1(CONQ),AL2(11+L'INVNUMB,12),C'</InvoiceNo>'                   
         DC    AL1(EORQ),AL2(4+11+L'INVNUMB+12)                                 
         DC    AL1(CONQ),AL2(0,09),C'<DueDate>'                                 
         DC    AL1(LWSQ),AL2(09,L'DUEDMDY,DUEDMDY-LWSD)                         
         DC    AL1(CONQ),AL2(09+L'DUEDMDY,10),C'</DueDate>'                     
         DC    AL1(EORQ),AL2(4+09+L'DUEDMDY+10)                                 
         DC    AL1(CONQ),AL2(0,11),C'<NetAmount>'                               
         DC    AL1(LWSQ),AL2(11,L'WBFDNET,WBFDNET-LWSD)                         
         DC    AL1(EORQ),AL2(4+11+L'WBFDNET)                                    
         DC    AL1(CONQ),AL2(0,18),C'<CommissionAmount>'                        
         DC    AL1(LWSQ),AL2(18,L'WBFDCOM,WBFDCOM-LWSD)                         
         DC    AL1(EORQ),AL2(4+18+L'WBFDCOM)                                    
         DC    AL1(CONQ),AL2(0,19),C'<IntegrationAmount>'                       
         DC    AL1(LWSQ),AL2(19,L'WBFDITG,WBFDITG-LWSD)                         
         DC    AL1(EORQ),AL2(4+19+L'WBFDITG)                                    
**OLD    DC    AL1(CONQ),AL2(0,40),C'<IntegrationAmount>0</IntegrationA         
**OLD          mount>'                                                          
**OLD    DC    AL1(EORQ),AL2(4+40)                                              
         DC    AL1(CONQ),AL2(0,08),C'<Client>'                                  
         DC    AL1(LWSQ),AL2(08,L'ADVCODE,ADVCODE-LWSD)                         
         DC    AL1(CONQ),AL2(08+L'ADVCODE,09),C'</Client>'                      
         DC    AL1(EORQ),AL2(4+08+L'ADVCODE+09)                                 
         DC    AL1(CONQ),AL2(0,09),C'<Product>'                                 
         DC    AL1(LWSQ),AL2(09,L'PRCODE1,PRCODE1-LWSD)                         
         DC    AL1(CONQ),AL2(09+L'PRCODE1,10),C'</Product>'                     
         DC    AL1(EORQ),AL2(4+09+L'PRCODE1+10)                                 
         DC    AL1(CONQ),AL2(0,07),C'<Media>'                                   
         DC    AL1(LWSQ),AL2(07,L'MEDIA,MEDIA-LWSD)                             
         DC    AL1(CONQ),AL2(07+L'MEDIA,08),C'</Media>'                         
         DC    AL1(EORQ),AL2(4+07+L'MEDIA+08)                                   
*                                                                               
         DC    AL1(CONQ),AL2(0,14),C'</InvoiceItem>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
**                                                                              
**  END OF INVOICE DATA                                                         
**                                                                              
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
MEWBEND  DS    0X                                                               
         DC    AL1(ROUQ),AL2(MEWBENDQ)                                          
         DC    AL1(CONQ),AL2(0,12),C'<BatchTotal>'                              
         DC    AL1(LWSQ),AL2(12,L'WBFDTOT,WBFDTOT-LWSD)                         
         DC    AL1(EORQ),AL2(4+12+L'WBFDTOT)                                    
         DC    AL1(CONQ),AL2(0,14),C'</DataObjects>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
                                                                                
***OLD** DC    AL1(CONQ),AL2(0,10),C'</Request>'                                
***OLD** DC    AL1(EORQ),AL2(4+10)                                              
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
         TITLE 'YNRO - CHEVRON'                                                 
*********************************************************************           
* YNRO   CHEVRON                                               *                
*********************************************************************           
YNCVROU  NTR1  BASE=*,LABEL=*                                                   
         CHI   R1,BEFRQ            BEFORE CARDS                                 
         BNE   YNCVAFTR                                                         
         MVC   PNAME2,SPACES                                                    
         MVC   CHVPO#,SPACES                                                    
         MVC   BILLCM,SPACES                                                    
         MVC   PCODE1,SPACES                                                    
         MVC   PCODE2,SPACES                                                    
         J     XIT                                                              
*                                                                               
YNCVAFTR CHI   R1,AFTRQ            AFTER CARDS                                  
         BNE   YNCVR                                                            
         MVI   SKIPINV,C'N'                                                     
*                                                                               
         MVC   CHVSPLR,=C'0050066495'    SUPPLIER ID FOR RETAIL                 
*                                                                               
         CLC   PNAME1(06),=C'RETAIL' PRINT - DIVISION RETAIL                    
         BE    YNCVAFT0                                                         
         CLC   PNAME1(15),=C'HISPANIC RETAIL'  PRINT DIV. RETAIL                
         BE    YNCVAFT0                                                         
         CLC   PNAME2(06),=C'RETAIL'  SPOT/NET - PRD GROUP                      
         BE    YNCVAFT0                                                         
         CLC   PNAME2(15),=C'HISPANIC RETAIL'                                   
         BE    YNCVAFT0                                                         
*                                                                               
*        CHECKS FOR LUBRICATION DIVISION                                        
*                                                                               
         CLC   PCODE1(3),=C'001'  LUBRICATION DIVISION                          
         BE    YNCVAFTC                                                         
         CLC   PCODE1(4),=C'X101' LUBRICATION PRD GROUP                         
         BE    YNCVAFTC                                                         
         CLC   PCODE2(4),=C'X101' LUB. PRD GRP MIGHT BE LEVEL 2                 
         BE    YNCVAFTC                                                         
*                                                                               
*        CHECKS FOR CORPORATE DIVISION                                          
*                                                                               
         CLC   PCODE1(3),=C'005'  CORPORATE DIVISION                            
         BE    YNCVAFTC                                                         
         CLC   PCODE1(4),=C'X201' CORPORATE PRODUCT GROUP                       
         BE    YNCVAFTC                                                         
         CLC   PCODE2(4),=C'X201' CORPORATE PRD GRP - MIGHT BE LEVEL 2          
         BE    YNCVAFTC                                                         
*                                                                               
*                                                                               
*        CHECKS FOR CREDIT CARD DIVISION                                        
*                                                                               
         CLC   PCODE1(3),=C'007'  CREDIT CARD DIVISION                          
         BE    YNCVAFTC                                                         
         CLC   PCODE1(4),=C'X107' CREDIT CARD PRODUCT GROUP                     
         BE    YNCVAFTC                                                         
         CLC   PCODE2(4),=C'X107' CREDIT CARD PRD GRP - LEVEL 2                 
         BE    YNCVAFTC                                                         
*                                                                               
         MVI   SKIPINV,C'Y'      ONLY PROCESS RETAIL DIV/PRD GRP                
         MVI   SKIPEIN,5                                                        
         J     EIXIT                                                            
*                                                                               
*        MAY NEED A DIFFERENT CODE FOR LUBRICATION                              
*                                                                               
YNCVAFTC DS    0H                                                               
         MVC   CHVSPLR,=C'0050066495'    SUPPLIER ID FOR BOTH                   
**TEST** MVC   CHVSPLR,=C'0050073316'  SUPPLIER IF FOR CORPORATE BILLS          
*                                                                               
YNCVAFT0 DS    0H                                                               
         MVI   FILESW,C'Y'       TELL EB THAT I PROCESSED A BILL                
         MVC   CHVFTR(100),SPACES                                               
         MVC   CHVFTR+100(100),SPACES                                           
         MVC   CHVFTR+200(40),SPACES                                            
         MVC   CHVFTR(2),MEDIA       (SYSTEM AND MEDIA)                         
         MVI   CHVFTR+2,C'/'                                                    
         MVC   CHVFTR+3(3),ADVCODE       CLIENT                                 
         MVC   CHVFTR+7(L'ADVNAME),ADVNAME                                      
*                                                                               
         LA    R1,CHVFTR+43                                                     
         CLI   0(R1),C' '     SCAN BACKWARD FOR FIRST NON-SPACE                 
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVI   2(R1),C'/'                                                       
         MVC   3(L'PNAME1,R1),PNAME1  PRD GRP LEVEL 1 NAME                      
*                                                                               
         CLI   SYSTM,C'P'     PRINT HAS ONLY DIVISIONS                          
         BE    YNCVAFT1                                                         
*                                                                               
         LA    R1,CHVFTR+103                                                    
         CLI   0(R1),C' '     SCAN BACKWARD FOR FIRST NON-SPACE                 
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVI   2(R1),C'/'                                                       
         MVC   3(L'PNAME2,R1),PNAME2  PRD GRP LEVEL 2 NAME                      
*                                                                               
YNCVAFT1 LA    R1,CHVFTR+130                                                    
         CLI   0(R1),C' '     SCAN BACKWARD FOR FIRST NON-SPACE                 
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVI   2(R1),C'/'                                                       
         MVC   3(3,R1),PRCODE1                                                  
         MVC   7(L'PRNAME1,R1),PRNAME1                                          
*                                                                               
         LA    R1,CHVFTR+160                                                    
         CLI   0(R1),C' '     SCAN BACKWARD FOR FIRST NON-SPACE                 
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVI   2(R1),C'/'                                                       
         MVC   3(3,R1),ESCODE1                                                  
         ST    R1,SAVER1                                                        
*                                                                               
         LA    RE,L'ESNAME1                                                     
         LA    R1,ESNAME1                                                       
YNCVAFT2 CLI   0(R1),C'&&'                                                      
         BNE   *+8                                                              
         MVI   0(R1),C'+'   CHANGE & TO + (XML CAN'T HANDLE &)                  
         LA    R1,1(R1)                                                         
         BCT   RE,YNCVAFT2                                                      
*                                                                               
         L     R1,SAVER1                                                        
         MVC   7(L'ESNAME1,R1),ESNAME1                                          
*                                                                               
         CLI   SYSTM,C'P'     PRINT MAY HAVE A SECOND ESTIMATE NAME             
         BNE   YNCVAFT6                                                         
*                                                                               
         LA    RE,L'ESNAME2                                                     
         LA    R1,ESNAME2                                                       
YNCVAFT3 CLI   0(R1),C'&&'                                                      
         BNE   *+8                                                              
         MVI   0(R1),C'+'   CHANGE & TO + (XML CAN'T HANDLE &)                  
         LA    R1,1(R1)                                                         
         BCT   RE,YNCVAFT3                                                      
*                                                                               
         LA    R1,CHVFTR+186                                                    
         CLI   0(R1),C' '     SCAN BACKWARD FOR FIRST NON-SPACE                 
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVC   2(L'ESNAME2,R1),ESNAME2                                          
*                                                                               
YNCVAFT6 DS    0H            NOW APPEND THE ENDING PIDX                         
*                                                                               
         LA    R1,CHVFTR+L'CHVFTR-1                                             
         CLI   0(R1),C' '     SCAN BACKWARD FOR FIRST NON-SPACE                 
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVC   1(23,R1),=C'</pidx:ReferenceNumber>'                             
         MVC   CHVFTR2,CHVFTR                                                   
         MVC   1(25,R1),=C'</pidx:FieldTicketNumber>'                           
*                                                                               
*                            NOW APPEND THE ENDING PIDX TO CHVE1                
         MVI   CHVPO#-1,X'FF'    TO PREVENT GOING PAST BEGINNING                
         MVC   CHVPO#,SPACES                                                    
         MVC   CHVPO#(L'CHVE1),CHVE1       SERVICE ORDER #                      
         LA    R1,CHVPO#+L'CHVPO#-1                                             
         CLI   0(R1),C' '     SCAN BACKWARD FOR FIRST NON-SPACE                 
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVC   1(27,R1),=C'</pidx:PurchaseOrderNumber>'                         
*                                                                               
*                            NOW APPEND THE ENDING PIDX TO CHVE1                
         MVI   CHVPLNTC-1,X'FF'  TO PREVENT GOING PAST BEGINNING                
         MVC   CHVPLNTC,SPACES                                                  
         OC    CHVP1,SPACES                                                     
         MVC   CHVPLNTC(L'CHVP1),CHVP1     PLANT CODE                           
         LA    R1,CHVPLNTC+L'CHVPLNTC-1                                         
         CLI   0(R1),C' '     SCAN BACKWARD FOR FIRST NON-SPACE                 
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVC   1(29,R1),=C'</pidx:JobLocationIdentifier>'                       
*                                                                               
*                            NOW APPEND THE ENDING PIDX TO CHVUCE1              
         MVI   CHVSEA-1,X'FF'    TO PREVENT GOING PAST BEGINNING                
         MVC   CHVSEA,SPACES                                                    
         MVC   CHVSEA(L'CHVUCE1),CHVUCE1     SERVICE ENTRY APPROVER             
         LA    R1,CHVSEA+L'CHVSEA-1                                             
         CLI   0(R1),C' '     SCAN BACKWARD FOR FIRST NON-SPACE                 
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVC   1(25,R1),=C'</pidx:ContactIdentifier>'                           
*                                                                               
*                            NOW APPEND THE ENDING PIDX TO INV NUM              
         MVI   CHVINVN-1,X'FF'   TO PREVENT GOING PAST BEGINNING                
         MVC   CHVINVN,SPACES                                                   
         MVC   CHVINVN(L'INVNUMB),INVNUMB                                       
         LA    R1,CHVINVN+L'CHVINVN-1                                           
         CLI   0(R1),C' '     SCAN BACKWARD FOR FIRST NON-SPACE                 
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         MVC   1(21,R1),=C'</pidx:InvoiceNumber>'                               
*                                                                               
         J     XIT                                                              
*                                                                               
YNCVR    SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
*        NOTE: THE NUMBER OF BRANCHES BELOW MUST MATCH                          
*              THE NUMBER OF ENTRIES IN MEWBF (THE TABLE                        
*              OF BRANCH ADDRESS FOR THE MAP TABLES).                           
*              THIS IS TRUE FOR ALL INTERFACES.                                 
*                                                                               
         B     YNCVHDR                                                          
         B     YNCVINV     FOR FIRST INVOICE                                    
         B     YNCVPIN     FOR PREVIOUS INVOICE (ONLY IF CREDIT INV)            
         B     YNCVRES     RESET SKIPSET                                        
         B     YNCVDTL                                                          
         B     YNCVIT1                                                          
         B     YNCVFEND                                                         
*                                                                               
                                                                                
YNCVHDR  DS    0H                                                               
         MVI   FIRSTINV,C'Y'                                                    
         MVI   SKIPSET,C'N'                                                     
         MVC   AGYPROF,SPACES                                                   
         MVC   AGYPROF(13),=CL13'CHV-BLNG'                                      
         CLI   HDRSENT,C'Y'        HAVE I SENT A HEADER?                        
         BE    YNCVHDR5                                                         
         MVI   HDRSENT,C'Y'                                                     
         J     XIT                                                              
*                                                                               
YNCVHDR5 MVI   SKIPSET,C'Y'        ONLY SEND ONE HEADER                         
         MVI   FIRSTINV,C'N'                                                    
         J     XIT                                                              
*                                                                               
YNCVINV  DS    0H                                                               
         MVI   SKIPSET,C'N'                                                     
         ZAP   ITMCNT,PZERO                                                     
         XC    ALINITM,ALINITM     CLEAR A(NEXT LINE ITEM)                      
         XC    AIDPITM,AIDPITM     CLEAR A(NEXT ID RECORD)                      
*                                                                               
*        BUILD SOME OF INVOICE PDIX LINE                                        
*                                                                               
         MVC   CHVINVL(100),SPACES                                              
         MVC   CHVINVL+100(L'CHVINVL-100),SPACES                                
*                                                                               
         MVC   CHVINVL(84),=C'xsi:schemaLocation="http:www.api.org/pidXX        
               ML/v1.0 Invoice-2002-02-14-V1-0-Merge.xsd" '                     
*                                                                               
         LA    R1,CHVINVL+84                                                    
         CLI   FIRSTINV,C'Y'                                                    
         BE    YNCVIN5                                                          
         MVC   CHVINVL(84),SPACES  CLEAR IF NOT FIRST INVOICE                   
         LA    R1,CHVINVL                                                       
*                                                                               
YNCVIN5  MVC   0(63,R1),=C'pidx:version="1.0" pidx:transactionPurposeInX        
               dicator="Original">'                                             
*                                                                               
YNCVINX  J     XIT                                                              
*                                                                               
YNCVPIN  CP    DUEAMT,PZERO        IS THIS A CREDIT INVOICE?                    
         JNL   *+14                NO - SKIP CreditMemo SECTION                 
         OC    PINVNUMB,PINVNUMB   HAVE A PREVIOUS INVOICE NUMBER?              
         BNZ   *+8                 YES -  SEND CreditMemo SECTION               
         MVI   SKIPSET,C'Y'        NO - SKIP CreditMemo SECTION                 
         J     XIT                                                              
*                                                                               
YNCVRES  MVI   SKIPSET,C'N'        RESET SKIPSET                                
         J     XIT                                                              
*                                  DETAIL LINE HOOK                             
YNCVDTL  DS    0H                                                               
*                                                                               
         MVC   IDATSTA,SPACES                                                   
         MVI   CHVDESC-1,X'FF'    TO PREVENT GOING PAST BEGINNING               
         MVC   CHVDESC(100),SPACES                                              
         MVC   CHVDESC+100(100),SPACES                                          
         MVI   CHVITEM-1,X'FF'    TO PREVENT GOING PAST BEGINNING               
         MVC   CHVITEM,SPACES                                                   
         MVI   CHVLITEM-1,X'FF'   TO PREVENT GOING PAST BEGINNING               
         MVC   CHVLITEM,SPACES                                                  
         MVI   CHVPUB-1,X'FF'     TO PREVENT GOING PAST BEGINNING               
         MVC   CHVPUB,SPACES                                                    
*                                                                               
         CLI   SYSTM,C'P'         PRINT?                                        
         BE    YNCVDTLP                                                         
*                                                                               
         ICM   R4,15,ALINITM       R4=A(NEXT LINE ITEM)                         
         BZ    *+12                                                             
         LA    R4,LINLNQ(R4)                                                    
         B     *+8                                                              
         ICM   R4,15,ALINTAB       R4=A(FIRST IN LINE TABLE)                    
*                                                                               
         USING LIND,R4                                                          
         ST    R4,ALINITM          SAVE ADDRESS OF LINE ITEM                    
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
         ZAP   MYDUB,LINNET                                                     
         AP    MYDUB,LINTAX           INCLUDE TAX                               
*                                                                               
         EDIT  MYDUB,CLINDUE,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                  
*                                                                               
         MVI   LINSIGN,C'+'                                                     
         MVI   LINDRCR,C'D'                                                     
         CP    LINNET,PZERO                                                     
         BNL   *+12                                                             
         MVI   LINDRCR,C'C'                                                     
         MVI   LINSIGN,C'-'                                                     
*                                                                               
         MVI   QNTY1,C'0'          SET QUANTITY OF ONE                          
         MVC   QNTY1+1(L'QNTY1-1),QNTY1                                         
         MVI   QNTY1+(L'QNTY1-1),C'1'                                           
         CP    LINNET,PZERO                                                     
         BNL   *+8                                                              
         MVI   QNTY1+(L'QNTY1-1),X'98' SET TO MINUS ONE                         
         ZAP   TAXAMT,TOTTAX                                                    
*                                                                               
         MVC   CHVLDUE,SPACES                                                   
         MVC   CHVLDUE(L'CLINDUE),CLINDUE                                       
         LA    R1,CHVLDUE+L'CHVLDUE-1                                           
YNCVD3   CLI   0(R1),C' '                                                       
         BH    YNCVD5                                                           
         BCT   R1,YNCVD3           SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVD5   MVC   1(22,R1),=C'</pidx:MonetaryAmount>'                              
*                                                                               
         AP    ITMCNT,=P'1'        ITEM COUNT                                   
         OI    ITMCNT+(L'ITMCNT-1),X'0F'                                        
         UNPK  CITMCNT,ITMCNT                                                   
*                                                                               
***      MVC   CHVITEM(L'CITMCNT),CITMCNT                                       
         MVC   CHVITEM(6),=C'000001'                                            
         LA    R1,CHVITEM+L'CHVITEM-1                                           
YNCVD7   CLI   0(R1),C' '                                                       
         BH    YNCVD8                                                           
         BCT   R1,YNCVD7           SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVD8   MVC   1(22,R1),=C'</pidx:LineItemNumber>'                              
*                                                                               
*                                                                               
*        FOR SPOT SET CHVDESC TO MARKET NAME FOLLWED BY STATION                 
*        FOLLOWED BY MOS                                                        
*                                                                               
*        FOR NET SHOW STATION FOLLOWED BY MOS-THERE WON'T BE A MARKET           
*                                                                               
         OC    MRKTNAM,SPACES     JUST IN CASE                                  
         MVC   CHVDESC(L'MRKTNAM),MRKTNAM                                       
         LA    R1,CHVDESC+L'CHVDESC-1                                           
YNCVD10  CLI   0(R1),C' '                                                       
         BH    YNCVD10M                                                         
         BCT   R1,YNCVD10          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVD10M MVC   2(L'LINSTA,R1),LINSTA                                            
*                                                                               
         LA    R1,CHVDESC+L'CHVDESC-1                                           
YNCVD12  CLI   0(R1),C' '                                                       
         BH    YNCVD12M                                                         
         BCT   R1,YNCVD12          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVD12M MVC   2(6,R1),LINMOS                                                   
*                                                                               
         MVC   8(26,R1),=C'</pidx:LineItemIdentifier>'                          
         MVC   CHVDESC2,CHVDESC                                                 
         MVC   8(27,R1),=C'</pidx:LineItemDescription>'                         
*                                                                               
         MVI   RTNROU,C'N'                                                      
         LA    R4,LINLNQ(R4)                                                    
         CLI   0(R4),EOT                                                        
         JE    XIT                                                              
         MVI   RTNROU,C'Y'         SET RETURN FOR NEXT                          
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
YNCVDTLP DS    0H                  PRINT USES INSERTION TABLE                   
*                                                                               
         ICM   R4,15,AIDPITM       R4=A(NEXT LINE ITEM)                         
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
***      MVC   CHVITEM(L'CITMCNT),CITMCNT                                       
         MVC   CHVITEM(6),=C'000001'                                            
         LA    R1,CHVITEM+L'CHVITEM-1                                           
YNCVD1P  CLI   0(R1),C' '                                                       
         BH    YNCVD2P                                                          
         BCT   R1,YNCVD1P          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVD2P  MVC   1(22,R1),=C'</pidx:LineItemNumber>'                              
*                                                                               
*                                                                               
* NOTE: IDPONET NOW ALREADY HAS PREVIOUSLY BILLED NET SUBTRACTED                
*       LIKEWISE IDPOCSD HAS PREVIOUSLY BILLED CD SUBTRACTED                    
*                                                                               
         ZAP   MYDUB,IDPONET                                                    
         SP    MYDUB,IDPOCSD      LESS CD BILLABLE                              
*                                                                               
*        NEXT 2 LINES OF CODE MAY BE NEEDED IF THEY HAVE A PROBLEM              
*        WITH US TAXES FOR PRINT                                                
*                                                                               
**TAX**  AP    MYDUB,IDPOTAX      ADD ORDERED TAX                               
**TAX**  SP    MYDUB,IDPPTAX      LESS PREVIOUS TAX                             
*                                                                               
         EDIT  MYDUB,CLINDUE,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                  
*                                                                               
         MVC   CHVLDUE,SPACES                                                   
         MVC   CHVLDUE(L'CLINDUE),CLINDUE                                       
         LA    R1,CHVLDUE+L'CHVLDUE-1                                           
YNCVD3P  CLI   0(R1),C' '                                                       
         BH    YNCVD5P                                                          
         BCT   R1,YNCVD3P          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVD5P  MVC   1(22,R1),=C'</pidx:MonetaryAmount>'                              
*                                                                               
         MVI   LINSIGN,C'+'                                                     
         MVI   LINDRCR,C'D'                                                     
         CP    MYDUB,PZERO                                                      
         BNL   *+12                                                             
         MVI   LINDRCR,C'C'                                                     
         MVI   LINSIGN,C'-'                                                     
*                                                                               
         MVC   VNDNAME,IDPPUB             PUB NAME                              
         MVC   VNDNAMC,VNDNAME                                                  
         LA    RE,L'VNDNAMC                                                     
         LA    R1,VNDNAMC                                                       
YNCVD5P4 CLI   0(R1),C'&&'                                                      
         BNE   *+8                                                              
         MVI   0(R1),C'+'  CHANGE & TO + SINCE XML CAN'T HANDLE &               
         LA    R1,1(R1)     BUMP TO NEXT POSITION                               
         BCT   RE,YNCVD5P4  CHECK FOR MORE                                      
*                                                                               
*        IDPMISC (IF PRESENT WILL BE YN OUTDOOR MKT)                            
*                                                                               
         MVC   CHVDESC(L'IDPMISC),IDPMISC                                       
         LA    R1,CHVDESC+L'CHVDESC-1                                           
YNCVD5PA CLI   0(R1),C' '                                                       
         BH    YNCVD5PB                                                         
         BCT   R1,YNCVD5PA         SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVD5PB MVC   2(L'VNDNAMC,R1),VNDNAMC     VENDOR NAME                          
*                                                                               
         LA    R1,CHVDESC+L'CHVDESC-1                                           
YNCVD5PC CLI   0(R1),C' '                                                       
         BH    YNCVD5PD                                                         
         BCT   R1,YNCVD5PC         SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVD5PD MVC   2(L'IDPDATE,R1),IDPDATE     INSERTION DESCRIPTION                
*                                                                               
         LA    R1,CHVDESC+L'CHVDESC-1                                           
YNCVD5PE CLI   0(R1),C' '                                                       
         BH    YNCVD5PF                                                         
         BCT   R1,YNCVD5PE         SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVD5PF MVC   2(L'IDPDESC,R1),IDPDESC     INSERTION DESCRIPTION                
*                                                                               
         LA    R1,CHVDESC+L'CHVDESC-1                                           
YNCVD7P  CLI   0(R1),C' '                                                       
         BH    YNCVD8P                                                          
         BCT   R1,YNCVD7P          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVD8P  MVC   1(26,R1),=C'</pidx:LineItemIdentifier>'                          
         MVC   CHVDESC2,CHVDESC                                                 
         MVC   1(27,R1),=C'</pidx:LineItemDescription>'                         
*                                                                               
         MVI   RTNROU,C'N'                                                      
         LA    R4,IDPLNQ(R4)                                                    
         CLI   0(R4),EOT                                                        
         JE    XIT                                                              
         MVI   RTNROU,C'Y'         SET RETURN FOR NEXT                          
         J     XIT                                                              
         DROP  R4                                                               
*                                                                               
YNCVIT1  MVC   VNDNAME,SPACES                                                   
         MVC   VNDNAMC,SPACES                                                   
         MVI   CHVPUB-1,X'FF'     TO PREVENT GOING PAST BEGINNING               
         MVC   CHVPUB,SPACES                                                    
         L     R2,ALINITM          CURRENT LINE ITEM                            
         USING LIND,R2                                                          
         CLI   SYSTM,C'S'                                                       
         BNE   YNCVIT20                                                         
         OC    LINSTA,SPACES   IT SOMETIMES STARTED WITH BINARY 0               
         MVC   VNDNAME(L'LINSTA),LINSTA    STATION CODE                         
         MVC   VNDNAMC,VNDNAME                                                  
*&&DO                                                                           
*                                                                               
*        FOR SPOT SET CHVPUB TO MARKET NAME FOLLWED BY STATION                  
*                                                                               
         MVC   CHVPUB(L'MRKTNAM),MRKTNAM                                        
         LA    R1,CHVPUB+L'CHVPUB-1                                             
YNCVIT2  CLI   0(R1),C' '                                                       
         BH    YNCVIT2M                                                         
         BCT   R1,YNCVIT2          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVIT2M MVC   2(L'LINSTA,R1),LINSTA                                            
*&&                                                                             
*                                                                               
         BAS   RE,SETMEDN          SET MEDIA NAME                               
*                                                                               
         MVI   CHVDESC,C' '        INIT TO SPACES                               
         MVC   CHVDESC+1(L'CHVDESC-1),CHVDESC                                   
         MVC   CHVDESC(L'CHVPUB),CHVPUB                                         
*                                                                               
         LA    R1,CHVDESC+L'CHVDESC-1                                           
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8              SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
         MVC   1(27,R1),=C'</pidx:LineItemDescription>'                         
*                                                                               
         LA    R1,CHVPUB+L'CHVPUB-1                                             
YNCVIT2S CLI   0(R1),C' '                                                       
         BH    YNCVIT2X                                                         
         BCT   R1,YNCVIT2S         SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVIT2X MVC   1(26,R1),=C'</pidx:LineItemIdentifier>'                          
*                                                                               
YNCVIT17 DS    0H                                                               
         ZAP   MYDUB,LINNET                                                     
         AP    MYDUB,LINTAX       INCLUDE BILLABLE TAX                          
         SP    MYDUB,LINCSD       NET-CD                                        
*                                                                               
         EDIT  MYDUB,CLINDUE,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                  
*                                                                               
         B     YNCVITX                                                          
         DROP  R2                                                               
*                                                                               
YNCVIT20 DS    0H                                                               
         L     R2,AIDPITM            CURRENT INSERTION DATA                     
         USING IDPD,R2                                                          
         MVC   VNDNAME,IDPPUB             PUB NAME                              
         MVC   VNDNAMC,VNDNAME                                                  
         LA    RE,L'VNDNAMC                                                     
         LA    R1,VNDNAMC                                                       
YNCVI1P  CLI   0(R1),C'&&'                                                      
         BNE   *+8                                                              
         MVI   0(R1),C'+'  CHANGE & TO + SINCE XML CAN'T HANDLE &               
         LA    R1,1(R1)     BUMP TO NEXT POSITION                               
         BCT   RE,YNCVI1P   CHECK FOR MORE                                      
*                                                                               
***      MVC   CHVPUB(L'VNDNAMC),VNDNAMC  PUB NAME AND CODE                     
*                                                                               
         BAS   RE,SETMEDN          SET MEDIA NAME                               
*                                                                               
         MVI   CHVDESC,C' '        INIT TO SPACES                               
         MVC   CHVDESC+1(L'CHVDESC-1),CHVDESC                                   
         MVC   CHVDESC(L'CHVPUB),CHVPUB                                         
*                                                                               
         LA    R1,CHVDESC+L'CHVDESC-1                                           
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8              SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
         MVC   1(27,R1),=C'</pidx:LineItemDescription>'                         
*                                                                               
         LA    R1,CHVPUB+L'CHVPUB-1                                             
YNCVI2P  CLI   0(R1),C' '                                                       
         BH    YNCVI2PX                                                         
         BCT   R1,YNCVI2P          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVI2PX MVC   1(26,R1),=C'</pidx:LineItemIdentifier>'                          
*                                                                               
*                                   ORDERED NET                                 
*                                   NOW LESS PREV. BILLED NET                   
*       LIKEWISE IDPOCSD HAS PREVIOUSLY BILLED CD SUBTRACTED                    
*                                                                               
         ZAP   MYDUB,IDPONET                                                    
         SP    MYDUB,IDPOCSD     LESS CD BILLABLE                               
*                                                                               
*        NEXT 2 LINES OF CODE MAY BE NEEDED IF THEY HAVE A PROBLEM              
*        WITH US TAXES FOR PRINT                                                
*                                                                               
**TAX**  AP    MYDUB,IDPOTAX      ADD ORDERED TAX                               
**TAX**  SP    MYDUB,IDPPTAX      LESS PREVIOUS TAX                             
*                                                                               
         EDIT  MYDUB,CLINDUE,2,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-                  
*                                                                               
         MVI   LINSIGN,C'+'                                                     
         MVI   LINDRCR,C'D'                                                     
         CP    MYDUB,PZERO                                                      
         BNL   *+12                                                             
         MVI   LINDRCR,C'C'                                                     
         MVI   LINSIGN,C'-'                                                     
*                                                                               
         B     YNCVITX                                                          
         DROP  R2                                                               
*                                                                               
***      UNPK  CNETAMT,LINNET                                                   
***      LA    R1,CNETAMT+L'CNETAMT-1                                           
***      GOTOR FIXNEG                                                           
YNCVITX  MVC   CHVLDUE,SPACES                                                   
         MVC   CHVLDUE(L'CLINDUE),CLINDUE                                       
         LA    R1,CHVLDUE+L'CHVLDUE-1                                           
YNCVI3P  CLI   0(R1),C' '                                                       
         BH    YNCVI5P                                                          
         BCT   R1,YNCVI3P          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVI5P  MVC   1(22,R1),=C'</pidx:MonetaryAmount>'                              
*                                                                               
***      MVC   CHVLITEM(L'CITMCNT),CITMCNT                                      
         MVC   CHVLITEM(6),=C'000001'                                           
*                                                                               
         LA    R1,CHVLITEM+L'CHVLITEM-1                                         
YNCVFE2  CLI   0(R1),C' '                                                       
         BH    YNCVFE4                                                          
         BCT   R1,YNCVFE2          SCAN BACKWARD FOR NON-SPACE                  
*                                                                               
YNCVFE4  MVC   1(22,R1),=C'</pidx:TotalLineItems>'                              
*                                                                               
         J     XIT                                                              
*                                                                               
YNCVFEND DS    0H             DO NOTHING                                        
         J     XIT                                                              
*                                                                               
SETMEDN  NTR1                                                                   
*                                                                               
         LA    RE,MEDTABP          MEDIA NAME TABLE FOR PRINT                   
         CLI   SYSTM,C'S'          SPOT OR NET SYSTEM?                          
         BNE   *+8                 NO                                           
         LA    RE,MEDTABS          YES - MEDIA NAME TABLE FOR SPOT/NET          
         LA    R1,CHVPUB           SET CHVPUB TO MEDIA NAME                     
         MVC   0(5,R1),=C'OTHER'   SET TO DEFAULT IN CASE NOT FOUND             
         XR    RF,RF               CLEAR RF                                     
*                                                                               
SET00    CLI   0(RE),X'FF'         END OF TABLE?                                
         BE    SETX                YES - DONE                                   
         IC    RF,1(RE)            L' MEDIA NAME                                
         CLC   0(1,RE),MEDIA+1     MATCH ON MEDIA?                              
         BE    SET10               YES - SET CHVPUB TO MEDIA NAME               
         LA    RE,2(RE,RF)         BUMP TO NEXT ENTRY                           
         B     SET00                                                            
*                                                                               
SET10    BCTR  RF,0                DECREMENT RF FOR EX                          
         EX    RF,*+8              MOVE MEDIA NAME                              
         B     *+10                FOR IDF                                      
         MVC   0(0,R1),2(RE)       ** EXECUTED **                               
*                                                                               
SETX     J     XIT                 DONE                                         
*                                                                               
MEDTABS  DC    C'T',X'07',C'SPOT TV'                                            
         DC    C'R',X'0A',C'SPOT RADIO'                                         
         DC    C'X',X'0A',C'NTWK RADIO'                                         
         DC    C'C',X'05',C'CABLE'                                              
         DC    C'N',X'0A',C'NETWORK TV'                                         
         DC    C'S',X'0B',C'SYNDICATION'                                        
         DC    X'FF'                                                            
*                                                                               
MEDTABP  DC    C'I',X'0B',C'INTERACTIVE'                                        
         DC    C'L',X'06',C'SOCIAL'                                             
         DC    C'M',X'08',C'MAGAZINE'                                           
         DC    C'N',X'09',C'NEWSPAPER'                                          
         DC    C'O',X'07',C'OUTDOOR'                                            
         DC    C'S',X'06',C'SEARCH'                                             
         DC    C'T',X'05',C'TRADE'                                              
         DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* YNRO - CHEVRON                                                                
YNCHV    DC    AL2(YNCVROU-XML)      CONTROL ROUTINE                            
         DC    AL2(YNCVHDM-XML)      FILE HEADER RECORD(S)                      
         DC    AL2(YNCVIVM-XML)      INVOICE HEADER RECORDS                     
         DC    AL2(YNCVLNM-XML)      INVOICE LINE ITEM RECORDS                  
         DC    AL2(YNCVTOM-XML)      INVOICE TOTAL RECORDS                      
         DC    AL2(YNCVEND-XML)      FILE TOTAL RECORDS                         
         DC    AL1(EOT)                                                         
*                                                                               
YNCVHDRQ EQU   1                                                                
YNCVINVQ EQU   2     HEADER                                                     
YNCVPINQ EQU   3     PREVIOUS INVOICE                                           
YNCVRESQ EQU   4     RESET FLAG TO SKIP CREDIT SECTION                          
YNCVDTLQ EQU   5                                                                
YNCVIT1Q EQU   6                                                                
YNCVENDQ EQU   7                                                                
         EJECT                                                                  
*********************************************************************           
* YNRO CHEVRON  XML MAP TABLE(S) - SEE MAPD                                     
*********************************************************************           
                                                                                
YNCVHDM  DS    0X                  YNRO - CHEVRON VERSION 4010                  
*                                                                               
         DC    AL1(ROUQ),AL2(YNCVHDRQ)                                          
         DC    AL1(CONQ),AL2(0,39),C'<?xml version="1.0" encoding="UTF-X        
               8" ?>'                                                           
         DC    AL1(EORQ),AL2(4+39)                                              
*                                                                               
**OLD    DC    AL1(CONQ),AL2(0,63),C'<pidx:DataObjects xmlns:pidx="http         
**OLD          ://www.api.org/pidxXML/v1.0">'                                   
**OLD    DC    AL1(EORQ),AL2(4+63)                                              
**OLD                                                                           
**OLD                                                                           
**OLD    DC    AL1(EOTQ,EOTQ)                                                   
**                                                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,89),C'<dds:DataObjects xmlns:dds="http:/X        
               /www.dds.net/schemas" xmlns:xsi="http://www.w3.org/2001'         
         DC    AL1(CONQ),AL2(89,93),C'/XMLSchema-instance" xsi:schemaLoX        
               cation="http://www.dds.net/schemas generic-wrapper-1.0.xX        
               sd">'                                                            
         DC    AL1(EORQ),AL2(4+89+93)                                           
*                                                                               
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
**  END OF HEADER DATA                                                          
**                                                                              
YNCVIVM  DS    0X              START OF INVOICE HEADER FOR 1ST INV              
         DC    AL1(ROUQ),AL2(YNCVINVQ)                                          
*                                                                               
         DC    AL1(CONQ),AL2(0,58),C'<pidx:Invoice xmlns:pidx="http://wX        
               ww.api.org/pidXML/v1.0" '                                        
         DC    AL1(LWSQ),AL2(58,L'CHVINVL,CHVINVL-LWSD)                         
         DC    AL1(EORQ),AL2(4+58+L'CHVINVL)                                    
*                                                                               
**OLD    DC    AL1(CONQ),AL2(58,86),C'xsi:schemaLocation="http://www.ap         
**OLD          i.org/pidXML/v1.0 Invoice-2002-02-14-v1-0-Merge.xsd" '           
**OLD    DC    AL1(CONQ),AL2(144,63),C'pidx:version="1.0" pidx:transact         
**OLD          ionPurposeIndicator="Original">'                                 
**OLD    DC    AL1(EORQ),AL2(4+77+67+63)                                        
**OLD                                                                           
**OLD    DC    AL1(EOTQ,EOTQ)                                                   
**OLD                                                                           
**OLD    DC    AL1(CONQ),AL2(0,77),C'<pidx:Invoice pidx:version="1.0" p         
**OLD          idx:transactionPurposeIndicator="Original">'                     
**OLD    DC    AL1(EORQ),AL2(4+77)                                              
*                                                                               
**OLDN2  DC    AL1(ROUQ),AL2(YNCVIN2Q)                                          
**OLD    DC    AL1(CONQ),AL2(0,58),C'<pidx:Invoice xmlns:pidx="http://w         
**OLD          ww.api.org/pidXML/v1.0" '                                        
**OLD    DC    AL1(CONQ),AL2(058,63),C'pidx:version="1.0" pidx:transact         
**OLD          ionPurposeIndicator="Original">'                                 
**OLD    DC    AL1(EORQ),AL2(4+58+63)                                           
**OLD                                                                           
**OLD    DC    AL1(EOTQ,EOTQ)                                                   
**OLD                                                                           
**OLD           REST OF INVOICE HEADER FOR ALL INVOICES                         
**OLD                                                                           
**OLDVP  DC    AL1(ROUQ),AL2(YNCVINPQ)                                          
*                                                                               
         DC    AL1(CONQ),AL2(0,24),C'<pidx:InvoiceProperties>'                  
         DC    AL1(EORQ),AL2(4+24)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,20),C'<pidx:InvoiceNumber>'                      
         DC    AL1(LWSQ),AL2(20,L'CHVINVN,CHVINVN-LWSD)                         
         DC    AL1(EORQ),AL2(4+20+L'CHVINVN)                                    
*                                                                               
         DC    AL1(CONQ),AL2(0,65),C'<pidx:ReferenceInformation referenX        
               ceInformationIndicator="Other">'                                 
         DC    AL1(EORQ),AL2(4+65)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,22),C'<pidx:ReferenceNumber>'                    
         DC    AL1(LWSQ),AL2(22,L'CHVFTR2,CHVFTR2-LWSD)                         
         DC    AL1(EORQ),AL2(4+22+L'CHVFTR2)                                    
*                                                                               
         DC    AL1(CONQ),AL2(0,18),C'<pidx:Description>'                        
         DC    AL1(CONQ),AL2(18,8),C'CVXTitle'                                  
         DC    AL1(CONQ),AL2(26,19),C'</pidx:Description>'                      
         DC    AL1(EORQ),AL2(4+18+8+19)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,28),C'</pidx:ReferenceInformation>'              
         DC    AL1(EORQ),AL2(4+28)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,18),C'<pidx:InvoiceDate>'                        
         DC    AL1(LWSQ),AL2(18,4,CCYY-LWSD)                                    
         DC    AL1(CONQ),AL2(22,1),C'-'                                         
         DC    AL1(LWSQ),AL2(23,2,MM-LWSD)                                      
         DC    AL1(CONQ),AL2(25,1),C'-'                                         
         DC    AL1(LWSQ),AL2(26,2,DD-LWSD)                                      
         DC    AL1(CONQ),AL2(28,19),C'</pidx:InvoiceDate>'                      
         DC    AL1(EORQ),AL2(4+18+4+1+2+1+2+19)                                 
*                                                                               
         DC    AL1(CONQ),AL2(0,55),C'<pidx:PartnerInformation partnerRoX        
               leIndicator="Seller">'                                           
         DC    AL1(EORQ),AL2(4+55)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,24),C'<pidx:PartnerIdentifier '                  
         DC    AL1(CONQ),AL2(24,45),C'partnerIdentifierIndicator="AssigX        
               nedByBuyer">'                                                    
*****    DC    AL1(CONQ),AL2(69,10),C'0050066495'                               
         DC    AL1(LWSQ),AL2(69,10,CHVSPLR-LWSD)                                
*                                                                               
         DC    AL1(CONQ),AL2(79,25),C'</pidx:PartnerIdentifier>'                
         DC    AL1(EORQ),AL2(4+24+45+10+25)                                     
*                                                                               
*****    DC    AL1(CONQ),AL2(0,25),C'<pidx:ContactInformation '                 
*****    DC    AL1(CONQ),AL2(25,51),C'contactInformationIndicator="Offi         
*****          ceRepresentative">'                                              
*****    DC    AL1(EORQ),AL2(4+25+51)                                           
*****                                                                           
*****    DC    AL1(CONQ),AL2(0,19),C'<pidx:EmailAddress>'                       
*****    DC    AL1(CONQ),AL2(19,16),C'ERROR EMAIL ADDR'                         
*****                                                                           
*****    DC    AL1(CONQ),AL2(31,20),C'</pidx:EmailAddress>'                     
*****    DC    AL1(EORQ),AL2(4+19+16+20)                                        
*****                                                                           
*****    DC    AL1(CONQ),AL2(0,26),C'</pidx:ContactInformation>'                
*****    DC    AL1(EORQ),AL2(4+26)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,26),C'</pidx:PartnerInformation>'                
         DC    AL1(EORQ),AL2(4+26)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,55),C'<pidx:PartnerInformation partnerRoX        
               leIndicator="SoldTo">'                                           
         DC    AL1(EORQ),AL2(4+55)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,24),C'<pidx:PartnerIdentifier '                  
         DC    AL1(CONQ),AL2(24,46),C'partnerIdentifierIndicator="AssigX        
               nedBySeller">'                                                   
         DC    AL1(CONQ),AL2(70,25),C'</pidx:PartnerIdentifier>'                
         DC    AL1(EORQ),AL2(4+24+46+25)                                        
*                                                                               
         DC    AL1(CONQ),AL2(0,25),C'<pidx:ContactInformation '                 
         DC    AL1(CONQ),AL2(25,46),C'contactInformationIndicator="BuyeX        
               rDepartment">'                                                   
         DC    AL1(EORQ),AL2(4+25+46)                                           
*                                                                               
         DC    AL1(CONQ),AL2(0,24),C'<pidx:ContactIdentifier '                  
         DC    AL1(CONQ),AL2(24,40),C'contactIdentifierIndicator="EmploX        
               yeeID">'                                                         
         DC    AL1(LWSQ),AL2(24+40,L'CHVSEA,CHVSEA-LWSD)                        
         DC    AL1(EORQ),AL2(4+24+40+L'CHVSEA)                                  
*                                                                               
         DC    AL1(CONQ),AL2(0,26),C'</pidx:ContactInformation>'                
         DC    AL1(EORQ),AL2(4+26)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,26),C'</pidx:PartnerInformation>'                
         DC    AL1(EORQ),AL2(4+26)                                              
*                                                                               
         DC    AL1(ROUQ),AL2(YNCVPINQ)                                          
         DC    AL1(CONQ),AL2(0,22),C'<pidx:InvoiceTypeCode>'                    
         DC    AL1(CONQ),AL2(22,10),C'CreditMemo'                               
         DC    AL1(CONQ),AL2(32,23),C'</pidx:InvoiceTypeCode>'                  
         DC    AL1(EORQ),AL2(4+32+23)                                           
*                                                                               
         DC    AL1(ROUQ),AL2(YNCVRESQ)                                          
         DC    AL1(CONQ),AL2(0,31),C'<pidx:PurchaseOrderInformation>'           
         DC    AL1(EORQ),AL2(4+31)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,26),C'<pidx:PurchaseOrderNumber>'                
         DC    AL1(LWSQ),AL2(26,L'CHVPO#,CHVPO#-LWSD)                           
         DC    AL1(EORQ),AL2(4+26+L'CHVPO#)                                     
*                                                                               
         DC    AL1(CONQ),AL2(0,32),C'</pidx:PurchaseOrderInformation>'          
         DC    AL1(EORQ),AL2(4+32)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,29),C'<pidx:FieldTicketInformation>'             
         DC    AL1(EORQ),AL2(4+29)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,24),C'<pidx:FieldTicketNumber>'                  
         DC    AL1(LWSQ),AL2(24,L'CHVFTR,CHVFTR-LWSD)                           
         DC    AL1(EORQ),AL2(4+24+L'CHVFTR)                                     
*                                                                               
         DC    AL1(CONQ),AL2(0,30),C'</pidx:FieldTicketInformation>'            
         DC    AL1(EORQ),AL2(4+30)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,29),C'<pidx:JobLocationInformation>'             
         DC    AL1(EORQ),AL2(4+29)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,67),C'<pidx:JobLocationIdentifier jobLocX        
               ationIdentifierIndicator="Other">'                               
         DC    AL1(LWSQ),AL2(67,L'CHVPLNTC,CHVPLNTC-LWSD)                       
         DC    AL1(EORQ),AL2(4+67+L'CHVPLNTC)                                   
*                                                                               
         DC    AL1(CONQ),AL2(0,30),C'</pidx:JobLocationInformation>'            
         DC    AL1(EORQ),AL2(4+30)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,61),C'<pidx:ServiceDateTime dateTypeIndiX        
               cator="ServicePeriodStart">'                                     
         DC    AL1(LWSQ),AL2(61,4,CCYY-LWSD)                                    
         DC    AL1(CONQ),AL2(65,1),C'-'                                         
         DC    AL1(LWSQ),AL2(66,2,MM-LWSD)                                      
         DC    AL1(CONQ),AL2(68,1),C'-'                                         
         DC    AL1(LWSQ),AL2(69,2,DD-LWSD)                                      
         DC    AL1(CONQ),AL2(71,9),C'T12:00:00'                                 
         DC    AL1(CONQ),AL2(80,23),C'</pidx:ServiceDateTime>'                  
         DC    AL1(EORQ),AL2(4+61+4+1+2+1+2+9+23)                               
*                                                                               
         DC    AL1(CONQ),AL2(0,59),C'<pidx:ServiceDateTime dateTypeIndiX        
               cator="ServicePeriodEnd">'                                       
         DC    AL1(LWSQ),AL2(59,4,CCYY-LWSD)                                    
         DC    AL1(CONQ),AL2(63,1),C'-'                                         
         DC    AL1(LWSQ),AL2(64,2,MM-LWSD)                                      
         DC    AL1(CONQ),AL2(66,1),C'-'                                         
         DC    AL1(LWSQ),AL2(67,2,DD-LWSD)                                      
         DC    AL1(CONQ),AL2(69,9),C'T12:00:00'                                 
         DC    AL1(CONQ),AL2(78,23),C'</pidx:ServiceDateTime>'                  
         DC    AL1(EORQ),AL2(4+59+4+1+2+1+2+9+23)                               
*                                                                               
         DC    AL1(ROUQ),AL2(YNCVPINQ)                                          
         DC    AL1(CONQ),AL2(0,81),C'<pidx:ReferenceInformation referenX        
               ceInformationIndicator="OriginalInvoiceNumber">'                 
         DC    AL1(EORQ),AL2(4+81)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,22),C'<pidx:ReferenceNumber>'                    
         DC    AL1(LWSQ),AL2(22,10,PINVNUMB-LWSD)                               
         DC    AL1(CONQ),AL2(32,23),C'</pidx:ReferenceNumber>'                  
         DC    AL1(EORQ),AL2(4+32+23)                                           
*                                                                               
         DC    AL1(CONQ),AL2(0,19),C'<pidx:Description/>'                       
         DC    AL1(EORQ),AL2(4+19)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,28),C'</pidx:ReferenceInformation>'              
         DC    AL1(EORQ),AL2(4+28)                                              
*                                                                               
***                                                                             
* COMMENT GETS FILLED IN AT PUTRCHV                                             
***                                                                             
         DC    AL1(ROUQ),AL2(YNCVRESQ)                                          
         DC    AL1(CONQ),AL2(0,14),C'<pidx:Comment>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,15),C'</pidx:Comment>'                           
         DC    AL1(EORQ),AL2(4+15)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,25),C'</pidx:InvoiceProperties>'                 
         DC    AL1(EORQ),AL2(4+25)                                              
*                                                                               
*        INVOICE DETAIL HEADER (NOT PART OF LOOP)                               
*                                                                               
         DC    AL1(CONQ),AL2(0,21),C'<pidx:InvoiceDetails>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
*                                                                               
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
*        INVOICE DETAILS (LOOP)                                                 
*                                                                               
YNCVLNM  DC    AL1(BEGQ),AL2(YNCVDTLQ)      BEGIN LOOP                          
         DC    AL1(ROUQ),AL2(YNCVIT1Q)                                          
         DC    AL1(CONQ),AL2(0,22),C'<pidx:InvoiceLineItem>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,21),C'<pidx:LineItemNumber>'                     
         DC    AL1(LWSQ),AL2(21,L'CHVITEM,CHVITEM-LWSD)                         
         DC    AL1(EORQ),AL2(4+21+L'CHVITEM)                                    
*                                                                               
         DC    AL1(CONQ),AL2(0,22),C'<pidx:InvoiceQuantity>'                    
         DC    AL1(CONQ),AL2(22,15),C'<pidx:Quantity>'                          
         DC    AL1(CONQ),AL2(22+15,1),C'1'                                      
         DC    AL1(CONQ),AL2(22+15+1,16),C'</pidx:Quantity>'                    
         DC    AL1(EORQ),AL2(4+22+15+1+16)                                      
*                                                                               
         DC    AL1(CONQ),AL2(0,24),C'<pidx:UnitOfMeasureCode>'                  
         DC    AL1(CONQ),AL2(24,2),C'EA'                                        
         DC    AL1(CONQ),AL2(26,25),C'</pidx:UnitOfMeasureCode>'                
         DC    AL1(EORQ),AL2(4+24+2+25)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,23),C'</pidx:InvoiceQuantity>'                   
         DC    AL1(EORQ),AL2(4+23)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,26),C'<pidx:LineItemInformation>'                
         DC    AL1(CONQ),AL2(26,64),C'<pidx:LineItemIdentifier identifiX        
               erIndicator="AssignedBySeller">'                                 
         DC    AL1(LWSQ),AL2(26+64,L'CHVPUB,CHVPUB-LWSD)                        
         DC    AL1(EORQ),AL2(4+26+64+L'CHVPUB)                                  
*                                                                               
         DC    AL1(CONQ),AL2(0,63),C'<pidx:LineItemIdentifier identifieX        
               rIndicator="AssignedByBuyer">'                                   
***      DC    AL1(LWSQ),AL2(63,L'CHVDESC2,CHVDESC2-LWSD)                       
***      DC    AL1(EORQ),AL2(4+63+L'CHVDESC2)                                   
         DC    AL1(LWSQ),AL2(63,L'CHVPUB,CHVPUB-LWSD)                           
         DC    AL1(EORQ),AL2(4+63+L'CHVPUB)                                     
*                                                                               
         DC    AL1(CONQ),AL2(0,26),C'<pidx:LineItemDescription>'                
         DC    AL1(LWSQ),AL2(26,L'CHVDESC,CHVDESC-LWSD)                         
         DC    AL1(EORQ),AL2(4+26+L'CHVDESC)                                    
*                                                                               
         DC    AL1(CONQ),AL2(0,27),C'</pidx:LineItemInformation>'               
         DC    AL1(EORQ),AL2(4+27)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,14),C'<pidx:Pricing>'                            
         DC    AL1(EORQ),AL2(4+14)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,16),C'<pidx:UnitPrice>'                          
         DC    AL1(EORQ),AL2(4+16)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,21),C'<pidx:MonetaryAmount>'                     
***      DC    AL1(LWSQ),AL2(21,L'CHVLDUE,CHVLDUE-LWSD)                         
***      DC    AL1(EORQ),AL2(4+21+L'CHVLDUE)                                    
         DC    AL1(LWSQ),AL2(21,L'CHVDDUE,CHVDDUE-LWSD)                         
         DC    AL1(EORQ),AL2(4+21+L'CHVDDUE)                                    
*                                                                               
         DC    AL1(CONQ),AL2(0,24),C'<pidx:UnitOfMeasureCode>'                  
         DC    AL1(CONQ),AL2(24,2),C'EA'                                        
         DC    AL1(CONQ),AL2(26,25),C'</pidx:UnitOfMeasureCode>'                
         DC    AL1(EORQ),AL2(4+24+2+25)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,19),C'<pidx:CurrencyCode>'                       
         DC    AL1(CONQ),AL2(19,3),C'USD'                                       
         DC    AL1(CONQ),AL2(22,20),C'</pidx:CurrencyCode>'                     
         DC    AL1(EORQ),AL2(4+19+3+20)                                         
         DC    AL1(CONQ),AL2(0,17),C'</pidx:UnitPrice>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,15),C'</pidx:Pricing>'                           
         DC    AL1(EORQ),AL2(4+15)                                              
*                                                                               
         DC    AL1(ROUQ),AL2(YNCVPINQ)                                          
         DC    AL1(CONQ),AL2(0,65),C'<pidx:ReferenceInformation referenX        
               ceInformationIndicator="Other">'                                 
         DC    AL1(EORQ),AL2(4+65)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,22),C'<pidx:ReferenceNumber>'                    
         DC    AL1(CONQ),AL2(22,6),C'000001'                                    
         DC    AL1(CONQ),AL2(28,23),C'</pidx:ReferenceNumber>'                  
         DC    AL1(EORQ),AL2(4+28+23)                                           
*                                                                               
         DC    AL1(CONQ),AL2(0,18),C'<pidx:Description>'                        
         DC    AL1(CONQ),AL2(18,25),C'OriginalInvoiceLineNumber'                
         DC    AL1(CONQ),AL2(43,19),C'</pidx:Description>'                      
         DC    AL1(EORQ),AL2(4+43+19)                                           
*                                                                               
         DC    AL1(CONQ),AL2(0,28),C'</pidx:ReferenceInformation>'              
         DC    AL1(EORQ),AL2(4+28)                                              
*                                                                               
         DC    AL1(ROUQ),AL2(YNCVRESQ)                                          
         DC    AL1(CONQ),AL2(0,10),C'<pidx:Tax>'                                
         DC    AL1(EORQ),AL2(4+10)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,18),C'<pidx:TaxTypeCode>'                        
         DC    AL1(CONQ),AL2(18,5),C'Other'                                     
         DC    AL1(CONQ),AL2(18+5,19),C'</pidx:TaxTypeCode>'                    
         DC    AL1(EORQ),AL2(4+18+5+19)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,16),C'<pidx:TaxAmount>'                          
         DC    AL1(EORQ),AL2(4+16)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,21),C'<pidx:MonetaryAmount>'                     
         DC    AL1(CONQ),AL2(21,4),C'0.00'                                      
         DC    AL1(CONQ),AL2(21+4,22),C'</pidx:MonetaryAmount>'                 
         DC    AL1(EORQ),AL2(4+21+4+22)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,19),C'<pidx:CurrencyCode>'                       
         DC    AL1(CONQ),AL2(19,3),C'USD'                                       
         DC    AL1(CONQ),AL2(22,20),C'</pidx:CurrencyCode>'                     
         DC    AL1(EORQ),AL2(4+19+3+20)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,17),C'</pidx:TaxAmount>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,11),C'</pidx:Tax>'                               
         DC    AL1(EORQ),AL2(4+11)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,23),C'</pidx:InvoiceLineItem>'                   
         DC    AL1(EORQ),AL2(4+23)                                              
*                                                                               
         DC    AL1(EOTQ)                                                        
         DC    AL1(ENDQ)          END OF LOOP                                   
*                                                                               
*        END OF INVOICE                                                         
*                                                                               
YNCVLNM2 DC    AL1(BEGQ),AL2(YNCVDTLQ)      BEGIN LOOP                          
         DC    AL1(EORQ),AL2(4)                                                 
*                                                                               
         DC    AL1(EOTQ)                                                        
         DC    AL1(ENDQ)          END OF LOOP                                   
*                                                                               
*        END OF INVOICE                                                         
*                                                                               
YNCVTOM  DS    0X                                                               
         DC    AL1(CONQ),AL2(0,22),C'</pidx:InvoiceDetails>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
*                                                                               
*        INVOICE SUMMARY                                                        
*                                                                               
         DC    AL1(CONQ),AL2(0,21),C'<pidx:InvoiceSummary>'                     
         DC    AL1(EORQ),AL2(4+21)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,21),C'<pidx:TotalLineItems>'                     
         DC    AL1(LWSQ),AL2(21,L'CHVLITEM,CHVLITEM-LWSD)                       
         DC    AL1(EORQ),AL2(4+21+L'CHVLITEM)                                   
*                                                                               
         DC    AL1(CONQ),AL2(0,19),C'<pidx:InvoiceTotal>'                       
         DC    AL1(EORQ),AL2(4+19)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,21),C'<pidx:MonetaryAmount>'                     
         DC    AL1(LWSQ),AL2(21,L'CHVDDUE,CHVDDUE-LWSD)                         
         DC    AL1(EORQ),AL2(4+21+L'CHVDDUE)                                    
*                                                                               
         DC    AL1(CONQ),AL2(0,19),C'<pidx:CurrencyCode>'                       
         DC    AL1(CONQ),AL2(19,3),C'USD'                                       
         DC    AL1(CONQ),AL2(22,20),C'</pidx:CurrencyCode>'                     
         DC    AL1(EORQ),AL2(4+19+3+20)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,20),C'</pidx:InvoiceTotal>'                      
         DC    AL1(EORQ),AL2(4+20)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,10),C'<pidx:Tax>'                                
         DC    AL1(EORQ),AL2(4+10)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,18),C'<pidx:TaxTypeCode>'                        
         DC    AL1(CONQ),AL2(18,5),C'Other'                                     
         DC    AL1(CONQ),AL2(18+5,19),C'</pidx:TaxTypeCode>'                    
         DC    AL1(EORQ),AL2(4+18+5+19)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,16),C'<pidx:TaxAmount>'                          
         DC    AL1(EORQ),AL2(4+16)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,21),C'<pidx:MonetaryAmount>'                     
         DC    AL1(CONQ),AL2(21,4),C'0.00'                                      
         DC    AL1(CONQ),AL2(21+4,22),C'</pidx:MonetaryAmount>'                 
         DC    AL1(EORQ),AL2(4+21+4+22)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,19),C'<pidx:CurrencyCode>'                       
         DC    AL1(CONQ),AL2(19,3),C'USD'                                       
         DC    AL1(CONQ),AL2(22,20),C'</pidx:CurrencyCode>'                     
         DC    AL1(EORQ),AL2(4+19+3+20)                                         
*                                                                               
         DC    AL1(CONQ),AL2(0,17),C'</pidx:TaxAmount>'                         
         DC    AL1(EORQ),AL2(4+17)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,11),C'</pidx:Tax>'                               
         DC    AL1(EORQ),AL2(4+11)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,22),C'</pidx:InvoiceSummary>'                    
         DC    AL1(EORQ),AL2(4+22)                                              
*                                                                               
         DC    AL1(CONQ),AL2(0,15),C'</pidx:Invoice>'                           
         DC    AL1(EORQ),AL2(4+15)                                              
*                                                                               
**                                                                              
**  END OF INVOICE DATA                                                         
**                                                                              
         DC    AL1(EOTQ,EOTQ)                                                   
*                                                                               
YNCVEND  DS    0X       END OF INVOICES                                         
**       DC    AL1(ROUQ),AL2(YNCVENDQ)                                          
**OLD    DC    AL1(CONQ),AL2(0,19),C'</pidx:DataObjects>'                       
         DC    AL1(CONQ),AL2(0,18),C'</dds:DataObjects>'                        
         DC    AL1(EORQ),AL2(4+18)                                              
         DC    AL1(EOTQ,EOTQ)                                                   
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
EIWBFQ   EQU   4            . NO WB FLIGHT (NON-THEATRICAL)                     
EINOTR   EQU   5            . DIVISION/PRD GRP NOT RETAIL                       
*                                                                               
EITAB    DS    0H                      ERROR INFO TABLE                         
**NOP    DC    AL1(0),AL1(16),C'INVOICE INCLUDED'                               
         DC    AL1(EIZEROQ),AL1(12),C'ZERO INVOICE'                             
         DC    AL1(EIMUFQ),AL1(23),C'MISSING USER FIELD DATA'                   
         DC    AL1(EIREGQ),AL1(19),C'MISSING REGION DATA'                       
         DC    AL1(EIWBFQ),AL1(14),C'NON-THEATRICAL'                            
         DC    AL1(EINOTR),AL1(49),C'DIV/PRD GRP NOT RETAIL AND NOT CORX        
               P. AND NOT LUB.'                                                 
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
                                                                                
MXLIN    EQU   1000                                                             
MXMIT    EQU   13        12 MTHS + TOTAL                                        
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
AEDINME  DS    AL4                 IF FIRST BYTE IS C'T' = TEST                 
*                                  DATASET NAME WILL START WITH                 
*                                  SPTFDISK.TEST (INSTEAD OF .PROD)             
COMFAC   DS    AL4                                                              
DYNALLOC DS    AL4                                                              
ADMASTC  DS    AL4                                                              
APLINE   DS    AL4                                                              
AREPORT  DS    AL4                                                              
PARMSLNQ EQU   *-PARMS                                                          
*                                                                               
SAVR1    DS    F                                                                
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
MQMAPNM  DS    CL14                SFTPDISK.PROD                                
*                                                                               
DSNAME   DS    CL35  DSN -  M2.WBBILL.SYS.AGID.DYYYMMDD.THHMMSS                 
*                            (SYS=SYSTEM,AGID=4-CHARACTER AGY ID)               
*                                                                               
MAPNME   DS    0CL20               (I.E. SPTTAPE.SP0EMXX)                       
MAPTAPE  DS    CL7                 SPTTAPE                                      
         DS    CL1                 .                                            
MAPSYS   DS    CL3                 SP0                                          
MAPPGM   DS    CL2                 EB                                           
MAPAGY   DS    CL2                 XX                                           
MAPSUFX  DS    CL1                 SUFFIX                                       
         DS    CL4                 N/D                                          
*                                                                               
SYSTM    DS    CL1                 SYSTEM                                       
CLI      DS    CL3                 CLIENT                                       
MEDIA    DS    CL2                 MEDIA                                        
MEDDESC  DS    CL10                MEDIA DESCRIPTION                            
*                                                                               
PCODE1   DS    CL5             PRD GROUP LEVEL 1 CODE  (DIV FOR PRT)            
PNAME1   DS    CL(L'P1NAME)    PRD GROUP LEVEL 1 NAME                           
PCODE2   DS    CL5             PRD GROUP LEVEL 2 CODE                           
PNAME2   DS    CL(L'P2NAME)    PRD GROUP LEVEL 2 NAME                           
PRCODE1  DS    CL(L'PRCODE)        PRCODE                                       
PRNAME1  DS    CL(L'PRNAME)        PRNAME                                       
ESCODE1  DS    CL(L'ESCODE)        ESCODE                                       
ESNAME1  DS    CL(L'ESNAME)        ESNAME                                       
ESNAME2  DS    CL(L'ESPNME2)       ESNAME                                       
*                                                                               
CTODAY   DS    CL8                                                              
         ORG   CTODAY                                                           
TYYYY    DS    CL4                 TODAY'S YEAR,MONTH,DAY                       
TMM      DS    CL2                                                              
TDAY     DS    CL2                                                              
*                                                                               
TIMEOFD  DS    CL8                 TIME OF DAY HH.MM.SS                         
*                                                                               
FULL     DS    F                                                                
MYFULL   DS    F                                                                
SAVER1   DS    F                                                                
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
INPRVGRS DS    PL(BKLQ)            ID PREV  BILLED  - GRS                       
INORDNET DS    PL(BKLQ)            ID       ORDERED - NET                       
INPRVNET DS    PL(BKLQ)            ID PREV  BILLED  - NET                       
INORDCSD DS    PL(BKLQ)            ID       ORDERED - CD                        
INPRVCSD DS    PL(BKLQ)            ID PREV  BILLED  - CD                        
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
SPACES   DS    CL132                                                            
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
FILESW   DS    CL1                                                              
RTNROU   DS    C                   RETURN TO ROUTINE 'Y' OR 'N'                 
PRTNROU  DS    C                   RETURN TO ROUTINE 'Y' OR 'N'                 
SVRTNROU DS    C                   SAVED RTNROU                                 
RTNADR   DS    F                   A(START OF MAP DATA)                         
PREVADR  DS    F                   A(START OF MAP DATA)                         
*                                                                               
INVDRCR  DS    CL1                 C= CREDIT INV, D= DEBIT INV                  
INVSIGN  DS    CL1                 += CREDIT INV, -= DEBIT INV                  
LINDRCR  DS    CL1                 C= CREDIT LIN, D= DEBIT LIN                  
LINSIGN  DS    CL1                 += CREDIT INV, -= DEBIT INV                  
COMDRCR  DS    CL1                 C= CREDIT COM, D= DEBIT COM                  
COMSIGN  DS    CL1                 += CREDIT INV, -= DEBIT INV                  
*                                                                               
HDRSENT  DS    CL1                 Y=HEADER HAS BEEN SENT                       
*                                                                               
FIRSTINV DS    CL1                 USED BY CHEVRON                              
*                                                                               
SKIPSET  DS    CL1                                                              
SKIPINV  DS    CL1                                                              
SKIPEIN  DS    XL1                                                              
SKIPCNT  DS    XL2                                                              
SKIPAMT  DS    PL8                                                              
PIDCNT   DS    PL8                 TEMPORARY COUNTER                            
INSCNT   DS    PL8                 TEMPORARY INSERTIONS COUNTER                 
MYDUB    DS    PL8                                                              
FILEOPT  DS    CL1                 SAVED FROM AGCFOPT - P= PRT/SPTTAPE          
*                                  INSTEAD SFTPDISK                             
*                                                                               
ANYTAX   DS    CL1                                                              
INTGONLY DS    CL1                 Y - IF INTEGRATION ONLY (NETPAK)             
PASSCON  DS    CL1                                                              
*                                                                               
AIDPITM  DS    A                   A(CURRENT ID RECORD DETAIL)                  
IDPCNT   DS    F                   NUMBER IN LINE ITEM TABLE                    
ALINITM  DS    A                   A(CURRENT LINE ITEM DETAIL)                  
LINCNT   DS    F                   NUMBER IN LINE ITEM TABLE                    
AMITITM  DS    A                   A(CURRENT MONTHLY INVOICE TOTAL)             
MITCNT   DS    F                   NUMBER IN MONTHY INVOICE TABLE               
*                                                                               
AGYALPHA DS    CL2                 AGENCY APHHA                                 
AGYNAME  DS    CL40                AGENCY NAME                                  
AGYNAM33 DS    CL33                AGENCY NAME - FROM BILLING                   
CHVFLAGS DS    CL1                 CHEVRON FLAGS                                
CHVACTV  EQU   X'80'               PROCESSING CHEVRON FOR H7                    
CHVDTL   EQU   X'40'               ALREADY PROCESSED FIRST DETAIL               
*                                                                               
         DS    CL13                SPARE                                        
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
VNDNAME  DS    CL60               PUB AND CODE                                  
VNDNAMC  DS    CL60                                                             
IDATSTA  DS    CL(L'PIDDESC)      INSERTION DATE OR STATION                     
*                                                                               
INVNUMB  DS    CL(L'IHINVN)                                                     
INVDATE  DS    CL(L'IHINVD)                                                     
INVDDATE DS    CL(L'IHDUED)                                                     
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
PO#      DS    CL10                                                             
MVEBCAN  DS    CL16           MEDIAVEST KRAFT EBCANUMBER (EST USER2)            
*                                                                               
CHVE1    DS    CL25           CHEVRON SERVICE ORDER #  (EST USER 1)             
CHVP1    DS    CL32           CHEVRON PLANT CODE       (PRD USER 1)             
CHVUCE1  DS    CL8            CHEVRON SERVICE ENTRY APPROVER UCOMM E1           
*                                                                               
MDZUE1   DS    CL32           MONDELEZ UCOMM E1                                 
MDZUE2   DS    CL16           MONDELEZ UCOMM E2                                 
*                                                                               
WBFLT    DS    CL10                                                             
BILLCM   DS    CL80                                                             
*                                                                               
YRMRKT   DS    CL(L'ITMMCY+L'MRKTNAM+1)                                         
*                                                                               
*        NEXT 3 FIELDS SAVED FROM AMOUNT DUE RECORD                             
*                                                                               
BASIS    DS    CL1                                                              
ABASIS   DS    CL1                 PCT OF BASIS                                 
BASISPCT DS    CL7                 FORMULA %                                    
*                                                                               
ABIGLINE DS    A                   A(LINE WITH BIGGEST COMMISSION)              
*                                                                               
TRADESW  DS    CL1                 SET TO Y IF SPOT TRADE BILL                  
*                                  (NOW ONLY FOR CHEVRON)                       
ITMCNT   DS    PL4                 ITEM COUNT                                   
CITMCNT  DS    CL6                                                              
*                                                                               
INVDCYMD DS    CL8                 INVOICE DATE CCYYMMDD                        
         ORG   INVDCYMD                                                         
CCYY     DS    CL4                 CCYY                                         
MM       DS    CL2                 MM  (MONTH)                                  
DD       DS    CL2                 DD  (DAY)                                    
DUEDCYMD DS    CL8                 INVOICE DUE DATE CCYYMMDD                    
DUEDMDY  DS    CL10                INVOICE DUE DATE MM/DD/YYYY                  
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
CDUEAMT  DS    CL(L'TDSAMT)        CHARACTER AMT DUE                            
CNETAMT  DS    CL(L'CTPUP)         CHARACTER NET AMOUNT                         
CCOMAMT  DS    CL(L'CTPUP)         CHARACTER COMMISSION AMOUNT                  
CDUENET  DS    CL(L'TDSAMT)        CHARACTER DUE NET                            
CDUENETP DS    CL(L'TDSAMT)        CHARACTER DUE NET - POSITIVE                 
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
SVPO#    DS    CL(L'PO#)           SAVED PO# (BEFORE DASHES REMOVED)            
*                                                                               
ORCLDUE  DS    CL20                FROM CLINDUE WITH </VALUE>                   
ORCDNET  DS    CL20                FROM CDUENET WITH </VALUE>                   
ORCDCOM  DS    CL20                FROM CDUECOM WITH </VALUE>                   
*                                                                               
*   CHEVRON DATA                                                                
*                                                                               
CHVLDUE  DS    CL35                FROM CLINDUE                                 
CHVDNET  DS    CL35                FROM CDUENET                                 
CHVDCOM  DS    CL35                FROM CDUECOM                                 
         DS    CL1    NOT SPARE                                                 
CHVPO#   DS    CL53   25 + </pidx... (27+1)                                     
         DS    CL1    NOT SPARE                                                 
CHVPLNTC DS    CL61   32 + </pidx... (29+1)                                     
*                                                                               
         DS    CL1    NOT SPARE                                                 
CHVSEA   DS    CL34   8 + </pidx... (25+1)                                      
         DS    CL1    NOT SPARE                                                 
CHVINVL  DS    CL150  USED IN INVOICE HEADER                                    
*                                                                               
CHVSPLR  DS    CL10   CHEVRON SUPPLIER ID                                       
*                                                                               
*        WARNER BROTHERS XML SPECIAL FIELDS                                     
*                                                                               
WBFDDUE  DS    CL38              FROM CDUENET WITH </InvoiceItem...             
WBFDNET  DS    CL25              FROM CDUENET WITH </NetAmount>                 
WBFDITG  DS    CL33              FROM CDUENET WITH </IntegrationAmount>         
WBFDCOM  DS    CL31              FROM CDUECOM WITH </CommissionAmount>          
WBFDTOT  DS    CL26                TOTAL OF DUEAMTS WITH </BatchTotal>          
*                                                                               
         EJECT                                                                  
AGYDATA  DS    0X                  SPECIAL AGENCY DATA                          
*                                                                               
ORCDATA  DS    0X                  SPECIAL STARCOM ORACLE DATA                  
ORCPO#   DS    CL24                PO# - DASHES + </DOCU...                     
ORCINVND DS    CL24                INVOICE NUMBER + </DOCU...                   
ORCINVNR DS    CL25                INVOICE NUMBER + </REFER...                  
ORCDESC  DS    CL165               DESCRIPTION                                  
*                                  MED/PRD/EST/MOS/PO# WITH </DESC..            
         DS    XL1                 GETS SET TO X'FF'                            
ORCREF   DS    CL20                REFERENCE NUMBER  (WITH </LIN...             
         DS    XL1                 GETS SET TO X'FF'                            
ORCPUB   DS    CL72                PUB + CODE + </DESC...                       
         ORG   AGYDATA                                                          
OMDDATA  DS    0X                                                               
OMACCT   DS    CL10                                                             
OMNUFDAT DS    CL(L'UDTXT)         ATTENTION NAME/USER FIELD DATA               
OMN1NAME DS    CL(L'M2NAMES)                                                    
OMGRS    DS    CL(L'IT1UP)         MCCANN GROSS(CHARACTER)                      
*                                                                               
         ORG   AGYDATA                                                          
*                                  SJR WBF DATA                                 
SJWBDATA DS    0X                                                               
WBFTOT   DS    PL8                 BATCH TOTAL                                  
WBTODAY  DS    CL10                TODAY AS MM/DD/YYYY                          
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
*                                                                               
         ORG   AGYDATA                                                          
*        CHEVRON XML SPECIAL FIELDS                                             
         DS    CL1      NOT SPARE                                               
CHVINVN  DS    CL35                                                             
         DS    CL1      NOT SPARE                                               
CHVDDUE  DS    CL38              FROM CDUENET WITH </pidx:Monetary...           
*                                                                               
         DS    CL1      NOT SPARE                                               
CHVFTR   DS    CL240             FIELD TICKET NUMBER                            
*                   CLT/PGR1/PGR2/PRD +NAME/EST +NAME +</pidx...                
         DS    CL1      NOT SPARE                                               
CHVPUB   DS    CL100                                                            
*                                                                               
         DS    CL1      NOT SPARE                                               
CHVDESC  DS    CL200                                                            
*                                                                               
         DS    CL1      NOT SPARE                                               
CHVITEM  DS    CL50     LINE ITEM NUMBER                                        
         DS    CL1      NOT SPARE                                               
CHVLITEM DS    CL50     LINE ITEM TOTAL                                         
         DS    CL1      NOT SPARE                                               
CHVFTR2  DS    CL240    FIELD TICKET NUMBER 2                                   
         DS    CL1      NOT SPARE                                               
CHVDESC2 DS    CL200                                                            
*                                                                               
         ORG   AGYDATA                                                          
*  MediaVest Kraft data                                                         
         DS    CL1               NOT SPARE                                      
MVKTE2   DS    CL38              EBCANUMBER with </eBCANumber...                
         DS    CL1               NOT SPARE                                      
MVKDDUE  DS    CL38              FROM CDUENET WITH </InvAmount..                
         DS    CL1               NOT SPARE                                      
MVKTTAX  DS    CL38     CANADIAN TAX TOTAL WITH </TAX-HST>                      
         DS    CL1               NOT SPARE                                      
MVKTDUE  DS    CL38              FROM TOTDUEC WITH </AmountDue..                
         DS    CL1               NOT SPARE                                      
MVKESTN  DS    CL60              ESTIMATE NAME WITH </EstName>                  
         DS    CL140               SPARE JUST IN CASE                           
*                                                                               
         ORG   AGYDATA                                                          
*  Carat Mondelez data                                                          
         DS    CL1               NOT SPARE                                      
UBMZE1   DS    CL38              REQNUMBER with </ReqNumber...                  
         DS    CL1               NOT SPARE                                      
UBMZE2   DS    CL38              EBCANUMBER with </eBCANumber...                
         DS    CL1               NOT SPARE                                      
UBMZDDUE DS    CL38              FROM CDUENET WITH </InvAmount..                
         DS    CL1               NOT SPARE                                      
UBMZTAX  DS    CL38     CANADIAN TAX TOTAL WITH </TAX-HST>                      
         DS    CL1               NOT SPARE                                      
UBMZDUE  DS    CL38              FROM TOTDUEC WITH </AmountDue..                
         DS    CL1               NOT SPARE                                      
UBMZESTN DS    CL60              ESTIMATE NAME WITH </EstName>                  
         DS    CL140               SPARE JUST IN CASE                           
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
AGCFOPT  DS    CL1                 FILE OPTION P-PRTTAPE                        
*                                  BLANK= SFTPDISK                              
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
IDPDATE  DS    CL11                INSERTION DATE                               
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
IDPREF   DS    CL10                REFERENCE NUMBER                             
IDPMISC  DS    CL50                MISC DATA                                    
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
MITTYP   DS    CL1                 TYPE                                         
MITLNQ   EQU   *-MITD                                                           
         EJECT                                                                  
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
IDREF    DS    CL10                REFERENCE NUMBER                             
IDMISC   DS    CL50                MISC. DATA FOR INSERTION                     
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
                                                                                
* DSECT FOR N4  SEGMENT                                                         
N4D      DSECT                     GEOGRAPHIC LOCATION                          
N4ID     DS    CL3                 ID                                           
N4SEQ    DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
N4CITY   DS    CL30                CITY NAME                                    
N4STATE  DS    CL2                 STATE                                        
                                                                                
                                                                                
* DSECT FOR DTM SEGMENT                                                         
DTMD     DSECT                     DATE/TIME REFERENCE                          
DTMID    DS    CL3                 ID                                           
DTMSEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
DTMDTQ   DS    CL3                 DATE/TIME QUALIFIER                          
DTMDATE  DS    CL8                 DATE                                         
         DS    CL8                 TIME                                         
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
                                                                                
* DSECT FOR SAC SEGMENT                                                         
SACD     DSECT                     SERVICE                                      
SACID    DS    CL3                 ID                                           
SACSEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
SACACI   DS    CL1                 ALLOWANCE OR CHARGE INDICATOR                
SACCODE  DS    CL4                 SERV. PROMO. ALLOW. OR CHARGE CODE           
         DS    CL2                 AGENCY QUALIFIER CODE                        
         DS    CL10                AGENCY SERV. PROMO.  ALLOW. OR C C           
SACNDP   DS    CL1                 NUMBER OF DECIMAL PLACES                     
SACAMT   DS    CL15                AMOUNT                                       
                                                                                
* DSECT FOR CTT SEGMENT                                                         
CTTD     DSECT                     SERVICE                                      
CTTID    DS    CL3                 ID                                           
CTTSEQ   DS    CL3                 SEQUENCE                                     
         DS    CL5                 N/D                                          
CTTNDP   DS    CL1                 NUMBER OF DP                                 
CTTITM   DS    CL6                 NUMBER OF LINE ITEMS                         
         EJECT                                                                  
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
PLIDES   DS    CL50                ERROR DESCRIPTION                            
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
**PAN#1  DC    CL21'062DDXMLMAP  12/04/20'                                      
         END                                                                    
