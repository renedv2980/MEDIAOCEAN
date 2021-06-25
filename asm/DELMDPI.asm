*          DATA SET DELMDPI    AT LEVEL 007 AS OF 08/01/18                      
*PROCESS USING(WARN(15))                                                        
*PHASE DELMDPIA                                                                 
         TITLE 'DEMO CONVERSION'                                                
* WE DO NOT PUT OUT UNIVERSE RECORD - OUTPUT PHASE DOES NOT PRODUCE             
* THEM.  THE OLD CONVERSION HAD A BUG IN THE INPUT PHASE WHICH                  
* PREVENTED THE CREATION OF THE UNIVERSE RECORD.                                
*                                                                               
* ALSO I WILL NOT CREATE THE OLD STYLE, PUTS OR SHARES                          
* PROPOSER DOES NOT USE PUTS OF SHARES FOR DAYPART CUMES                        
DELMDP10 CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DELMDP10                                                       
         LR    RA,RB                                                            
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING DELMDP10+4096,RA                                                 
         USING DEMCOND,R8                                                       
         L     RC,ARREC                                                         
         USING WD_LMDSECT,RC                                                    
         L     R2,AIREC                                                         
         USING INTERD,R2                                                        
         B     *+4(R1)                                                          
         SPACE 2                                                                
         B     READ                GET INPUT (ARREC - INT)                      
         B     CNVWR               CONVERT TO OUTPUT (INT - WORK)               
         B     MORET               CLOSE INPUT                                  
         B     EXIT                                                             
         EJECT                                                                  
READ     CLI   INTAPESW,1                                                       
         BE    OPENOK                                                           
         OPEN  (IN1,(INPUT))                                                    
*                                                                               
*&&DO                                                                           
*                                                                               
* REMOVED BY DEIS: AUG/18. THIS IS BEING KEPT IN THE SOURCE CODE ONLY           
* TO SHOW AN EXAMPLE OF HOW THIS FLAG WORKS. WE NO LONGER WANT TO SET           
* THE FLAG, BECAUSE THE PREVIEW DATA HAS BEEN RETIRED BY NIELSEN, IN            
* FAVOR OF THE NEW "L7DMA" DATA.                                                
*                                                                               
*======================================================================         
* NOTE. FOR LPM DMA PREVIEW MARKETS:                                            
*   1. DELMICEDPT LEGITIMATELY GENERATES NO DATA. THEREFORE:                    
*   2. THE INPUT TO THIS PROGRAM WILL BE AN EMPTY DATASET. THEREFORE:           
*   3. AN EMPTY OTAPE DATASET IS GENERATED. THEREFORE:                          
*   4. DEMCNV WANTS TO SEND AN "EMPTY OTAPE GENERATED" WARNING E-MAIL.          
*                                                                               
* WE DO *NOT* WANT THAT WARNING E-MAIL SENT FOR LPM DMA PREVIEW DATA,           
* BECAUSE THE EMPTY OTAPE DATASET IS LEGITIMATE. SO WE SET A FLAG HERE          
* TO TELL DEMCNV TO INHIBIT THAT E-MAIL. NOTE THAT WE CANNOT TELL THE           
* DIFFERENCE IN THIS PROGRAM BETWEEN A LEGITIMATE EMPTY OTAPE DATASET           
* AND AN EMPTY OTAPE DATASET WHICH SHOULDN'T BE EMPTY. SO IT COULD              
* THEORETICALLY HAPPEN THAT WE WON'T GET A WARNING E-MAIL WHEN WE               
* REALLY DO WANT ONE. THAT'S A CALCULATED RISK WE'RE CHOOSING TO TAKE.          
*                                                                               
         OI    FLAGS1,EMPTY_OTAPE_OKAY   EMPTY OTAPE DATASET IS OKAY            
*======================================================================         
*&&                                                                             
         EJECT                                                                  
OPENOK   L     R4,ARREC                                                         
         CLI   BYPREAD,1                                                        
         BE    READOK                                                           
         GET   IN1,(R4)                                                         
*                                                                               
         L     RE,ANIINPUT         SET FOR RECORD COUNTS                        
         L     R0,0(RE)                                                         
         AH    R0,=H'1'                                                         
         ST    R0,0(RE)                                                         
*                                                                               
READOK   CLI   WD_RECTYPE,WD_RECTYPE_UER UNIVERSE RECORD                        
         BE    PROC_UNV                                                         
         CLI   WD_RECTYPE,WD_RECTYPE_CDPT DAYPART CUMES RECORD                  
         BE    PROCREC                                                          
         CLI   WD_RECTYPE,WD_RECTYPE_HPT HUT/PUT RECORD                         
         BE    PROCREC                                                          
*                                                                               
         DC    H'0'                                                             
         DC    CL12'UNKNOWN REC'                                                
*------------------ BUFFER THE UNIVERSE RECORD --------------------             
PROC_UNV DS    0H                                                               
         LA    R1,RRECL                                                         
         LA    R5,UERRECA                                                       
         MOVE  ((R5),(R1)),(R4)                                                 
* INITIALIZE DEMO BUFFER                                                        
         CLI   FRST,1                                                           
         BNE   PROCUV10                                                         
         GOTO1 VDEDPTAC,DMCB,(C'I',DEMCOND),MODLIST                             
         MVI   FRST,0                                                           
PROCUV10 L     RE,=A(STALIST)     CLEAR MARKET STATION LIST                     
         LA    RF,1400                                                          
         XCEF                                                                   
*                                                                               
         MVC   FULL(2),WD_MHQRPRD       FIELDS ARE REVERSED ON TAPE             
         MVC   FULL+2(2),WD_MHQRYEAR+2  FOR SURVEY PERIOD                       
         LA    R1,FULL                                                          
         BAS   R9,CNVBK                                                         
         MVC   NSIBOOK,HALF        SET UP SURVEY PERIOD                         
         B     OPENOK                                                           
*                                                                               
* CONVERT NSI BOOKS TO DDS BOOKS                                                
CNVBK    XC    HALF,HALF           BOOK YEAR                                    
         CLI   0(R1),C' '          MAKE SURE IT IS  PRESENT                     
         BER   R9                                                               
         PACK  DUB,2(2,R1)                                                      
         CVB   RE,DUB                                                           
         LA    RE,100(RE)          ADJUST FOR 2000                              
         STC   RE,HALF                                                          
         PACK  DUB,0(2,R1)        BOOK MONTH                                    
         CVB   RE,DUB                                                           
         STC   RE,HALF+1                                                        
         BR    R9                                                               
*                                                                               
         EJECT                                                                  
*------------------------------------------------------------------             
         B     EXIT                                                             
PROCREC  DS    0H                                                               
         MVC   INTRSTAT,WD_DHRREPST  SET THE REPORTABILTY STATUS                
*        DROP  RF                                                               
*                                                                               
GETM1S3  MVC   INTMKTCL,WD_MHQMKCLS                                             
         MVI   INTDMA,C'N'                                                      
         CLC   WD_DHRMOOC,SPACES                                                
         BE    *+8                                                              
         MVI   INTDMA,C'Y'                                                      
*                                                                               
         MVC   INTTZ,WD_MHQMKTZ+1                                               
         CLI   INTTZ,C'5'          FORCE ALASKA TO 6                            
         BNE   *+8                                                              
         MVI   INTTZ,C'6'                                                       
*                                                                               
         MVC   INTAFFL,WD_DHRAFFL1                                              
*                                                                               
* DIG OUT THE AFFILIATE                                                         
         LA    RF,INTAFFL          INTERIM AFFILIATE FIELD                      
         LA    RE,WD_DHRAFFL1      INPUT AFFILIATE FIELD                        
         LA    R0,L'WD_DHRAFFL1     L'INPUT                                     
         LA    R1,L'INTAFFL        L'OUTPUT                                     
GETNET   CLI   0(RE),C' '          SEARCH FOR SIGNIFICANT CHARACTERS            
         BNE   *+16                FOUND                                        
GETNET2  LA    RE,1(RE)                                                         
         BCT   R0,GETNET           TRY NXT                                      
         B     GETNETX             END OF INPUT                                 
         SPACE 2                                                                
         MVC   0(1,RF),0(RE)       SEND TO OUTPUT                               
         LA    RF,1(RF)                                                         
         BCT   R1,GETNET2          NEXT                                         
GETNETX  DS    0C                                                               
*                                                                               
         SPACE 2                                                                
* THIS NEEDS TO BE REVISITED LATER                                              
***      OC    WD_DHRBCHAN,ZEROS       CONVERT THE CHANNEL                      
***      PACK  DUB,WD_DHRBCHAN                                                  
***      CVB   RE,DUB                                                           
***      STC   RE,INTCHNL                                                       
*                                                                               
         MVC   INTSTA(4),WD_DHROCALL                                            
         MVI   INTSTA+4,C'T'                                                    
         CLI   WD_RECTYPE,WD_RECTYPE_HPT HUT/PUT RECORD                         
         BNE   NOHUT                                                            
         XC    DUB,DUB                                                          
         PACK  DUB,WD_MHQMKTC                                                   
         CVB   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  INTSTA(4),DUB                                                    
         MVI   INTSTA+4,C'T'                                                    
         MVI   INTSTYP,PARENTE                                                  
*                                                                               
NOHUT    XC    DUB,DUB                                                          
         PACK  DUB,WD_MHQMKTC    CONVERT MARKET NUMBER                          
         CVB   RE,DUB                                                           
         STCM  RE,3,INTMRKT                                                     
         BAS   RE,FLTMKT                                                        
         CLI   PASSFLT,C'Y'                                                     
         BNE   OPENOK                                                           
         MVC   INTBOOK,NSIBOOKY                                                 
*                                                                               
* SET INTERIM RECORD INFO                                                       
SETDPT   MVC   INTPNAM,WD_PNRPNAME                                              
         MVC   INTDAY,WD_CDRKDSEQ                                               
         MVC   INTDPT,WD_CDRIDPTC                                               
         MVC   INTDAYWK,WD_CDRKDSEQ                                             
         MVC   INTSQH,WD_CDRKDSEQ+1                                             
         MVC   INTEQH,WD_CDRKDSEQ+2                                             
*                                                                               
         CLI   WD_DHRSTYPE,C'1'                                                 
         BNE   *+8                                                              
         MVI   INTSTYP,PARENTE                                                  
         CLI   WD_DHRSTYPE,C'2'                                                 
         BNE   *+8                                                              
         MVI   INTSTYP,PARENTE                                                  
         CLI   WD_DHRSTYPE,C'4'                                                 
         BNE   *+8                                                              
         MVI   INTSTYP,PARENTE                                                  
         CLI   WD_DHRSTYPE,C'5'                                                 
         BNE   *+8                                                              
         MVI   INTSTYP,PS1E                                                     
*                                                                               
         CLI   WD_DHRSTYPE,C'8'    THIS ONE IS NEW - NEVER HAD                  
         BNE   *+8                 SATELITE ONLY                                
         MVI   INTSTYP,SATELITE                                                 
         CLI   WD_DHRSTYPE,C'9'                                                 
         BNE   *+8                                                              
         MVI   INTSTYP,OUTMARE                                                  
*                                                                               
* FORCE KTNC P+S1 TO SPILL FOR SACRAMENTO HISPANIC                              
         CLI   INTSTYP,PS1E                                                     
         BNE   STYPOK                                                           
         CLC   INTMRKT,=H'462'     SACRAMENTO                                   
         BNE   STYPOK                                                           
         CLC   INTSTA(4),=C'KTNC'  NHSI MISCODING - HOME IS SANFRAN             
         BNE   STYPOK                                                           
         MVI   INTSTYP,OUTMARE     FORCE IT TO OUTSIDE                          
STYPOK   DS    0H                                                               
CHKMTROB CLC   INTMRKT,=H'178'     METRO B MARKETS                              
         BNE   *+8                                                              
         MVI   INTMTYP,METROBE                                                  
         CLC   INTMRKT,=H'253'                                                  
         BNE   *+8                                                              
         MVI   INTMTYP,METROBE                                                  
         CLC   INTMRKT,=H'254'                                                  
         BNE   *+8                                                              
         MVI   INTMTYP,METROBE                                                  
*                                                                               
*CHECK FOR FIRST TIME FOR STATION                                               
         L     R9,=A(STALIST)                                                   
CHKSTA   CLI   0(R9),0             END OF LIST                                  
         BNE   CHKSTA2              NO TRY NEXT                                 
         MVI   BYPREAD,1            YES - INSERT AND SEND 'M' RECORD            
         MVC   0(5,R9),INTSTA                                                   
         MVI   STASW,1                                                          
         BAS   RE,SETKEY                                                        
         B     EXIT                                                             
CHKSTA2  CLC   0(5,R9),INTSTA      STATION IN LIST                              
         BE    *+12                 YES - DO NORMAL CONVERSION                  
         LA    R9,5(R9)                                                         
         B     CHKSTA                                                           
         MVI   STASW,0             RESET CREATE 'M' RECORD SWITCHES             
         MVI   BYPREAD,0                                                        
         MVI   INTWEEKS,X'0F'      SET UP ACTIVE WEEKS                          
                                                                                
*                                                                               
*----------------FORMAT INTACCS WITH DEMOS -------------------------            
*                                                                               
         LA    R4,CDMA2INT         TABLE CONTROL DMA                            
         LA    R7,WD_QDRDEMOS      DMA IMPS OR DMA PUTS                         
         MVI   PROC_FLAG,DMA_DEMOS                                              
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
*                                                                               
         MVI   PROC_FLAG,TSA_DEMO                                               
         LA    R4,CTSA2INT         TABLE CONTROL TSA                            
         LA    R7,WD_STRDEMOS      TSA IMPS OR TSA PUTS                         
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
*                                                                               
         MVI   PROC_FLAG,CUME_DEMO                                              
         LA    R4,CCUM2INT         TABLE CONTROL CUMES                          
* LPM AND SET METERED MARKERS USES 4 WEEK CUMES                                 
         SELECT CLI,WD_MHQCMETH,EQ                                              
           WHEN (W_COLLECTION_METHOD_SET_METER,                                 
                 W_COLLECTION_METHOD_LPM_PPM,                                   
                 W_COLLECTION_METHOD_SET_METER_PPM,                             
                 W_COLLECTION_METHOD_SET_METER_RPD,                             
                 W_COLLECTION_METHOD_LPM)                                       
             LA  R7,WD_CDR4WMTROA    LPM USE 4 WEEK  CUMES                      
           OTHRWISE ,                                                           
             LA  R7,WD_CDRDEMOS      CUMES IN -AVG WEEK CUMES                   
         ENDSEL ,                                                               
*                                                                               
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
*                                                                               
         MVI   PROC_FLAG,UNIV_DEMO                                              
         LA    R4,CUNV2INT         TABLE CONTROL UNIVERSE                       
         LA    R6,INTACCS                                                       
         LA    R7,UERRECA+(WD_CDRDEMOS-WD_REC)                                  
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
*                                                                               
         BAS   RE,BUILDDEM                                                      
*----------------------------------------------------------------------         
*    FORMAT INTACCS WITH THE CALCULATED 1217 AND 1824 CATEGORIES                
         MVI   PROC_FLAG,WRK_DEMOS                                              
         LA    R4,CWRK2INT         TABLE CONTROL REST OF WORK DEMOS             
         LA    R6,INTACCS                                                       
         LA    R7,WK_DEMOS                                                      
         LA    R6,INTACCS          OUT                                          
         BAS   RE,CNVRTOI                                                       
                                                                                
*                                                                               
         BAS   RE,SETKEY                                                        
         TM    INTSTA,X'F0'        ONLY BUFFER THE MARKET TOTALS                
         BNO   EXIT                                                             
         GOTO1 VDEDPTAC,DMCB,(C'P',DEMCOND)                                     
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
**********************************************************************          
*  BUILD REST OF THE DEMOS TO CONFORM WITH THE OLD SUPER DEMOS                  
*  THE OLD VIP USED TO GIVE US                                                  
**********************************************************************          
BUILDDEM NTR1                                                                   
         ZICM  RE,UERRECA+(WD_CDRAWHHLDS-WD_REC),(15)                           
         STCM  RE,15,WK_UHOMES                                                  
         ZICM  RE,UERRECA+(WD_CDRAWMTROA-WD_REC),(15)                           
         STCM  RE,15,WK_UMETROA                                                 
         ZICM  RE,UERRECA+(WD_CDRAWMTROB-WD_REC),(15)                           
         STCM  RE,15,WK_UMETROB                                                 
         ZICM  RE,UERRECA+(WD_CDRAWWW-WD_REC),(15)                              
         STCM  RE,15,WK_UWWRK                                                   
*                                                                               
                                                                                
* CALCULATE RHOMES                                                              
         ZICM  R2,UERRECA+(WD_CDRAWHHLDS-WD_REC),(15)   UNIV                    
         ZICM  RF,WD_QDRHHLDS,(15)    DHOMES                                    
         OR    R2,R2                                                            
         BZ    BLDDEM15                                                         
         XC    WORK,WORK                                                        
         XC    DUB1,DUB1                                                        
         CVD   RF,DUB1                HOMES                                     
         OI    DUB1+7,X'0C'                                                     
         MVC   WORK(4),=X'0100000C'     *100000                                 
         MP    DUB1(8),WORK(4)                                                  
         XC    WORK,WORK                                                        
         MVC   WORK+8(8),DUB1                                                   
         XC    DUB1,DUB1                                                        
         CVD   R2,DUB1                                                          
         OI    DUB1+7,X'0C'                                                     
         DP    WORK(16),DUB1(8)                                                 
         MVC   DUB1(8),WORK                                                     
         CVB   RF,DUB1                                                          
         STCM  RF,15,WK_RHOMES                                                  
*                                                                               
* CALCULATE RMETROA                                                             
BLDDEM15 ZICM  R2,UERRECA+(WD_CDRAWMTROA-WD_REC),(15)   UNIV                    
         ZICM  RF,WD_QDRMTROA,(15)    DMETROA                                   
         OR    R2,R2                                                            
         BZ    BLDDEM20                                                         
         XC    WORK,WORK                                                        
         XC    DUB1,DUB1                                                        
         CVD   RF,DUB1                HOMES                                     
         OI    DUB1+7,X'0C'                                                     
         MVC   WORK(4),=X'0100000C'     *100000                                 
         MP    DUB1(8),WORK(4)                                                  
         XC    WORK,WORK                                                        
         MVC   WORK+8(8),DUB1                                                   
         XC    DUB1,DUB1                                                        
         CVD   R2,DUB1                                                          
         OI    DUB1+7,X'0C'                                                     
         DP    WORK(16),DUB1(8)                                                 
         MVC   DUB1(8),WORK                                                     
         CVB   RF,DUB1                                                          
         STCM  RF,15,WK_RMETROA                                                 
*                                                                               
* CALCULATE RMETROB                                                             
BLDDEM20 ZICM  R2,UERRECA+(WD_CDRAWMTROB-WD_REC),(15)   UNIV                    
         ZICM  RF,WD_QDRMTROB,(15)    DMETROB                                   
         OR    R2,R2                                                            
         BZ    BLDDEM25                                                         
         XC    WORK,WORK                                                        
         XC    DUB1,DUB1                                                        
         CVD   RF,DUB1                HOMES                                     
         OI    DUB1+7,X'0C'                                                     
         MVC   WORK(4),=X'0100000C'     *100000                                 
         MP    DUB1(8),WORK(4)                                                  
         XC    WORK,WORK                                                        
         MVC   WORK+8(8),DUB1                                                   
         XC    DUB1,DUB1                                                        
         CVD   R2,DUB1                                                          
         OI    DUB1+7,X'0C'                                                     
         DP    WORK(16),DUB1(8)                                                 
         MVC   DUB1(8),WORK                                                     
         CVB   RF,DUB1                                                          
         STCM  RF,15,WK_RMETROB                                                 
*                                                                               
*----------------CREATE 1217 AND 1824 CATEGORIES -------------------            
* NEED UNIVERSE FOR  1217 AND U1824                                             
BLDDEM25 ZICM  RE,UERRECA+(WD_CDRAWM1214-WD_REC),(15)                           
         ZICM  RF,UERRECA+(WD_CDRAWM1517-WD_REC),(15)                           
         AR    RE,RF                                                            
         STCM  RE,15,WK_UM1217                                                  
         ZICM  RE,UERRECA+(WD_CDRAWF1214-WD_REC),(15)                           
         ZICM  RF,UERRECA+(WD_CDRAWF1517-WD_REC),(15)                           
         AR    RE,RF                                                            
         STCM  RE,15,WK_UW1217                                                  
*                                                                               
         ZICM  RE,UERRECA+(WD_CDRAWM1820-WD_REC),(15)                           
         ZICM  RF,UERRECA+(WD_CDRAWM2124-WD_REC),(15)                           
         AR    RE,RF                                                            
         STCM  RE,15,WK_UM1824                                                  
         ZICM  RE,UERRECA+(WD_CDRAWF1820-WD_REC),(15)                           
         ZICM  RF,UERRECA+(WD_CDRAWF2124-WD_REC),(15)                           
         AR    RE,RF                                                            
         STCM  RE,15,WK_UW1824                                                  
                                                                                
* BUILD UM1834, W1834=1824+2534                                                 
         ZICM  RE,WK_UM1824,(15)                                                
         ZICM  RF,UERRECA+(WD_CDRAWM2534-WD_REC),(15)                           
         AR    RE,RF         WK_CM1824 ALREADY MULTPLED BY 10                   
         STCM  RE,15,WK_UM1834                                                  
         ZICM  RE,WK_UW1824,(15)                                                
         ZICM  RF,UERRECA+(WD_CDRAWF2534-WD_REC),(15)                           
         AR    RE,RF                                                            
         STCM  RE,15,WK_UW1834                                                  
*                                                                               
* BUILD CM18349 W1849=1834+3549                                                 
         ZICM  RE,WK_UM1834,(15)                                                
         ZICM  RF,UERRECA+(WD_CDRAWM3549-WD_REC),(15)                           
         AR    RE,RF         WK_CM1824 ALREADY MULTPLED BY 10                   
         STCM  RE,15,WK_UM1849                                                  
         ZICM  RE,WK_UW1834,(15)                                                
         ZICM  RF,UERRECA+(WD_CDRAWF3549-WD_REC),(15)                           
         AR    RE,RF                                                            
         STCM  RE,15,WK_UW1849                                                  
*                                                                               
* BUILD UM2549, W2549=2534+3549                                                 
         ZICM  RE,UERRECA+(WD_CDRAWM2534-WD_REC),(15)                           
         ZICM  RF,UERRECA+(WD_CDRAWM3549-WD_REC),(15)                           
         AR    RE,RF                                                            
         STCM  RE,15,WK_UM2549                                                  
         ZICM  RE,UERRECA+(WD_CDRAWF2534-WD_REC),(15)                           
         ZICM  RF,UERRECA+(WD_CDRAWF3549-WD_REC),(15)                           
         AR    RE,RF                                                            
         STCM  RE,15,WK_UW2549                                                  
* BUILD CM2554, W2549=2549+5054                                                 
         ZICM  RE,WK_UM2549,(15)                                                
         ZICM  RF,UERRECA+(WD_CDRAWM5054-WD_REC),(15)                           
         AR    RE,RF         WK_CM2549 ALREADY MULTPLED BY 10                   
         STCM  RE,15,WK_UM2554                                                  
         ZICM  RE,WK_UW2549,(15)                                                
         ZICM  RF,UERRECA+(WD_CDRAWF5054-WD_REC),(15)                           
         AR    RE,RF                                                            
         STCM  RE,15,WK_UW2554                                                  
*                                                                               
* BUILD CM18+, W18+=1824+2554+5564+65+                                          
*                                                                               
         ZICM  RE,WK_UM1824,(15)                                                
         ZICM  RF,WK_UM2554,(15)                                                
         AR    RE,RF                                                            
         ZICM  RF,UERRECA+(WD_CDRAWM5564-WD_REC),(15)                           
         AR    RE,RF                                                            
         ZICM  RF,UERRECA+(WD_CDRAWM65P-WD_REC),(15)                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_UM18O                                                   
         ZICM  RE,WK_UW1824,(15)                                                
         ZICM  RF,WK_UW2554,(15)                                                
         AR    RE,RF                                                            
         ZICM  RF,UERRECA+(WD_CDRAWF5564-WD_REC),(15)                           
         AR    RE,RF                                                            
         ZICM  RF,UERRECA+(WD_CDRAWF65P-WD_REC),(15)                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_UW18O                                                   
*                                                                               
* BUILD V2-11=V25+V611                                                          
         ZICM  RE,UERRECA+(WD_CDRAWCH25-WD_REC),(15)                            
         ZICM  RF,UERRECA+(WD_CDRAWCH611-WD_REC),(15)                           
         AR    RE,RF                                                            
         STCM  RE,15,WK_UV211                                                   
*                                                                               
* BUILD V1217=M1217+W1217                                                       
         ZICM  RE,WK_UM1217,(15)                                                
         ZICM  RF,WK_UW1217,(15)                                                
         AR    RE,RF                                                            
         STCM  RE,15,WK_UV1217                                                  
*                                                                               
* BUILD V1224=V1217+M1824+W1824                                                 
         ZICM  RE,WK_UV1217,(15)                                                
         ZICM  RF,WK_UM1824,(15)                                                
         AR    RE,RF                                                            
         ZICM  RF,WK_UW1824,(15)                                                
         AR    RE,RF                                                            
         STCM  RE,15,WK_UV1224                                                  
*                                                                               
* BUILD V2+=V211+V1217+M18O+W18O                                                
         ZICM  RE,WK_UV211,(15)                                                 
         ZICM  RF,WK_UV1217,(15)                                                
         AR    RE,RF                                                            
         ZICM  RF,WK_UM18O,(15)                                                 
         AR    RE,RF                                                            
         ZICM  RF,WK_UW18O,(15)                                                 
         AR    RE,RF                                                            
         STCM  RE,15,WK_UV2O                                                    
                                                                                
* DMA IMPRESSIONS                                                               
         ZICM  RE,WD_QDRM1214,(15)                                              
         ZICM  RF,WD_QDRM1517,(15)                                              
         AR    RE,RF                                                            
         STCM  RE,15,WK_DM1217                                                  
         ZICM  RE,WD_QDRF1214,(15)                                              
         ZICM  RF,WD_QDRF1517,(15)                                              
         AR    RE,RF                                                            
         STCM  RE,15,WK_DW1217                                                  
*                                                                               
         ZICM  RE,WD_QDRM1820,(15)                                              
         ZICM  RF,WD_QDRM2124,(15)                                              
         AR    RE,RF                                                            
         STCM  RE,15,WK_DM1824                                                  
         ZICM  RE,WD_QDRF1820,(15)                                              
         ZICM  RF,WD_QDRF2124,(15)                                              
         AR    RE,RF                                                            
         STCM  RE,15,WK_DW1824                                                  
*                                                                               
* NEED TSA IMPS FOR  1217 AND 1824                                              
         ZICM  RE,WD_STRM1214,(15)                                              
         ZICM  RF,WD_STRM1517,(15)                                              
         AR    RE,RF                                                            
         STCM  RE,15,WK_TM1217                                                  
         ZICM  RE,WD_STRF1214,(15)                                              
         ZICM  RF,WD_STRF1517,(15)                                              
         AR    RE,RF                                                            
         STCM  RE,15,WK_TW1217                                                  
*                                                                               
         ZICM  RE,WD_STRM1820,(15)                                              
         ZICM  RF,WD_STRM2124,(15)                                              
         AR    RE,RF                                                            
         STCM  RE,15,WK_TM1824                                                  
         ZICM  RE,WD_STRF1820,(15)                                              
         ZICM  RF,WD_STRF2124,(15)                                              
         AR    RE,RF                                                            
         STCM  RE,15,WK_TW1824                                                  
*                                                                               
* -------------CALCULATE CUME DEMOS ---------------                             
         CLI   WD_MHQCMETH,W_COLLECTION_METHOD_LPM                              
         BE    BLDDEM30                                                         
         CLI   WD_MHQCMETH,W_COLLECTION_METHOD_LPM_PPM                          
         BE    BLDDEM30                                                         
*                                                                               
* BUILD CM1217, W1217                                                           
         ZICM  RE,WD_CDRAWM1214,(15)                                            
         ZICM  RF,WD_CDRAWM1517,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CM1217                                                  
         ZICM  RE,WD_CDRAWF1214,(15)                                            
         ZICM  RF,WD_CDRAWF1517,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CW1217                                                  
                                                                                
* BUILD CM1824, W1824=1820+2124                                                 
         ZICM  RE,WD_CDRAWM1820,(15)                                            
         ZICM  RF,WD_CDRAWM2124,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CM1824                                                  
         ZICM  RE,WD_CDRAWF1820,(15)                                            
         ZICM  RF,WD_CDRAWF2124,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CW1824                                                  
                                                                                
* BUILD CM1834, W1834=1824+2534                                                 
         ZICM  RE,WK_CM1824,(15)                                                
         ZICM  RF,WD_CDRAWM2534,(15)                                            
         AR    RE,RF         WK_CM1824 ALREADY MULTPLED BY 10                   
         STCM  RE,15,WK_CM1834                                                  
         ZICM  RE,WK_CW1824,(15)                                                
         ZICM  RF,WD_CDRAWF2534,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CW1834                                                  
*                                                                               
* BUILD CM18349 W1849=1834+3549                                                 
         ZICM  RE,WK_CM1834,(15)                                                
         ZICM  RF,WD_CDRAWM3549,(15)                                            
         AR    RE,RF         WK_CM1824 ALREADY MULTPLED BY 10                   
         STCM  RE,15,WK_CM1849                                                  
         ZICM  RE,WK_CW1834,(15)                                                
         ZICM  RF,WD_CDRAWF3549,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CW1849                                                  
*                                                                               
* BUILD CM2549, W2549=2534+3549                                                 
         ZICM  RE,WD_CDRAWM2534,(15)                                            
         ZICM  RF,WD_CDRAWM3549,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CM2549                                                  
         ZICM  RE,WD_CDRAWF2534,(15)                                            
         ZICM  RF,WD_CDRAWF3549,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CW2549                                                  
* BUILD CM2554, W2549=2549+5054                                                 
         ZICM  RE,WK_CM2549,(15)                                                
         ZICM  RF,WD_CDRAWM5054,(15)                                            
         AR    RE,RF         WK_CM2549 ALREADY MULTPLED BY 10                   
         STCM  RE,15,WK_CM2554                                                  
         ZICM  RE,WK_CW2549,(15)                                                
         ZICM  RF,WD_CDRAWF5054,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CW2554                                                  
*                                                                               
* BUILD CM18+, W18+=1824+2554+5564+65+                                          
*                                                                               
         ZICM  RE,WK_CM1824,(15)                                                
         ZICM  RF,WK_CM2554,(15)                                                
         AR    RE,RF                                                            
         ZICM  RF,WD_CDRAWM5564,(15)                                            
         AR    RE,RF                                                            
         ZICM  RF,WD_CDRAWM65P,(15)                                             
         AR    RE,RF                                                            
         STCM  RE,15,WK_CM18O                                                   
         ZICM  RE,WK_CW1824,(15)                                                
         ZICM  RF,WK_CW2554,(15)                                                
         AR    RE,RF                                                            
         ZICM  RF,WD_CDRAWF5564,(15)                                            
         AR    RE,RF                                                            
         ZICM  RF,WD_CDRAWF65P,(15)                                             
         AR    RE,RF                                                            
         STCM  RE,15,WK_CW18O                                                   
*                                                                               
* BUILD V2-11=V25+V611                                                          
         ZICM  RE,WD_CDRAWCH25,(15)                                             
         ZICM  RF,WD_CDRAWCH611,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CV211                                                   
*                                                                               
* BUILD V1217=M1217+W1217                                                       
         ZICM  RE,WK_CM1217,(15)                                                
         ZICM  RF,WK_CW1217,(15)                                                
         AR    RE,RF                                                            
         STCM  RE,15,WK_CV1217                                                  
*                                                                               
* BUILD V1224=V1217+M1824+W1824                                                 
         ZICM  RE,WK_CV1217,(15)                                                
         ZICM  RF,WK_CM1824,(15)                                                
         AR    RE,RF                                                            
         ZICM  RF,WK_CW1824,(15)                                                
         AR    RE,RF                                                            
         STCM  RE,15,WK_CV1224                                                  
*                                                                               
* BUILD V2+=V211+V1217+M18O+W18O                                                
         ZICM  RE,WK_CV211,(15)                                                 
         ZICM  RF,WK_CV1217,(15)                                                
         AR    RE,RF                                                            
         ZICM  RF,WK_CM18O,(15)                                                 
         AR    RE,RF                                                            
         ZICM  RF,WK_CW18O,(15)                                                 
         AR    RE,RF                                                            
         STCM  RE,15,WK_CV2O                                                    
*                                                                               
         ZICM  RE,WD_CDRAWHHLDS,(15)                                            
         STCM  RE,15,WK_CHOMES                                                  
         ZICM  RE,WD_CDRAWMTROA,(15)                                            
         STCM  RE,15,WK_CMETROA                                                 
         ZICM  RE,WD_CDRAWMTROB,(15)                                            
         STCM  RE,15,WK_CMETROB                                                 
         ZICM  RE,WD_CDRAWWW,(15)                                               
         STCM  RE,15,WK_CWWRK                                                   
         B     BLDDEM40                                                         
*                                                                               
*------------   LPM MARKET GRAB 4 WEEK CUMES -------------------                
*                                                                               
BLDDEM30 ZICM  RE,WD_CDR4WM1214,(15)                                            
         ZICM  RF,WD_CDR4WM1517,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CM1217                                                  
         ZICM  RE,WD_CDR4WF1214,(15)                                            
         ZICM  RF,WD_CDR4WF1517,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CW1217                                                  
*                                                                               
         ZICM  RE,WD_CDR4WM1820,(15)                                            
         ZICM  RF,WD_CDR4WM2124,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CM1824                                                  
         ZICM  RE,WD_CDR4WF1820,(15)                                            
         ZICM  RF,WD_CDR4WF2124,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CW1824                                                  
*                                                                               
* BUILD CM1834, W1834=1824+2534                                                 
         ZICM  RE,WK_CM1824,(15)                                                
         ZICM  RF,WD_CDR4WM2534,(15)                                            
         AR    RE,RF         WK_CM1824 ALREADY MULTPLED BY 10                   
         STCM  RE,15,WK_CM1834                                                  
         ZICM  RE,WK_CW1824,(15)                                                
         ZICM  RF,WD_CDR4WF2534,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CW1834                                                  
*                                                                               
* BUILD CM18349 W1849=1834+3549                                                 
         ZICM  RE,WK_CM1834,(15)                                                
         ZICM  RF,WD_CDR4WM3549,(15)                                            
         AR    RE,RF         WK_CM1824 ALREADY MULTPLED BY 10                   
         STCM  RE,15,WK_CM1849                                                  
         ZICM  RE,WK_CW1834,(15)                                                
         ZICM  RF,WD_CDR4WF3549,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CW1849                                                  
*                                                                               
* BUILD CM2549, W2549=2534+3549                                                 
         ZICM  RE,WD_CDR4WM2534,(15)                                            
         ZICM  RF,WD_CDR4WM3549,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CM2549                                                  
         ZICM  RE,WD_CDR4WF2534,(15)                                            
         ZICM  RF,WD_CDR4WF3549,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CW2549                                                  
* BUILD CM2554, W2549=2549+5054                                                 
         ZICM  RE,WK_CM2549,(15)                                                
         ZICM  RF,WD_CDR4WM5054,(15)                                            
         AR    RE,RF         WK_CM2549 ALREADY MULTPLED BY 10                   
         STCM  RE,15,WK_CM2554                                                  
         ZICM  RE,WK_CW2549,(15)                                                
         ZICM  RF,WD_CDR4WF5054,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CW2554                                                  
*                                                                               
* BUILD CM18+, W18+=1824+2554+5564+65+                                          
*                                                                               
         ZICM  RE,WK_CM1824,(15)                                                
         ZICM  RF,WK_CM2554,(15)                                                
         AR    RE,RF                                                            
         ZICM  RF,WD_CDR4WM5564,(15)                                            
         AR    RE,RF                                                            
         ZICM  RF,WD_CDR4WM65P,(15)                                             
         AR    RE,RF                                                            
         STCM  RE,15,WK_CM18O                                                   
         ZICM  RE,WK_CW1824,(15)                                                
         ZICM  RF,WK_CW2554,(15)                                                
         AR    RE,RF                                                            
         ZICM  RF,WD_CDR4WF5564,(15)                                            
         AR    RE,RF                                                            
         ZICM  RF,WD_CDR4WF65P,(15)                                             
         AR    RE,RF                                                            
         STCM  RE,15,WK_CW18O                                                   
                                                                                
* BUILD V2-11=V25+V611                                                          
         ZICM  RE,WD_CDR4WCH25,(15)                                             
         ZICM  RF,WD_CDR4WCH611,(15)                                            
         AR    RE,RF                                                            
         STCM  RE,15,WK_CV211                                                   
                                                                                
* BUILD V1217=M1217+W1217                                                       
         ZICM  RE,WK_CM1217,(15)                                                
         ZICM  RF,WK_CW1217,(15)                                                
         AR    RE,RF                                                            
         STCM  RE,15,WK_CV1217                                                  
                                                                                
* BUILD V1224=V1217+M1824+W1824                                                 
         ZICM  RE,WK_CV1217,(15)                                                
         ZICM  RF,WK_CM1824,(15)                                                
         AR    RE,RF                                                            
         ZICM  RF,WK_CW1824,(15)                                                
         AR    RE,RF                                                            
         STCM  RE,15,WK_CV1224                                                  
                                                                                
* BUILD V2+=V211+V1217+M18O+W18O                                                
         ZICM  RE,WK_CV211,(15)                                                 
         ZICM  RF,WK_CV1217,(15)                                                
         AR    RE,RF                                                            
         ZICM  RF,WK_CM18O,(15)                                                 
         AR    RE,RF                                                            
         ZICM  RF,WK_CW18O,(15)                                                 
         AR    RE,RF                                                            
         STCM  RE,15,WK_CV2O                                                    
*                                                                               
         ZICM  RE,WD_CDR4WHHLDS,(15)                                            
         STCM  RE,15,WK_CHOMES                                                  
         ZICM  RE,WD_CDR4WMTROA,(15)                                            
         STCM  RE,15,WK_CMETROA                                                 
         ZICM  RE,WD_CDR4WMTROB,(15)                                            
         STCM  RE,15,WK_CMETROB                                                 
         ZICM  RE,WD_CDR4WWW,(15)                                               
         STCM  RE,15,WK_CWWRK                                                   
                                                                                
* BEFORE WE EXIT CONVERT CUMES TO CUME RATINGS BY DIVDING BY UNIVERSE           
BLDDEM40 DS    0X                                                               
                                                                                
         LA    R9,CDEMTAB                                                       
BLDDM100 CLC   =X'FFFF',0(R9)                                                   
         BE    BLDDM120                                                         
         ZICM  R1,0(R9),(15)            A(CUME IMPS)                            
         ZICM  RF,0(R1),(15)            CUME IMPS                               
*                                                                               
         ZICM  RE,4(R9),(15)            A(UNIV)                                 
         ZICM  R3,0(RE),(15)            UNIV                                    
         OR    R3,R3                                                            
         BZ    BLDDM110                                                         
         XC    WORK,WORK                                                        
         XC    DUB1,DUB1                                                        
         CVD   RF,DUB1                  WK_CM1217                               
         OI    DUB1+7,X'0C'                                                     
         MVC   WORK(3),=X'00100C'        *100                                   
         MP    DUB1(8),WORK(3)                                                  
         XC    WORK,WORK                                                        
         MVC   WORK+8(8),DUB1           WORK=CUME IMPS*100                      
         SR    R2,R2                                                            
         LR    RF,R2                                                            
         LA    R0,10                                                            
         DR    R2,R0                    R2=UNIV/10                              
                                                                                
         XC    DUB1,DUB1                                                        
         CVD   R3,DUB1                                                          
         OI    DUB1+7,X'0C'                                                     
         DP    WORK(16),DUB1(8)                                                 
         MVC   DUB1(8),WORK                                                     
         CVB   RF,DUB1                                                          
*                                                                               
         SR    RE,RE                                                            
         AHI   RF,5                                                             
         LA    R0,10                                                            
         DR    RE,R0                                                            
         MHI   RF,10                                                            
*                                                                               
         STCM  RF,15,0(R1)                                                      
BLDDM110 AHI   R9,L'CDEMTAB                                                     
         B     BLDDM100                                                         
BLDDM120 DS    0F                                                               
                                                                                
         XIT1                                                                   
CDEMTAB  DS    0CL8                                                             
         DC    AL4(WK_CM1217),AL4(WK_UM1217)                                    
         DC    AL4(WK_CW1217),AL4(WK_UW1217)                                    
         DC    AL4(WK_CM1824),AL4(WK_UM1824)                                    
         DC    AL4(WK_CW1824),AL4(WK_UW1824)                                    
         DC    AL4(WK_CM1834),AL4(WK_UM1834)                                    
         DC    AL4(WK_CW1834),AL4(WK_UW1834)                                    
         DC    AL4(WK_CM1849),AL4(WK_UM1849)                                    
         DC    AL4(WK_CW1849),AL4(WK_UW1849)                                    
         DC    AL4(WK_CM2549),AL4(WK_UM2549)                                    
         DC    AL4(WK_CW2549),AL4(WK_UW2549)                                    
         DC    AL4(WK_CM2554),AL4(WK_UM2554)                                    
         DC    AL4(WK_CW2554),AL4(WK_UW2554)                                    
         DC    AL4(WK_CM18O),AL4(WK_UM18O)                                      
         DC    AL4(WK_CW18O),AL4(WK_UW18O)                                      
         DC    AL4(WK_CV211),AL4(WK_UV211)                                      
         DC    AL4(WK_CV1217),AL4(WK_UV1217)                                    
         DC    AL4(WK_CV1224),AL4(WK_UV1224)                                    
         DC    AL4(WK_CV2O),AL4(WK_UV2O)                                        
         DC    AL4(WK_CHOMES),AL4(WK_UHOMES)                                    
         DC    AL4(WK_CMETROA),AL4(WK_UMETROA)                                  
         DC    AL4(WK_CMETROB),AL4(WK_UMETROB)                                  
         DC    AL4(WK_CWWRK),AL4(WK_UWWRK)                                      
         DC    X'FFFF'                                                          
         EJECT                                                                  
*                                                                               
**********************************************************************          
*------------USE TABLE TO CONVERT FROM RREC TO IREC                             
* R4 = A(CONVERSION TABLE)                                                      
* R7 = A(START OF RATING SERVICE DEMO DATA)                                     
* R6 = A(START OF INTERIM RECORD DEMO DATA)                                     
CNVRTOI  NTR1                                                                   
         SHI   R6,4                                                             
         SHI   R7,4                                                             
CNVRTOI1 CLI   0(R4),X'FF'                                                      
         BE    CNVRTOIX                                                         
         ZIC   R5,0(R4)            GET RS FIELD NUMBER                          
         MH    R5,=H'4'            ADJUST FOR FLD LEN                           
         AR    R5,R7                                                            
         L     RF,0(R5)                                                         
         CLI   PROC_FLAG,CUME_DEMO                                              
         BNE   *+8                 OLD CONVERISON MULTIPLIES CUMES              
         MHI   RF,10               BY 10                                        
                                                                                
         ZIC   R5,1(R4)            GET INT FIELD NUMBER                         
         MH    R5,=H'4'            ADJUST FOR FLD LEN                           
         AR    R5,R6                                                            
         ST    RF,0(R5)            SAVE IT                                      
         LA    R4,2(R4)            NEXT FIELD                                   
*                                                                               
         B     CNVRTOI1                                                         
*                                                                               
CNVRTOIX XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*====================== CONVERT TO WORK RECORD =======================*         
CNVWR    L     R2,ASREC            SET TO SORT RECORD                           
         L     R6,ASREC                                                         
         LA    R6,4(R6)                                                         
         USING DRKEY,R6                                                         
         CLI   0(R6),C'R'          PURGE RATINGS RECORDS ONLY                   
         BNE   EXIT                                                             
         CLC   DRSTAT(5),PREVSTAT  SAME STATION                                 
         BNE   GOODREC             NO - CANNOT BE PARENT OF S1                  
         CLC   DRSTYP,PREVSTYP                                                  
         BE    GOODREC                                                          
         CLI   PREVSTYP,1          IF PREV NOT P+S1 MUST BE OK                  
         BNE   GOODREC                                                          
         CLI   DRSTYP,2            IS IT A PARENT                               
         BNE   GOODREC             NO - CANNOT HAVE S1                          
         MVI   INTRTYP,0           YES - PARENT OF S1 -PURGE                    
         B     EXIT                                                             
GOODREC  MVC   PREVSTYP,DRSTYP                                                  
         MVC   PREVSTAT,DRSTAT                                                  
         OI    INTWEEKS,B'00100000' SET TYPICAL SW                              
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
*============================== SETKEY ===============================*         
SETKEY   NTR1                                                                   
         L     R6,AIREC                                                         
         USING DRKEY,R6                                                         
         LA    R6,4(R6)                                                         
         MVI   DRCODE,DRCODEQU                                                  
         MVI   INTRTYP,C'R'                                                     
         MVI   DRMEDIA,C'D'                                                     
         MVI   DRSRC,C'N'                                                       
         MVC   DRSTAT,INTSTA                                                    
         TM    INTSTYP,X'20'                                                    
         BZ    *+14                SET FOR SPILL STATION                        
         MVC   DRKMKT,INTMRKT                                                   
         MVI   INTSPILL,C'Y'                                                    
         MVC   DRSTYP,INTSTYP      SET STATYP IN KEY                            
         MVI   INTSTYP,0           SET STATYP TO ZERO FOR OUTPUT PHASE          
         MVC   DRBOOK,INTBOOK                                                   
         CLI   STASW,1             CREAT 'M' RECORD SWITCH                      
         BE    SETKEY3                                                          
         MVC   DRHIGHD,INTDAY                                                   
         MVC   DRHIQHR,INTEQH                                                   
         MVC   DRHIQHR+1(L'INTSQH),INTSQH                                       
         B     SETKEYX                                                          
SETKEY3  MVI   DRCODE,C'M'                                                      
         MVI   INTRTYP,C'M'                                                     
         B     SETKEYX                                                          
SETKEYX  XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================== FLTMKT ===============================*         
*                                                                               
FLTMKT   SR    RF,RF                                                            
         MVI   PASSFLT,C'Y'                                                     
         ICM   RF,1,FILTMRKT                                                    
         BZR   RE                                                               
         OC    INTMRKT,INTMRKT                                                  
         BZR   RE                                                               
         LA    R1,FILTMRKT+1                                                    
         CLC   INTMRKT,0(R1)                                                    
         BER   RE                                                               
         LA    R1,L'INTMRKT(R1)                                                 
         BCT   RF,*-12                                                          
         MVI   PASSFLT,C'N'                                                     
         BR    RE                                                               
***********************************************************************         
         EJECT                                                                  
MORET    DS    0H'0'                                                            
ENDJOB   CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
PS1E     EQU   1                                                                
PARENTE  EQU   2                                                                
PS2E     EQU   4             DONT SEE PS2E BEING USED                           
SATELITE EQU   4             SO USE IT FOR NEW SATELITE                         
S1EQU    EQU   8                                                                
S2EQU    EQU   16                                                               
OUTMARE  EQU   32                                                               
CANMARE  EQU   64                                                               
METROAE  EQU   1                                                                
METROBE  EQU   2                                                                
ZEROS    DC    200C'0'                                                          
SPACES   DC    32C' '                                                           
MODLIST  DC    C'JW',C'FN'                                                      
         DC    X'FF'                                                            
PROC_FLAG DS   X                                                                
CUME_DEMO EQU  X'80'                                                            
TSA_DEMO  EQU  X'40'                                                            
UNIV_DEMO EQU  X'20'                                                            
WRK_DEMOS EQU  X'10'                                                            
DMA_DEMOS EQU  X'08'                                                            
         SPACE 2                                                                
***********************************************************************         
*===================== DAYPART CONVERSION TABLE ======================*         
E        EQU   C'1'                EASTERN                                      
C        EQU   C'2'                CENTRAL                                      
M        EQU   C'3'                MOUNTAIN                                     
P        EQU   C'4'                PACIFIC                                      
Y        EQU   C'5'                YUKON                                        
H        EQU   C'6'                HAWAII                                       
         EJECT                                                                  
*  THESE ARE TEMP PLACE HOLDER                                                  
WK_CM1217  DS    XL4                                                            
WK_CW1217  DS    XL4                                                            
WK_CM1824  DS    XL4                                                            
WK_CW1824  DS    XL4                                                            
WK_UM1834  DS    XL4                                                            
WK_UW1834  DS    XL4                                                            
WK_UM1849  DS    XL4                                                            
WK_UW1849  DS    XL4                                                            
WK_UM2549  DS    XL4                                                            
WK_UW2549  DS    XL4                                                            
WK_UM2554  DS    XL4                                                            
WK_UW2554  DS    XL4                                                            
WK_UM18O   DS    XL4                                                            
WK_UW18O   DS    XL4                                                            
WK_UV211   DS    XL4                                                            
WK_UV2O    DS    XL4                                                            
WK_UV1217  DS    XL4                                                            
WK_UV1224  DS    XL4                                                            
WK_UHOMES  DS    XL4                                                            
WK_UMETROA DS    XL4                                                            
WK_UMETROB DS    XL4                                                            
WK_UWWRK   DS    XL4                                                            
*================================                                               
*     WK_DEMOS ARE THE CALCULATED DEMOS USED TO SLOT DEMOS                      
WK_DEMOS   DS    0XL4                                                           
WK_CM1834  DS    XL4                                                            
WK_CW1834  DS    XL4                                                            
WK_CM1849  DS    XL4                                                            
WK_CW1849  DS    XL4                                                            
WK_CM2549  DS    XL4                                                            
WK_CW2549  DS    XL4                                                            
WK_CM2554  DS    XL4                                                            
WK_CW2554  DS    XL4                                                            
WK_CM18O   DS    XL4                                                            
WK_CW18O   DS    XL4                                                            
WK_CV211   DS    XL4                                                            
WK_CV2O    DS    XL4                                                            
WK_CV1217  DS    XL4                                                            
WK_CV1224  DS    XL4                                                            
WK_DM1217  DS    XL4                                                            
WK_DW1217  DS    XL4                                                            
WK_DM1824  DS    XL4                                                            
WK_DW1824  DS    XL4                                                            
WK_TM1217  DS    XL4                                                            
WK_TW1217  DS    XL4                                                            
WK_TM1824  DS    XL4                                                            
WK_TW1824  DS    XL4                                                            
WK_UM1217  DS    XL4                                                            
WK_UW1217  DS    XL4                                                            
WK_UM1824  DS    XL4                                                            
WK_UW1824  DS    XL4                                                            
WK_CHOMES  DS    XL4                                                            
WK_CMETROA DS    XL4                                                            
WK_CMETROB DS    XL4                                                            
WK_CWWRK   DS    XL4                                                            
WK_RHOMES  DS    XL4                                                            
WK_RMETROA DS    XL4                                                            
WK_RMETROB DS    XL4                                                            
*================================                                               
*                                                                               
*                                                                               
WCM1834  EQU   1                                                                
WCW1834  EQU   2                                                                
WCM1849  EQU   3                                                                
WCW1849  EQU   4                                                                
WCM2549  EQU   5                                                                
WCW2549  EQU   6                                                                
WCM2554  EQU   7                                                                
WCW2554  EQU   8                                                                
WCM18O   EQU   9                                                                
WCW18O   EQU   10                                                               
WCV211   EQU   11                                                               
WCV2O    EQU   12                                                               
WCV1217  EQU   13                                                               
WCV1224  EQU   14                                                               
WDM1217  EQU   15                                                               
WDW1217  EQU   16                                                               
WDM1824  EQU   17                                                               
WDW1824  EQU   18                                                               
WTM1217  EQU   19                                                               
WTW1217  EQU   20                                                               
WTM1824  EQU   21                                                               
WTW1824  EQU   22                                                               
WUM1217  EQU   23                                                               
WUW1217  EQU   24                                                               
WUM1824  EQU   25                                                               
WUW1824  EQU   26                                                               
WCHOMES  EQU   27                                                               
WCMETROA EQU   28                                                               
WCMETROB EQU   29                                                               
WCWWRK   EQU   30                                                               
WRHOMES  EQU   31                                                               
WRMETROA EQU   32                                                               
WRMETROB EQU   33                                                               
*                                                                               
***********************************************************************         
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
ORHOMES   EQU   1                                                               
OSHOMES   EQU   2                                                               
ORHHW1    EQU   3                                                               
ORHHW2    EQU   4                                                               
ORHHW3    EQU   5                                                               
ORHHW4    EQU   6                                                               
OPFEBP    EQU   7                                                               
OPMAYP    EQU   8                                                               
OPJULP    EQU   9                                                               
OPNOVP    EQU   10                                                              
ORFEBP    EQU   11                                                              
ORMAYP    EQU   12                                                              
ORJULP    EQU   13                                                              
ORNOVP    EQU   14                                                              
ORADJ1    EQU   15                                                              
ORADJ2    EQU   16                                                              
ORADJ3    EQU   17                                                              
OOHOMES   EQU   18                                                              
OOADJ1    EQU   19                                                              
OOADJ2    EQU   20                                                              
OOADJ3    EQU   21                                                              
OOCABLE   EQU   22                                                              
OCHOMES   EQU   23                                                              
OPMETROA  EQU   24                                                              
OSMETROA  EQU   25                                                              
ORMETROA  EQU   26                                                              
OOMETROA  EQU   27                                                              
OCMETROA  EQU   28                                                              
OPMETROB  EQU   29                                                              
OSMETROB  EQU   30                                                              
ORMETROB  EQU   31                                                              
OOMETROB  EQU   32                                                              
OCMETROB  EQU   33                                                              
OSINMKT   EQU   34                                                              
OTHOMES   EQU   35                                                              
ODHOMES   EQU   36                                                              
OTCABLE   EQU   37                                                              
ODCABLE   EQU   38                                                              
OTREACH   EQU   39                                                              
OTWWRK    EQU   40                                                              
ODWWRK    EQU   41                                                              
OTW65O    EQU   42                                                              
ODW65O    EQU   43                                                              
OTM65O    EQU   44                                                              
ODM65O    EQU   45                                                              
ODMETROA  EQU   46                                                              
ODCBLA    EQU   47                                                              
ODMETROB  EQU   48                                                              
ODCBLB    EQU   49                                                              
OTM1217   EQU   50                                                              
OTW1217   EQU   51                                                              
OTM1824   EQU   52                                                              
OTW1824   EQU   53                                                              
OTM2534   EQU   54                                                              
OTW2534   EQU   55                                                              
OTM3549   EQU   56                                                              
OTW3549   EQU   57                                                              
OTM5564   EQU   58                                                              
OTW5564   EQU   59                                                              
OTM5054   EQU   60                                                              
OTW5054   EQU   61                                                              
OTV25    EQU   62                                                               
ODV25    EQU   63                                                               
OTV611    EQU   64                                                              
ODV611    EQU   65                                                              
OTM1820   EQU   66                                                              
OTW1820   EQU   67                                                              
OTM2124   EQU   68                                                              
OTW2124   EQU   69                                                              
OUHOMES   EQU   70                                                              
OUM65O    EQU   71                                                              
OUW65O    EQU   72                                                              
OUWWRK    EQU   73                                                              
OUA1HH    EQU   74                                                              
OUA2HH    EQU   75                                                              
OUA3HH    EQU   76                                                              
OUMETROA  EQU   77                                                              
OUMETROB  EQU   78                                                              
OUM1217   EQU   79                                                              
OUW1217   EQU   80                                                              
OUM1824   EQU   81                                                              
OUW1824   EQU   82                                                              
OUM2534   EQU   83                                                              
OUW2534   EQU   84                                                              
OUM3549   EQU   85                                                              
OUW3549   EQU   86                                                              
OUM5564   EQU   87                                                              
OUW5564   EQU   88                                                              
OUM5054   EQU   89                                                              
OUW5054   EQU   90                                                              
OUV25     EQU   91                                                              
OUV611    EQU   92                                                              
OUM1820   EQU   93                                                              
OUW1820   EQU   94                                                              
OUM2124   EQU   95                                                              
OUW2124   EQU   96                                                              
ODM1217   EQU   97                                                              
ODW1217   EQU   98                                                              
ODM1824   EQU   99                                                              
ODW1824   EQU   100                                                             
ODM2534   EQU   101                                                             
ODW2534   EQU   102                                                             
ODM3549   EQU   103                                                             
ODW3549   EQU   104                                                             
ODM5564   EQU   105                                                             
ODW5564   EQU   106                                                             
ODM5054   EQU   107                                                             
ODW5054   EQU   108                                                             
ODM1820   EQU   109                                                             
ODW1820   EQU   110                                                             
ODM2124   EQU   111                                                             
ODW2124   EQU   112                                                             
OMHOMES   EQU   113                                                             
OMWWRK    EQU   114                                                             
OMW65O    EQU   115                                                             
OMM65O    EQU   116                                                             
OMMETROA  EQU   117                                                             
OMMETROB  EQU   118                                                             
OMM1217   EQU   119                                                             
OMW1217   EQU   120                                                             
OMM1824   EQU   121                                                             
OMW1824   EQU   122                                                             
OMM2534   EQU   123                                                             
OMW2534   EQU   124                                                             
OMM3549   EQU   125                                                             
OMW3549   EQU   126                                                             
OMM5564   EQU   127                                                             
OMW5564   EQU   128                                                             
OMM5054   EQU   129                                                             
OMW5054   EQU   130                                                             
OMV25     EQU   131                                                             
OMV611    EQU   132                                                             
OQHOMES   EQU   133                                                             
OQWWRK    EQU   134                                                             
OQW65O    EQU   135                                                             
OQM65O    EQU   136                                                             
OQM1217   EQU   137                                                             
OQW1217   EQU   138                                                             
OQM1824   EQU   139                                                             
OQW1824   EQU   140                                                             
OQM2534   EQU   141                                                             
OQW2534   EQU   142                                                             
OQM3549   EQU   143                                                             
OQW3549   EQU   144                                                             
OQM5564   EQU   145                                                             
OQW5564   EQU   146                                                             
OQM5054   EQU   147                                                             
OQW5054   EQU   148                                                             
OQV25     EQU   149                                                             
OQV211    EQU   150                                                             
OCW18O    EQU   151                                                             
OCM18O    EQU   152                                                             
OCW1834   EQU   153                                                             
OCM1834   EQU   154                                                             
OCW2549   EQU   155                                                             
OCM2549   EQU   156                                                             
OCW2554   EQU   157                                                             
OCM2554   EQU   158                                                             
OCWWRK    EQU   159                                                             
OCV2O     EQU   160                                                             
OCV1224   EQU   161                                                             
OCV1217   EQU   162                                                             
OCV211    EQU   163                                                             
OCW1849   EQU   164                                                             
OCM1849   EQU   165                                                             
OCV18O    EQU   166                                                             
*          DATA SET DETN0110   AT LEVEL 077 AS OF 08/23/01                      
         EJECT                                                                  
* FIELD DISPLACEMENTS FOR RATING SERVICE RECORDS                                
*                                                                               
RRMETROA EQU   1                                                                
RRMETROB EQU   2                                                                
RRHOMES  EQU   3                                                                
RRV25    EQU   4                                                                
RRV611   EQU   5                                                                
RRM1214  EQU   6                                                                
RRM1517  EQU   7                                                                
RRM1820  EQU   8                                                                
RRM2124  EQU   9                                                                
RRM2534  EQU   10                                                               
RRM3549  EQU   11                                                               
RRM5054  EQU   12                                                               
RRM5564  EQU   13                                                               
RRM65O   EQU   14                                                               
RRW1214  EQU   15                                                               
RRW1517  EQU   16                                                               
RRW1820  EQU   17                                                               
RRW2124  EQU   18                                                               
RRW2534  EQU   19                                                               
RRW3549  EQU   20                                                               
RRW5054  EQU   21                                                               
RRW5564  EQU   22                                                               
RRW65O   EQU   23                                                               
RRWWRK   EQU   24                                                               
*                                                                               
TRHOMES  EQU   1                                                                
TRV25    EQU   2                                                                
TRV611   EQU   3                                                                
TRM1214  EQU   4                                                                
TRM1517  EQU   5                                                                
TRM1820  EQU   6                                                                
TRM2124  EQU   7                                                                
TRM2534  EQU   8                                                                
TRM3549  EQU   9                                                                
TRM5054  EQU   10                                                               
TRM5564  EQU   11                                                               
TRM65O   EQU   12                                                               
TRW1214  EQU   13                                                               
TRW1517  EQU   14                                                               
TRW1820  EQU   15                                                               
TRW2124  EQU   16                                                               
TRW2534  EQU   17                                                               
TRW3549  EQU   18                                                               
TRW5054  EQU   19                                                               
TRW5564  EQU   20                                                               
TRW65O   EQU   21                                                               
TRWWRK   EQU   22                                                               
*                                                                               
*                                                                               
RRECL     EQU   WD_LMRECLQ                                                      
*          DATA SET DETN0110   AT LEVEL 077 AS OF 08/23/01                      
* TABLE TO CONVERT UNIVERSES FROM NSI TO DDS INTREC (M2 RECORD)                 
CUNV2INT DS    0C                                                               
         DC    AL1(RRHOMES,OUHOMES)                                             
         DC    AL1(RRMETROA,OUMETROA)                                           
         DC    AL1(RRMETROB,OUMETROB)                                           
         DC    AL1(RRV25,OUV25)                                                 
         DC    AL1(RRV611,OUV611)                                               
         DC    AL1(RRM1820,OUM1820)                                             
         DC    AL1(RRM2124,OUM2124)                                             
         DC    AL1(RRM2534,OUM2534)                                             
         DC    AL1(RRM3549,OUM3549)                                             
         DC    AL1(RRM5054,OUM5054)                                             
         DC    AL1(RRM5564,OUM5564)                                             
         DC    AL1(RRM65O,OUM65O)                                               
         DC    AL1(RRW1820,OUW1820)                                             
         DC    AL1(RRW2124,OUW2124)                                             
         DC    AL1(RRW2534,OUW2534)                                             
         DC    AL1(RRW3549,OUW3549)                                             
         DC    AL1(RRW5054,OUW5054)                                             
         DC    AL1(RRW5564,OUW5564)                                             
         DC    AL1(RRW65O,OUW65O)                                               
         DC    AL1(RRWWRK,OUWWRK)                                               
         DC    X'FF'                                                            
CDMA2INT DS    0C                                                               
         DC    AL1(RRHOMES,ODHOMES)                                             
         DC    AL1(RRMETROA,ODMETROA)                                           
         DC    AL1(RRMETROB,ODMETROB)                                           
         DC    AL1(RRV25,ODV25)                                                 
         DC    AL1(RRV611,ODV611)                                               
         DC    AL1(RRM1820,ODM1820)                                             
         DC    AL1(RRM2124,ODM2124)                                             
         DC    AL1(RRM2534,ODM2534)                                             
         DC    AL1(RRM3549,ODM3549)                                             
         DC    AL1(RRM5054,ODM5054)                                             
         DC    AL1(RRM5564,ODM5564)                                             
         DC    AL1(RRM65O,ODM65O)                                               
         DC    AL1(RRW1820,ODW1820)                                             
         DC    AL1(RRW2124,ODW2124)                                             
         DC    AL1(RRW2534,ODW2534)                                             
         DC    AL1(RRW3549,ODW3549)                                             
         DC    AL1(RRW5054,ODW5054)                                             
         DC    AL1(RRW5564,ODW5564)                                             
         DC    AL1(RRW65O,ODW65O)                                               
         DC    AL1(RRWWRK,ODWWRK)                                               
         DC    X'FF'                                                            
CTSA2INT DS    0C                                                               
         DC    AL1(TRHOMES,OTHOMES)                                             
         DC    AL1(TRV25,OTV25)                                                 
         DC    AL1(TRV611,OTV611)                                               
         DC    AL1(TRM1820,OTM1820)                                             
         DC    AL1(TRM2124,OTM2124)                                             
         DC    AL1(TRM2534,OTM2534)                                             
         DC    AL1(TRM3549,OTM3549)                                             
         DC    AL1(TRM5054,OTM5054)                                             
         DC    AL1(TRM5564,OTM5564)                                             
         DC    AL1(TRM65O,OTM65O)                                               
         DC    AL1(TRW1820,OTW1820)                                             
         DC    AL1(TRW2124,OTW2124)                                             
         DC    AL1(TRW2534,OTW2534)                                             
         DC    AL1(TRW3549,OTW3549)                                             
         DC    AL1(TRW5054,OTW5054)                                             
         DC    AL1(TRW5564,OTW5564)                                             
         DC    AL1(TRW65O,OTW65O)                                               
         DC    AL1(TRWWRK,OTWWRK)                                               
         DC    X'FF'                                                            
CCUM2INT DS  0C                                                                 
         DC    AL1(RRHOMES,OCHOMES)                                             
         DC    AL1(RRMETROA,OCMETROA)                                           
         DC    AL1(RRMETROB,OCMETROB)                                           
         DC    AL1(RRWWRK,OCWWRK)                                               
         DC  X'FF'                                                              
CWRK2INT DS  0C                                                                 
         DC    AL1(WCM1834,OCM1834)                                             
         DC    AL1(WCW1834,OCW1834)                                             
         DC    AL1(WCM1849,OCM1849)                                             
         DC    AL1(WCW1849,OCW1849)                                             
         DC    AL1(WCM2549,OCM2549)                                             
         DC    AL1(WCW2549,OCW2549)                                             
         DC    AL1(WCM2554,OCM2554)                                             
         DC    AL1(WCW2554,OCW2554)                                             
         DC    AL1(WCM18O,OCM18O)                                               
         DC    AL1(WCW18O,OCW18O)                                               
         DC    AL1(WCV211,OCV211)                                               
         DC    AL1(WCV2O,OCV2O)                                                 
         DC    AL1(WCV1217,OCV1217)                                             
         DC    AL1(WCV1224,OCV1224)                                             
         DC    AL1(WDM1217,ODM1217)                                             
         DC    AL1(WDW1217,ODW1217)                                             
         DC    AL1(WDM1824,ODM1824)                                             
         DC    AL1(WDW1824,ODW1824)                                             
         DC    AL1(WTM1217,OTM1217)                                             
         DC    AL1(WTW1217,OTW1217)                                             
         DC    AL1(WTM1824,OTM1824)                                             
         DC    AL1(WTW1824,OTW1824)                                             
         DC    AL1(WUM1217,OUM1217)                                             
         DC    AL1(WUW1217,OUW1217)                                             
         DC    AL1(WUM1824,OUM1824)                                             
         DC    AL1(WUW1824,OUW1824)                                             
         DC    AL1(WCHOMES,OCHOMES)                                             
         DC    AL1(WCMETROA,OCMETROA)                                           
         DC    AL1(WCMETROB,OCMETROB)                                           
         DC    AL1(WCWWRK,OCWWRK)                                               
         DC    AL1(WRHOMES,ORHOMES)                                             
         DC    AL1(WRMETROA,ORMETROA)                                           
         DC    AL1(WRMETROB,ORMETROB)                                           
         DC  X'FF'                                                              
         EJECT                                                                  
PASSFLT  DC    X'00'                                                            
PREVQH   DC    X'00'                                                            
HAV2WH   DC    X'00'                                                            
NFRST    DC    X'01'                                                            
FRST     DC    X'01'                                                            
NPRES    DC    X'00'                                                            
BYPREAD  DC    X'00'                                                            
STASW    DC    X'00'                                                            
VAPROC   DS    F                                                                
VYPROC   DS    F                                                                
NSIBOOK  DS    0CL2                                                             
NSIBOOKY DS    CL1                                                              
NSIBOOKM DS    CL1                                                              
PREVSTYP DS    CL1                                                              
PREVSTAT DS    CL5                                                              
SVMTYP   DS    CL1                                                              
SVMCL    DS    CL1                                                              
SVMDIND  DS    CL1                                                              
SVTSCL   DS    CL1                                                              
SVDMA    DS    CL1                                                              
SVROUND  DS    CL1                                                              
SVTZ     DS    CL1                                                              
SVSTATUS DS    CL1                                                              
SVADJMNO DS    CL6                                                              
NADJMNO  EQU   L'SVADJMNO/2                                                     
SVPRBKS  DS    CL8                                                              
NPRBKS   EQU   L'SVPRBKS/2                                                      
SEQPBKS  DS    CL20                                                             
SVM1SLST DS    CL250                                                            
UERRECA  DS    (RRECL)C                                                         
LENRREC  EQU   1406                                                             
         EJECT                                                                  
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               MACRF=GM,                                               X        
               EODAD=MORET                                                      
STALIST  DS    CL1400                                                           
IN1A     DS    14060C                                                           
         SPACE 1                                                                
         DROP  R2,RA,R6,RB,RC                                                   
         SPACE 1                                                                
NMOD1Q1  EQU   ((((*-DELMDP10)/4096)+1)*4096)                                   
         ORG   DELMDP10+NMOD1Q1                                                 
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
         EJECT                                                                  
*                                                                               
       ++INCLUDE DEDEMCNVD                                                      
         EJECT                                                                  
       ++INCLUDE DELMDSECT                                                      
         EJECT                                                                  
       ++INCLUDE DEINTD                                                         
         SPACE 3                                                                
       ++INCLUDE DEINTDPTD                                                      
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007DELMDPI   08/01/18'                                      
         END                                                                    
