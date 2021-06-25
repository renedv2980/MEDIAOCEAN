*          DATA SET ACINQ02    AT LEVEL 011 AS OF 05/01/02                      
*PHASE T60602A                                                                  
         TITLE 'ACCOUNT ENQUIRY MK2 - DETAIL - T60602'                          
T60602   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
***********************************************************************         
* OVERLAY ADDRESSES                                                   *         
***********************************************************************         
         DC    A(KEYTABLE-T60602)                                               
         DC    A(FILTABLE-T60602)                                               
         DC    A(KNTRYPNT-T60602)                                               
         DC    A(FNTRYPNT-T60602)                                               
         DC    A(DNTRYPNT-T60602)                                               
         DS    A                                                                
         DS    A                                                                
         EJECT                                                                  
***********************************************************************         
* CHANGE OF KEY                                                       *         
* CLEAR TOTALS - SET TWA SWITCHES *                                             
***********************************************************************         
         SPACE 1                                                                
KNTRYPNT NMOD1 0,**INQ2**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60602,RB,R9                                                     
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T606TWA,RA                                                       
         ZAP   TOTAL,=P'0'                                                      
         ZAP   HRTOTAL,=P'0'                                                    
         MVI   OPTN,0                                                           
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CHECK HOURS=MMMYY OPTION AT VALIDATION                              *         
* CHECK CHECKNUM,DEND & DSTART FILTERS                                *         
* EXECUTE ABOVE FILTERING                                             *         
***********************************************************************         
         SPACE 1                                                                
FNTRYPNT NMOD1 0,**INQ2**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60602,RB,R9                                                     
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T606TWA,RA                                                       
         USING FILTERSD,R4                                                      
         USING FTBD,R6                                                          
         LA    RF,INFFILTH                                                      
         CR    RF,R2                                                            
         BNE   FN20                NOT VALIDATION                               
*                                                                               
FN01     CLC   FILFULKW,HOURSKW    VALIDATE HOURS                               
         BNE   FN03                                                             
         CLC   KEY+1(2),=C'1R'     MUST BE PERSONNEL LEDGER                     
         BNE   FNERR                                                            
         CLI   FTBSIGN,C'N'                                                     
         BE    FNXIT                                                            
         OI    OPTIONS,HOURS       SET OPTIONS BIT                              
         B     FNXIT                                                            
*                                                                               
FN03     CLC   FILFULKW,APPRVSW                                                 
         BNE   FN05                                                             
         OI    OPTN,APPROVED                                                    
         B     FNXIT                                                            
*                                                                               
FN05     CLC   FILFULKW,TIMESKW    TIME=B/N/R                                   
         BNE   FN07                                                             
         MVI   FTBSTAT,1           DISABLE BASE FILTER ROUTINE                  
         CLI   FTBVAL,C'B'                                                      
         BNE   *+12                                                             
         OI    OPTN,TIMEB                                                       
         B     FNXIT                                                            
         CLI   FTBVAL,C'N'                                                      
         BNE   *+12                                                             
         OI    OPTN,TIMEN                                                       
         B     FNXIT                                                            
         CLI   FTBVAL,C'R'                                                      
         BNE   FNERRX                                                           
         OI    OPTN,TIMER                                                       
         B     FNXIT                                                            
*                                                                               
FN07     CLC   FILFULKW,TTYPESKW   TTYPE=T/M/A                                  
         BNE   FN09                                                             
         MVI   FTBSTAT,1           DISABLE BASE FILTER ROUTINE                  
         CLI   FTBVAL,C'T'                                                      
         BNE   *+12                                                             
         OI    OPTN,TTYPET                                                      
         B     FNXIT                                                            
         CLI   FTBVAL,C'M'                                                      
         BNE   *+12                                                             
         OI    OPTN,TTYPEM                                                      
         B     FNXIT                                                            
         CLI   FTBVAL,C'A'                                                      
         BNE   FNERRX                                                           
         OI    OPTN,TTYPEA                                                      
         B     FNXIT                                                            
*                                                                               
FN09     CLC   KEY+1(2),=C'SR'     VALIDATE CHECKNUM/DEND/DSTART                
         BNE   FNERR               MUST BE RECEIVABLE LEDGER                    
         CLC   FILFULKW,DEND       SET A BIT TO DISTINGUISH START/END           
         BNE   FNXIT                                                            
         OI    FTBSTAT,ENDDATE                                                  
         B     FNXIT                                                            
*                                                                               
FNERR    MVI   ERROR,LEDGNVAL                                                   
         B     FNERRX                                                           
*                                                                               
FNXIT    MVI   DMCB,0                                                           
         B     *+8                                                              
*                                                                               
FNERRX   MVI   DMCB,1                                                           
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DO NARRATION SEARCH FOR CHECK NO & DEPOSIT DATE FILTERS                       
***********************************************************************         
         SPACE 1                                                                
FN20     TM    FTBSTAT,X'04'                                                    
         BNZ   FN30                                                             
         LA    RF,CLCTAB           POINT TO COMPARE LENGTH AND STRING           
         CLI   FTBSTAT,0           FOR FILTER                                   
         BE    *+8                                                              
         LA    RF,L'CLCTAB(RF)     DSTART/DEND HAVE NONZERO STATUS              
         LA    R4,1                PREPARE BXLE TO SCAN TRNSNARR                
         L     R5,ATRN                                                          
         ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         BCTR  R5,0                R5 = A(END OF TRNSNARR)                      
         IC    R1,0(RF)                                                         
         SR    R5,R1               LESS LENGTH OF COMPARE STRING                
         BCTR  R1,0                                                             
FN22     EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),1(RF)                                                    
         BE    FN24                                                             
         BXLE  R2,R4,FN22                                                       
         B     FNOX                                                             
*                                                                               
FN24     LA    R2,1(R1,R2)         MATCH FOUND - POINT TO FILTER VALUE          
         CLI   FTBSTAT,0                                                        
         BE    FNXIT3              CHECK NUMBER IS NOW HANDLED BY ROOT          
         MVC   WORK(3),FTBVAL      DEPOSIT DATE - PRESET COMPARAND              
         CLI   0(R2),C' '          TO PASS ROOT FILTER CHECK                    
         BH    *+12                                                             
         LA    R2,1(R2)                                                         
         B     *-12                                                             
         GOTO1 VDATVAL,DMCB,(R2),WORK+3                                         
         OC    DMCB,DMCB           INVALID DATE                                 
         BZ    FNOX                OTHERWISE CONVERT TO PWOS                    
         GOTO1 VDATCON,DMCB,WORK+3,(1,WORK+10)                                  
         CLC   FTBVAL(3),WORK+10                                                
         BE    FNXIT2              EQUAL IS OK FOR START AND END                
         BH    FN26                                                             
         TM    FTBSTAT,ENDDATE     LESS IS OK FOR START                         
         BO    FNOX                                                             
         B     FNXIT2                                                           
FN26     TM    FTBSTAT,ENDDATE     GTR IS OK FOR END                            
         BO    FNXIT2                                                           
*                                                                               
FNOX     XC    WORK(3),WORK        UNEQUATE COMPARANDS                          
FNXIT2   LA    R2,WORK             RETURN R2                                    
FNXIT3   XIT1  REGS=(R2)                                                        
         EJECT                                                                  
**********************************************************************          
* GET STARTING POINT FOR CONTRA ACCOUNT                              *          
**********************************************************************          
         SPACE 1                                                                
FN30     LA    R2,WORK             SET UP DUMMY COMPARANDS                      
         BCTR  R3,0                R3 = LENGTH MINUS 1                          
         EX    R3,*+4              EQUATE COMPARANDS                            
         MVC   WORK(0),INFCAC                                                   
         LA    R4,1                R4 = DISPLACEMENT INTO CA/C                  
         L     R5,AIO                                                           
         USING ACKEYD,R5                                                        
         CLC   ACKEYCON(3),SPACES                                               
         BNE   FN32                                                             
         LA    R4,3                                                             
         CLC   ACKEYCON(12),SPACES                                              
         BNE   FN32                                                             
         LA    R4,12                                                            
*                                                                               
FN32     LA    R7,ACKEYCON(R4)                                                  
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R7),INFCAC                                                   
         BL    FN38                                                             
         BE    FN36                                                             
         CH    R4,=H'1'            ARE WE PAST BILLING SOURCE C/AC'S            
         BNE   FN38                NO                                           
         CLC   =C'DF',INFREC       TEST RUNNING BY OFFICE                       
         BNE   FN34                NO                                           
         MVI   ACKEYCON,X'FF'      IF SO, FORCE NEXT OFFICE                     
         B     FN38                AND TRY AGAIN                                
*                                                                               
FN34     MVI   ACKEYACC,RUNLAST                                                 
         B     FNX                 END OF C/AC                                  
*                                                                               
FN36     CLC   SAVECACN,SPACES     SAVE NAME IF AVAILABLE                       
         BNE   FNX                                                              
         LA    R1,L'ACKEYCON                                                    
         SR    R1,R4                                                            
         BCTR  R1,0                                                             
         LA    R7,ACKEYCON(R4)                                                  
         EX    R1,*+8              DOES THE CA/C READ MATCH THE FILTER          
         B     *+10                                                             
         CLC   0(0,R7),INFCAC                                                   
         BNE   FNX                 IN FULL                                      
         LA    R7,ACKEYD                                                        
         AH    R7,DATADISP                                                      
         USING TRSUBHD,R7                                                       
         CLI   TRSBEL,X'43'        IF SO IS THIS A CA/C HEADER                  
         BNE   FNX                                                              
         IC    R4,TRSBLEN                                                       
         SH    R4,=H'18'                                                        
         BM    FNX                                                              
         EX    R4,*+4                                                           
         MVC   SAVECACN(0),TRSBNAME                                             
         B     FNX                                                              
*                                                                               
FN38     MVI   WORK,0              UNEQUATE COMPARANDS                          
         MVC   KEY(L'ACCKEY),ACKEYD                                             
         MVI   KEY+32,X'FF'        FORCE NEXT CONTRA                            
         MVI   KEY+41,0                                                         
         MVI   DMARK,1                                                          
*                                                                               
FNX      LA    R3,1(R3)                                                         
         LA    R5,INFCAC                                                        
         XIT1  REGS=(R2,R5)                                                     
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SET HEADLINES                                                                 
* FILTER TRANSACTIONS                                                           
***********************************************************************         
         SPACE 1                                                                
DNTRYPNT NMOD1 0,**INQ2**                                                       
         L     RC,0(R1)                                                         
         L     RB,APHASE                                                        
         USING T60602,RB,R9                                                     
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         L     R5,AIO                                                           
         USING ACKEYD,R5                                                        
         CLI   ACKEYACC,RUNLAST    END                                          
         BNE   DN02                                                             
         CLI   LASTKMK,0                                                        
         BNE   DN20                                                             
         CLI   VIRGIN,C'H'                                                      
         BNE   DNEND1                                                           
         B     DN20                                                             
*                                                                               
DN02     CLI   ACKEYDTE,C' '       MUST BE TRANSACTION                          
         BE    DNXIT                                                            
*                                                                               
DN04     L     R1,AOFFBLK          OFFICE SECURITY                              
         USING OFFALD,R1                                                        
         MVC   OFFAREC,AIO                                                      
         MVC   OFFAOPOS,SAVLTOFF                                                
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         BNE   DNXIT                                                            
         DROP  R1                                                               
*                                                                               
DN06     TM    OPTIONS,MOSRANGE    HANDLE MOS RANGE FILTER                      
         BZ    DN10                                                             
         LA    R6,FILTAB                                                        
         USING FTBD,R6                                                          
         TM    FTBSTAT,5           DISABLED+SUBRECORD = MUST BE MOSRNGE         
         BO    *+12                IN WHICH CASE FTBVAL(12),FTBVAL+2(2)         
         LA    R6,FTBTBLEN(R6)     CONTAIN LOW & HIGH MOS CODES                 
         B     *-12                                                             
         ICM   R5,15,ATRN                                                       
         BZ    DNXIT                                                            
         USING TRANSD,R5                                                        
         GOTO1 CONVMOS,DMCB,(X'FE',TRANSD),WORK                                 
         CLC   WORK(2),FTBVAL+0                                                 
         BL    DNXIT                                                            
         CLC   WORK(2),FTBVAL+2                                                 
         BH    DNXIT                                                            
         TM    TRNSSTAT,TRNSREV     TEST TRANS IS REVERSED                      
         B     DN10                ALWAYS INCLUDE ALL                           
         ICM   R5,15,ATRS          A(STATUS EL)                                 
         BZ    DNXIT               NO STATUS EL - DROP TRANSACTION              
         USING TRSELD,R5                                                        
         CLI   TRSLN,TRSLNQ        TEST LONG ELEMENT                            
         BNE   DN08                NO - ASSUME REVS'D MOS = TRANS MOS           
         OC    TRSRMOS,TRSRMOS     TEST REVERSED MOS SET                        
         BZ    DN08                NO - ASSUME REV'D MOS = TRANS MOS            
         CLC   TRSRMOS,FTBVAL+0    INCLUDE THIS TRANSACTION IF THE              
         BL    DN10                OTHER TRANSACTION WILL BE FILTER-            
         CLC   TRSRMOS,FTBVAL+2    ED OUT BECAUSE OF REQUESTED MOS              
         BH    DN10                                                             
*                                                                               
DN08     TM    OPTIONS,REVERSED    TRANS MOS & REVERSED MOS BOTH IN             
         BNO   DNXIT               RANGE - DROP IT UNLESS REVS'D WANTED         
         DROP  R6                                                               
*                                                                               
DN10     MVI   DMCB,0                                                           
         L     RF,AFILTER                                                       
         BASR  RE,RF                                                            
         BZ    DNXIT                                                            
*                                                                               
DN12     TM    OPTN,TIMEB+TIMEN+TIMER TIME=B/N/R                                
         BZ    DN14                OPTION NOT SELECTED                          
         ICM   R4,15,APER                                                       
         BZ    DNXIT               NO PERSONNEL ELEMENT                         
         USING ACPERSD,R4                                                       
         TM    OPTN,TIMEB          TIME=B                                       
         BNO   *+12                                                             
         TM    ACPSSTAT,ACPSBIL    IS IT B TIME                                 
         BNO   DNXIT                                                            
         TM    OPTN,TIMEN          TIME=N                                       
         BNO   *+12                                                             
         TM    ACPSSTAT,ACPSNOT    IS IT N TIME                                 
         BNO   DNXIT                                                            
         TM    OPTN,TIMER          TIME=R                                       
         BNO   *+12                                                             
         TM    ACPSSTAT,ACPSRTE    IS IT R TIME                                 
         BNO   DNXIT                                                            
*                                                                               
DN14     TM    OPTN,TTYPET+TTYPEM+TTYPEA TTYPE=T/M/A                            
         BZ    DN16                OPTION NOT SELECTED                          
         ICM   R5,15,ATRS                                                       
         BZ    DNXIT               NO STATUS ELEMENT                            
         USING TRSELD,R5                                                        
         TM    OPTN,TTYPET         TTYPE=T                                      
         BNO   *+12                                                             
         TM    TRSSTAT2,TRSSTIME   IS IT TYPE T                                 
         BNO   DNXIT                                                            
         TM    OPTN,TTYPEM         TTYPE=M                                      
         BNO   *+12                                                             
         TM    TRSSTAT2,TRSSTMSS   IS IT TYPE M                                 
         BNO   DNXIT                                                            
         TM    OPTN,TTYPEA         TTYPE=A                                      
         BNO   *+12                                                             
         TM    TRSSTAT2,TRSSTADJ   IS IT TYPE A                                 
         BNO   DNXIT                                                            
*                                                                               
DN16     CLI   VIRGIN,C'H'                                                      
         BE    DN20                                                             
         CLI   LINE+1,4            FIRST TIME THROUGH                           
         BH    DN18                IF WE HAVENT GOT THEM                        
         LA    R6,INFDAT2H                                                      
         GOTO1 EDITACNM            SET UP ACCOUNT & CONTRA NAMES                
         MVC   INFDAT3,HEADING                                                  
         OI    INFDAT3H+6,X'80'                                                 
         MVC   INFDAT4,HEADING2                                                 
         OI    INFDAT4H+6,X'80'                                                 
         MVI   LINE+1,4                                                         
         TM    OPTIONS,HOURS       HOURS OPTION SHOWS HOURS INSTEAD OF          
         BNO   DN18                DESCRIPTION                                  
         MVC   INFDAT3+41(11),SPACES                                            
         MVC   INFDAT4+41(11),SPACES                                            
         MVC   INFDAT3+57(5),=C'HOURS'                                          
         MVC   INFDAT4+57(5),INFDAT4+69                                         
DN18     MVI   VIRGIN,C'H'                                                      
         B     DN22                                                             
*                                                                               
DN20     CLI   LINE+1,18           CHECK FOR SCREEN FULL                        
         BH    DNFULL                                                           
         TM    OPTIONS,XDETAIL                                                  
         BZ    *+12                                                             
         CLI   LINE+1,18                                                        
         BNL   DNFULL                                                           
         B     DN22                                                             
         EJECT                                                                  
***********************************************************************         
* SET UP DISPLAY FOR A RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
DN22     LH    R6,LINE                                                          
         MH    R6,=H'86'                                                        
         LA    R6,INFDATAH(R6)                                                  
         USING LINED,R6                                                         
         L     R5,AIO                                                           
         USING ACKEYD,R5                                                        
         CLI   ACKEYACC,RUNLAST                                                 
         BE    DNEND                                                            
         TM    OPTIONS,PEELED      SKIP PEELED UNLESS PEELED=YES                
         BZ    DN24                IN WHICH CASE SHOW ONLY PEELED               
         OC    ACDTPEEL,ACDTPEEL                                                
         BZ    DNXIT                                                            
         B     *+14                                                             
*                                                                               
DN24     OC    ACDTPEEL,ACDTPEEL                                                
         BNZ   DNXIT                                                            
         TM    OPTN,APPROVED       IF FILTERING APPROVED                        
         BNO   *+14                                                             
         OC    ACDTUSED,ACDTUSED   SKIP USED                                    
         BNZ   DNXIT                                                            
         ICM   R5,15,ATRN                                                       
         BZ    DNXIT                                                            
         USING TRANSD,R5                                                        
         TM    OPTIONS,MOSRANGE    IF MOS RANGE WAS SPECIFIED,                  
         BO    DN26                REVERSALS HAVE BEEN HANDLED                  
*                                                                               
DN26     TM    OPTIONS,HOURS                                                    
         BO    DN88                                                             
*                                                                               
DN28     XC    SCANBLCK(250),SCANBLCK   BUILD CONTENTS OF DESCRIP. FLD          
         MVI   ATTRIN,C' '              SPACE FILL ATTRIN                       
         MVC   ATTRIN+1(L'ATTRIN-1),ATTRIN                                      
         MVI   ATTROUT,C' '             SPACE FILL ATTROUT                      
         MVC   ATTROUT+1(L'ATTROUT-1),ATTROUT                                   
         LA    R3,SCANBLCK         IN 21XN LINES                                
         SR    R7,R7               R7 = # LINES USED                            
         L     RE,AIO                                                           
         TM    ACSTATUS-ACKEYD(RE),X'40' TEST DRAFT TRANSACTION                 
         BZ    DN30                                                             
         MVC   0(9,R3),=C'**DRAFT**'                                            
         LA    R3,21(R3)                                                        
         LA    R7,1(R7)            NEED TO SET STUPID FUCKING COUNTER           
*                                                                               
DN30     TM    OPTIONS,GROSS       IF GROSS=YES OPTION IN USE AND THERE         
         BO    *+12                IS A GROSS, IT TAKES THE PLACE OF            
         TM    OPTIONS,NET         NARRATIVE. SIMILARLY FOR NET=YES             
         BNO   DN36                                                             
         ICM   R4,15,ACSH                                                       
         BZ    DN36                                                             
         USING TRCASHD,R4                                                       
         CLI   TRCSTYPE,C'G'                                                    
         BNE   DN32                                                             
         TM    OPTIONS,GROSS                                                    
         BNO   DN36                                                             
         MVC   0(6,R3),=C'GROSS='                                               
         LA    RF,6(R3)                                                         
         B     DN34                                                             
*                                                                               
DN32     CLI   TRCSTYPE,C'N'                                                    
         BNE   DN36                                                             
         TM    OPTIONS,NET                                                      
         BNO   DN36                                                             
         MVC   0(4,R3),=C'NET'                                                  
         LA    RF,4(R3)                                                         
*                                                                               
DN34     EDIT  (P6,TRCSAMNT),(13,0(RF)),2,MINUS=YES,ALIGN=LEFT                  
         LA    R7,1(R7)            INCREMENT STUPID FUCKING COUNTER             
         LA    R3,21(R3)                                                        
         B     DN38                                                             
         DROP  R4                                                               
*                                                                               
DN36     ZIC   R8,TRNSLEN          HANDLE NARRATIVE DESCRIPTION                 
         SH    R8,=H'29'                                                        
         BM    DN38                                                             
         EX    R8,*+8                                                           
         B     *+10                                                             
         TR    TRNSNARR(0),TRTAB   CLEAR UNPRINTABLE CHARACTERS                 
         LA    R8,1(R8)                                                         
         GOTO1 VCHOPPER,DMCB,((R8),TRNSNARR),(21,0(R3)),10                      
         ICM   R1,15,DMCB+8                                                     
         BZ    DN38                                                             
         AR    R7,R1                                                            
         MH    R1,=H'21'                                                        
         AR    R3,R1                                                            
*                                                                               
DN38     ICM   R4,15,AXNO          ORDER NO ELEMENT                             
         BZ    DN40                                                             
         MVC   0(4,R3),=C'ORD='                                                 
         USING ACNOD,R4                                                         
         ZIC   R1,ACNOLEN                                                       
         SH    R1,=H'3'                                                         
         BM    DN40                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R3),ACNO                                                     
         LA    R3,21(R3)                                                        
         LA    R7,1(R7)                                                         
*                                                                               
DN40     ICM   R4,15,AOTH          OTHERS (X'23') EL FOR SUBREF                 
         BZ    DN42                                                             
         OC    AMTR,AMTR           SKIP IF MEDIA TRANSFER                       
         BNZ   DN42                                                             
         USING ACOTHERD,R4                                                      
         MVC   0(7,R3),=C'SUBREF='                                              
         MVC   7(6,R3),ACOTNUM                                                  
         LA    R7,1(R7)                                                         
         LA    R3,21(R3)                                                        
*                                                                               
DN42     ICM   R5,15,ATRN                                                       
         USING TRANSD,R5                                                        
         CLI   TRNSTYPE,36                                                      
         BNE   DN43                                                             
         MVC   0(12,R3),=C'MANUAL CHECK'                                        
         LA    R7,1(R7)                                                         
         LA    R3,21(R3)                                                        
*                                                                               
DN43     CLI   INVREG,C'Y'         SHOW AUTH/UNAUTH IF USING INV REG            
         BNE   DN44                                                             
         L     RE,AIO                                                           
         CLI   1(RE),C'S'          MAIN UNIT ONLY                               
         BNE   DN44                                                             
         MVC   0(4,R3),=C'AUTH'                                                 
         TM    TRNSSTAT,X'08'                                                   
         BO    *+10                                                             
         MVC   0(6,R3),=C'UNAUTH'                                               
         LA    R7,1(R7)                                                         
         LA    R3,21(R3)                                                        
*                                                                               
DN44     TM    TRNSSTAT,X'04'      TEST IF ITEM HELD                            
         BNO   DN46                                                             
         MVC   0(4,R3),=C'HELD'    HELD                                         
         LA    R7,1(R7)                                                         
         LA    R3,21(R3)                                                        
*                                                                               
DN46     TM    TRNSSTAT,X'02'      TEST IF ITEM SELECTED                        
         BNO   DN50                                                             
         LA    RF,IO                                                            
         USING ACKEYD,RF                                                        
         CLC   ACKEYACC+1(2),=C'SC' ON SC X'02' = RECONCILED                    
         BNE   DN48                                                             
         MVC   0(10,R3),=C'RECONCILED'                                          
         LA    R7,1(R7)                                                         
         LA    R3,21(R3)                                                        
         B     DN50                                                             
*                                                                               
DN48     OC    ACDTUSED,ACDTUSED                                                
         BNZ   DN50                                                             
         MVC   0(8,R3),=C'APPROVED'                                             
         LA    R7,1(R7)                                                         
         LA    R3,21(R3)                                                        
         DROP  RF                                                               
*                                                                               
DN50     ICM   R4,15,ATRS          A(STATUS EL)                                 
         BZ    DN52                NO STATUS EL                                 
         USING TRSELD,R4                                                        
         TM    TRSSTAT,TRSSOFFS    TEST MARKED AS OFFSET                        
         BZ    DN52                NO                                           
         MVC   0(7,R3),=C'OFFSET='                                              
         LA    RF,IO                                                            
         USING ACKEYD,RF                                                        
         GOTO1 VDATCON,DMCB,(2,ACDTUSED),(8,7(R3))                              
         LA    R7,1(R7)                                                         
         LA    R3,21(R3)                                                        
*                                                                               
DN52     TM    TRNSSTAT,X'20'      TEST IF ITEM REVERSED                        
         BNO   DN54                                                             
         CLI   TRNSTYPE,X'25'      TYPE 37 - HAS PROPER NARRATIVE               
         BE    DN54                DON'T DISPLAY REVERSED INFO                  
         MVC   0(8,R3),=C'REVERSED'                                             
         LR    RF,R3                                                            
         LA    R7,1(R7)                                                         
         LA    R3,21(R3)                                                        
         ICM   R4,15,ATRS          A(STATUS EL)                                 
         BZ    DN54                NO STATUS EL                                 
         USING TRSELD,R4                                                        
         OC    TRSREVD,TRSREVD     TEST REVERSED DATE                           
         BZ    DN54                NO                                           
         MVC   0(9,RF),=C'REVERSED='                                            
         LA    R0,9(RF)                                                         
         CLI   TRNSTYPE,X'81'      IF IT'S A CHECK                              
         BNE   *+14                                                             
         MVC   0(9,RF),=C'VOIDED ON'  IT WAS VOIDED                             
         LA    R0,10(RF)                                                        
         GOTO1 VDATCON,DMCB,(2,TRSREVD),(8,(R0))                                
*                                                                               
DN54     ICM   R4,15,AMTR          MEDIA TRANSFER ELEMENT                       
         BZ    DN58                                                             
         USING ACMTD,R4                                                         
         MVC   0(3,R3),ACMTCLI     CLIENT                                       
         MVI   3(R3),C'/'                                                       
         MVC   4(3,R3),ACMTPRD     PRODUCT                                      
         MVI   7(R3),C'/'                                                       
         MVC   8(6,R3),ACMTEST     ESTIMATE/JOB                                 
         LA    R7,1(R7)                                                         
         LA    R3,21(R3)                                                        
         CLI   ACMTMOS,C' '        MEDIA MONTH OF SERVICE                       
         BNH   DN56                                                             
         CLI   ACMTMOS,C'0'                                                     
         BNL   DN56                                                             
         MVC   0(4,R3),=C'MOS='                                                 
         MVC   DUB(2),ACMTMOS                                                   
         MVI   DUB+2,1                                                          
         GOTO1 VDATCON,DMCB,(1,DUB),(6,4(R3)) MMM/YY                            
         LA    R7,1(R7)                                                         
         LA    R3,21(R3)                                                        
*                                                                               
DN56     CLI   ACMTDSCP,C' '       ANY DESCRIPTION                              
         BNH   DN58                                                             
         MVC   0(21,R3),ACMTDSCP                                                
         LA    R7,1(R7)                                                         
         LA    R3,21(R3)                                                        
*                                                                               
DN58     ICM   R4,15,APER          PERSONEL ELEMENT                             
         BZ    DN60                                                             
         USING ACPERSD,R4                                                       
         MVC   0(6,R3),=C'HOURS='                                               
         EDIT  ACPSHOUR,(9,6(R3)),2,MINUS=YES,ALIGN=LEFT                        
         LA    R7,1(R7)                                                         
         LA    R3,21(R3)                                                        
         TM    ACPSSTAT,ACPSBIL+ACPSNOT+ACPSRTE                                 
         BZ    DN60                                                             
         MVC   0(5,R3),=C'TIME='                                                
         TM    ACPSSTAT,ACPSBIL    IS IT BILLABLE ?                             
         BNO   *+8                                                              
         MVI   5(R3),C'B'                                                       
         TM    ACPSSTAT,ACPSNOT    NOT BILLABLE ?                               
         BNO   *+8                                                              
         MVI   5(R3),C'N'                                                       
         TM    ACPSSTAT,ACPSRTE    SPECIAL NOT BILLABLE ?                       
         BNO   *+8                                                              
         MVI   5(R3),C'R'                                                       
         LA    R7,1(R7)                                                         
         LA    R3,21(R3)                                                        
*                                                                               
DN60     ICM   R4,15,ATRS          TRANSACTION STATUS                           
         BZ    DN62                                                             
         USING TRSELD,R4                                                        
         TM    TRSSTAT2,TRSSTADJ+TRSSTMSS+TRSSTIME                              
         BZ    DN62                                                             
         MVC   0(6,R3),=C'TTYPE='                                               
         TM    TRSSTAT2,TRSSTADJ   ADJUSTED                                     
         BNO   *+8                                                              
         MVI   6(R3),C'A'                                                       
         TM    TRSSTAT2,TRSSTMSS   MISSING                                      
         BNO   *+8                                                              
         MVI   6(R3),C'M'                                                       
         TM    TRSSTAT2,TRSSTIME   REGULAR TIME                                 
         BNO   *+8                                                              
         MVI   6(R3),C'T'                                                       
         LA    R7,1(R7)                                                         
         LA    R3,21(R3)                                                        
*                                                                               
DN62     L     R4,AIO              JOB=CLI/PRO/JOB OR EXP=ACCOUNT               
         AH    R4,DATADISP                                                      
         SR    R0,R0                                                            
*                                                                               
DN64     CLI   0(R4),0                                                          
         BE    DN76                                                             
         CLI   0(R4),X'4F'                                                      
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DN64                                                             
         USING TRCPJD,R4                                                        
         CLI   TRCPTYPE,C'E'                                                    
         BNE   DN66                                                             
         MVC   0(4,R3),=C'EXP='                                                 
         MVC   4(12,R3),TRCPACCT                                                
         B     DN74                                                             
*                                                                               
DN66     CLI   TRCPTYPE,C'J'                                                    
         BNE   DN68                                                             
         MVC   0(21,R3),SPACES                                                  
         B     DN76                                                             
*                                                                               
DN68     CLI   TRCPTYPE,C'C'                                                    
         BNE   DN76                                                             
         MVC   0(3,R3),=C'COM'                                                  
         MVC   0(22,R3),SPACES                                                  
         MVC   0(2,R3),=C'C='                                                   
         MVC   2(2,R3),TRCPUL                                                   
         CLI   TRCPUL+1,C' '                                                    
         BNH   DN70                                                             
         GOTO1 VHEXOUT,DMCB,TRCPUL+1,3(R3),1,=C'MIX'                            
*                                                                               
DN70     MVC   6(3,R3),TRCPCLI                                                  
         MVC   9(3,R3),TRCPPROD                                                 
         MVC   12(6,R3),TRCPCOM                                                 
         MVC   19(3,R3),TRCPCAT                                                 
*                                                                               
DN72     GOTO1 VSQASHER,DMCB,4(R3),18                                           
DN74     LA    R7,1(R7)                                                         
         LA    R3,21(R3)                                                        
*                                                                               
DN76     ICM   R4,15,ADUE                                                       
         BZ    DN78                                                             
         USING TRDUED,R4                                                        
         MVC   0(4,R3),=C'DUE='                                                 
         GOTO1 VDATCON,DMCB,(2,TRDUDATE),(8,4(R3))                              
         LA    R7,1(R7)                                                         
         LA    R3,21(R3)                                                        
*                                                                               
DN78     XC    DMCB+8(4),DMCB+8                                                 
         TM    OPTIONS,ATTRIBUT                                                 
         BZ    DN82                                                             
         ICM   R3,15,AANP                 ADDRESS OF ANALYSIS POINTER           
         BZ    DN82                       OR NO ANALYSIS POINTER                
         USING ACAPD,R3                                                         
         LA    R1,ATTRIN                                                        
         MVC   0(10,R1),=C'ATTRIBUTE='     ATTRIBUTE=                           
         LA    R1,10(R1)                                                        
         ZIC   R0,ACAPMIN                                                       
         LA    R3,ACAPMLEN                                                      
         USING ACAPMLEN,R3                                                      
*                                                                               
DN80     ZIC   R4,ACAPMLEN                                                      
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R1),ACAPACCT           MOVE IN ANALYSIS ACCOUNT              
         AR    R1,R4                      BUMP TO                               
         LA    R1,2(R1)                   NEXT IN ATTRIN                        
         LA    R4,3(R4)                                                         
         AR    R3,R4                      NEXT MINI ELEMENT                     
         BCT   R0,DN80                                                          
         GOTO1 VCHOPPER,DMCB,(150,ATTRIN),(75,ATTROUT),2                        
*                                                                               
DN82     A     R7,DMCB+8           ADD NUMBER OF ANALYSIS LINES                 
         BZ    DN92                                                             
         LH    R1,LINE                                                          
         LA    R1,1(R7,R1)                                                      
         CH    R1,=H'20'                                                        
         BNL   DNFULL                                                           
         TM    OPTIONS,XDETAIL                                                  
         BZ    DN84                                                             
         CH    R1,=H'19'                                                        
         BNL   DNFULL                                                           
*                                                                               
DN84     S     R7,DMCB+8                                                        
         BZ    DN92                                                             
         LR    RF,R6                                                            
         USING LINED,RF                                                         
         LA    R3,SCANBLCK                                                      
*                                                                               
DN86     OI    LINEHDR+6,X'80'                                                  
         MVC   LINEDATA+41(21),0(R3)                                            
         LA    R3,21(R3)                                                        
         LA    RF,86(RF)                                                        
         LH    R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STH   R1,LINE                                                          
         BCT   R7,DN86                                                          
         B     DN94                                                             
         DROP  RF                                                               
*                                                                               
DN88     ICM   R4,15,ACSH          HOURS OPTION                                 
         BZ    DN92                                                             
         USING TRCASHD,R4                                                       
         CLI   TRCSTYPE,C'H'                                                    
         BNE   DN92                                                             
*                                                                               
DN90     AP    HRTOTAL,TRCSAMNT                                                 
         LA    RF,LINEDATA+54                                                   
         EDIT  (P6,TRCSAMNT),(9,0(RF)),2,MINUS=YES                              
         DROP  R4                                                               
*                                                                               
DN92     LH    R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STH   R1,LINE                                                          
*                                                                               
DN94     MVC   LINEDATA+31(6),TRNSBTCH                                          
         MVC   LINEDATA+38(2),TRNSANAL                                          
         TM    LINEDATA+38,X'40'                                                
         BO    *+8                                                              
         MVI   LINEDATA+38,X'40'                                                
         LA    R8,LINEDATA+63                                                   
         EDIT  TRNSAMNT,(13,0(R8)),2,MINUS=YES                                  
         MVC   LINEDATA+76(2),=C'CR'                                            
         TM    TRNSSTAT,X'80'                                                   
         BO    *+14                                                             
         SP    TOTAL,TRNSAMNT                                                   
         B     DN96                                                             
         MVI   LINEDATA+76,C'D'                                                 
         AP    TOTAL,TRNSAMNT                                                   
*                                                                               
DN96     L     R5,AIO                                                           
         USING ACKEYD,R5                                                        
         LA    R3,13                                                            
         LA    R4,ACKEYCON+1                                                    
*                                                                               
DN98     CLI   0(R4),C' '                                                       
         BH    DN100                                                            
         LA    R4,1(R4)                                                         
         BCT   R3,DN98                                                          
*                                                                               
DN100    EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   LINEDATA(0),0(R4)                                                
         GOTO1 VDATCON,DMCB,(1,ACKEYDTE),(8,LINEDATA+15)                        
         MVC   LINEDATA+24(6),ACKEYREF                                          
         ICM   RF,15,AOAM          ORDER AMOUNT                                 
         BZ    DN102                                                            
         USING ACOAMTD,RF                                                       
         ICM   R5,15,ATRN                                                       
         BZ    DNXIT                                                            
         USING TRANSD,R5                                                        
         AP    TRNSAMNT,ACOAMT                                                  
         SP    TRNSAMNT,ACOAIVAL                                                
         LA    R8,LINEDATA+63                                                   
         EDIT  TRNSAMNT,(13,0(R8)),2,MINUS=YES,BRACKET=YES                      
*                                                                               
DN102    OI    LINEHDR+6,X'80'                                                  
         TM    OPTIONS,ATTRIBUT                                                 
         BZ    DN104                                                            
         BAS   RE,ANLP             BUILD ANALYSIS POINTERS                      
*                                                                               
DN104    TM    OPTIONS,XDETAIL                                                  
         BZ    DNXIT                                                            
         BAS   RE,XD                                                            
*                                                                               
DNXIT    LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
DNEND    MVC   LINEDATA+41(13),=C'ACCOUNT TOTAL'                                
         TM    OPTIONS,HOURS                                                    
         BNO   DNEND0                                                           
         LA    R8,LINEDATA+54                                                   
         EDIT  HRTOTAL,(9,0(R8)),2,MINUS=YES                                    
*                                                                               
DNEND0   LA    R8,LINEDATA+63                                                   
         EDIT  TOTAL,(13,0(R8)),2,MINUS=YES                                     
         OI    LINEHDR+6,X'80'                                                  
         MVI   VIRGIN,C'H'                                                      
*                                                                               
DNEND1   SR    RF,RF               CC = EQU FOR END                             
         B     XIT                                                              
*                                                                               
DNFULL   MVI   LINE+1,4            PRESERVE HEADS                               
         LNR   RB,RB               SET CC TO NEGATIVE FOR SCREEN FULL           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SET UP DISPLAY LINE FOR DDS=YES, XDETAIL=YES OPTIONS                *         
***********************************************************************         
         SPACE 1                                                                
XD       NTR1  ,                                                                
         LH    R6,LINE             R6 = A(LINE HEADER) - WE ALREADY             
         MH    R6,=H'86'           KNOW THERE'S ROOM FOR IT                     
         LA    R6,INFDATAH(R6)                                                  
         USING LINED,R6                                                         
         MVI   UNSCANBK,C' '                                                    
         MVC   UNSCANBK+1(199),UNSCANBK                                         
         SR    R3,R3               R3 = COUNT OF UNSCAN FIELDS                  
         LA    R4,UNSCANBK         R4 = A(UNSCAN BLOCK)                         
         L     R5,ATRN                                                          
         USING TRANSD,R5                                                        
*                                                                               
XD07     CLI   TRNSTYPE,0          TYPE                                         
         BE    XD10                                                             
         MVC   0(2,R4),=C'TY'                                                   
         EDIT  TRNSTYPE,(4,10(R4)),ALIGN=LEFT,ZERO=BLANK                        
         LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
*                                                                               
XD10     CLI   T606TWA+1,C'*'      STATUS FOR DDS ONLY                          
         BNE   XD13                                                             
         MVC   0(2,R4),=C'ST'      STATUS IN HEX                                
         GOTO1 VHEXOUT,DMCB,TRNSSTAT,10(R4),1,=C'MIX'                           
         LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
*                                                                               
XD13     ICM   R5,15,AOTH          OTHERS                                       
         BZ    XD40                                                             
         USING ACOTHERD,R5                                                      
         CLC   ACOTNUM(L'ACOTNUM+L'ACOTPROF),SPACES                             
         BE    XD40                                                             
         MVC   0(2,R4),=C'OT'                                                   
         LA    R8,10(R4)                                                        
         ZIC   R1,1(R5)            LENGTH OF ELEMENT                            
         SH    R1,=H'2'            LESS FIRST TWO BYTES                         
         LA    R5,2(R5)            BUMP TO BEGINNING OF DATA                    
         CLI   0(R5),C' '                                                       
         BE    XD31                                                             
*                                                                               
XD16     XR    RF,RF               RF=LENGTH FOR EXECUTED MOVE                  
         LA    RE,1(R5)            RE=LOCATION IN ELEMENT                       
         BCT   R1,*+8                                                           
         B     XD28                END OF ELEMENT - WRITE OUT STRING            
*                                                                               
XD19     CLI   0(RE),C'A'          SEARCH FOR END OF STRING OR DATE             
         BL    XD25                                                             
*                                                                               
XD22     LA    RE,1(RE)            KEEP ON GOING                                
         LA    RF,1(RF)                                                         
         BCT   R1,XD19                                                          
         B     XD28                END OF ELEMENT                               
*                                                                               
XD25     CLI   0(RE),X'80'                                                      
         BH    XD28                SHOULD BE A PACKED DATE - STOP               
         CLI   0(RE),C' '                                                       
         BH    XD22                SPECIAL CHARACTER - KEEP ON GOING            
*                                                                               
XD28     EX    RF,*+8              AT END - MOVE TO UNSCAN BLOCK                
         B     *+10                                                             
         MVC   0(0,R8),0(R5)                                                    
         LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
         LR    R8,R4                                                            
         LTR   R1,R1                                                            
         BZ    XD40                END OF ELEMENT                               
         LR    R5,RE                                                            
*                                                                               
XD31     CLI   0(R5),C' '          SCAN FOR NEXT FIELD (IF ANY)                 
         BNH   XD34                SEARCH FARTHER                               
         CLI   0(R5),C'A'          IS THIS A STRING                             
         BNL   XD16                YES                                          
         CLI   0(R5),X'80'         OR A SPECIAL CHARACTER                       
         BL    XD16                YES                                          
         B     XD37                NO - MUST BE A DATE                          
*                                                                               
XD34     LA    R5,1(R5)                                                         
         BCT   R1,XD31                                                          
         B     XD40                END OF ELEMENT                               
*                                                                               
XD37     MVC   DUB(2),0(R5)                                                     
         MVI   DUB+2,1                                                          
         GOTO1 VDATCON,DMCB,(1,DUB),(6,0(R8))                                   
         LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
*                                                                               
XD40     ICM   R5,15,APER          PERSONEL ELEMENT                             
         BZ    XD46                                                             
         USING ACPERSD,R5                                                       
         MVC   0(4,R4),=C'TIME'                                                 
         MVI   10(R4),C'B'                                                      
         TM    ACPSSTAT,ACPSBIL    IS IT BILLABLE ?                             
         BO    XD43                YES                                          
         MVI   10(R4),C'R'         NO                                           
         TM    ACPSSTAT,ACPSRTE    IS IT SPECIAL RATE ?                         
         BO    XD43                YES                                          
         MVI   10(R4),C'N'         NO, MUST BE NON-BILLABLE                     
*                                                                               
XD43     LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
*                                                                               
XD46     ICM   R5,15,ATRS          TRANSACTION STATUS                           
         BZ    XD49                                                             
         USING TRSELD,R5                                                        
         TM    TRSSTAT2,TRSSTADJ+TRSSTMSS+TRSSTIME                              
         BZ    XD49                                                             
         MVC   0(5,R4),=C'TTYPE'                                                
         TM    TRSSTAT2,TRSSTADJ   ADJUSTED                                     
         BNO   *+8                                                              
         MVI   10(R4),C'A'                                                      
         TM    TRSSTAT2,TRSSTMSS   MISSING                                      
         BNO   *+8                                                              
         MVI   10(R4),C'M'                                                      
         TM    TRSSTAT2,TRSSTIME   REGULAR TIME                                 
         BNO   *+8                                                              
         MVI   10(R4),C'T'                                                      
         LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
*                                                                               
XD49     ICM   R5,15,ACSHD         SUBSIDIARY CASH DISCOUNT                     
         BZ    XD52                                                             
         USING TRCASHD,R5                                                       
         MVC   0(4,R4),=C'DISC'                                                 
         EDIT  (P6,TRCSAMNT),(10,10(R4)),2,MINUS=YES,ALIGN=LEFT                 
         LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
*                                                                               
XD52     ICM   R5,15,ACSH          SUBSIDIARY CASH                              
         BZ    XD55                                                             
         USING TRCASHD,R5                                                       
         CLI   TRCSTYPE,C'D'       DISCOUNT 50'S HANDLED ABOVE                  
         BE    XD55                                                             
         CLI   TRCSTYPE,C'T'       TAX 50'S ARE HANDLED BELOW                   
         BE    XD55                                                             
         MVC   0(2,R4),=C'AM'                                                   
         EDIT  (P6,TRCSAMNT),(10,10(R4)),2,MINUS=YES,ALIGN=LEFT                 
         MVC   20(1,R4),TRCSTYPE                                                
         LA    R3,2(R3)                                                         
         LA    R4,2*L'UNSCANBK(R4)                                              
*                                                                               
XD55     ICM   R5,15,ATRS          ACTIVITY DATE                                
         BZ    XD58                                                             
         USING TRSTATD,R5                                                       
         MVC   0(2,R4),=C'DA'                                                   
         GOTO1 VDATCON,DMCB,(2,TRSTDATE),(X'20',10(R4))                         
         LA    R3,1(R3)                                                         
         LA    R4,L'UNSCANBK(R4)                                                
*                                                                               
XD58     L     R5,AIO              USED DATE                                    
         USING ACKEYD,R5                                                        
         OC    ACDTUSED,ACDTUSED                                                
         BZ    XD61                                                             
         MVC   0(2,R4),=C'US'                                                   
         GOTO1 VDATCON,DMCB,(2,ACDTUSED),(X'20',10(R4))                         
         LA    R4,L'UNSCANBK(R4)                                                
         LA    R3,1(R3)                                                         
*                                                                               
XD61     OC    ACDTPEEL,ACDTPEEL                                                
         BZ    XD64                                                             
         MVC   0(2,R4),=C'PE'                                                   
         GOTO1 VDATCON,DMCB,(2,ACDTPEEL),(X'20',10(R4))                         
         LA    R4,L'UNSCANBK(R4)                                                
         LA    R3,1(R3)                                                         
*                                                                               
XD64     ICM   R5,15,ATAX          TAX BUCKET                                   
         BZ    XD67                                                             
         USING TRCASHD,R5                                                       
         MVC   0(3,R4),=C'TAX'                                                  
         EDIT  (P6,TRCSAMNT),(10,10(R4)),2,MINUS=YES,ALIGN=LEFT                 
         LA    R4,L'UNSCANBK(R4)                                                
         LA    R3,1(R3)                                                         
*                                                                               
XD67     L     R5,AIO              USED DATE                                    
         USING ACKEYD,R5                                                        
         MVC   0(2,R4),=C'SQ'      SUBREFERENCE                                 
         SR    R0,R0                                                            
         IC    R0,ACKEYSBR                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  10(3,R4),DUB                                                     
         LA    R3,1(R3)                                                         
*                                                                               
XD70     LTR   R3,R3               ANYTHING TO UNSCAN                           
         BZ    XIT                                                              
         MVC   WORK,SPACES                                                      
         GOTO1 VUNSCAN,DMCB,((R3),UNSCANBK),(C'C',WORK)                         
         OI    LINEHDR+6,X'80'                                                  
         MVI   LINEDATA,C'('       PARENTHESES ROUND IT                         
         MVC   LINEDATA+1(77),WORK                                              
         LA    R4,LINEDATA+76                                                   
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C')'                                                       
         LH    R1,LINE             BUMP LINE NUMBER                             
         LA    R1,1(R1)                                                         
         STH   R1,LINE                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD ANALYSIS POINTERS                                             *         
***********************************************************************         
         SPACE 1                                                                
ANLP     NTR1  ,                   BUILD ANALYSIS POINTERS                      
         LH    R6,LINE             R6 = A(LINE HEADER) - WE ALREADY             
         MH    R6,=H'86'           KNOW THERE'S ROOM FOR IT                     
         LA    R6,INFDATAH(R6)                                                  
         USING LINED,R6                                                         
         CLC   ATTROUT(75),SPACES                                               
         BE    XIT                                                              
         OI    LINEHDR+6,X'80'                                                  
         MVC   LINEDATA(75),ATTROUT   MOVE IN FIRST LINE                        
         LH    R1,LINE             BUMP LINE COUNT                              
         LA    R1,1(R1)                                                         
         STH   R1,LINE                                                          
         CLC   ATTROUT+75(75),SPACES                                            
         BE    XIT                                                              
         LA    R6,86(R6)              NEXT LINE ON SCREEN                       
         OI    LINEHDR+6,X'80'                                                  
         MVC   LINEDATA(75),ATTROUT+75 MOVE IN SECOND LINE                      
         LH    R1,LINE             BUMP LINE COUNT                              
         LA    R1,1(R1)                                                         
         STH   R1,LINE                                                          
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
HEADING  DC    CL39'CONTRA         DATE     REF    BATCH  O'                    
         DC    CL39'F DESCRIPTION                 AMOUNT'                       
HEADING2 DC    CL39'------         ----     ---    -----  -'                    
         DC    CL39'- -----------                 ------'                       
*                                                                               
CLCTAB   DS    0CL15                                                            
         DC    AL1(13),CL14'CHECK NUMBER'                                       
         DC    AL1(13),CL14'DEPOSITED ON'                                       
*                                                                               
TRTAB    DS    0CL256              TR TABLE TO CLEAR UNPRINTABLES               
*                   0123456789ABCDEF                                            
         DC    CL16'                '   X'00' - X'0F'                           
         DC    CL16'                '   X'10'                                   
         DC    CL16'                '   X'20'                                   
         DC    CL16'                '   X'30'                                   
         DC    CL16'           . (+ '   X'40'                                   
         DC    CL16'&&          $*)  '   X'50'                                  
         DC    CL16'-/         ,%  ?'   X'60'                                   
         DC    CL16'              = '   X'70'                                   
         DC    CL16'                '   X'80'                                   
         DC    CL16'                '   X'90'                                   
         DC    CL16'                '   X'A0'                                   
         DC    CL16'                '   X'B0'                                   
         DC    CL16' ABCDEFGHI      '   X'C0'                                   
         DC    CL16' JKLMNOPQR      '   X'D0'                                   
         DC    CL16'  STUVWXYZ      '   X'E0'                                   
         DC    CL16'0123456789      '   X'F0' - X'FF'                           
*                                                                               
XDETAIL  EQU   X'80'               XDETAIL=YES OPTION IN USE                    
GROSS    EQU   X'40'               GROSS=YES OPTION IN USE                      
PEELED   EQU   X'20'               PEELED=YES OPTION IN USE                     
ENDDATE  EQU   X'20'               ENDDATE FILTER                               
NET      EQU   X'10'               NET=YES,OPTION IN USE                        
HOURS    EQU   X'08'               HOURS=MMMYY OPTION IN USE                    
REVERSED EQU   X'04'               REVERSED=YES OPTION IN USE                   
MOSRANGE EQU   X'02'               MOS RANGE FILTER IN USE                      
ATTRIBUT EQU   X'01'               SHOW ANAL/ATTRIBU POSTINGS                   
         EJECT                                                                  
**********************************************************************          
* KEYS TABLE COVERED BY DSECT KEYTABD                                *          
**********************************************************************          
         SPACE 1                                                                
KEYTABLE DC    CL10'COMPANY'                                                    
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(MYCO-GWS)                                                    
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'UNIT'                                                       
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS)                                                
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'LEDGER'                                                     
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(SCANBLCK-GWS+1)                                              
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'ACCOUNT'                                                    
         DC    C'M'                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(3)                                                           
         DC    AL1(12)                                                          
         DC    AL2(SCANBLCK-GWS+2)                                              
         DC    AL2(EDITACC-GWS)                                                 
*                                                                               
         DC    CL10'THE REST'                                                   
         DC    C' '                                                             
         DC    C'V'                                                             
         DC    X'00'                                                            
         DC    AL1(15)                                                          
         DC    AL1(27)                                                          
         DC    AL2(SPACES-GWS)                                                  
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* FILTER TABLE COVERED BY DSECT FILTERSD                              *         
***********************************************************************         
         SPACE 1                                                                
FILTABLE DC    CL10'AMOUNT'                                                     
         DC    CL2'AM'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSAMNT-TRANSD)                                             
         DC    AL1(10)                                                          
         DC    AL2(EDITCASH-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
APPRVSW  DC    CL10'APPROVE'                                                    
         DC    CL2'AP'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'02'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSSTAT-TRANSD)                                             
         DC    AL1(1)                                                           
         DC    X'FF00'                                                          
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ATTRIBUTE'                                                  
         DC    CL2'AT'                                                          
         DC    X'00'                                                            
         DC    CL8'YES'                                                         
         DC    X'01'                                                            
         DC    AL2(0)                                                           
         DS    6C                                                               
*                                                                               
         DC    CL10'BATCH'                                                      
         DC    CL2'BA'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSBTCH-TRANSD)                                             
         DC    AL1(L'TRNSBTCH)                                                  
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'CHECKNUM'                                                   
         DC    CL2'CH'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSNARR-TRANSD)                                             
         DC    AL1(6)                                                           
         DC    X'FF00'             CHECK FOR LEDGER SR                          
         DC    X'FF00'             NARRATION SEARCH                             
*                                                                               
         DC    CL10'CREDIT'                                                     
         DC    CL2'CR'                                                          
         DC    X'02'                                                            
         DC    CL8'YES'                                                         
         DC    X'80'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSSTAT-TRANSD)                                             
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'DATE'                                                       
         DC    CL2'DA'                                                          
         DC    X'58'                                                            
         DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYDTE-ACKEYD)                                             
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'DEBIT'                                                      
         DC    CL2'DR'                                                          
         DC    X'00'                                                            
         DC    CL8'YES'                                                         
         DC    X'80'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSSTAT-TRANSD)                                             
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'DDS'                                                        
         DC    CL2'DD'                                                          
         DC    X'80'                                                            
         DC    CL8'YES'                                                         
         DC    X'80'                                                            
         DC    AL2(0)                                                           
         DS    6C                                                               
*                                                                               
DEND     DC    CL10'DEND'                                                       
         DC    CL2'DE'                                                          
         DC    X'40'                                                            
         DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSNARR-TRANSD)                                             
         DC    AL1(8)                                                           
         DC    X'FF00'             CHECK FOR LEDGER SR                          
         DC    X'FF00'             NARRATION SEARCH                             
*                                                                               
         DC    CL10'DRAFT'                                                      
         DC    CL2'DF'                                                          
         DC    X'00'                                                            
         DC    CL8'Y/N/ONLY'                                                    
         DC    X'00'                                                            
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(8)                                                           
         DC    X'0000'             CHECK FOR LEDGER SR                          
         DC    X'0000'             NARRATION SEARCH                             
*                                                                               
DSTART   DC    CL10'DSTART'                                                     
         DC    CL2'DS'                                                          
         DC    X'40'                                                            
         DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSNARR-TRANSD)                                             
         DC    AL1(8)                                                           
         DC    X'FF00'             CHECK FOR LEDGER SR                          
         DC    X'FF00'             NARRATION SEARCH                             
*                                                                               
         DC    CL10'END'                                                        
         DC    CL2'EN'                                                          
         DC    X'48'                                                            
         DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYDTE-ACKEYD)                                             
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
         DC    AL2(EDITEND-GWS)                                                 
*                                                                               
         DC    CL10'GROSS'                                                      
         DC    CL2'GR'                                                          
         DC    X'00'                                                            
         DC    CL8'YES'                                                         
         DC    X'40'                                                            
         DC    AL2(0)                                                           
         DS    6C                                                               
*                                                                               
         DC    CL10'HELD'                                                       
         DC    CL2'HE'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'04'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSSTAT-TRANSD)                                             
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
HOURSKW  DC    CL10'HOURS'                                                      
         DC    CL2'HO'                                                          
         DC    X'20'                                                            
         DC    CL8'MMMYY'                                                       
         DC    X'00'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSBTCH-TRANSD)                                             
         DC    AL1(2)                                                           
         DC    X'FF00'                                                          
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'MARKED'                                                     
         DC    CL2'MA'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'08'                                                            
         DC    AL2(ATRS-GWS)                                                    
         DC    AL1(TRSSTAT-TRSELD)                                              
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'MOS'                                                        
         DC    CL2'MO'                                                          
         DC    X'20'                                                            
         DC    CL8'MMMYY'                                                       
         DC    X'00'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSBTCH-TRANSD)                                             
         DC    AL1(2)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'NEGATIVE'                                                   
         DC    CL2'NG'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'01'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSAMNT+5-TRANSD)                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'OFFICE'                                                     
         DC    CL2'OF'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSANAL-TRANSD)                                             
         DC    AL1(2)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'PEELED'                                                     
         DC    CL2'PE'                                                          
         DC    X'80'                                                            
         DC    CL8'YES'                                                         
         DC    X'20'                                                            
         DC    AL2(0)                                                           
         DS    6C                                                               
*                                                                               
         DC    CL10'REFERENCE'                                                  
         DC    CL2'RE'                                                          
         DC    X'18'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYREF-ACKEYD)                                             
         DC    AL1(L'ACKEYREF)                                                  
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'REVERSALS'                                                  
         DC    CL2'RV'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'20'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSSTAT-TRANSD)                                             
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'START'                                                      
         DC    CL2'ST'                                                          
         DC    X'50'                                                            
         DC    CL8'MMDDYY'                                                      
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(ACKEYDTE-ACKEYD)                                             
         DC    AL1(8)                                                           
         DC    AL2(0)                                                           
         DC    AL2(EDITSTRT-GWS)                                                
*                                                                               
TIMESKW  DC    CL10'TIME'                                                       
         DC    CL2'TI'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    XL2'FF00'                                                        
         DC    AL2(0)                                                           
*                                                                               
TTYPESKW DC    CL10'TTYPE'                                                      
         DC    CL2'TT'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    XL2'FF00'                                                        
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'TYPE'                                                       
         DC    CL2'TY'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSTYPE-TRANSD)                                             
         DC    AL1(3)                                                           
         DC    AL2(EDITYPE-GWS)                                                 
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'URGENT'                                                     
         DC    CL2'UR'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'40'                                                            
         DC    AL2(ATRN-GWS)                                                    
         DC    AL1(TRNSSTAT-TRANSD)                                             
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'XDETAIL'                                                    
         DC    CL2'XD'                                                          
         DC    X'00'                                                            
         DC    CL8'YES'                                                         
         DC    X'80'                                                            
         DC    AL2(0)                                                           
         DS    6C                                                               
*                                                                               
         DC    X'00'               END OF FILTER TABLE                          
         EJECT                                                                  
*                                                                               
* FATWA                                                                         
* ACINQDSECT                                                                    
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FATWA                                                          
       ++INCLUDE ACINQDSECT                                                     
         PRINT ON                                                               
         SPACE 2                                                                
OPTN     DS    XL1                 LOCAL OPTION                                 
APPROVED EQU   X'80'               APPROVED FOR PAYMENT                         
TIMEB    EQU   X'40'               TIME=B                                       
TIMEN    EQU   X'20'               TIME=N                                       
TIMER    EQU   X'10'               TIME=R                                       
TTYPET   EQU   X'08'               TTYPE=T                                      
TTYPEM   EQU   X'04'               TTYPE=M                                      
TTYPEA   EQU   X'02'               TTYPE=A                                      
TOTAL    DS    PL8       P         SAVED TOTAL AMOUNT FOR ACCOUNT               
HRTOTAL  DS    PL6       P         SAVED TOTAL HOURS FOR ACCOUNT                
ATTRIN   DS    CL150               LIST OF ATTRIBUTE ACCOUNTS                   
ATTROUT  DS    CL150               OUTPUT LIST AS ABOVE.                        
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACINQ02   05/01/02'                                      
         END                                                                    
