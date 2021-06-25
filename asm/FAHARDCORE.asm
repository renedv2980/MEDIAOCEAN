*          DATA SET FAHARDCORE AT LEVEL 002 AS OF 12/10/09                      
         TITLE 'PUT PRINT QUEUE INDEX INTO CORE'                                
*PHASE HARDCORA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
HARDCORE CSECT                                                                  
         ENTRY JOBSTEP                                                          
         ENTRY DSPACE                                                           
         ENTRY DDSTAG                                                           
         ENTRY WTLOG                                                            
         ENTRY PQNAME                                                           
         ENTRY QLEN                                                             
         ENTRY WORKAREA                                                         
         ENTRY SCANNER                                                          
         EJECT                                                                  
*                                                                               
         NBASE WORKL,HARDCORE,RA,WORK=VWORK                                     
         USING WORKD,RC                                                         
         L     R9,ALITS                                                         
         USING LITERALS,R9                                                      
*                                                                               
MAIN02   BRAS  RE,INIT             INITIALISE PRINT QUEUES                      
         BL    MAINX                                                            
*                                                                               
MAIN04   BRAS  RE,WAIT             WAIT UNTIL POSTED OR OPS COMMAND             
*                                                                               
         TM    JOBSTEP,JOBSTOP     FORCE EOJ POSTED?                            
         BO    MAINX               YES                                          
         TM    JOBSTEP,JOBEOJ      EOJ POSTED?                                  
         BZ    MAIN06              NO                                           
         LHI   R1,POPSCAN                                                       
         BRAS  RE,DOMSG                                                         
         B     MAINX                                                            
*                                                                               
MAIN06   TM    JOBSTEP,JOBINIT     RE-INITIALISE PQ?                            
         BZ    MAIN08              NO                                           
         LHI   R1,POPSINI                                                       
         BRAS  RE,DOMSG                                                         
         BRAS  RE,PQRBLD                                                        
         B     MAIN04                                                           
*                                                                               
MAIN08   BRAS  RE,CYCLE            MAIN PROGRAM CYCLE                           
         B     MAIN04                                                           
*                                                                               
MAINX    BRAS  RE,PRINTX                                                        
         J     XBASE                                                            
*                                                                               
VWORK    DC    V(WORKAREA)                                                      
ALITS    DC    A(LITERALS)                                                      
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         BRAS  RE,PRINTI                                                        
         BRAS  RE,CARDIN           READ AND VALIDATE INPUT CARDS                
         BL    EXITL               ERROR                                        
*                                                                               
         L     RF,VDDSIO           SET DDSIO VERSION                            
         MVC   0(8,RF),DDSTAG                                                   
*                                                                               
         L     R1,APQNAMES         FIND CORRECT ENTRY FOR PQ TABLE              
         USING PQNAMESD,R1                                                      
         XR    RF,RF                                                            
*                                                                               
INIT02   CLI   0(R1),EOT           LOOP THROUGH VALID NAMES                     
         BE    INIT04                                                           
         IC    RF,PQNXLEN                                                       
         EX    RF,PQNCLC                                                        
         BE    INIT06                                                           
         AHI   R1,PQNAMESL                                                      
         B     INIT02                                                           
*                                                                               
PQNCLC   CLC   PQNNAME(0),PQNAME                                                
*                                                                               
INIT04   LHI   R1,PBADNAME         NO MATCH - OUTPUT ERROR MESSAGE              
         BRAS  RE,DOMSG                                                         
         B     EXITL                                                            
*                                                                               
INIT06   ST    R1,APQNAME                                                       
         DROP  R1                                                               
*                                                                               
         LHI   RE,CXHDR-WORKD      SET HEADERS                                  
         AR    RE,RC                                                            
         MVC   0(L'CXHDRT,RE),CXHDRT                                            
         LHI   RE,CIHDR-WORKD                                                   
         AR    RE,RC                                                            
         MVC   0(L'CIHDRT,RE),CIHDRT                                            
*                                                                               
         LHI   RE,CXREC-WORKD      SET ADCONS FOR BUFFERS                       
         AR    RE,RC                                                            
         ST    RE,ACXREC                                                        
         LHI   RE,CIREC-WORKD                                                   
         AR    RE,RC                                                            
         ST    RE,ACIREC                                                        
*                                                                               
         BRAS  RE,GETSPC           BIND TO  REQUESTED DATASPACE                 
         BRAS  RE,FILLSPC          POPULATE REQUESTED DATASPACE                 
         BRAS  RE,SETOPS           SET UP OPERATOR COMMS                        
         BRAS  RE,PQOPEN           OPEN PRINT QUEUES AND BUILD INDICES          
*                                                                               
         LA    R0,4                MAKE NON-SWAPPABLE                           
         LNR   R0,R0                                                            
         SVC   247                                                              
*                                                                               
         LA    R1,PINITX           OUTPUT INITIALISATION COMPLETE MSG           
         BRAS  RE,DOMSG                                                         
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET WAIT UNTIL POSTED EITHER BY OPERATOR OR FACPAK                  *         
***********************************************************************         
         SPACE 1                                                                
WAIT     NTR1  ,                                                                
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,ALET                                                     
         L     R2,ASELF                                                         
         SAC   512                                                              
         USING TPQD,R2                                                          
         MVI   TPQFLAG,TPQFSLP     SET IMAGE WAITING FOR WORK                   
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
*                                                                               
         CLI   WTLOG,YES           TRACING APPLICATION OPTION ACTIVE?           
         BNE   WAIT02              NO                                           
         LHI   R1,PWAIT                                                         
         BRAS  RE,DOMSG                                                         
*                                                                               
WAIT02   L     R1,AECBLIST         BUILD LIST OF ECBS TO WAIT ON                
         LA    RF,POSTECB          WAKEUP ECB                                   
         ST    RF,0(R1)                                                         
*                                                                               
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),AOPERECB    OPERATOR ECB COMES LAST                      
         OI    0(R1),X'80'         FLAG EOL                                     
*                                                                               
         L     R1,AECBLIST                                                      
         WAIT  ECBLIST=(R1)                                                     
*                                                                               
         XC    JOBSTEP,JOBSTEP                                                  
         L     RF,AOPERECB         OPERATOR ECB POST MEANS TERMINATE            
         TM    0(RF),X'40'                                                      
         BZ    *+12                                                             
         BRAS  RE,OPSCOMS                                                       
         B     EXITOK                                                           
*                                                                               
         LA    RF,POSTECB          TURN OFF WAKEUP ECB FLAG                     
         NI    0(RF),255-X'40'                                                  
*                                                                               
         CLI   WTLOG,YES           TRACING APPLICATION OPTION ACTIVE?           
         BNE   EXITOK              NO                                           
         LHI   R1,PWKPOST          OUTPUT STARTING TO DO WORK MESSAGE           
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MAIN PROGRAM CYCLE CALLED WHEN WAKE-UP POST SENT BY FACPAK          *         
***********************************************************************         
         SPACE 1                                                                
CYCLE    NTR1  ,                                                                
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,ALET                                                     
         L     R2,ASELF                                                         
         SAC   512                                                              
         USING TPQD,R2                                                          
         MVI   TPQFLAG,TPQFWRK     SET BUSY                                     
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
*                                                                               
CYCLE02  L     R2,AQHEAD           GO TO QUEUE HEADER                           
         USING TPQHD,R2                                                         
*                                                                               
CYCLE04  ICM   R4,15,TPQFINQ       A(FIRST IN INPUT QUEUE)                      
         BZ    CYCLEX              NO NEW WORK                                  
         USING TPNTRYD,R4                                                       
*                                                                               
         LA    R1,TPQHLCK          R1=LOCKWORD                                  
         L     R5,TPNNEXT          R5=A(NEXT IN INPUT QUEUE)                    
         LA    R0,CS                                                            
         PLO   R4,TPQFINQ,R0,0(R0) MOVE IN NEXT TO HEAD OF QUEUE                
         BNE   CYCLE04                                                          
         DROP  R4                                                               
*                                                                               
         LR    R2,R4                                                            
         USING TPNTRYD,R2                                                       
         STCM  R2,15,THISHDR       SET A(CURRENT ENTRY)                         
         MVI   TPNFLAG,TPNFPRC     SET PROCESSING THIS ENTRY                    
         MVC   TIMEARR,TPNARR      GET ARRIVAL TIME                             
         DROP  R2                                                               
*                                                                               
         CLI   WTLOG,YES           TRACE OPTION ACTIVE?                         
         BNE   CYCLE06             NO                                           
         TIME  DEC                                                              
         ST    R0,TIMEIN           SET PROCESSING START TIME                    
*                                                                               
CYCLE06  BRAS  RE,DOWORK           GO DO REQUESTED WORK                         
         BRAS  RE,POSTFAC          POST WORK COMPLETE                           
         B     CYCLE02                                                          
*                                                                               
CYCLEX   J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PARSE INDICES FOR REQUESTED INDEX RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
DOWORK   NTR1  ,                                                                
         ICM   R2,15,THISHDR                                                    
         USING TPNTRYD,R2                                                       
         XC    TPNRTN,TPNRTN                                                    
         MVC   LCLNTRY,0(R2)                                                    
         LA    RF,TPACTTAB                                                      
*                                                                               
DWRK02   CLI   0(RF),X'FF'                                                      
         BNE   *+12                                                             
         MVI   TPNRTN,TPNRIACT                                                  
         J     EXITOK                                                           
*                                                                               
         CLC   TPNACT,0(RF)                                                     
         BE    *+12                                                             
         LA    RF,L'TPACTTAB(RF)                                                
         B     DWRK02                                                           
*                                                                               
         ICM   RF,7,1(RF)                                                       
         BASR  RE,RF                                                            
         B     EXITOK                                                           
         DROP  R2                                                               
*                                                                               
TPACTTAB DS    0XL4                                                             
         DC    AL1(TPNANDX),AL3(INDEX)   RETURN A(INDEX BLOCK FOR D/A)          
         DC    AL1(TPNAPUT),AL3(PUTREC)  WRITE SINGLE INDEX RECORD              
         DC    AL1(TPNANEW),AL3(NXTFRE)  GET NEXT FREE INDEX RECORD             
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO RETURN A(INDEX) IN THIS ADDRESS SPACE TO CALLER          *         
* NTRY: R2=A(TPNTRYD)                                                 *         
*       ACCESS REGISTERS OFF                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TPNTRYD,R2                                                       
INDEX    NTR1  ,                                                                
         BRAS  RE,GETPQ            GET NDXXTNT ENTRY AND BLKTAB ENTRY           
         BNE   EXITL                                                            
         USING BLKTABD,R4                                                       
*                                                                               
         L     RF,BLKADD                                                        
         STCM  RF,15,TPNBADD       SET A(BUFFER)                                
         MVC   TPNBLEN,BLKLEN      MOVE BACK L'BUFFER                           
         J     EXITOK                                                           
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WRITE INDEX RECORD TO FILE                               *         
* NTRY: R2=A(TPNTRYD)                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING TPNTRYD,R2                                                       
PUTREC   NTR1  ,                                                                
         BRAS  RE,GETPQ            GET NDXXTNT ENTRY AND BLKTAB ENTRY           
         JNE   EXITL                                                            
         USING NDXXTNTD,R3                                                      
         USING BLKTABD,R4                                                       
*                                                                               
         L     RF,BLKADD           GET A(BLOCK)                                 
         XR    RE,RE                                                            
         ICM   RE,3,TPNRNUM        PICK UP INDEX NUMBER                         
         AR    RF,RE               INDEX INTO BLOCK AND MOVE IN RECORD          
         MVC   0(L'PQINDEX,RF),TPNNDX                                           
*                                                                               
         LA    R1,DMCB                                                          
         MVC   0(4,R1),VWTID                                                    
         OI    0(R1),X'80'         SET SPECIAL NO WAIT                          
*                                                                               
         L     RF,BLKADD                                                        
         ST    RF,4(R1)                                                         
*                                                                               
         MVC   10(2,R1),BLKLEN                                                  
         MVC   12(4,R1),NDXADTF                                                 
         LA    RF,BLKDA                                                         
         ST    RF,16(R1)                                                        
*                                                                               
         GOTO1 VDADDS,(R1)         WRITE BACK BLOCK                             
         J     EXITOK                                                           
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* GET NEXT FREE INDEX RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TPNTRYD,R2                                                       
NXTFRE   NTR1  ,                                                                
         MVC   LOGRKEY,TPNNKEY                                                  
         BRAS  RE,GETPQ            GET NDXXTNT ENTRY AND BLKTAB ENTRY           
         BNE   EXITL                                                            
         BRAS  RE,GETDATE          GET DATE/TIME                                
*                                                                               
         USING NDXXTNTD,R3                                                      
         USING CIDATAD,NDXDATA                                                  
         BRAS  RE,BLDAVT           BUILD TABLE OF AVAILABLE ENTRIES             
*                                                                               
         LA    R5,AVTAB            END OF INDEX SEARCH                          
         USING AVTABD,R5                                                        
         TM    TPNNTYPE,TPNNTBIG   BIG ENTRY REQUIRED?                          
         BZ    *+8                 IGNORE FIRST ENTRY IF PURGE SEARCH           
NXTF02   AHI   R5,AVTABL                                                        
         CLI   AVTSTA1,X'FF'       TEST LAST ENTRY IN AVAIL TABLE               
         BE    NXTF04                                                           
         OC    AVBLK,AVBLK         FIND FIRST AVAIL ENTRY                       
         BNZ   NXTF08                                                           
         B     NXTF02                                                           
*                                                                               
NXTF04   TM    TPNNTYPE,TPNNTP2    NO SPACE IN PART2 INDEX                      
         BZ    NXTF06                                                           
         MVI   TPNRTN,X'90'        SET EOF/NF                                   
         B     NXTFX                                                            
*                                                                               
NXTF06   OC    AVBLK,AVBLK         TEST IF SENT/RETAINED IN LAST ENTRY          
         BNZ   NXTF08                                                           
         AHI   R5,AVTABL                                                        
         OC    AVBLK,AVBLK         TEST IF PRTD/RETAINED IN LAST ENTRY          
         BNZ   NXTF08                                                           
         MVI   TPNRTN,X'90'        SET EOF/NF                                   
         B     NXTFX                                                            
*                                                                               
NXTF08   ICM   R4,15,AVBLK         AVBLK=A(BLKTAB ENTRY)                        
         USING BLKTABD,R4                                                       
         MVC   TPNDA,BLKDA         SET D/A OF BLOCK                             
         L     RF,BLKADD                                                        
         STCM  RF,15,TPNNADD       SET A(BLOCK)                                 
         MVC   TPNNLEN,BLKLEN      SET LENGTH OF BLOCK                          
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,AVENTRY                                                     
         MH    R0,CINDXLN                                                       
         STCM  R0,3,TPNNDSP        SET DISP TO ENTRY IN BLOCK                   
*                                                                               
         ICM   R1,15,BLKADD        R1=A(BLOCK)                                  
         AR    R1,R0                                                            
*                                                                               
         XC    TPNNKEY,0(R1)       SWAP KEYS (OLD/NEW)                          
         XC    0(L'TPNNKEY,R1),TPNNKEY                                          
         XC    TPNNKEY,0(R1)                                                    
         XC    TPNRTN,TPNRTN                                                    
                                                                                
         LA    R1,DMCB                                                          
         MVC   0(4,R1),VWTID                                                    
         OI    0(R1),X'80'         SET SPECIAL NO WAIT                          
*                                                                               
         L     RF,BLKADD                                                        
         ST    RF,4(R1)                                                         
*                                                                               
         MVC   10(2,R1),BLKLEN                                                  
         MVC   12(4,R1),NDXADTF                                                 
         LA    RF,BLKDA                                                         
         ST    RF,16(R1)                                                        
*                                                                               
         GOTO1 VDADDS,(R1)         WRITE BACK BLOCK                             
*                                                                               
NXTFX    B     EXITOK                                                           
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SEARCH PQ INDEX TO BUILD AVAILABLE INDEX TABLE           *         
***********************************************************************         
         SPACE 1                                                                
         USING TPNTRYD,R2                                                       
DS       USING PQRECD,TPNNKEY                                                   
         USING NDXXTNTD,R3                                                      
         USING CIDATAD,NDXDATA                                                  
BLDAVT   NTR1  ,                                                                
         XC    TPNNSEQ,TPNNSEQ     RETURN HIGH FILE NUMBER FOR USERID           
         XC    CXENTRY,CXENTRY                                                  
         LA    R0,AVTAB            RESET AVTAB                                  
         LHI   R1,AVSTAL                                                        
         LA    RE,AVSTA                                                         
         LHI   RF,AVSTAL                                                        
         MVCL  R0,RE                                                            
*                                                                               
         TM    TPNNTYPE,TPNNTP1    WANT PART 1 INDEX?                           
         BZ    BAVT04              YES                                          
*                                                                               
         L     R4,NDXBUFF                                                       
         USING BLKTABD,R4                                                       
         ICM   R5,15,BLKADD        GET A(FIRST BLOCK)                           
         USING PQRECD,R5                                                        
         LH    RF,CICINDX                                                       
         STH   RF,CXENTRY                                                       
         MH    RF,CINDXLN                                                       
         AR    R5,RF               R5=A(FIRST PART 1 ENTRY)                     
*                                                                               
         TM    TPNNTYPE,TPNNTBIG   BIG PART1 SEARCH?                            
         BO    BAVT02              YES                                          
         ICM   RE,15,NDXRP1N       BUMP PART 1 SEARCHES                         
         AL    RE,ONE                                                           
         STCM  RE,15,NDXRP1N                                                    
         B     BAVT06                                                           
*                                                                               
BAVT02   ICM   RE,15,NDXRP1O       BUMP PART 1 BIG SEARCHES                     
         AL    RE,ONE                                                           
         STCM  RE,15,NDXRP1O                                                    
         B     BAVT06                                                           
*                                                                               
BAVT04   L     R4,NDXBUF2          R5=A(FIRST PART 2 ENTRY BLOCK)               
         ICM   R5,15,BLKADD        GET A(BLOCK)                                 
         LH    RF,CJENTRY                                                       
         STH   RF,CXENTRY                                                       
         MH    RF,CINDXLN                                                       
         AR    R5,RF               R5=A(FIRST PART 2 ENTRY)                     
*                                                                               
         ICM   RE,15,NDXRP2A       BUMP PART 2 SEARCHES                         
         AL    RE,ONE                                                           
         STCM  RE,15,NDXRP2A                                                    
*                                                                               
BAVT06   TM    TPNNTYPE,TPNNTP2    PART 2 INDEX SEARCH?                         
         BZ    BAVT08              NO                                           
         CLI   PQSTAT,PQSTPU       PART 2 IS PURGED?                            
         BNE   BAVT30              NO                                           
*                                                                               
         LA    R6,AVTAB            FIRST AVTAB ENTRY IS PURGED SLOT             
         USING AVTABD,R6                                                        
         MVC   AVENTRY,CXENTRY     SAVE ENTRY NUMBER                            
         ST    R4,AVBLK            SAVE BLOCK                                   
         MVC   AVTIME,PQAGERD      SAVE DATE/TIME                               
         MVC   AVSIZE,PQAGES       SAVE SIZE FACTOR                             
         B     EXITOK              EXIT AT ONCE IF FIND PURGED PART 2           
         DROP  R6                                                               
*                                                                               
BAVT08   CLI   PQSTAT,PQSTPU       PART 1 INDEX PURGED?                         
         BNE   BAVT10              NO                                           
         TM    TPNNTYPE,TPNNTBIG   WANT LARGE REPORT?                           
         BO    BAVT30              YES - IGNORE PURGED                          
*                                                                               
         LA    R6,AVTAB            FIRST AVTAB ENTRY IS PURGED SLOT             
         USING AVTABD,R6                                                        
         OC    AVBLK,AVBLK         TABLE ALREADY CONTAINS AN ENTRY?             
         BZ    BAVT09              NO                                           
         CLC   AVTIME,PQAGERD      COMPARE RETN DATE/TIME                       
         BNH   BAVT30              IGNORE IF ALREADY HAVE OLDER ENTRY           
*                                                                               
BAVT09   MVC   AVENTRY,CXENTRY     SAVE ENTRY NUMBER                            
         ST    R4,AVBLK            SAVE BLOCK                                   
         MVC   AVTIME,PQAGERD      SAVE DATE/TIME                               
         MVC   AVSIZE,PQAGES       SAVE SIZE FACTOR                             
         B     BAVT30                                                           
         DROP  R6                                                               
*                                                                               
BAVT10   CLC   PQSRCID,DS.PQSRCID  SAVE HIGH FILE NUMBER FOR USER ID            
         BNE   BAVT12                                                           
         CLC   TPNNSEQ,PQREPNO                                                  
         BH    BAVT12                                                           
         MVC   TPNNSEQ,PQREPNO                                                  
*                                                                               
BAVT12   TM    PQSTAT,PQSTTE       IGNORE TEMP AND PRINTING REPORTS             
         BO    BAVT30                                                           
         TM    TPNNTYPE,TPNNTBIG   IGNORE SINGLE ENTRYS IF BIG SEARCH           
         BZ    *+12                                                             
         CLI   PQSEQ,0                                                          
         BE    BAVT30                                                           
*                                                                               
*&&UK*&& TM    PQSTAT,PQSTAC+PQSTKE                                             
*&&UK*&& BNO   *+14                                                             
*&&UK*&& CLC   PQSUBID,=C'LU1'     IGNORE AVAILABLE LINE UP REPORTS             
*&&UK*&& BE    BAVT30                                                           
*                                                                               
         CLC   DATEC(3),PQAGERD    RETAINED REPORTS ARE DIFFERENT               
         BL    BAVT28                                                           
*                                                                               
         LA    R6,AVTAB            FIRST AVTAB ENTRY IS PURGED SLOT             
         AHI   R6,AVTABL           GO PAST IT                                   
         USING AVTABD,R6                                                        
*                                                                               
BAVT14   MVC   BYTE,AVTSTA1        TEST FOR STATUS BITS                         
         NC    BYTE,PQSTAT         COMPARE STATUS                               
         BZ    BAVT26                                                           
*                                                                               
BAVT18   TM    AVTSTA2,AVT2USER    TEST IF MUST HAVE SAME USER ID               
         BZ    BAVT20                                                           
         CLC   PQSRCID,DS.PQSRCID                                               
         BNE   BAVT26                                                           
*                                                                               
BAVT20   TM    AVTSTA2,AVT2SIZE    TEST IF REPORT SIZE IMPORTANT                
         BZ    BAVT22                                                           
         CLI   PQSEQ,0                                                          
         BE    BAVT22                                                           
         CLI   PQAGES,100          ONLY FOR LARGE REPORTS                       
         BL    BAVT22                                                           
         CLC   AVSIZE,PQAGES       COMPARE SIZE                                 
         BH    BAVT30              EXIT IF ALREADY HAVE LARGER ENTRY            
         BL    BAVT24                                                           
*                                                                               
BAVT22   OC    AVBLK,AVBLK         TABLE ALREADY CONTAINS AN ENTRY?             
         BZ    *+14                NO                                           
         CLC   AVTIME,PQAGERD      COMPARE RETN DATE/TIME                       
         BNH   BAVT26              IGNORE IF ALREADY HAVE OLDER ENTRY           
*                                                                               
BAVT24   STCM  R4,15,AVBLK         SAVE PAGE ENTRY IN AVTAB                     
         MVC   AVENTRY,CXENTRY     SAVE INDEX NUMBER IN AVTAB                   
         MVC   AVTIME,PQAGERD      SAVE DATE/TIME                               
         MVC   AVSIZE,PQAGES       SAVE SIZE FACTOR                             
         B     BAVT30                                                           
*                                                                               
BAVT26   AHI   R6,AVTABL           NEXT AVTAB ENTRY                             
         CLI   AVTSTA1,255         END OF REGULAR ENTRIES?                      
         BNE   BAVT14              NO                                           
         B     BAVT30                                                           
         DROP  R6                                                               
*                                                                               
BAVT28   TM    PQSTAT,PQSTKE       RETAINED REPORT CANT BE KEEP                 
         BO    BAVT30                                                           
         TM    PQSTAT,PQSTDEAD     RETAINED REPORT MUST BE DEAD                 
         BZ    BAVT30                                                           
*                                                                               
         LA    R6,AVTAB            SAVE OLDEST REPORT FOR TYPE                  
         USING AVTABD,R6                                                        
         CLI   0(R6),255           LOOK FOR FF SPECIALS AT END                  
         BE    *+12                                                             
         AHI   R6,AVTABL           NEXT IN AVTAB                                
         B     *-12                                                             
*                                                                               
         TM    PQSTAT,PQSTSE                                                    
         BO    *+8                                                              
         AHI   R6,AVTABL           POINT TO LAST ENTRY (PRTD STATUS)            
*                                                                               
         CLC   AVTIME,PQAGERD      COMPARE RETN DATE/TIME                       
         BNH   BAVT30              EXIT IF ALREADY HAVE OLDER ENTRY             
         MVC   AVENTRY,CXENTRY     SAVE ENTRY NUMBER                            
         ST    R4,AVBLK            SAVE BLOCK                                   
         MVC   AVTIME,PQAGERD      SAVE DATE/TIME                               
         MVC   AVSIZE,PQAGES       SAVE SIZE FACTOR                             
         B     BAVT30                                                           
         DROP  R6                                                               
*                                                                               
BAVT30   LH    RF,CXENTRY                                                       
         AHI   RF,1                                                             
         STH   RF,CXENTRY                                                       
         CH    RF,CIENTRYS                                                      
         BNL   BAVT32                                                           
         AH    R5,CINDXLN                                                       
         CLC   0(2,R5),FF32                                                     
         BNE   BAVT06                                                           
         J     EXITOK              END OF INDEX                                 
*                                                                               
BAVT32   XC    CXENTRY,CXENTRY                                                  
         AHI   R4,NDXXTNTL                                                      
         CLI   0(R4),C' '                                                       
         JNH   EXITOK              END OF INDEX ENTRIES                         
         ICM   R5,15,BLKADD                                                     
         B     BAVT06              END OF PAGE                                  
         DROP  DS,R2,R3,R4                                                      
         EJECT                                                                  
***********************************************************************         
* AVAILABLE REPORT USAGE SEQUENCE TABLES                              *         
*                                                                     *         
* LINES IN TABLE CORRESPOND TO FOLLOWING. COVERED BY AVTABD           *         
* COPY OF TABLE IS MOVED INTO AVTAB EVERY TIME INDEX IS SEARCHED      *         
*                                                                     *         
*   PURGED    N/A                                                     *         
*   SENT      SAMEUSER  LARGEST                                       *         
*   PRTD      SAMEUSER  LARGEST                                       *         
*   LIVE      SAMEUSER                                                *         
*                                                                     *         
*   SENT      ANYUSER   LARGEST                                       *         
*   PRTD      ANYUSER   LARGEST                                       *         
*   LIVE      ANYUSER                                                 *         
*                                                                     *         
*   LAST/SENT N/A                                                     *         
*   LAST/PRTD N/A                                                     *         
***********************************************************************         
         SPACE 1                                                                
AVSTA    DC    AL1(PQSTPU),AL1(0)                                               
         DC    XL2'0000',XL4'00000000',XL3'FFFFFF',X'00',XL4'00000000'          
         DC    AL1(PQSTSE),AL1(AVT2USER+AVT2SIZE)                               
         DC    XL2'0000',XL4'00000000',XL3'FFFFFF',X'00',XL4'00000000'          
         DC    AL1(PQSTPR),AL1(AVT2USER+AVT2SIZE)                               
         DC    XL2'0000',XL4'00000000',XL3'FFFFFF',X'00',XL4'00000000'          
         DC    AL1(PQSTLIVE),AL1(AVT2USER)                                      
         DC    XL2'0000',XL4'00000000',XL3'FFFFFF',X'00',XL4'00000000'          
*                                                                               
         DC    AL1(PQSTSE),AL1(AVT2SIZE)                                        
         DC    XL2'0000',XL4'00000000',XL3'FFFFFF',X'00',XL4'00000000'          
         DC    AL1(PQSTPR),AL1(AVT2SIZE)                                        
         DC    XL2'0000',XL4'00000000',XL3'FFFFFF',X'00',XL4'00000000'          
         DC    AL1(PQSTLIVE),AL1(AVT2SIZE)                                      
         DC    XL2'0000',XL4'00000000',XL3'FFFFFF',X'00',XL4'00000000'          
*                                                                               
         DC    AL1(255),AL1(0)                                                  
         DC    XL2'0000',XL4'00000000',XL3'FFFFFF',X'00',XL4'00000000'          
         DC    AL1(255),AL1(0)                                                  
         DC    XL2'0000',XL4'00000000',XL3'FFFFFF',X'00',XL4'00000000'          
AVSTAL   EQU   *-AVSTA                                                          
*                                                                               
AVTABD   DSECT                                                                  
AVTSTA   DS    0XL2                                                             
AVTSTA1  DS    XL1                 FLAGS FROM PQSTAT TO MATCH                   
AVTSTA2  DS    XL1                                                              
AVT2USER EQU   X'80'               USERID IMPORTANT                             
AVT2SIZE EQU   X'40'               SIZE IMPORTANT                               
*                                                                               
AVENTRY  DS    XL2                 ENTRY NUMBER                                 
AVBLK    DS    XL4                 BLOCK ADDRESS                                
AVTIME   DS    XL3                 REPORT TIME (FROM PQAGERD(3))                
AVSIZE   DS    XL1                 REPORT SIZE (FROM PQAGES)                    
         DS    XL4                 N/D                                          
AVTABL   EQU   *-AVTABD                                                         
*                                                                               
HARDCORE CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET TODAYS TIME AND DATE AND STORE IT IN VARIOUS FORMATS *         
***********************************************************************         
         SPACE 1                                                                
GETDATE  NTR1  ,                                                                
         DATE  DATEE               DATEE=C'YYMMDDCC'                            
*                                                                               
         MVC   DATEB(2),DATEE      TEST CHANGE IN CENTURY                       
         XR    R1,R1                                                            
         CLI   DATEB,C'9'                                                       
         BNH   GDT02                                                            
         IC    R1,DATEB                                                         
         SH    R1,=H'10'                                                        
         STC   R1,DATEB                                                         
         LA    R1,100                                                           
*                                                                               
GDT02    PACK  DUB,DATEB+0(2)                                                   
         CVB   R0,DUB                                                           
         AR    R0,R1                                                            
         STC   R0,DATEB+0                                                       
         PACK  DUB,DATEE+2(2)                                                   
         CVB   R0,DUB                                                           
         STC   R0,DATEB+1                                                       
         PACK  DUB,DATEE+4(2)                                                   
         CVB   R0,DUB                                                           
         STC   R0,DATEB+2          DATEB=X'YYMMDD'                              
         ICM   R1,15,DATEB                                                      
         SLDL  R0,8                                                             
         SLL   R1,4                                                             
         SLDL  R0,4                                                             
         SLL   R1,3                                                             
         SLDL  R0,5                                                             
         STCM  R0,3,DATEC          DATEC=B'YYYYYYYMMMMDDDDD'                    
*                                                                               
         TIME  TU                  R0=TIME IN 1/38400 SECS                      
*                                                                               
         SRDL  R0,32                                                            
         D     R0,=F'38400'        R1=TIME IN SECONDS                           
         LR    RF,R1                                                            
         MHI   R1,3                                                             
         SRL   R1,2                R1=(SECS*3)/4                                
         STH   R1,TIMEC            TIMEC=TIME IN SPECIAL UNITS                  
         LR    R1,RF                                                            
         SR    R0,R0                                                            
         D     R0,=F'60'           R1=BINARY MINUTES                            
         SR    RE,RE                                                            
         LR    RF,R1                                                            
         D     RE,=F'10'                                                        
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         AHI   RF,1                                                             
         STC   RF,TIMEI            TIMEI=SINGLE BYTE 10MIN INCREMENT            
         XR    R0,R0                                                            
         D     R0,=F'60'           R0=MINS,R1=HOURS                             
         STC   R1,TIMEB                                                         
         STC   R0,TIMEB+1          TIMEB=B'HHHHHHHHMMMMMMMM'                    
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIND PQ IN PQXTNTS                                       *         
* NTRY: R2     = A(TPNTRY)                                            *         
*                                                                     *         
* EXIT: R3     = A(PQ ENTRY)                                          *         
*       R4     = A(BLOCK FOR D/A SPECIFIED)                           *         
*       CC EQ  = PQ ENTRY WAS FOUND AND BLOCK WAS FOUND               *         
* OR                                                                  *         
*       TPNRTN SET TO TPNRIPQ/TPNRIDA                                 *         
*       CC NEQ = COULD NOT MATCH PQ ENTRY OR FIND BLOCK               *         
***********************************************************************         
         SPACE 1                                                                
         USING TPNTRYD,R2                                                       
GETPQ    NTR1  ,                                                                
         LA    R3,PQXTNTS                                                       
         USING NDXXTNTD,R3                                                      
GETPQ02  CLI   0(R3),C' '                                                       
         BH    GETPQ04                                                          
         MVI   TPNRTN,TPNRIPQ      SET INVALID PQ                               
         J     GETPQL                                                           
*                                                                               
GETPQ04  CLC   NDXENUM,TPNPQ       MATCH PQ?                                    
         BE    GETPQ06             YES                                          
         AHI   R3,NDXXTNTL                                                      
         B     GETPQ02                                                          
*                                                                               
GETPQ06  ICM   R4,15,NDXBUFF       GET A(BUFFER LIST)                           
         USING BLKTABD,R4                                                       
GETPQ08  CLC   BLKNUM,=X'FFFF'                                                  
         BNE   GETPQ10                                                          
         MVI   TPNRTN,TPNRIDA      SET INVALID D/A                              
         J     GETPQL              SET CC NEQ                                   
*                                                                               
GETPQ10  OC    TPNDA,TPNDA         FIRST BLOCK?                                 
         JZ    GETPQOK             YES                                          
         CLC   TPNDA,BLKDA         MATCH D/A REQUESTED?                         
         JE    GETPQOK             YES                                          
         AHI   R4,BLKTABLQ                                                      
         B     GETPQ08                                                          
*                                                                               
GETPQL   CLI   *,255                                                            
         XIT1  REGS=(R3,R4)                                                     
*                                                                               
GETPQOK  CR    RB,RB                                                            
         XIT1  REGS=(R3,R4)                                                     
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* SET UP OPERATOR COMMUNICATIONS                                      *         
***********************************************************************         
         SPACE 1                                                                
SETOPS   NTR1  ,                                                                
         LHI   R1,PSETOP           OUTPUT INITIALISING OPERATOR MSG             
         BRAS  RE,DOMSG                                                         
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         L     R2,COMCIBPT         GET A(CIB)                                   
         USING CIBNEXT,R2                                                       
         LA    R3,COMCIBPT         SET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTART    TEST FOR 'S JOBNAME' (IE NOT BATCH)          
         BNE   SETOP2                                                           
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)        RELEASE THE CIB                    
*                                                                               
SETOP2   QEDIT ORIGIN=(R3),CIBCTR=1          ACCEPT MODIFY COMMANDS             
*                                                                               
         LHI   R1,PSETOPX          OUTPUT OPERATOR ECB INITIALISED              
         BRAS  RE,DOMSG                                                         
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PROCESS OPERATOR COMMUNICATIONS                                     *         
***********************************************************************         
         SPACE 1                                                                
OPSCOMS  NTR1  ,                                                                
         LA    R1,POPSCOM          OUTPUT PROCEESING OPS COMMAND MSG            
         BRAS  RE,DOMSG                                                         
*                                                                               
         L     RF,ACOMM            SET RF TO COMM BLOCK                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         USING CIBNEXT,R2                                                       
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BNE   OPSC02                                                           
         MVI   JOBSTEP,JOBSTOP     SET EOJ FLAG                                 
         LA    R3,POPSSTOP                                                      
         BRAS  RE,DOMSG                                                         
         J     OPSCX                                                            
*                                                                               
OPSC02   CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         BE    OPSC04              YES                                          
         LA    R1,PBADCMDU         BAD COMMAND (UNKNOWN VERB)                   
         BRAS  RE,DOMSG                                                         
         J     OPSCX                                                            
*                                                                               
OPSC04   XR    R1,R1               GET DATA LEN IN R1                           
         ICM   R1,3,CIBDATLN                                                    
         CHI   R1,L'CARD                                                        
         BNH   OPSC06                                                           
         LA    R1,PBADCMDL         BAD COMMAND (TOO LONG)                       
         BRAS  RE,DOMSG                                                         
         J     OPSCX                                                            
*                                                                               
OPSC06   MVC   CARD,SPACES         MOVE DATA TO WORK AND SCAN IT                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CARD(0),CIBDATA                                                  
*                                                                               
         L     R1,ACMDTAB                                                       
         BRAS  RE,CARDPRC          VALIDATE OPERATOR COMMAND                    
*                                                                               
OPSCX    BRAS  RE,CLROPS           CLEAR OPERATOR COMMAND FROM CIB              
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CLEAR OPS COMMAND (FREE CIB)                                        *         
***********************************************************************         
         SPACE 1                                                                
CLROPS   NTR1                                                                   
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         ICM   R2,15,COMCIBPT      A(CIB)                                       
         JZ    EXITOK                                                           
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)                                           
         J     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* GET REQUESTED DATASPACE                                             *         
***********************************************************************         
         SPACE 1                                                                
GETSPC   NTR1  ,                                                                
         LHI   R1,PGETSPC          OUTPUT ATTEMPT TO BIND MESSAGE               
         BRAS  RE,DOMSG                                                         
*                                                                               
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'GETA'                                                 
         MVC   WORK+4(12),DSPACE                                                
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   OFFS,WORK+20        EXTRACT VALUES                               
         MVC   ALET,WORK+24                                                     
         MVC   STOKN,WORK+28                                                    
*                                                                               
         LHI   R1,PGETSPCX         OUTPUT BIND SUCCESSFUL MESSAGE               
         BRAS  RE,DOMSG                                                         
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* REGISTER SELF WITHIN DATASPACE                                      *         
***********************************************************************         
         SPACE 1                                                                
FILLSPC  NTR1  ,                                                                
         LHI   R1,PFILSPC          OUTPUT INITIALISING DATASPACE MSG            
         BRAS  RE,DOMSG                                                         
*                                                                               
         EXTRACT RESULTS,'S',FIELDS=(TIOT,ASID)                                 
         ALESERV EXTRACTH,STOKEN=ASTOKEN                                        
*                                                                               
         L     RF,RXTIOT                                                        
         MVC   JOBNAME,0(RF)       GET JOBNAME FOR THIS IMAGE                   
*                                                                               
         ICM   R0,15,QLEN          GET ENOUGH SPACE FOR QUEUE                   
         MHI   R0,TPNTRYLQ                                                      
         AHI   R0,TPQHDLQ                                                       
         AHI   R0,4095                                                          
         SRL   R0,12                                                            
         SLL   R0,12               ROUND TO NEXT 4K                             
*                                                                               
         GETMAIN RU,LV=(0),LOC=(BELOW,ANY),BNDRY=DBLWD                          
         LTR   RF,RF                                                            
         BZ    FILLS02                                                          
         WTO   'PQSVR PROGRAM GETMAIN FAILURE - INCREASE REGION'                
         ABEND 922,DUMP                                                         
*                                                                               
FILLS02  LR    R2,R1                                                            
         MVC   0(TPQLID,R2),PQQHDR                                              
         AHI   R2,TPQLID                                                        
         MVC   0(TPQLID,R2),PQQHDR                                              
         AHI   R2,TPQLID                                                        
         ST    R2,AQHEAD           SET A(QUEUE HEADER)                          
         USING TPQHD,R2                                                         
         MVC   TPQHLCK,LOCKWORD    SET LOCKWORD                                 
         DROP  R2                                                               
*                                                                               
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,ALET                                                     
         L     R2,OFFS                                                          
         SAC   512                                                              
         ICM   R2,15,TABSPQNX-FATABSD(R2)                                       
         AHI   R2,TPQLID           GO PAST HEADER                               
*                                                                               
         L     RE,APQNAME                                                       
         XR    RF,RF                                                            
         ICM   RF,1,PQNINDX-PQNAMESD(RE)                                        
         BCTR  RF,0                                                             
         MHI   RF,TPQLENQ          INDEX INTO ENTRY FOR THIS FACPAK             
         AR    R2,RF                                                            
         ST    R2,ASELF            SET A(SELF) WITHIN DATASPACE                 
         USING TPQD,R2                                                          
*                                                                               
         XC    TPQD(TPQLENQ),TPQD                                               
         MVI   TPQFLAG,TPQFINI     SET INITIALISING                             
         MVC   TPQJOB,JOBNAME      REGISTER SELF WITHIN DATASPACE               
         MVC   TPQSTKN,ASTOKEN                                                  
         LA    RF,POSTECB                                                       
         ST    RF,TPQPOST                                                       
         MVC   TPQASID,RXASID+2                                                 
         MVC   TPQAQHD,AQHEAD                                                   
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
*                                                                               
         LHI   R1,PFILSPCX         OUTPUT COMPLETED INITIALISE MSG              
         BRAS  RE,DOMSG                                                         
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* READ AND VALIDATE INPUT CARDS                                       *         
***********************************************************************         
         SPACE 1                                                                
CARDIN   NTR1  ,                                                                
         LA    R1,PCARDS           PROCESSING PARAMETER CARDS                   
         BRAS  RE,DOMSG                                                         
*                                                                               
         XC    ERRCNT,ERRCNT                                                    
CARD02   GOTO1 VCARDS,DMCB,CARD,=C'RE00'                                        
         CLC   =C'/*',CARD         END OF CARDS?                                
         JNE   CARD04              NO                                           
*                                                                               
         LA    R1,PCARDX           PROCESSING PARAMETER CARDS COMPLETE          
         BRAS  RE,DOMSG                                                         
*                                                                               
         OC    ERRCNT,ERRCNT       ERRORS ON PROCESSING?                        
         JZ    EXITOK              NO                                           
         J     EXITL                                                            
*                                                                               
CARD04   MVC   PLINE+12(L'CARD),CARD  PRINT PARAMETER CARD                      
         XR    R1,R1                                                            
         BRAS  RE,DOMSG                                                         
*                                                                               
         L     R1,ACARDTAB                                                      
         BRAS  RE,CARDPRC          PROCESS INPUT CARD                           
         BE    CARD02                                                           
         LH    R1,ERRCNT                                                        
         AHI   R1,1                                                             
         B     CARD02                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD INDICES IN CORE FOR ALL PRINT QUEUES               *         
***********************************************************************         
         SPACE 1                                                                
PQOPEN   NTR1  ,                                                                
         CLI   REBUILD,C'Y'        REBUILDING?                                  
         BE    *+12                YES                                          
         LA    R1,POPENPQ                                                       
         BRAS  RE,DOMSG                                                         
*                                  GET LIST OF PQ FILES                         
         XC    UKEY,UKEY                                                        
         GOTO1 VDMGR,DMCB,GLIST,PRTQU,UKEY,ACXREC,ACIREC                        
         MVC   APRTQLST,UKEY+(UKUSRINF-UKRECD)                                  
*                                                                               
         LA    R7,PQXTNTS                                                       
         USING NDXXTNTD,R7                                                      
         USING CIDATAD,NDXDATA                                                  
*                                                                               
PQOP04   L     RF,APRTQLST         NEXT PQ FILE IN LIST                         
         AHI   RF,8                                                             
         ST    RF,APRTQLST                                                      
         CLI   0(RF),0             TEST END OF LIST                             
         BE    PQOPX                                                            
*                                                                               
         MVI   FSTBLK,C'N'         RESET FIRST BLOCK IN PQ PROCESSED            
         XC    BLOCK,BLOCK         RESET RELATIVE BLOCK NUMBER                  
         MVC   NDXADTF,4(RF)                                                    
*                                                                               
         MVC   NDXINUM,0(RF)       SAVE PQ FILE INTERNAL NUMBER                 
         MVC   NDXENUM,4(RF)       SAVE PQ FILE EXTERNAL NUMBER                 
         MVC   NDXID,PRTQU         SAVE PQ FILE NAME                            
         MVC   NDXID+4(1),1(RF)    SET PRTQ FILE DMCB/ENQ NAME                  
         MVC   NDXLAST,FF32                                                     
*                                                                               
         XC    CXADDR,CXADDR                                                    
         XC    P1(24),P1           P1 DADDS PARAM LIST FOR INDEX READS          
         MVC   P1,VDAOPEN                                                       
         MVC   P2,ACXREC                                                        
         MVC   P4,NDXADTF                                                       
         LA    RE,CXADDR                                                        
         ST    RE,P5                                                            
         GOTO1 VDADDS,P1           OPEN FILE                                    
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P1,VRDID                                                         
         MVC   CXADDR,=X'00010100'                                              
         GOTO1 VDADDS,P1           READ FIRST REC AND INIT EXTENTS              
         OC    P3(2),P3            USE FIRST REC TO SET PQTAB                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   NDXDATA+00(20),CXREC            SET PART 1 INDEX DATA            
         TM    NDXDATA,X'80'                                                    
         BZ    *+14                                                             
         MVC   NDXDATA+20(20),CXREC+L'PQINDEX  SET PART 2 INDEX DATA            
         NI    NDXDATA,X'7F'                                                    
*                                                                               
         MVC   NDXDATA+14(1),NDXINUM           SET PRTQ FILE NUM                
         MVC   NDXDATA+15(5),NDXID             SET PRTQ FILE NAME               
*                                                                               
         LHI   RE,L'PQINDEX                                                     
         STH   RE,CINDXLN                                                       
*                                                                               
         L     RF,NDXADTF          SET F/L RECORD SIZE IN DTF                   
         MVC   DBLKSZ-DTFPHD(L'DBLKSZ,RF),CIBLKLN                               
         OI    DBLKSZ-DTFPHD(RF),X'80'                                          
*                                  CALCULATE SIZE OF INDEX ADDR TABLE           
         LH    R0,CICINDX            # C.I.S                                    
         MH    R0,CITRKS           * # TRKS/C.I.                                
         MH    R0,CIHIREC          * # BLKS/TRK                                 
         MHI   R0,BLKTABLQ         * SIZE OF ENTRY                              
         AHI   R0,32               FOR HEADER AND END IDENTIFIER                
*                                                                               
         BRAS  RE,GETMAIN          OBTAIN STORAGE                               
         LM    R0,R1,DUB                                                        
         AHI   R0,-16                                                           
         ST    R0,NDXBUFFL                                                      
*                                                                               
         LR    R8,R1               R8=A(INDEX BLOCK TABLE)                      
         MVC   0(16,R8),=CL16'XXXXX BLCK TABLE'                                 
         MVC   0(L'NDXID,R8),NDXID                                              
         AHI   R8,16               SET A HEADER                                 
         ST    R8,NDXBUFF                                                       
         USING BLKTABD,R8                                                       
         B     PQOP08                                                           
*                                                                               
PQOP06   XR    RE,RE               READ IN NEXT PAGE                            
         LH    RF,CXPAGE           XADT=CXP/TRKS + 1                            
         LH    R0,CIHIREC          XADB=REM + 1                                 
         DR    RE,R0                                                            
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         STH   RF,CXADDR                                                        
         STC   RE,CXADDR+2                                                      
         MVI   CXADDR+3,0                                                       
*                                                                               
         GOTO1 VDADDS,P1,VRDID                                                  
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         AHI   R8,BLKTABLQ         NEXT TABLE ENTRY                             
*                                                                               
PQOP08   LH    R0,CIBLKLN          SIZE OF BLOCK                                
         AHI   R0,16               PLUS LENGTH OF HEADER                        
*                                                                               
         BRAS  RE,GETMAIN          OBTAIN STORAGE                               
         LM    R0,R1,DUB                                                        
         MVC   0(16,R1),=CL16'XXXXX BLCK HDRXX'                                 
         MVC   0(L'NDXID,R1),NDXID                                              
*                                                                               
         L     RF,BLOCK                                                         
         AHI   RF,1                                                             
         ST    RF,BLOCK                                                         
         STH   RF,BLKNUM           SET RELATIVE BLOCK NUMBER                    
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  14(2,R1),DUB                                                     
         AHI   R1,16               GO PAST HEADER                               
         ST    R1,BLKADD           SET REAL ADDRESS                             
*                                                                               
         MVC   BLKDA,CXADDR        SET BLOCK D/A                                
         LH    RF,CIBLKLN                                                       
         STH   RF,BLKLEN           SET LENGTH OF BLOCK                          
*                                                                               
         L     RE,BLKADD           MOVE BUFFER INTO BLOCK                       
         LH    RF,BLKLEN                                                        
         LA    R0,CXREC                                                         
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         CLI   FSTBLK,C'Y'         FIRST BLOCK FOR THIS PQ                      
         BE    PQOP10              NO                                           
         MVI   FSTBLK,C'Y'         SET PROCESSED                                
*                                                                               
         XC    CXPAGE,CXPAGE                                                    
         LH    R5,CICINDX                                                       
         STH   R5,CXENTRY                                                       
         MH    R5,CINDXLN                                                       
         LA    R5,CXREC(R5)                                                     
         USING PQRECD,R5           R5=A(PRTQUE INDEX RECORD)                    
*                                                                               
PQOP10   CLC   PQRECD(2),=X'FFFF'  END OF THE INDEX?                            
         BE    PQOP14              YES                                          
*                                                                               
         LH    RF,CXENTRY          BUMP TO NEXT INDEX ENTRY                     
         LA    RF,1(RF)                                                         
         STH   RF,CXENTRY                                                       
         CH    RF,CIENTRYS                                                      
         BNL   PQOP12                                                           
         AH    R5,CINDXLN                                                       
         B     PQOP10                                                           
*                                                                               
PQOP12   XC    CXENTRY,CXENTRY                                                  
         LH    RF,CXPAGE                                                        
         LA    RF,1(RF)                                                         
         STH   RF,CXPAGE                                                        
         LA    R5,CXREC                                                         
         B     PQOP06              END OF INDEX PAGE                            
*                                                                               
PQOP14   MVC   HALF,CXPAGE         SAVE LAST INDEX PAGE NUMBER                  
         CLC   CXPAGE(4),CJPAGE                                                 
         BH    PQOP16              EXIT IF END OF PART 2 INDEX                  
         OC    CJCITOT,CJCITOT                                                  
         BZ    PQOP16              EXIT IF NO PART 2 INDEX                      
*                                                                               
         ST    R8,NDXBUF2          SET A(FIRST PART 2 INDEX)                    
         MVC   CXPAGE,CJPAGE                                                    
         LH    R5,CJENTRY                                                       
         STH   R5,CXENTRY                                                       
         MH    R5,CINDXLN                                                       
         LA    R5,CXREC(R5)                                                     
         CLC   CXPAGE,HALF                                                      
         BE    PQOP10              STARTS IN SAME PAGE AS END OF PART 1         
         B     PQOP06                                                           
*                                                                               
PQOP16   AHI   R7,NDXXTNTL                                                      
         AHI   R8,BLKTABLQ                                                      
         MVC   0(16,R8),FF32                                                    
         B     PQOP04              BACK FOR NEXT PRTQ FILE                      
*                                                                               
PQOPX    CLI   REBUILD,C'Y'                                                     
         BE    *+12                                                             
         LA    R1,POPENPQX                                                      
         BRAS  RE,DOMSG                                                         
         J     EXITOK                                                           
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO REBUILD INDICES IN CORE FOR ALL PRINT QUEUES             *         
***********************************************************************         
         SPACE 1                                                                
PQRBLD   NTR1  ,                                                                
         LA    R1,PBRBLD                                                        
         BRAS  RE,DOMSG                                                         
         MVI   REBUILD,C'Y'        SET REBUILDING PQS                           
*                                                                               
         LA    R0,PQXTNTS          CLEAR PQXTNTS TABLE                          
         LHI   R1,PQXTNTSL                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BRAS  RE,FREEMAIN         CLEAR OUT ALL OF GMLIST                      
         BRAS  RE,PQOPEN           REBUILD PQ LIST                              
*                                                                               
         XC    REBUILD,REBUILD                                                  
         LA    R1,PBRBLDX                                                       
         BRAS  RE,DOMSG                                                         
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DO GETMAIN CALLS AND SAVE A() AND L' OF AREA IN GMLIST   *         
* NTRY: R0 = LENGTH OF AREA REQUIRED                                  *         
* EXIT: DUB HOLDS (R0)(R1) AS RETURNED BY GETMAIN                     *         
*       GMLIST UPDATED AS L' FOLLOWED BY A() OF AREA                  *         
***********************************************************************         
         SPACE 1                                                                
GETMAIN  NTR1  ,                                                                
         GETMAIN RU,LV=(0),LOC=(BELOW,ANY),BNDRY=DBLWD                          
         LTR   RF,RF                                                            
         BZ    GMN02                                                            
         WTO   'HARDCORE INTERNAL ERROR - GETMAIN FAILURE'                      
         ABEND 922,DUMP                                                         
*                                                                               
GMN02    STM   R0,R1,DUB                                                        
*                                                                               
         L     RF,AGMLIST                                                       
GMN04    CLI   0(RF),EOT                                                        
         BNE   GMN06                                                            
         WTO   'HARDCORE INTERNAL ERROR - INCREASE GMLIST SIZE'                 
         ABEND 922,DUMP                                                         
*                                                                               
GMN06    OC    0(L'GMLIST,RF),0(RF)                                             
         BZ    GMN08                                                            
         AHI   RF,L'GMLIST                                                      
         B     GMN04                                                            
*                                                                               
GMN08    ST    R0,0(RF)            SAVE AREA AND LENGTH IN CASE RE-INIT         
         ST    R1,4(RF)                                                         
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FREE ALL STORAGE OBTAINED IN GMLIST                                 *         
***********************************************************************         
         SPACE 1                                                                
FREEMAIN NTR1  ,                                                                
         L     R2,AGMLIST                                                       
*                                                                               
FMN02    CLI   0(R2),EOT                                                        
         JE    EXITOK              END OF TABLE                                 
         OC    0(L'GMLIST,R2),0(R2)                                             
         JZ    EXITOK              LAST ENTRY PROCESSED                         
*                                                                               
         LM    R0,R1,0(R2)                                                      
         FREEMAIN RC,A=(1),LV=(0)  FREE STORAGE                                 
         LTR   RF,RF                                                            
         BZ    FMN04                                                            
         WTO   'HARDCORE INTERNAL ERROR - FREEMAIN FAILURE'                     
         ABEND 922,DUMP                                                         
*                                                                               
FMN04    XC    0(L'GMLIST,R2),0(R2)                                             
         AHI   R2,L'GMLIST                                                      
         B     FMN02                                                            
*                                                                               
         DROP  RB,RA                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISE PRINTING FOR TRANSACTION LOGGING AND ERROR LOGGING       *         
***********************************************************************         
         SPACE 1                                                                
PRINTI   NTR1  BASE=*,LABEL=*                                                   
         L     R9,PILITS                                                        
         USING LITERALS,R9                                                      
*                                                                               
         OPEN  (TRNLOG,OUTPUT)                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (ERRLOG,OUTPUT)                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZAP   ELINE,PZERO         RESET LINECOUNTS                             
         ZAP   EPAGE,PZERO                                                      
         ZAP   TLINE,PZERO                                                      
         ZAP   TPAGE,PZERO                                                      
         BRAS  RE,PTTRN                                                         
         BRAS  RE,PTERR                                                         
         J     EXITOK                                                           
*                                                                               
PILITS   DC    A(LITERALS)                                                      
         EJECT                                                                  
***********************************************************************         
* CLOSE PRINTING FOR TRANSACTION AND ERROR LOGGING                    *         
***********************************************************************         
         SPACE 1                                                                
PRINTX   NTR1  BASE=*,LABEL=*                                                   
         L     R9,PXLITS                                                        
         USING LITERALS,R9                                                      
*                                                                               
         CLOSE TRNLOG                                                           
         CLOSE ERRLOG                                                           
         J     EXITOK                                                           
*                                                                               
PXLITS   DC    A(LITERALS)                                                      
         EJECT                                                                  
***********************************************************************         
* PRINT TITLE FOR TRANSACTION LOGGING                                 *         
***********************************************************************         
         SPACE 1                                                                
PTTRN    NTR1  BASE=*,LABEL=*                                                   
         L     R9,PTTLITS                                                       
         USING LITERALS,R9                                                      
*                                                                               
         L     R2,ATT                                                           
         AP    TPAGE,PONE          BUMP PAGECOUNT                               
         ZAP   DUB,TPAGE                                                        
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  35(4,R2),DUB                                                     
*                                                                               
         ZAP   TLINE,PFOUR                                                      
         PUT   TRNLOG,0(R2)        PRINT TITLE                                  
         L     R2,ATTU                                                          
         PUT   TRNLOG,0(R2)        PRINT TITLE UNDERLINE                        
         PUT   TRNLOG,SPACES       PRINT CLEAR SPACE UNDERNEATH                 
         B     EXITOK                                                           
*                                                                               
PTTLITS  DC    A(LITERALS)                                                      
         EJECT                                                                  
***********************************************************************         
* PRINT TITLE FOR ERROR LOG                                           *         
***********************************************************************         
         SPACE 1                                                                
PTERR    NTR1  BASE=*,LABEL=*                                                   
         L     R9,PETLITS                                                       
         USING LITERALS,R9                                                      
*                                                                               
         AP    EPAGE,PONE          BUMP PAGECOUNT                               
*                                                                               
         L     R2,AET                                                           
         ZAP   DUB,EPAGE                                                        
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  35(4,R2),DUB                                                     
*                                                                               
         PUT   ERRLOG,0(R2)        PRINT TITLE                                  
         L     R2,AETU                                                          
         PUT   ERRLOG,0(R2)        PRINT TITLE UNDERLINE                        
         PUT   ERRLOG,SPACES       PRINT CLEAR SPACE UNDERNEATH                 
         ZAP   ELINE,PFOUR                                                      
         B     EXITOK                                                           
*                                                                               
PETLITS  DC    A(LITERALS)                                                      
         EJECT                                                                  
***********************************************************************         
* PRINT TRANSACTION DETAIL LINE                                       *         
***********************************************************************         
         SPACE 1                                                                
PLTRN    NTR1  BASE=*,LABEL=*                                                   
         L     R9,PTLLITS                                                       
         USING LITERALS,R9                                                      
*                                                                               
         AP    TLINE,PONE          BUMP LINECOUNT                               
         CP    TLINE,SMAXL         TEST FOR MAX LINES                           
         BL    *+8                                                              
         BRAS  RE,PTTRN            PRINT NEW TITLES                             
*                                                                               
         PUT   TRNLOG,PLINE        PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         B     EXITOK                                                           
*                                                                               
PTLLITS  DC    A(LITERALS)                                                      
         EJECT                                                                  
***********************************************************************         
* PRINT ERROR LOG LINE                                                *         
***********************************************************************         
         SPACE 1                                                                
PLERR    NTR1  BASE=*,LABEL=*                                                   
         L     R9,PLLLITS                                                       
         USING LITERALS,R9                                                      
*                                                                               
         AP    ELINE,PONE          BUMP LINECOUNT                               
         CP    ELINE,SMAXL         TEST FOR MAX LINES                           
         BL    *+8                                                              
         BRAS  RE,PTERR            PRINT NEW TITLES                             
*                                                                               
         PUT   ERRLOG,PLINE        PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         B     EXITOK                                                           
*                                                                               
PLLLITS  DC    A(LITERALS)                                                      
         EJECT                                                                  
***********************************************************************         
* OUTPUT INFORMATION MESSAGE                                          *         
* NTRY: R1  NZ      INDEX TO MESSAGE                                  *         
*       R1  ZERO    MESSAGE IS ALREADY ON PRINT LINE                  *         
***********************************************************************         
         SPACE 1                                                                
DOMSG    NTR1  BASE=*,LABEL=*                                                   
         L     R9,DOLITS                                                        
         USING LITERALS,R9                                                      
         LTR   R1,R1                                                            
         BZ    DOMSG02                                                          
*                                                                               
         XR    R0,R0                                                            
         BRAS  RE,TIMER                                                         
         MVC   PLINE(L'TIME),TIME                                               
*                                                                               
         BCTR  R1,0                                                             
         MHI   R1,L'MESSTAB                                                     
         LA    R1,MESSTAB(R1)                                                   
         MVC   PLINE+L'TIME+1(L'MESSTAB),0(R1)                                  
*                                                                               
DOMSG02  BRAS  RE,PLTRN                                                         
         B     EXIT                                                             
*                                                                               
DOLITS   DC    A(LITERALS)                                                      
*                                                                               
MESSTAB  DS    0CL60                                                            
  DC CL60'Began Processing Input Parameters from Cards                '         
  DC CL60'Ended Processing Input Parameters from Cards                '         
  DC CL60'Attempting Dataspace bind                                   '         
  DC CL60'Completed  Dataspace bind                                   '         
  DC CL60'Began Initialising Dataspace                                '         
  DC CL60'Ended Initialising Dataspace                                '         
  DC CL60'Began Initialising Operator Communications                  '         
  DC CL60'Ended Initialising Operator Communications                  '         
  DC CL60'Began Building PQ Indices in core                           '         
  DC CL60'Ended Building PQ Indices in core                           '         
  DC CL60'*** System Initialisation Complete ***                      '         
  DC CL60'Nothing to do - Application waiting for wake-up post        '         
  DC CL60'Operator console post - Application Terminating             '         
  DC CL60'Facpak wake-up post - Application Restarting                '         
  DC CL60'Counter error - attempting to continue                      '         
  DC CL60'Operator cancel command issued                              '         
  DC CL60'Forced cancel by operator                                   '         
  DC CL60'Began processing operator command                           '         
  DC CL60'Application terminating normally                            '         
  DC CL60'Unknown operator command verb                               '         
  DC CL60'Operator command too long to process (80 byte maximum)      '         
  DC CL60'Operator requested PQ re-initialisation                     '         
  DC CL60'Began rebuilding PQ Indices in core                         '         
  DC CL60'Ended rebuilding PQ Indices in core                         '         
  DC CL60'Invalid PQ Type Card - ADV/TST/REP/DARE only valid input    '         
         EJECT                                                                  
***********************************************************************         
* LOG PQ WORK TO LOGFILE                                              *         
***********************************************************************         
         SPACE 1                                                                
LOGGER   NTR1  BASE=*,LABEL=*                                                   
         L     R9,LOLITS                                                        
         USING LITERALS,R9                                                      
*                                                                               
         MVC   LINE1,SPACES        CLEAR OUTPUT BLOCK                           
         MVC   LINE2,SPACES                                                     
         MVC   LINE3,SPACES                                                     
         MVC   LINE4,SPACES                                                     
         MVC   LINE5,SPACES                                                     
         MVC   LINE6,SPACES                                                     
*                                                                               
         MVC   LINE1,LN1           SET INFO FOR ALL TYPES                       
L1       USING LN1,LINE1                                                        
         MVC   LINE2,LN2                                                        
L2       USING LN2,LINE2                                                        
         MVC   LINE3,LN3                                                        
L3       USING LN3,LINE3                                                        
*                                                                               
         XR    R0,R0                                                            
         BRAS  RE,TIMER                                                         
         MVC   L1.LN1TIME,TIME     SET TRANSACTION LOG TIME                     
*                                                                               
         MVC   PLINE,SEPERATE                                                   
         BRAS  RE,PLTRN                                                         
*                                                                               
         LA    R2,LCLNTRY          USE LOCAL COPY OF ENTRY                      
         USING TPNTRYD,R2                                                       
         CLI   TPNFAC,C'X'         ONLINE?                                      
         BE    LGR02               NO                                           
*                                                                               
         MVC   L1.LN1ONOFF,ONLINE                                               
         MVC   L1.LN1WHO(LOGONLL),LOGONL                                        
         BRAS  RE,SETFAC                                                        
         MVC   L1.LN1WHO+LOGFAC-LOGONL(L'LOGFAC),FULL                           
         MVC   L1.LN1WHO+LOGTSK-LOGONL(L'LOGTSK),TPNTSK                         
         LA    R0,L1.LN1WHO+(LOGASID1-LOGONL)                                   
         GOTO1 VHEXOUT,DMCB,TPNASID,(R0),L'TPNASID,0                            
         B     LGR04                                                            
*                                                                               
LGR02    MVC   L1.LN1ONOFF,OFFLINE                                              
         MVC   L1.LN1WHO(LOGOFFLL),LOGOFFL                                      
         LA    R0,L1.LN1WHO+(LOGASID2-LOGOFFL)                                  
         GOTO1 VHEXOUT,DMCB,TPNASID,(R0),L'TPNASID,0                            
*                                                                               
LGR04    L     R0,TIMEARR          ARRIVAL TIME                                 
         BRAS  RE,TIMER                                                         
         MVC   L2.LN2ARR,TIME                                                   
         L     R0,TIMEIN           START TIME                                   
         BRAS  RE,TIMER                                                         
         MVC   L2.LN2STD,TIME                                                   
         L     R0,TIMEFIN          END TIME                                     
         BRAS  RE,TIMER                                                         
         MVC   L2.LN2FIN,TIME                                                   
*                                                                               
         BRAS  RE,SETPQ            SET PQ=PRTQ#                                 
         MVC   L3.LN3PQ,DUB                                                     
         GOTO1 VHEXOUT,DMCB,TPNRTN,L3.LN3RC,L'TPNRTN,0                          
         LA    R0,TPNDA                                                         
         GOTO1 (RF),(R1),(R0),L3.LN3DA,L'TPNDA                                  
*                                                                               
*                              *** INDIVIDUAL ACTIONS                           
*                                                                               
         MVC   L1.LN1ACT,LACRD                                                  
         CLI   TPNACT,TPNANDX      READ INDEX                                   
         BE    LGR06                                                            
         MVC   L1.LN1ACT,LACWRT                                                 
         CLI   TPNACT,TPNAPUT      WRITE RECORD                                 
         BE    LGR08                                                            
         MVC   L1.LN1ACT,LACGET                                                 
         CLI   TPNACT,TPNANEW      GET INDEX                                    
         BE    LGR10                                                            
*                                                                               
         MVC   L1.LN1ACT,LACUNK     UNKNOWN ACTION                              
         MVC   PLINE,LINE1                                                      
         BRAS  RE,PLERR                                                         
         MVC   PLINE,LINE2                                                      
         BRAS  RE,PLERR                                                         
         MVC   PLINE,LINE3                                                      
         BRAS  RE,PLERR                                                         
         GOTO1 VHEXOUT,DMCB,LCLNTRY,PLINE,L'LCLNTRY,0                           
         BRAS  RE,PLERR                                                         
         B     EXITOK                                                           
*                                                                               
LGR06    MVC   LINE4,LNRD          READ INDEX RECORD                            
L4       USING LNRD,LINE4                                                       
*                                                                               
         GOTO1 VHEXOUT,DMCB,TPNBADD,L4.LNRADR,L'TPNBADD,0                       
         GOTO1 (RF),(R1),TPNBLEN,L4.LNRLEN,L'TPNBLEN                            
         B     LGR12                                                            
*                                                                               
LGR08    MVC   LINE4,LNWT          WRITE INDEX RECORD                           
L4       USING LNWT,LINE4                                                       
*                                                                               
         MVC   L4.LNWAKEY,TPNNDX                                                
         GOTO1 VHEXOUT,DMCB,TPNRNUM,L4.LNWDSP,L'TPNRNUM,0                       
         GOTO1 (RF),(R1),TPNNDX,L4.LNWXKEY,L'TPNNDX,0                           
         B     LGR12                                                            
*                                                                               
LGR10    MVC   LINE4,LNG1          GET NEW INDEX RECORD                         
L4       USING LNG1,LINE4                                                       
         MVC   LINE5,LNG2                                                       
L5       USING LNG2,LINE5                                                       
         MVC   LINE6,LNG2                                                       
L6       USING LNG3,LINE6                                                       
*                                                                               
         GOTO1 VHEXOUT,DMCB,TPNNTYPE,L4.LG1TYPE,L'TPNNTYPE,0                    
         GOTO1 (RF),(R1),TPNNADD,L4.LG1ADD,L'TPNNADD                            
         GOTO1 (RF),(R1),TPNNDSP,L4.LG1DSP,L'TPNNDSP                            
         GOTO1 (RF),(R1),TPNNSEQ,L4.LG1SEQ,L'TPNNSEQ                            
*                                                                               
         MVC   L5.LG2AKEY,LOGRKEY                                               
         GOTO1 (RF),(R1),LOGRKEY,L5.LG2XKEY,L'LOGRKEY                           
         MVC   L6.LG3AKEY,TPNNDX                                                
         GOTO1 (RF),(R1),TPNNDX,L6.LG3XKEY,L'TPNNDX                             
*                                                                               
LGR12    MVC   PLINE,LINE1                                                      
         BRAS  RE,PLTRN                                                         
         MVC   PLINE,LINE2                                                      
         BRAS  RE,PLTRN                                                         
         MVC   PLINE,LINE3                                                      
         BRAS  RE,PLTRN                                                         
         MVC   PLINE,LINE4                                                      
         BRAS  RE,PLTRN                                                         
         CLC   LINE5,SPACES                                                     
         BNH   LGR14                                                            
         MVC   PLINE,LINE5                                                      
         BRAS  RE,PLTRN                                                         
         CLC   LINE6,SPACES                                                     
         BNH   LGR14                                                            
         MVC   PLINE,LINE6                                                      
         BRAS  RE,PLTRN                                                         
*                                                                               
LGR14    CLI   TPNRTN,0            TRANSACTION IN ERROR?                        
         BE    EXITOK              NO                                           
         MVC   PLINE,LINE1                                                      
         BRAS  RE,PLERR                                                         
         MVC   PLINE,LINE2                                                      
         BRAS  RE,PLERR                                                         
         MVC   PLINE,LINE3                                                      
         BRAS  RE,PLERR                                                         
         MVC   PLINE,LINE4                                                      
         BRAS  RE,PLERR                                                         
         CLC   LINE5,SPACES                                                     
         BNH   LGR16                                                            
         MVC   PLINE,LINE5                                                      
         BRAS  RE,PLERR                                                         
         CLC   LINE6,SPACES                                                     
         BNH   LGR16                                                            
         MVC   PLINE,LINE6                                                      
         BRAS  RE,PLERR                                                         
*                                                                               
LGR16    B     EXITOK                                                           
*                                                                               
         DROP  L1,L2,L3,L4,L5,L6                                                
*                                                                               
LOLITS   DC    A(LITERALS)                                                      
*                                                                               
ONLINE   DC    CL7'Online'                                                      
OFFLINE  DC    CL7'Offline'                                                     
LACRD    DC    CL11'Read Index'                                                 
LACGET   DC    CL11'Get Index'                                                  
LACWRT   DC    CL11'Write Index'                                                
LACUNK   DC    CL11'**UNKNOWN**'                                                
*                                                                               
LINE1    DC    CL166' '            OUTPUT LINES                                 
LINE2    DC    CL166' '                                                         
LINE3    DC    CL166' '                                                         
LINE4    DC    CL166' '                                                         
LINE5    DC    CL166' '                                                         
LINE6    DC    CL166' '                                                         
*                                                                               
LN1      DC    CL166' '            HEADER INFORMATION - LINE 1                  
         ORG   LN1                                                              
LN1TIME  DC    CL11' '                                                          
         DC    CL01' '                                                          
LN1ONOFF DC    CL07' '                                                          
         DC    CL01' '                                                          
LN1ACT   DC    CL11' '                                                          
         DC    CL01' '                                                          
LN1WHO   DC    CL(LOGONLL)' '                                                   
         ORG                                                                    
*                                                                               
LN2      DC    CL166' '            HEADER INFORMATION - LINE 2                  
         ORG   LN2                                                              
LN2TIME  DC    CL11' '                                                          
         DC    CL01' '                                                          
         DC    C'Arrived='                                                      
LN2ARR   DC    CL11' '                                                          
         DC    CL01' '                                                          
         DC    C'Started='                                                      
LN2STD   DC    CL11' '                                                          
         DC    CL01' '                                                          
         DC    C'Completed='                                                    
LN2FIN   DC    CL11' '                                                          
         ORG                                                                    
*                                                                               
LN3      DC    CL166' '            HEADER INFORMATION - LINE 3                  
         ORG   LN3                                                              
LN3TIME  DC    CL11' '                                                          
         DC    CL01' '                                                          
         DC    CL03'PQ='                                                        
LN3PQ    DC    CL05' '                                                          
         DC    CL01' '                                                          
         DC    CL03'RC='                                                        
LN3RC    DC    CL02' '                                                          
         DC    CL01' '                                                          
         DC    CL04'D/A='                                                       
LN3DA    DC    CL08' '                                                          
         DC    CL01' '                                                          
         ORG                                                                    
*                                                                               
LNRD     DC    CL166' '            COVERS READ INDEX ACTION                     
         ORG   LNRD                                                             
LNRTIME  DC    CL11' '                                                          
         DC    CL01' '                                                          
         DC    C'Address='                                                      
LNRADR   DC    CL08' '                                                          
         DC    C' '                                                             
         DC    C'Block Length='                                                 
LNRLEN   DC    CL04' '                                                          
         ORG                                                                    
*                                                                               
LNWT     DC    CL166' '            COVERS WRITE INDEX ACTION                    
         ORG   LNWT                                                             
LNWTIME  DC    CL11' '                                                          
         DC    CL01' '                                                          
         DC    C'Disp='                                                         
LNWDSP   DC    CL04' '                                                          
         DC    C' '                                                             
         DC    C'Key='                                                          
LNWAKEY  DC    CL24' '                                                          
         DC    C' '                                                             
         DC    C'XKey='                                                         
LNWXKEY  DC    CL48' '                                                          
         ORG                                                                    
*                                                                               
LNG1     DC    CL166' '            COVERS GET INDEX RECORD ACTION               
         ORG   LNG1                                                             
LG1TIME  DC    CL11' '                                                          
         DC    CL01' '                                                          
         DC    CL05'Type='                                                      
LG1TYPE  DC    CL02' '                                                          
         DC    CL01' '                                                          
         DC    CL08'Address='                                                   
LG1ADD   DC    CL08' '                                                          
         DC    CL01' '                                                          
         DC    CL05'Disp='                                                      
LG1DSP   DC    CL04' '                                                          
         DC    CL01' '                                                          
         DC    CL06'Seq #='                                                     
LG1SEQ   DC    CL04' '                                                          
         ORG                                                                    
LNG2     DC    CL166' '            COVERS GET INDEX RECORD ACTION               
         ORG   LNG2                                                             
LG2TIME  DC    CL11' '                                                          
         DC    CL01' '                                                          
         DC    C'Input  AKey='                                                  
LG2AKEY  DC    CL24' '                                                          
         DC    C' '                                                             
         DC    C'Input  XKey='                                                  
LG2XKEY  DC    CL48' '                                                          
         ORG                                                                    
*                                                                               
LNG3     DC    CL166' '            COVERS GET INDEX RECORD ACTION               
         ORG   LNG3                                                             
LG3TIME  DC    CL11' '                                                          
         DC    CL01' '                                                          
         DC    C'Output AKey='                                                  
LG3AKEY  DC    CL24' '                                                          
         DC    C' '                                                             
         DC    C'Output XKey='                                                  
LG3XKEY  DC    CL48' '                                                          
         ORG                                                                    
*                                                                               
LOGONL   DC    CL07'Facpak='   *** ONLINE INFORMATION - GOES ON LINE 1          
LOGFAC   DC    CL04'    '                                                       
         DC    CL01' '                                                          
         DC    CL05'Task='                                                      
LOGTSK   DC    CL01' '                                                          
         DC    CL01' '                                                          
         DC    CL05'ASID='                                                      
LOGASID1 DC    CL04'    '                                                       
LOGONLL  EQU   *-LOGONL                                                         
*                                                                               
LOGOFFL  DC    CL05'ASID='     *** OFFLINE INFORMATION - GOES ON LINE 1         
LOGASID2 DC    CL04'    '                                                       
LOGOFFLL EQU   *-LOGOFFL                                                        
         EJECT                                                                  
***********************************************************************         
* SET FACPAK 4 CHARACTER NAME IN OUTPUT LOG FILE PRINT LINE           *         
***********************************************************************         
         SPACE 1                                                                
SETFAC   LA    RF,FACIDTAB                                                      
         USING FACITABD,RF                                                      
SFAC02   CLC   FACISN4,FF32        END OF TABLE?                                
         JNE   *+12                NO                                           
         MVC   LOGFAC,UNKNOWN                                                   
         BR    RE                                                               
         CLC   FACIID,TPNFAC       MATCH FACPAK 1 CHAR NAME                     
         JNE   *+12                NO                                           
         MVC   FULL,FACISN4                                                     
         BR    RE                                                               
         LA    RF,L'FACITAB(RF)    NEXT ENTRY                                   
         J     SFAC02                                                           
         DROP  RF                                                               
*                                                                               
*FACIDTAB                                                                       
         PRINT OFF                                                              
       ++INCLUDE FACIDTAB                                                       
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* SET PRINT QUEUE NAME INTO DUB FOR OUTPUT                            *         
***********************************************************************         
         SPACE 1                                                                
SETPQ    MVC   DUB,SPACES                                                       
         LA    RF,PQXTNTS                                                       
         USING NDXXTNTD,RF                                                      
SPQ02    CLI   0(RF),C' '          END OF TABLE?                                
         BNH   SPQ04               NO                                           
         CLC   NDXENUM,TPNPQ       MATCH PQ IDENTIFIER?                         
         BNE   *+12                NO                                           
         MVC   DUB(5),NDXID                                                     
         BR    RE                                                               
         LA    RF,NDXXTNTL(RF)     NEXT PQ ENTRY                                
         B     SPQ02                                                            
         DROP  RF                                                               
*                                                                               
SPQ04    XR    R0,R0                                                            
         IC    R0,TPNPQ                                                         
         SRDL  R0,4                                                             
         SRL   R1,32-4                                                          
         LA    R1,HEXTAB(R1)                                                    
         MVC   DUB+1(1),0(R1)                                                   
         SRDL  R0,4                                                             
         SRL   R1,32-4                                                          
         LA    R1,HEXTAB(R1)                                                    
         MVC   DUB(1),0(R1)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* OUTPUT TIME FROM MVS TIME MACRO                                     *         
* NTRY R0 = ZERO USE CURRENT TIME (FROM MVS TIME MACRO)               *         
*      R0 = NZ   USE TIME IN HHMMSSXX IN R0                           *         
* EXIT TIME HOLDS HH:MM:SS:XX WHERE XX IS 1/100 SECS                  *         
***********************************************************************         
         SPACE 1                                                                
TIMER    NTR1  BASE=*,LABEL=*                                                   
         L     R9,TILITS                                                        
         USING LITERALS,R9                                                      
         LTR   R0,R0                                                            
         BNZ   TIME02                                                           
*                                                                               
         TIME  DEC                 R0=TIME                                      
*                                                                               
TIME02   STC   R0,TIME+10                                                       
         OI    TIME+10,X'F0'                                                    
         SRL   R0,4                                                             
         STC   R0,TIME+9           XX PORTION                                   
         OI    TIME+9,X'F0'                                                     
*                                                                               
         SRL   R0,4                                                             
         STC   R0,TIME+7                                                        
         OI    TIME+7,X'F0'                                                     
         SRL   R0,4                                                             
         STC   R0,TIME+6           SS PORTION                                   
         OI    TIME+6,X'F0'                                                     
*                                                                               
         SRL   R0,4                                                             
         STC   R0,TIME+4                                                        
         OI    TIME+4,X'F0'                                                     
         SRL   R0,4                                                             
         STC   R0,TIME+3           MM PORTION                                   
         OI    TIME+3,X'F0'                                                     
*                                                                               
         SRL   R0,4                                                             
         STC   R0,TIME+1                                                        
         OI    TIME+1,X'F0'                                                     
         SRL   R0,4                                                             
         STC   R0,TIME+0           HH PORTION                                   
         OI    TIME+0,X'F0'                                                     
*                                                                               
         MVI   TIME+2,C':'                                                      
         MVI   TIME+5,C':'                                                      
         MVI   TIME+8,C':'                                                      
         B     EXIT                                                             
*                                                                               
TILITS   DC    A(LITERALS)                                                      
         EJECT                                                                  
***********************************************************************         
* POST FACPAK COMPLETION                                              *         
* NTRY: R2 = A(TPNTRY IN DATASPACE)                                   *         
*       AR MODE OFF ALL ARS MUST BE CLEARED                           *         
***********************************************************************         
         SPACE 1                                                                
POSTFAC  NTR1  BASE=*,LABEL=*                                                   
         L     R9,POLITS                                                        
         USING LITERALS,R9                                                      
         ST    R2,DOINGME                                                       
         USING TPNTRYD,R2                                                       
         MVC   LASTHDR(TPNTRYLQ),0(R2)                                          
         MVC   LCLNTRY,0(R2)       FOR LOGGER                                   
         MVI   TPNFLAG,TPNFDNE     SET PROCESSED REQUEST                        
         MVC   CMPECB,TPNECB       SAVE A(COMPLETION ECB)                       
         MVC   CMPASID,TPNASID     SAVE A(COMPLETION ASID)                      
*                                                                               
         L     R2,AQHEAD                                                        
         USING TPQHD,R2                                                         
PFAC02   ICM   RE,15,TPQHRDY       INCREMENT COMPLETED WORK COUNT               
         LA    RF,1(RE)                                                         
         CS    RE,RF,TPQHRDY                                                    
         BNE   PFAC02                                                           
         DROP  R2                                                               
*                                                                               
         CLI   WTLOG,YES                                                        
         BNE   PFAC04                                                           
         TIME  DEC                                                              
         ST    R0,TIMEFIN          SET END TIME                                 
         BRAS  RE,LOGGER           LOG OUTPUT TRACE TO SYSMSG                   
*                                                                               
PFAC04   XR    R4,R4                                                            
         ICM   R4,3,CMPASID        GET ASCB FOR THE FACPAK                      
         LOCASCB ASID=(R4)                                                      
         LTR   RF,RF               ASCB RETURNED IN R1 OK?                      
         BNZ   PFAC06              NO - FACPAK HAS ABENDED                      
*                                                                               
         ICM   R3,15,CMPECB        GET A(POST ECB) IN R3                        
         LR    R4,R1               SET ASCB IN R4                               
         POST (R3),88,ASCB=(R4),LINKAGE=SYSTEM,ECBKEY=8,MF=(E,POSTLF)           
         LTR   RF,RF                                                            
         BZ    EXITOK              GOOD RETURN FROM POST                        
*                                                                               
PFAC06   XR    R0,R0               THERE IS A PROBLEM - LOG THE ERROR           
         BRAS  RE,TIMER                                                         
         MVC   PLINE(L'TIME),TIME  SET ERROR TIME + BAD ASID                    
         MVC   HALF,LASTHDR+(TPNASID-TPNTRYD)                                   
         GOTO1 VHEXOUT,DMCB,HALF,DWNASID,L'HALF,0                               
         MVC   PLINE+L'TIME+1(DWNMSGL),DWNMSG                                   
         BRAS  RE,PLERR            LOG THE BAD MESSAGE                          
*                                                                               
         L     R2,DOINGME          CLEAR ENTRY FROM QUEUE                       
         XC    0(TPNTRYLQ,R2),0(R2)                                             
*                                                                               
         L     R2,AQHEAD                                                        
         USING TPQHD,R2                                                         
PFAC08   ICM   RE,15,TPQHRDY       REDUCE COMPLETED WORK COUNT                  
         BZ    EXITOK                                                           
         LR    RF,RE                                                            
         BCTR  RF,0                                                             
         CS    RE,RF,TPQHRDY                                                    
         BNE   PFAC08                                                           
         B     EXITOK                                                           
         DROP  R2                                                               
*                                                                               
POSTLF   POST  ECBKEY=YES,MF=L                                                  
*                                                                               
POLITS   DC    A(LITERALS)                                                      
         EJECT                                                                  
***********************************************************************         
* PROCESS CARD VALUE FROM TABLE                                       *         
* NTRY: CARD = CARD VALUE TO PROCESS                                  *         
*       R1   = A(CORRECT TABLE)                                       *         
***********************************************************************         
         SPACE 1                                                                
CARDPRC  NTR1  BASE=*,LABEL=*                                                   
         L     R9,CALITS                                                        
         USING LITERALS,R9                                                      
*                                                                               
         LR    R3,R1                                                            
         USING CARDTABD,R3                                                      
*                                                                               
         LA    R2,CARD                                                          
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         JE    EXITOK                                                           
         GOTO1 VSCANNER,DMCB,(C'C',(R2)),(1,SCNBLK)                             
         CLI   4(R1),0                                                          
         BE    CEINVLIN            INVALID LINE                                 
*                                                                               
         LA    R2,SCNBLK                                                        
         USING SCANBLKD,R2                                                      
         XR    RF,RF                                                            
*                                                                               
CPRC02   CLI   CNAME,EOT           END OF TABLE                                 
         BE    CEINVKEY            INVALID KEYWORD                              
         ICM   RF,1,CXLEN                                                       
         EX    RF,CARDCMP                                                       
         BE    CPRC04                                                           
*                                                                               
         LA    R3,CARDTABL(R3)                                                  
         B     CPRC02                                                           
*                                                                               
CARDCMP  CLC   SC1STFLD(0),CNAME                                                
*                                                                               
CPRC04   CLI   CTYPE,CTNUM         NUMERIC INPUT?                               
         BNE   CPRC06              NO                                           
         TM    SC2NDVAL,SCNUMQ                                                  
         BNO   CENOTNUM                                                         
         CLC   SC2NDNUM,CMIN       SCOPE FOR MAX/MIN VALUES                     
         BL    CETOOLOW                                                         
         CLC   SC2NDNUM,CMAX                                                    
         BH    CETOOBIG                                                         
         ICM   RF,15,COUT                                                       
         MVC   0(4,RF),SC2NDNUM    SET NUMERIC VALUE INTO OUTPUT                
         J     EXITOK                                                           
*                                                                               
CPRC06   CLI   CTYPE,CTCHR         CHARACTER INPUT                              
         BNE   CPRC08              NO                                           
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BZ    CENOINP                                                          
         C     RF,CMIN             SCOPE FOR LENGTH                             
         BL    CETOOSHT                                                         
         C     RF,CMAX                                                          
         BH    CETOOLNG                                                         
         ICM   RE,15,COUT          MOVE IN FIELD                                
         ICM   RF,1,CLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SC2NDFLD                                                 
         J     EXITOK                                                           
*                                                                               
CPRC08   CLI   CTYPE,CTNOP         NO SECOND INPUT                              
         BNE   CPRC10              NO                                           
         J     EXITOK                                                           
*                                                                               
CPRC10   CLI   CTYPE,CTNOPF        NO SECOND INPUT / TURN ON FLAG               
         BNE   CPRC12              NO                                           
         ICM   RF,15,COUT                                                       
         OC    0(L'CFLAG,RF),CFLAG                                              
         J     EXITOK                                                           
*                                                                               
CPRC12   DC    H'0'                EXTRA TYPES HERE                             
*                                                                               
CEINVLIN LA    R1,CINVLIN          INVALID LINE                                 
         B     CERR                                                             
CEINVKEY LA    R1,CINVKEY          INVALID KEYWORD                              
         B     CERR                                                             
CENOTNUM LA    R1,CNOTNUM          NOT A NUMBER                                 
         B     CERR                                                             
CENOTCHR LA    R1,CNOTCHR          NOT CHARACTER                                
         B     CERR                                                             
CETOOSHT LA    R1,CTOOSHT          TOO SHORT                                    
         B     CERR                                                             
CETOOLNG LA    R1,CTOOLNG          TOO LONG                                     
         B     CERR                                                             
CETOOLOW LA    R1,CTOOLOW          TOO SMALL                                    
         B     CERR                                                             
CETOOBIG LA    R1,CTOOBIG          TOO BIG                                      
         B     CERR                                                             
CENOINP  LA    R1,CNOINP           NO INPUT                                     
         B     CERR                                                             
*                                                                               
CERR     XR    R0,R0                                                            
         BRAS  RE,TIMER                                                         
         MVC   PLINE(L'TIME),TIME                                               
         MVC   PLINE+L'TIME+1(L'CERRHDR),CERRHDR                                
         MVC   PLINE+L'TIME+CERRHDRL+1(CERRML),0(R1)                            
         XR    R1,R1                                                            
         BRAS  RE,DOMSG                                                         
         J     EXITL                                                            
         DROP  R2,R3                                                            
*                                                                               
CALITS   DC    A(LITERALS)                                                      
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                      *         
***********************************************************************         
         SPACE 1                                                                
CERRML   EQU   40                                                               
CERRHDRL EQU   20                                                               
*                                                                               
CERRHDR  DC    CL20' *** CARD ERROR *** '                                       
CINVLIN  DC    CL40'Invalid Line Format'                                        
CINVKEY  DC    CL40'Invalid Keyword'                                            
CNOTNUM  DC    CL40'Value not a valid number'                                   
CNOTCHR  DC    CL40'Value not a valid character string'                         
CTOOSHT  DC    CL40'Length of input string too short'                           
CTOOLNG  DC    CL40'Length of input string too long'                            
CTOOLOW  DC    CL40'Numeric value too small'                                    
CTOOBIG  DC    CL40'Numeric value too large'                                    
CNOINP   DC    CL40'Invalid/missing value'                                      
         EJECT                                                                  
***********************************************************************         
* INPUT CARDS TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
CARDTAB  DS    0D                                                               
         DC    CL8'DSPACE  ',F'002',F'012'                                      
         DC    AL1(004,CTCHR,L'DSPACE,0),V(DSPACE)                              
         DC    CL8'DDSIO   ',F'005',F'008'                                      
         DC    AL1(004,CTCHR,L'DDSTAG,0),V(DDSTAG)                              
         DC    CL8'TRNSLOG ',F'001',F'003'                                      
         DC    AL1(006,CTCHR,L'WTLOG,0),V(WTLOG)                                
         DC    CL8'PQTYPE  ',F'002',F'006'                                      
         DC    AL1(005,CTCHR,L'PQNAME,0),V(PQNAME)                              
         DC    CL8'QLEN    ',F'100',F'10000'                                    
         DC    AL1(003,CTNUM,L'QLEN,0),V(QLEN)                                  
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* OPERATOR COMMANDS TABLE                                             *         
***********************************************************************         
         SPACE 1                                                                
CMDTAB   DS    0D                                                               
         DC    CL8'EOJ     ',F'000',F'000'                                      
         DC    X'02',AL1(CTNOPF),AL1(0),AL1(JOBEOJ),V(JOBSTEP)                  
         DC    CL8'INIT    ',F'000',F'000'                                      
         DC    X'03',AL1(CTNOPF),AL1(0),AL1(JOBINIT),V(JOBSTEP)                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* CARD TABLE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
CARDTABD DSECT                                                                  
CNAME    DS    CL8                 INPUT CARD                                   
CMIN     DS    F                   MINIMUM (VALUE OR LENGTH)                    
CMAX     DS    F                   MAXIMUM (VALUE OR LENGTH)                    
CXLEN    DS    XL1                 LEN-1 OF CNAME VALUE FOR COMPARE             
CTYPE    DS    AL1                 INPUT TYPE                                   
CTNUM    EQU   1                   NUMERIC                                      
CTCHR    EQU   2                   CHARACTER                                    
CTNOP    EQU   3                   NO SECOND INPUT FIELD                        
CTNOPF   EQU   4                   NO SECOND INPUT / TURN ON FLAG               
CLEN     DS    X                   OUTPUT AREA LENGTH (CHAR ONLY)               
CFLAG    DS    XL1                 FLAG VALUE (CTNOPF)                          
COUT     DS    AL4                 A(OUTPUT AREA)                               
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
HARDCORE CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* INCLUDED PRINT QUEUES                                               *         
***********************************************************************         
         SPACE 1                                                                
*FAHARDPQ                                                                       
       ++INCLUDE FAHARDPQ                                                       
*                                                                               
HARDCORE CSECT                                                                  
         SPACE 1                                                                
***********************************************************************         
* LITERALS AND EQUATES                                                *         
***********************************************************************         
         SPACE 1                                                                
LITERALS DS    0D                                                               
*                                                                               
PQNUM    EQU   32                  NUMBER OF PRINT QUEUE SLOTS                  
EOT      EQU   X'FF'               TABLE TERMINATOR                             
YES      EQU   C'Y'                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXITS AND HANDY ROUTINES                                            *         
***********************************************************************         
         SPACE 1                                                                
EXITL    CLI   *,255               SET CC LOW                                   
         J     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         J     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XBASE    LA    R1,PAPPEND                                                       
         BRAS  RE,DOMSG                                                         
         XBASE ,                                                                
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
FF32     DC    32X'FF'                                                          
ONE      DC    A(00000001)                                                      
LOCKWORD DC    CL4'LOCK'                                                        
GLIST    DC    CL8'GLIST   '                                                    
PRTQU    DC    CL8'PRTQU   '                                                    
ARZERO   DC    16F'0'                                                           
HEXTAB   DC    C'0123456789ABCDEF'                                              
UNKNOWN  DC    CL10'??????????'                                                 
*                                                                               
ATT      DC    A(TTITLE)           TRNLOG TITLE                                 
ATTU     DC    A(TTITLEU)                                                       
AET      DC    A(ETITLE)           ERRLOG TITLE                                 
AETU     DC    A(ETITLEU)                                                       
*                                                                               
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
PFOUR    DC    P'4'                                                             
SMAXL    DC    P'60'                                                            
TLINE    DC    PL3'0'                                                           
TPAGE    DC    PL3'0'                                                           
ELINE    DC    PL3'0'                                                           
EPAGE    DC    PL3'0'                                                           
*                                                                               
CXHDRT   DC    CL16'**CXREC**CXREC**'                                           
CIHDRT   DC    CL16'**CIREC**CIREC**'                                           
PQQHDR   DC    CL16'*PQ SERVER QUEUE'                                           
*                                                                               
DWNMSG   DC    C'XM POST failure - ASID='                                       
DWNASID  DC    CL4'?????'                                                       
         DC    C' - Attempting to continue'                                     
DWNMSGL  EQU   *-DWNMSG                                                         
*                                                                               
SPACES   DC    166C' '                                                          
SEPERATE DC    166C'*'                                                          
         EJECT                                                                  
***********************************************************************         
* INPUT CARD STORAGE AREAS                                            *         
***********************************************************************         
         SPACE 1                                                                
DSPACE   DC    CL12' '             NAME OF DATASPACE                            
DDSTAG   DC    CL8'DDSIO   '       DDSIO VERSION TO USE                         
WTLOG    DC    CL3'N'              LOG ALL TRANSACTIONS                         
PQNAME   DC    CL6'TST   '         PRINT QUEUE TYPE                             
QLEN     DC    F'100'              NUMBER OF ENTRIES IN QUEUE                   
         SPACE 2                                                                
***********************************************************************         
* ADCONS                                                              *         
***********************************************************************         
         SPACE 2                                                                
APQNAMES DC    A(PQNAMES)                                                       
AGMLIST  DC    A(GMLIST)                                                        
AGMLISTX DC    A(GMLISTX)                                                       
AECBLIST DC    A(ECBLIST)                                                       
ACARDTAB DC    A(CARDTAB)                                                       
ACMDTAB  DC    A(CMDTAB)                                                        
VCARDS   DC    V(CARDS)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VSCANNER DC    V(SCANNER)                                                       
VDDSIO   DC    V(DDSIO)                                                         
VDMGR    DC    V(DATAMGR)                                                       
VDADDS   DC    V(DADDS)                                                         
VRDID    DC    A(00000001)         ROUTINE NUMBER HAS TO BE SET OFFLINE         
VWTID    DC    A(00000004)         ROUTINE NUMBER HAS TO BE SET OFFLINE         
VDAOPEN  DC    A(00000014)         ROUTINE NUMBER HAS TO BE SET OFFLINE         
         EJECT                                                                  
***********************************************************************         
* EXTRACTED VALUES                                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0L                                                               
         DC    CL8'TRNLOG*'                                                     
TRNLOG   DCB   DSORG=PS,MACRF=PM,DDNAME=TRNLOG,RECFM=FBA,LRECL=(166)            
*                                                                               
         DC    CL8'ERRLOG*'                                                     
ERRLOG   DCB   DSORG=PS,MACRF=PM,DDNAME=ERRLOG,RECFM=FBA,LRECL=(166)            
*                                                                               
         DS    0L                                                               
         DC    CL8'PLINE***'                                                    
PLINE    DC    CL166' '                                                         
*                                                                               
         DS    0L                                                               
         DC    CL8'DOINGME='                                                    
DOINGME  DC    A(0)                                                             
         DC    A(0)                                                             
*                                                                               
         DC    CL8'JOBSTEP='                                                    
JOBSTEP  DC    X'00'               OPERATOR COMMAND FLAGS                       
JOBEOJ   EQU   X'80'               TERMINATE                                    
JOBINIT  EQU   X'40'               RE-INITIALISE PQ                             
JOBSTOP  EQU   X'20'               FORCE CANCEL                                 
*                                                                               
REBUILD  DC    X'00'               SET TO C'Y' IF REBUILDING INDICES            
         DC    XL6'00'                                                          
*                                                                               
         DC    CL8'EXTRACTR'                                                    
RESULTS  DS    0F                  RESULTS FROM EXTRACT MACRO                   
RXTIOT   DC    F'0'                                                             
RXASID   DC    F'0'                                                             
         DC    CL8'STOKEN  '       STOKEN                                       
ASTOKEN  DC    XL8'00'                                                          
         DC    CL8'JOBNAME '                                                    
JOBNAME  DC    CL8' '              JOBNAME                                      
         DC    CL8'POSTECB '                                                    
POSTECB  DC    A(0)                WAKEUP ECB                                   
         DC    F'0'                                                             
         DC    CL8'AOPERECB'                                                    
AOPERECB DC    A(0)                OPERATOR ECB                                 
         DC    F'0'                                                             
         DC    CL8'ACOMM   '                                                    
ACOMM    DC    A(0)                COMMS BLOCK                                  
         DC    F'0'                                                             
         DC    CL8'ASELF   '                                                    
ASELF    DC    A(0)                A(OWN ENTRY IN TABS)                         
         DC    F'0'                                                             
         DC    CL8'AQHEAD  '                                                    
AQHEAD   DC    A(0)                A(HEAD OF QUEUE)                             
         DC    F'0'                                                             
*                                                                               
*                                                                               
         DC    CL8'LASTHDR '                                                    
LASTHDR  DC    XL(TPNTRYLQ)'00'    LAST PROCESSED HEADER                        
*                                                                               
         DS    0L                                                               
         DC    CL8'PQXTNTS '                                                    
PQXTNTS  DC    (PQNUM)XL(NDXXTNTL)'00'                                          
PQXTNTSL EQU   *-PQXTNTS                                                        
         EJECT                                                                  
***********************************************************************         
* TITLES                                                              *         
***********************************************************************         
         SPACE 1                                                                
TTITLE   DC    CL166'PQ INDEX TRANSACTION LOG      PAGE  '                      
TTITLEU  DC    CL166'-- ----- ----------- ---'                                  
ETITLE   DC    CL166'PQ INDEX ERROR MESSAGES       PAGE  '                      
ETITLEU  DC    CL166'-- ----- ----- --------'                                   
*                                                                               
         EJECT                                                                  
***********************************************************************         
* TRACE MESSAGES AND EQUATES                                          *         
***********************************************************************         
         SPACE 1                                                                
PCARDS   EQU   1                                                                
PCARDX   EQU   2                                                                
PGETSPC  EQU   3                                                                
PGETSPCX EQU   4                                                                
PFILSPC  EQU   5                                                                
PFILSPCX EQU   6                                                                
PSETOP   EQU   7                                                                
PSETOPX  EQU   8                                                                
POPENPQ  EQU   9                                                                
POPENPQX EQU   10                                                               
PINITX   EQU   11                                                               
PWAIT    EQU   12                                                               
POPPOST  EQU   13                                                               
PWKPOST  EQU   14                                                               
PCTRERR  EQU   15                                                               
POPSCAN  EQU   16                                                               
POPSSTOP EQU   17                                                               
POPSCOM  EQU   18                                                               
PAPPEND  EQU   19                                                               
PBADCMDU EQU   20                                                               
PBADCMDL EQU   21                                                               
POPSINI  EQU   22                                                               
PBRBLD   EQU   23                                                               
PBRBLDX  EQU   24                                                               
PBADNAME EQU   25                                                               
         EJECT                                                                  
***********************************************************************         
* GETMAIN ADDRESS LIST FORMAT XL4(ADDRESS),AL4(LENGTH)                *         
***********************************************************************         
         SPACE 1                                                                
         DS    0L                                                               
         DC    CL16'**GETMAIN LIST**'                                           
GMLIST   DC    1000XL8'00'                                                      
GMLISTX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* ECB LIST BUILD AREA                                                 *         
***********************************************************************         
         SPACE 1                                                                
         DS    0L                                                               
         DC    CL16'****ECB LIST****'                                           
ECBLIST  DC    20F'0'                                                           
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 2                                                                
WORKD    DSECT                                                                  
AVTAB    DS    (AVSTAL)X                                                        
*                                                                               
DUB      DS    D                                                                
LOGDUB   DS    D                                                                
LOGFULL  DS    F                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
BLOCK    DS    F                                                                
*                                                                               
APQNAME  DS    A                   A(PQ TYPE NAME)                              
THISHDR  DS    F                   A(THIS HEADER)                               
CMPECB   DS    F                   A(COMPLETION ECB)                            
CMPASID  DS    H                   A(COMPLETION ASID)                           
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
AINDEX   DS    A                   A(CURRENT INDEX TABLE)                       
LINDEX   DS    F                   L'CURRENT INDEX TABLE                        
*                                                                               
TIMEARR  DS    F                   TIME PLACED IN QUEUE                         
TIMEIN   DS    F                   TIME PROCESSING BEGAN                        
TIMEFIN  DS    F                   TIME PROCESSING ENDED                        
*                                                                               
OFFS     DS    A                   DATASPACE OFFSET                             
ALET     DS    A                   ALET                                         
STOKN    DS    CL8                 STOKEN                                       
ERRCNT   DS    H                                                                
*                                                                               
DATEE    DS    CL8                 EBCDIC DATE C'YYMMDD  '                      
DATEB    DS    XL3                 BINARY DATE X'YMD'                           
DATEC    DS    XL2                 CMPRSD DATE B'YYYYYYYMMMMDDDDD'              
TIMEI    DS    XL1                 BINARY TIME X'I' I=10MIN INTERVAL            
TIMEB    DS    XL2                 BINARY TIME X'HM'                            
TIMEC    DS    XL2                 BINARY TIME CREATED UNITS=SECS*3/4           
         DS    XL2                 N/D                                          
*                                                                               
LCLNTRY  DS    XL(TPNTRYLQ)        LOCAL COPY OF DSPACE NTRY FOR LOGGER         
LOGRKEY  DS    XL24                                                             
*                                                                               
TIME     DS    CL11                TIME HH:MM:SS:XX                             
WORK     DS    CL64                                                             
CARD     DS    CL80                                                             
*                                                                               
FSTBLK   DS    X                                                                
SCNBLK   DS    3CL(SCBLKLQ)                                                     
*                                                                               
APRTQLST DS    A                   A(NEXT ENTRY IN LIST OF PRTQ FILES)          
ACIREC   DS    A                                                                
ACXREC   DS    A                                                                
PRVADDR  DS    F                                                                
*                                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
*                                                                               
UKEY     DS    XL40                USER INDEX/KEY FOR PRTQUE                    
*                                                                               
PLINES   DS    CL166               SAVED PRINT LINE (FOR ERROR LOG)             
*                                                                               
         DS    0L                                                               
CXHDR    DS    CL16                                                             
CXREC    DS    18432C                                                           
*                                                                               
         DS    0L                                                               
CIHDR    DS    CL16                                                             
CIREC    DS    18432C                                                           
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* EXTENT BLOCK HEADER DSECT                                           *         
***********************************************************************         
         SPACE 1                                                                
BLKTABD  DSECT                                                                  
BLKNUM   DS    H                   RELATIVE BLOCK NUMBER: 1-N                   
BLKLEN   DS    H                   LENGTH OF BLOCK                              
BLKDA    DS    A                   D/A OF BLOCK                                 
BLKADD   DS    A                   A(BLOCK HEADER)                              
         DS    XL4                 N/D                                          
BLKTABLQ EQU   *-BLKTABD                                                        
         EJECT                                                                  
***********************************************************************         
* EXTENT BLOCK INFO DSECT                                             *         
***********************************************************************         
         SPACE 1                                                                
NDXXTNTD DSECT                                                                  
NDXID    DS    CL5                 INDEX ID                                     
         DS    CL1                 N/D                                          
NDXINUM  DS    CL1                 INDEX INTERNAL NUMBER                        
NDXENUM  DS    CL1                 INDEX EXTERNAL NUMBER                        
NDXADTF  DS    AL4                 A(DTF)                                       
NDXBUFF  DS    AL4                 A(P1 INDEX BUFFER)                           
NDXBUFFL DS    AL4                 L'P1 INDEX BUFFER                            
NDXBUF2  DS    AL4                 A(FIRST P2 BUFFER)                           
NDXLAST  DS    H                   LAST REPORT ADDED                            
NDXLAST2 DS    H                                                                
NDXDATA  DS    CL78                CI DATA (COVERED BY DMPRTQW)                 
NDXRPT   DS    0XL32               COUNTERS FOR REPORTING                       
NDXRP1N  DS    F                   PART 1 SEARCH - NEW EXTENT                   
NDXRP1O  DS    F                   PART 1 SEARCH - OLD EXTENT TO PURGE          
NDXRP2A  DS    F                   PART 2 SEARCH FOR AVAILABLE CI               
         DS    F                                                                
         DS    F                                                                
         DS    F                                                                
         DS    F                                                                
         DS    F                                                                
         DS    F                                                                
NDXXTNTL EQU   *-NDXXTNTD                                                       
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 2                                                                
* IEZCIB                                                                        
         PRINT OFF                                                              
         DSECT                                                                  
         IEZCIB                                                                 
         PRINT ON                                                               
* IEZCOM                                                                        
         PRINT OFF                                                              
         IEZCOM                                                                 
         PRINT ON                                                               
* ISTDNIB                                                                       
         PRINT OFF                                                              
         ISTDNIB                                                                
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* DMDSHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDSHDR                                                        
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* FACIDTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
* FATABSPQ                                                                      
         PRINT OFF                                                              
       ++INCLUDE FATABSPQ                                                       
         PRINT ON                                                               
* FATABSD                                                                       
         PRINT OFF                                                              
       ++INCLUDE FATABSD                                                        
         PRINT ON                                                               
CIDATAD  DSECT                                                                  
         PRINT  OFF                                                             
       ++INCLUDE DMPRTQW                                                        
         PRINT  ON                                                              
* DMPRTQD                                                                       
         PRINT  OFF                                                             
       ++INCLUDE DMPRTQD                                                        
         PRINT  ON                                                              
* DMDTFPH                                                                       
         PRINT  OFF                                                             
       ++INCLUDE DMDTFPH                                                        
         PRINT  ON                                                              
* DMPRTQK                                                                       
         PRINT  OFF                                                             
       ++INCLUDE DMPRTQK                                                        
         PRINT  ON                                                              
* FAPRQ                                                                         
         PRINT  OFF                                                             
       ++INCLUDE FAPRQ                                                          
         PRINT  ON                                                              
* FAPLO                                                                         
         PRINT  OFF                                                             
       ++INCLUDE FAPLO                                                          
         PRINT  ON                                                              
*                                                                               
         TITLE 'VARIABLE SCAN MODULE FOR CARD VALIDATION'                       
         EJECT                                                                  
***********************************************************************         
* VARIABLE SCAN MODULE FOR CARD VALIDATION                            *         
***********************************************************************         
         SPACE 1                                                                
SCANNER  CSECT                                                                  
         NMOD1 SWORKL,**SCAN**                                                  
         USING SWORKD,RC                                                        
         LR    R9,R1               R9=A(PARAMETER LIST)                         
         LM    R2,R3,0(R9)         R2=A(DATA STRING) R3=A(BLOCK)                
         MVC   MAXLINES,4(R9)                                                   
         XC    SDISP,SDISP                                                      
         XR    R4,R4                                                            
         IC    R4,5(R2)            L'DATA IF SCREEN FIELD                       
         LA    R2,8(R2)                                                         
         MVC   LROW,=H'42'         PRESET DEFAULT LENGTHS                       
         MVC   LRIGHT,=H'20'                                                    
         MVC   LBOTH,=H'30'                                                     
         CLI   0(R9),C'C'                                                       
         BE    SCAN02                                                           
*                                                                               
SCAN02   SH    R2,=H'8'                                                         
         LA    R4,80                                                            
         CLC   0(80,R2),SSPACES                                                 
         BE    SCERR2                                                           
         LA    R5,79(R2)                                                        
*                                                                               
SCAN04   CLI   0(R5),C' '                                                       
         BNE   SCAN06                                                           
         BCTR  R5,0                                                             
         BCT   R4,SCAN04                                                        
*                                                                               
SCAN06   LA    R5,0(R2,R4)         L'DATA IN R4                                 
         MVC   BORROW,0(R5)        SAVE THE NEXT CHARACTER                      
         MVC   0(1,R5),COMMA       AND POP IN A COMMA TO SIMPLIFY               
         XR    R6,R6               R6=NUMBER OF LINES USED                      
         EJECT                                                                  
***********************************************************************         
* HANDLE LINES OF DATA                                                *         
***********************************************************************         
         SPACE 1                                                                
SCAN08   XC    0(12,R3),0(R3)      PRESET A LINE                                
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
         BH    SCERR                                                            
         CLC   1(1,R3),LRIGHT+1                                                 
         BH    SCERR                                                            
         CLI   1(R3),0                                                          
         BE    SCAN10                                                           
         CLI   0(R3),10                                                         
         BH    SCERR                                                            
*                                                                               
SCAN10   XR    R7,R7                                                            
         ICM   R7,1,0(R3)                                                       
         BZ    SCAN16                                                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R3),0(R2)                                                   
         TM    2(R3),X'80'                                                      
         BZ    SCAN12                                                           
         CHI   R7,8                                                             
         BH    SCAN12                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,5(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING DISPS.               
         BO    SCAN12                                                           
         ST    R8,4(R3)                                                         
*                                                                               
SCAN12   LA    R2,2(R2,R7)                                                      
         ICM   R7,1,1(R3)                                                       
         BZ    SCAN18                                                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   22(0,R3),0(R2)                                                   
         TM    3(R3),X'80'                                                      
         BZ    SCAN14                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN14                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,9(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING SDISPS.              
         BO    SCAN14                                                           
         ST    R8,8(R3)                                                         
*                                                                               
SCAN14   LA    R2,2(R2,R7)                                                      
         B     SCAN18                                                           
*                                                                               
VARPAK   PACK  SDUB,0(0,R2)                                                     
*                                                                               
SCAN16   LA    R2,1(R2)                                                         
         CLI   1(R3),0                                                          
         BNE   SCERR                                                            
*                                                                               
SCAN18   LA    R6,1(R6)            BUMP N'LINES                                 
         AH    R3,LROW             BUMP TO NEXT LINE IN BLOCK                   
         CR    R2,R5               ARE WE NOW PAST LAST 'COMMA'                 
         BH    SCANOK                                                           
         ICM   R7,1,MAXLINES                                                    
         BZ    SCAN08                                                           
         CR    R6,R7               HAVE WE REACHED MAX N'LINES                  
         BNE   SCAN08                                                           
*                                                                               
SCANOK   MVC   0(1,R5),BORROW      RETURN THE BYTE                              
         STC   R6,4(R9)            SET NUMBER OF LINES USED                     
         B     SCANX                                                            
*                                                                               
SCERR    MVI   4(R9),0                                                          
         MVC   0(1,R5),BORROW                                                   
         MVC   2(2,R3),=X'FFFF'                                                 
         B     SCANX                                                            
*                                                                               
SCERR2   MVI   4(R9),0                                                          
         MVC   2(2,R3),=X'FFFF'                                                 
*                                                                               
SCANX    XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE AND GET LENGTHS OF FIELDS                                  *         
***********************************************************************         
         SPACE 1                                                                
GETL     NTR1  ,                                                                
         LR    R4,R3                                                            
         SR    R5,R5                                                            
         TM    4(R9),X'80'                                                      
         BZ    GETL02                                                           
         MVC   4(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
*                                                                               
GETL02   CLC   0(1,R2),COMMA       TEST FIELD SEPERATOR                         
         BE    GETL14                                                           
         CLC   0(1,R2),EQUAL                                                    
         BE    GETL16                                                           
*                                                                               
GETL04   LA    R5,1(R5)                                                         
         CLI   0(R2),C'9'                                                       
         BNH   *+8                                                              
         MVI   2(R4),0             (ALL INVALID)                                
         CLI   0(R2),C'0'                                                       
         BL    GETL06                                                           
         NI    2(R4),X'BF'         (INVALID ALPHA)                              
         B     GETL12                                                           
*                                                                               
GETL06   NI    2(R4),X'7F'         (INVALID NUM)                                
         CLI   0(R2),C'Z'                                                       
         BNH   GETL08                                                           
         MVI   2(R4),0             Z-0 = ALL INVALID                            
         B     GETL12                                                           
*                                                                               
GETL08   CLI   0(R2),C'A'          LESS THAN A = ALL INVALID                    
         BNL   GETL10                                                           
         MVI   2(R4),0                                                          
         B     GETL12                                                           
*                                                                               
GETL10   CLI   0(R2),C'F'          OK FOR ALPHA                                 
         BNH   GETL12                                                           
         NI    2(R4),X'DF'         G-Z = INVALID HEX                            
*                                                                               
GETL12   LA    R2,1(R2)                                                         
         B     GETL02                                                           
*                                                                               
GETL14   STC   R5,0(R4)            COMMA FOUND                                  
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         B     GETLX                                                            
*                                                                               
GETL16   CR    R4,R3               EQUAL FOUND - IS THIS THE FIRST ONE?         
         BNE   GETL04              TREAT AS NORMAL CHARACTER IF NOT             
         STC   R5,0(R4)            NOW STORE L1                                 
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         TM    4(R9),X'80'                                                      
         BZ    GETL18                                                           
         MVC   8(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
*                                                                               
GETL18   LA    R4,1(R4)            POINT TO FIELD2 DATA                         
         SR    R5,R5               CLEAR L2                                     
         LA    R2,1(R2)            POINT PAST EQUAL SIGN                        
         B     GETL02                                                           
*                                                                               
GETLX    XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* SCANNER LITERALS AND CONSTANTS                                      *         
***********************************************************************         
         SPACE 1                                                                
COMMA    DC    C','                                                             
EQUAL    DC    C'='                                                             
SSPACES  DC    CL80' '                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SCANNER WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
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
*                                                                               
SWORKL   EQU   *-SWORKD                                                         
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DC                                                  *         
***********************************************************************         
         SPACE 1                                                                
WORKAREA CSECT                                                                  
         DC    200000X'00'                                                      
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002FAHARDCORE12/10/09'                                      
         END                                                                    
