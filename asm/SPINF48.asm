*          DATA SET SPINF48    AT LEVEL 003 AS OF 05/01/02                      
*PHASE T21A48A                                                                  
         TITLE 'MARKET RECORD INFO  T21A48'                                     
*                                                                               
***********************************************************************         
*                                                                               
*   TITLE     : INFO-LISTING OF MARKET/STATION (TYPE-'N') RECORDS               
*                                                                               
*   COMMENTS  : LISTS THE TYPE-'N' (PSEUDO PASSIVE-POINTERS)                    
*               GIVEN THE MEDIA+MARKET IN KEY.                                  
*                                                                               
*   REG USED  : R0 -                                                            
*               R1 -                                                            
*               R2 -                                                            
*               R3 -                                                            
*               R4 -                                                            
*               R5 - POINTS TO LINES ON SCREEN (USED BY MKSTATD).               
*               R6 - POINTS TO REC.                                             
*               R7 - POINTS TO LOCATIONS TO PUT STATIONS.                       
*               R8 -                                                            
*               R9 -                                                            
*               RA - TWA POINTER.                                               
*               RB - BASE REGISTER.                                             
*               RC - USED BY GENOLD DSECT.                                      
*               RD -                                                            
*               RE -                                                            
*               RF -                                                            
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== ADMINISTRATIVE WORK ========================*         
T21A48   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21A48                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21AFFD,RA                                                       
         LA    R6,REC                                                           
         ST    R6,AREC                                                          
         USING SFXKEYD,R6          FORMAT OF TYPE-'N' RECORDS.                  
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=========================== SET SCREEN UP ===========================*         
         LA    R5,SINHDRH                                                       
         MVC   8(L'MKSTALBL,R5),MKSTALBL   HEADING.                             
         OI    6(R5),X'80'                                                      
         LA    R5,DISLINLQ(R5)                                                  
         MVI   8(R5),C'-'                  UNDERSCORING.                        
         MVC   9(3,R5),8(R5)                                                    
         MVI   13(R5),C'-'                 NAME                                 
         MVC   14(3,R5),13(R5)                                                  
         MVI   42(R5),C'-'                 STATION(CLIENT)                      
         MVC   43(15,R5),42(R5)                                                 
         OI    6(R5),X'80'                                                      
         LA    R5,T21AFFD                                                       
         LA    R5,LIN1DISQ(R5)     R5-->1ST DISPLAY LINE.                       
         USING MKSTATD,R5          FORMAT OF DISPLAY LINE.                      
         LA    R7,MKSSTA           R7-->1ST COLUMN OF STAT. CALL LTRS.          
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=========================== START LISTING ===========================*         
         CLI   SINIFLTH+5,0        ANY INPUT IN FILTER FIELD?                   
         BE    LST10               NO                                           
         MVC   SINIFLT,SPACES      YES, CLEAR IT OUT.                           
         OI    SINIFLTH+6,X'80'    TRANSMIT IT.                                 
*                                                                               
LST10    XC    KEY,KEY                                                          
         OC    PREVKEY,PREVKEY     FIRST TIME THROUGH?                          
         BNZ   LST20               NOPE                                         
*                                                                               
*---------------------- FIRST TIME, FIRST LINE ----------------------*          
*                                                                               
         MVC   TPAGYMED,SVKEY      REMEMBER WHICH RECORDS NEEDED.               
         MVC   KEY(SFXKEYLQ),SVKEY KEY=KEY OF 1ST RECORD REQUESTED.             
         BAS   RE,HIMSTA           READ RECORD.                                 
         CLC   TPAGYMED,REC        IS THIS A RECORD WE WANT?                    
         BNE   NOMORE              NO.                                          
         NI    FLAG,X'FF'-NDCONTQ  DON'T NEED CONTINUATION LABEL.               
         GOTO1 VMSUNPK,DMCB,SFXKMKCL,SVMARKET,CALLTTR                           
         BAS   RE,PTMKTNME                                                      
         B     LST50                                                            
*                                                                               
*-------------------- SUBSEQUENT TIME, FIRST LINE --------------------*         
*                                                                               
LST20    MVC   KEY(SFXKEYLQ),PREVKEY   PICK UP FROM LAST RECORD READ.           
         XC    PREVKEY,PREVKEY                                                  
         BAS   RE,HIMSTA           READ RECORD.                                 
         CLC   TPAGYMED,REC        IS THIS A RECORD WE WANT?                    
         BNE   NOMORE              NO.                                          
         GOTO1 VMSUNPK,DMCB,SFXKMKCL,MARKET,CALLTTR                             
         CLC   SVMARKET,MARKET     MARKET SAME AS LAST?                         
         BNE   LST25               NOPE.                                        
         OI    FLAG,NDCONTQ        NEED CONTINUATION LABEL.                     
         B     LST27                                                            
LST25    NI    FLAG,X'FF'-NDCONTQ  DON'T NEED CONTINUATION LABEL.               
         MVC   SVMARKET,MARKET                                                  
LST27    BAS   RE,PTMKTNME                                                      
         B     LST50                                                            
*                                                                               
*--------------------- SUBSEQUENT DISPLAY LINES ---------------------*          
*                                                                               
LST30    CLC   TPAGYMED,REC        TYPE OF RECORD CORRECT?                      
         BNE   NOMORE              NOPE                                         
*                                                                               
         GOTO1 VMSUNPK,DMCB,SFXKMKCL,MARKET,CALLTTR                             
         CLC   SVMARKET,MARKET     DIFFERENT MARKET?                            
         BE    LST40               NO                                           
*                                                                               
         OI    6(R5),X'80'         YES, TRANSMIT CURRENT LINE.                  
         BAS   RE,MKNEWLIN          AND START NEW LINE.                         
         TM    FLAG,NEWSCRNQ       NEED NEW SCREEN?                             
         BO    PAGEPROC            YES.                                         
         MVC   SVMARKET,MARKET     REMEMBER MARKET NUMBER.                      
         LA    R7,MKSSTA           NO, R7-->1ST COL OF STATION LTTRS.           
         BAS   RE,PTMKTNME         ALSO PRINT MARKET NAME ON NEW LINE.          
         B     LST50                                                            
*                                                                               
LST40    TM    FLAG,NEWLINEQ       SAME MARKET, BUT NEED A NEW LINE?            
         BZ    LST50               NO.                                          
*                                                                               
         BAS   RE,MKNEWLIN         YES, GOTO NEXT LINE.                         
         TM    FLAG,NEWSCRNQ       NEED NEW SCREEN?                             
         BO    PAGEPROC            YES.                                         
         LA    R7,MKSSTA                                                        
*                                                                               
*----------------------- STATION CALL LETTERS ------------------------*         
*                                                                               
LST50    LR    R1,R7               DON'T NEED NEW SCREEN OR LINE.               
         MVC   0(4,R1),CALLTTR     ONTO SCREEN: CALL LETTERS.                   
         CLI   CALLTTR+3,C' '      WAS 4TH CALL LETTER A BLANK?                 
         BNE   *+6                 NO.                                          
         BCTR  R1,0                YES, NO BLANKS WANTED.                       
         LA    R1,4(R1)            R1-->NEXT POSITION TO PRINT.                 
         CLI   CALLTTR+4,C'A'                                                   
         BNE   LST55                                                            
         MVI   0(R1),C'-'                                                       
         MVI   1(R1),C'A'                                                       
         LA    R1,2(R1)            R1-->NEXT POSITION TO PRINT.                 
LST55    CLC   SFXKCLT,=C'000'     IS THERE A CLIENT CODE?                      
         BE    LST60               NOPE                                         
         MVI   0(R1),C'/'                                                       
         MVC   1(3,R1),SFXKCLT     ONTO SCREEN: CLIENT.                         
*                                                                               
LST60    LA    R7,L'MKSSTA(R7)     R7-->NEXT POSITION TO PUT STATION.           
         LR    R1,R7                                                            
         SR    R1,R5               R1=AMT OF SPACE USED ON DSPLY LINE.          
         LA    R0,MKSTATDQ         R0=OVERALL LENGTH OF DSPLY LINE.             
         CR    R1,R0               USED UP WHOLE LINE YET?                      
         BL    *+12                NO                                           
         OI    FLAG,NEWLINEQ       YES, SIGNAL FOR NEW LINE.                    
         OI    6(R5),X'80'         TRANSMIT CURRENT LINE.                       
*                                                                               
         BAS   RE,SEQMSTA                                                       
         MVC   KEY(SFXKEYLQ),REC   KEY=KEY OF NEW RECORD.                       
         B     LST30                                                            
*                                                                               
*--------------------------- SCREEN IS FULL --------------------------*         
*                                                                               
PAGEPROC OI    6(R5),X'80'         TRANSMIT LAST LINE.                          
         NI    FLAG,X'FF'-NEWSCRNQ TURN OFF NEW-SCREEN FLAG.                    
         MVC   PREVKEY(SFXKEYLQ),KEY                                            
         LA    R2,SINENDH                                                       
         OI    6(R2),X'C0'                                                      
         B     EXIT                                                             
*                                                                               
*-------------------------- NO MORE RECORDS --------------------------*         
*                                                                               
NOMORE   OI    6(R5),X'80'         TRANSMIT LAST LINE.                          
         XC    PREVKEY,PREVKEY                                                  
         XC    SVMARKET,SVMARKET                                                
         LA    R2,SINIKEYH                                                      
         OI    6(R2),X'C0'                                                      
*                                                                               
         DROP  R5                                                               
EXIT     XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=============================== HIMSTA ==============================*         
HIMSTA   NTR1                                                                   
*               DOES A READ HIGH ON MKT/STATION RECORDS.                        
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'STATI',KEY,AREC                      
*                                                                               
XHIMSTA  B     EXIT                                                             
***********************************************************************         
         SPACE 4                                                                
***********************************************************************         
*============================== SEQMSTA ==============================*         
SEQMSTA  NTR1                                                                   
*               DOES A READ SEQUENTIAL ON MKT/STATION RECORDS.                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRSEQ',=C'STATI',KEY,AREC                      
*                                                                               
XSEQMSTA B     EXIT                                                             
***********************************************************************         
         SPACE 4                                                                
***********************************************************************         
*=========================== MAKE NEW LINE ===========================*         
MKNEWLIN NTR1                                                                   
*               TRANSMITS CURRENT LINE.                                         
*               POINTS R5 TO NEXT LINE.                                         
*               POINTS R7 TO STARTING POINT TO PRINT STATIONS.                  
*               TURNS ON NEW-SCREEN FLAG IF NEEDED.                             
*                                                                               
         OI    6(R5),X'80'                                                      
         LA    R5,DISLINLQ(R5)     R5-->NEW LINE.                               
         NI    FLAG,X'FF'-NEWLINEQ TURN OFF NEW-LINE FLAG.                      
         LA    R1,SINLAST          R1-->END OF SCREEN.                          
         CR    R5,R1               IS R5 @ END OF SCREEN?                       
         BL    XMKNEWLN            NOPE                                         
         OI    FLAG,NEWSCRNQ       YES, NEED NEW SCREEN.                        
*                                                                               
XMKNEWLN XIT1  REGS=(R5)           R5-->NEW LINE.                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== PRINT MARKET NAME ==========================*         
PTMKTNME NTR1                                                                   
*               EXTRACTS MARKET NAME FROM MARKET RECORDS AND PRINTS             
*                NAME, WHEN NECESSARY.                                          
*                                                                               
         USING MKSTATD,R5                                                       
         MVC   MKSNAME,SPACES      CLEAR OUT FIELD FIRST.                       
         XC    MKEY,MKEY           BUILD KEY TO READ MKT RECORDS.               
         LA    R1,MKEY                                                          
         USING MKTRECD,R1                                                       
         MVI   MKTKTYPE,C'M'       TYPE=M                                       
         MVC   MKTKMED,SFXKMED     MEDIA GOTTEN FROM REC'S KEY.                 
         MVC   MKTKMKT,SVMARKET    MARKET CODE.                                 
         MVC   MKTKAGY,SFXKAGCY    AGENCY.                                      
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'STATI',MKEY,REC2                     
         LA    R1,REC2                                                          
         MVC   MKSMKT,SVMARKET     ONTO SCREEN: MARKET CODE.                    
         XC    MKSNAME,MKSNAME                                                  
         MVC   MKSNAME(L'MKTNAME),MKTNAME   ONTO SCREEN: MARKET NAME.           
*                                                                               
         TM    FLAG,NDCONTQ        NEED A CONTINUATION LABEL?                   
         BZ    *+14                                                             
         MVC   MKSNAME+20(L'CONTDLBL),CONTDLBL                                  
         NI    FLAG,X'FF'-NDCONTQ                                               
*                                                                               
         DROP  R1,R5                                                            
         NI    FLAG,X'FF'-(NDMKNMQ+NEWLINEQ)                                    
*                                  DON'T NEED NEW LINE & MKT NAME.              
         BAS   RE,HIMSTA           RESTORE READING OF SUBJECT RECORDS.          
*                                                                               
XPTMKTNM B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=========================== MISCELLANEOUS ===========================*         
MKSTALBL DC    C'MRKT NAME                         STATION (CLIENT)'            
CONTDLBL DC    C' (CONT)'                                                       
*                                                                               
** STARTING COLUMN #S.                                                          
LFCOLQ   EQU   2                                                                
RTCOLQ   EQU   36                                                               
*                                                                               
DISLINLQ EQU   L'SINHDRH+L'SINHDR  LENGTH OF DISPLAY LINE.                      
LIN1DISQ EQU   SINHDRH-T21AFFD+2*DISLINLQ                                       
*                                  DISPLACEMENT OF FIRST DISPLAY LINE.          
*                                                                               
CALLTTR  DS    CL5                 CALL LETTERS.                                
*                                                                               
FLAG     DS    CL1                                                              
NEWSCRNQ EQU   X'80'               NEED NEW SCREEN.                             
NEWLINEQ EQU   X'40'               NEED NEW LINE.                               
NDCONTQ  EQU   X'20'               YES, NEED A CONTINUING SIGNAL.               
NDMKNMQ  EQU   X'10'               YES, NEED TO PRINT MARKET NAME.              
*                                                                               
MARKET   DS    CL4                 HOLDS MARKET NUMBER.                         
MKEY     DS    CL(SFXKEYLQ)        MARKET STATION KEY.                          
***********************************************************************         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE SPINFWORK                                                      
*                                                                               
PREVMKT  DS    CL4                                                              
SVMARKET DS    CL4                 SAVE MARKET CODE AROUND.                     
TPAGYMED DS    CL4                 HOLDS TYPE, AGENCY, AND MEDIA.               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE SPGENMSTA         TYPE-'N' RECORD DSECT.                       
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT          MARKET RECORDS DSECT.                        
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================ MY DSECTS ==============================*         
*                                                                               
*------------------- MARKET/STATION DISPLAY FORMAT -------------------*         
*                                                                               
MKSTATD  DSECT                                                                  
         DS    CL8                 TWA FIELD-HEADER.                            
MKSMKT   DS    CL4                 MARKET CODE.                                 
         DS    CL1                                                              
MKSNAME  DS    CL28                MARKET NAME (+ CONT'D LABEL).                
MKSSTA   DS    CL11                CALL LETTERS & CLIENT.                       
         DS    CL11                 "      "    "   "   .                       
         DS    CL11                 "      "    "   "   .                       
         DS    CL11                 "      "    "   "   .                       
MKSTATDQ EQU   *-MKSTATD                                                        
*                                                                               
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPINF48   05/01/02'                                      
         END                                                                    
