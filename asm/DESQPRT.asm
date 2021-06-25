*          DATA SET DESQPRT    AT LEVEL 003 AS OF 03/10/21                      
*PROCESS USING(WARN(15))                                                        
*PHASE DESQPRTE                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE DDINFO                                                                 
         TITLE 'SORT/COMBINE SQAD'                                              
SQADSORT CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,SQADSORT,VREGSAVE                                              
         USING MITD,R2              R2 = MIT DSECT                              
         GOTO1 =V(STXITER),DMCB,STXTAB                                          
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         B     INIT                                                             
*                                                                               
STXTAB   DS    0F                                                               
         DC    A(SQADSORT)                                                      
         DC    V(PDUMPER)                                                       
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
VREGSAVE DC    V(REGSAVE)                                                       
         EJECT                                                                  
INIT     OPEN  (FILIN1,(INPUT))                                                 
         MVI   INFILE,1            FIRST INPUT FILE                             
         LHI   R0,SRTRCLNQ                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCRD+21(3),DUB                                                 
         GOTO1 =V(SORTER),DMCB,SORTCRD,(X'80',RECCRD),(X'80',0)                 
         XC    INCNT,INCNT                                                      
         XC    OUTCNT,OUTCNT                                                    
         LA    RE,IOAREA                                                        
         LH    R1,=Y(RECLENQ)                                                   
         AR    RE,R1                                                            
         ST    RE,AREC             A(SAVED RECORD)                              
*                                                                               
         GOTO1 =V(DDINFO),DMCB,(6,DDNAME),TUKEY,0                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     RE,DMCB+8                                                        
         ICM   RF,1,DMCB+8                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(RE)                                                       
*                                                                               
         SR    RE,RE                                                            
         LA    R1,P+1(RF)                                                       
         BCTR  R1,0                GO BACKWARDS UNTIL /                         
         AHI   RE,1                                                             
         CLI   0(R1),C'/'                                                       
         BNE   *-10                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FILENAME(0),1(R1)   KEEP ONLY FILENAME AFTER /                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   P(0),FILENAME                                                    
         MVC   FQVID+1(2),FILENAME KEEP THE 2 CHAR FILE ID                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
SORT2    GET   FILIN1,IOAREA+SRTKDQ                                             
*                                                                               
         L     RE,INCNT            COUNT THE INPUT RECORDS                      
         LA    RE,1(RE)                                                         
         ST    RE,INCNT                                                         
         LA    R2,IOAREA+SRTKDQ                                                 
         LA    R3,IOAREA                                                        
         USING SRTKD,R3                                                         
*                                                                               
         CLC   =C'DATA_RELEASE',4(R2)                                           
         BNE   SORT3                                                            
         XC    0(SRTKDQ,R3),0(R3)                                               
         B     SORT4                                                            
*                                                                               
SORT3    DS    0H                                                               
         AHI   R2,4                                                             
         XC    0(SRTKDQ,R3),0(R3)                                               
         LA    R4,SORTKTAB                                                      
         ZIC   R0,0(R4)                                                         
SORT3A   BAS   RE,PARSER                                                        
         ZIC   R0,0(R4)                                                         
         ZIC   R1,1(R4)                                                         
         ZIC   RE,WORK                                                          
         CR    R1,RE               MAKE SURE LENGTH OF FLD NOT EXCEED           
         BNL   *+6                                                              
         DC    H'0'                                                             
*        BCTR  RE,0                NOT GOING TO SUBTRACT ONE                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK                                                     
         AR    R3,R1                                                            
         AHI   R3,1                                                             
         AHI   R4,2                                                             
         CLI   0(R4),X'FF'                                                      
         BE    SORT4                                                            
         ZIC   R0,0(R4)                                                         
         B     SORT3A                                                           
*                                                                               
SORT4    GOTO1 =V(SORTER),DMCB,=C'PUT',IOAREA                                   
         XC    IOAREA+SRTKDQ(SRTKDQ),IOAREA+SRTKDQ                              
         B     SORT2                                                            
*                                                                               
SORT10   CLOSE (FILIN1,)                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
SORT14   DS    0H                                                               
         OPEN  (FILOUT,(OUTPUT))                                                
*                                                                               
SORT15   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,DMCB+4        A(RETURNED RECORD)                           
         BZ    DONE                                                             
*                                                                               
         LA    R0,IOAREA                                                        
         LHI   R1,RECLENQ                                                       
         LR    RE,R2                                                            
         LHI   RF,SRTRCLNQ         SORT RECORD LENGTH                           
         MVCL  R0,RE                                                            
         JO    *+2                 DESTRUCTIVE MOVE!                            
*                                                                               
         LA    R2,IOAREA                                                        
         AHI   R2,SRTKDQ                                                        
         LH    RE,0(R2)                                                         
         AR    RE,R2               TRUE END OF LINE                             
*        SHI   RE,1                OVERWRITE END OF LINE                        
         MVI   0(RE),C','                                                       
         MVC   1(4,RE),FQVID                                                    
         LA    RE,5(RE)                                                         
         MVI   0(RE),X'0D'         NEW END OF LINE                              
         SR    RE,R2                                                            
         STH   RE,0(R2)                                                         
         PUT   FILOUT,(R2)                                                      
         B     SORT15                                                           
*                                                                               
DONE     L     RE,OUTCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,OUTCNT                                                        
         GOTO1 =V(PRINTER)         BLANK LINE                                   
         MVC   P(23),=C'TOTAL RECORDS OUTPUT = '                                
         EDIT  OUTCNT,(8,P+25)                                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
DONE10   CLOSE FILOUT                                                           
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
*PARSER  - PARSING FIELDS                                                       
* ON ENTRY R2-POINTS TO BEGINNING OF WHERE TO START PARSING                     
*          R0-CONTAINS WHICH FIELD WE ARE LOOKING FOR                           
* ON EXIT  WORK  HAS THE LENGTH OF FIELD                                        
*          WORK+1  HAS THE PARSED FIELD                                         
***********************************************************************         
*                                                                               
PARSER   NTR1                                                                   
         LA    RE,WORK+1                                                        
         XC    WORK,WORK                                                        
         LA    R5,1                STARTING WITH FIRST FIELD                    
         LA    R7,0                FIELD LENGTH                                 
         MVI   PARSEFLG,0                                                       
*                                                                               
PAR05    CR    R5,R0               THIS THE FIELD WE LOOKING FOR?               
         BL    PAR10                                                            
*                                                                               
         CLI   0(R2),C'"'          CURRENT FIELD " DELIMITED?                   
         BNE   *+12                                                             
         OI    PARSEFLG,PFQUOTEQ                                                
         LA    R2,1(R2)                                                         
*                                                                               
PAR06    MVC   0(1,RE),0(R2)                                                    
         LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         LA    R7,1(R7)                                                         
         STC   R7,WORK             KEEP TRACK OF LENGTH                         
*                                                                               
*PAR07    CHI   R0,R_TQ             LAST FIELD?                                 
PAR07    CHI   R0,R_DQ             LAST FIELD?                                  
         BNE   *+12                                                             
         CLI   0(R2),0             END OF FIELD YET?                            
         BE    PARXX                                                            
*                                                                               
         MVI   BYTE,C','                                                        
         TM    PARSEFLG,PFQUOTEQ                                                
         BZ    *+8                                                              
         MVI   BYTE,C'"'           CURRENT FIELD IS " DELIMITED                 
*                                                                               
         CLC   BYTE,0(R2)          END OF FIELD YET?                            
         BNE   PAR06                                                            
         B     PARXX                                                            
*                                                                               
PAR10    CLI   0(R2),C'"'          THIS FIELD " DELIMITED?                      
         BNE   PAR12                                                            
PAR11    LA    R2,1(R2)                                                         
         CLI   0(R2),C'"'                                                       
         BNE   PAR11                                                            
         LA    R2,2(R2)            BUMP PAST " AND ,                            
         LA    R5,1(R5)                                                         
         B     PAR05                                                            
*                                                                               
PAR12    LA    R2,1(R2)            BUMP THROUGH TILL NEXT FIELD                 
         CLI   0(R2),C','                                                       
         BNE   PAR10                                                            
         LA    R5,1(R5)                                                         
         LA    R2,1(R2)                                                         
         B     PAR05                                                            
*                                                                               
PARXX    XIT1                                                                   
*                                                                               
NDEMS    EQU   40                                                               
* ****************************************************************              
SORTCRD  DC    C'SORT FIELDS=(2,40,A,43,17,A,61,15,A,77,8,A),FORMAT=BI,+        
               EQUALS '                                                         
RECCRD   DC    C'RECORD TYPE=F,LENGTH=XXX '                                     
SRTRCLNQ EQU   401                                                              
*                                                                               
FILIN1   DCB   DDNAME=FILIN1,DSORG=PS,RECFM=VB,MACRF=(GM),             X        
               EODAD=SORT10,LRECL=1200                                          
*                                                                               
FILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=VB,MACRF=(PM),             X        
               LRECL=1200,BLKSIZE=3600                                          
*                                                                               
SORTKTAB DS    0H                      (FIELD #, MAX LENGTH OF FIELD)           
         DC    AL1(M_NQ),AL1(L'SRTM_N)                                          
         DC    AL1(Q_NQ),AL1(L'SRTQ_N)                                          
         DC    AL1(D_NQ),AL1(L'SRTD_N)                                          
         DC    AL1(D_CQ),AL1(L'SRTD_C)                                          
         DC    AL1(D_RQ),AL1(L'SRTD_R)                                          
         DC    AL1(H_CQ),AL1(L'SRTH_C)                                          
         DC    AL1(H_D_CQ),AL1(L'SRTH_D_C)                                      
*        DC    AL1(H_T_CQ),AL1(L'SRTH_T_C)                                      
         DC    AL1(A_CQ),AL1(L'SRTA_C)                                          
         DC    AL1(A_D_CQ),AL1(L'SRTA_D_C)                                      
*        DC    AL1(A_T_CQ),AL1(L'SRTA_T_C)                                      
         DC    AL1(L_CQ),AL1(L'SRTL_C)                                          
         DC    AL1(L_D_CQ),AL1(L'SRTL_D_C)                                      
*        DC    AL1(L_T_CQ),AL1(L'SRTL_T_C)                                      
         DC    AL1(P_DQ),AL1(L'SRTP_D)                                          
         DC    AL1(R_DQ),AL1(L'SRTR_D)                                          
*        DC    AL1(P_TQ),AL1(L'SRTP_T)                                          
*        DC    AL1(R_TQ),AL1(L'SRTR_T)                                          
         DC    X'FF'                                                            
*                                                                               
DDNAME   DC    C'FILIN1'                                                        
TUKEY    DC    X'C017'                                                          
INCNT    DC    F'0'                                                             
OUTCNT   DC    F'0'                                                             
FILENAME DS    CL50                                                             
FQVID    DC    C'"  "'                                                          
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    CL(SRTKDQ)                                                       
BYTE     DS    X                                                                
*                                                                               
PARSEFLG DS    X                                                                
PFQUOTEQ EQU   X'80'                                                            
*                                                                               
INFILE   DS    X                                                                
AREC     DS    F                                                                
         DS    F                                                                
         LTORG                                                                  
RECLENQ  EQU   5000                                                             
IOAREA   DS    CL(RECLENQ)                                                      
*                                                                               
*  FIELD # EQUATES                                                              
*                                                                               
D_RQ     EQU   1                   DATA RELEASE                                 
M_NQ     EQU   2                   MARKET NAME                                  
D_NQ     EQU   3                   DAYPART NAME                                 
D_CQ     EQU   4                   DEMO CD                                      
Q_NQ     EQU   5                   QUARTER NAME                                 
H_CQ     EQU   6                   HIGH CPP                                     
H_D_CQ   EQU   7                   HIGH DMA CPM                                 
*                                                                               
* SPSUG-2738 NEW FORMAT                                                         
*                                                                               
A_CQ     EQU   8                   AVG CPP                                      
A_D_CQ   EQU   9                   AVG DMA CPM                                  
L_CQ     EQU   10                  LOW CPP                                      
L_D_CQ   EQU   11                  LOW DMA CPM                                  
P_DQ     EQU   12                  POP DMA                                      
R_DQ     EQU   13                  RANK DMA                                     
*                                                                               
*&&DO                                                                           
H_T_CQ   EQU   8                   HIGH TSA CPA                                 
A_CQ     EQU   9                   AVH CPP                                      
A_D_CQ   EQU   10                  AVG DMA CPM                                  
A_T_CQ   EQU   11                  AVG TSA CPA                                  
L_CQ     EQU   12                  LOW CPP                                      
L_D_CQ   EQU   13                  LOW DMA CPM                                  
L_T_CQ   EQU   14                  LOW TSA CPA                                  
P_DQ     EQU   15                  POP DMA                                      
R_DQ     EQU   16                  RANK DMA                                     
P_TQ     EQU   17                  POP TSA                                      
R_TQ     EQU   18                  RANK TSA                                     
*&&                                                                             
*                                                                               
SRTKD    DSECT                                                                  
SRTKY    DS    CL1                 USED FOR INPUT LENGTH                        
SRTM_N   DS    CL40                MARKET NAME                                  
         DS    CL1                                                              
SRTQ_N   DS    CL17                QUARTER NAME                                 
         DS    CL1                                                              
SRTD_N   DS    CL15                DAYPART NAME                                 
         DS    CL1                                                              
SRTD_C   DS    CL8                 DEMO CD                                      
         DS    CL1                                                              
SRTD_R   DS    CL8                 DATA RELEASE                                 
         DS    CL1                                                              
SRTH_C   DS    CL8                 HIGH CPP                                     
         DS    CL1                                                              
SRTH_D_C DS    CL8                 HIGH DMA CPM                                 
         DS    CL1                                                              
*SRTH_T_C DS    CL8                 HIGH TSA CPA                                
*         DS    CL1                                                             
SRTA_C   DS    CL8                 AVG CPP                                      
         DS    CL1                                                              
SRTA_D_C DS    CL8                 AVG DMA CPA                                  
         DS    CL1                                                              
*SRTA_T_C DS    CL8                 AVG TSA CPM                                 
*         DS    CL1                                                             
SRTL_C   DS    CL8                 LOW CPP                                      
         DS    CL1                                                              
SRTL_D_C DS    CL8                 LOW DMA CPA                                  
         DS    CL1                                                              
*SRTL_T_C DS    CL8                 LOW TSA CPM                                 
*         DS    CL1                                                             
SRTP_D   DS    CL8                 POP DMA                                      
         DS    CL1                                                              
SRTR_D   DS    CL8                 RANK DMA                                     
*         DS    CL1                                                             
*SRTP_T   DS    CL8                 POP TSA                                     
*         DS    CL1                                                             
*SRTR_T   DS    CL8                 RANK TSA                                    
SRTKDQ   EQU   *-SRTKY                                                          
*                                                                               
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DEMITD                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DESQPRT   03/10/21'                                      
         END                                                                    
