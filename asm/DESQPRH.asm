*          DATA SET DESQPRH    AT LEVEL 002 AS OF 06/15/17                      
*PROCESS USING(WARN(15))                                                        
*PHASE DESQPRHA                                                                 
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
         GOTO1 =V(SORTER),DMCB,SORTCRD,RECCRD                                   
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
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    DONE                                                             
         L     R2,DMCB+4           ADDRESS OF RECD FROM SORT                    
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
SORTCRD  DC    CL80'SORT FIELDS=(2,40,A,43,17,A,61,15,A,77,8,A),FORMAT=*        
               BI'                                                              
RECCRD   DC    CL80'RECORD TYPE=F,LENGTH=401'                                   
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
         DC    AL1(A_CQ),AL1(L'SRTA_C)                                          
         DC    AL1(A_D_CQ),AL1(L'SRTA_D_C)                                      
         DC    AL1(P_DQ),AL1(L'SRTP_D)                                          
         DC    AL1(R_DQ),AL1(L'SRTR_D)                                          
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
A_CQ     EQU   6                   AVH CPP                                      
A_D_CQ   EQU   7                   AVG DMA CPM                                  
P_DQ     EQU   8                   POP DMA                                      
R_DQ     EQU   9                   RANK DMA                                     
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
SRTA_C   DS    CL8                 AVG CPP                                      
         DS    CL1                                                              
SRTA_D_C DS    CL8                 AVG DMA CPA                                  
         DS    CL1                                                              
SRTP_D   DS    CL8                 POP DMA                                      
         DS    CL1                                                              
SRTR_D   DS    CL8                 RANK DMA                                     
SRTKDQ   EQU   *-SRTKY                                                          
*                                                                               
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DEMITD                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DESQPRH   06/15/17'                                      
         END                                                                    
