*          DATA SET SQADSORT   AT LEVEL 090 AS OF 06/02/06                      
*PHASE SQADSORA SQADSORT                                                        
*INCLUDE REGSAVE                                                                
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
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
SORT2    GET   FILIN1,IOAREA+SORTKDQ                                            
*                                                                               
         L     RE,INCNT            COUNT THE INPUT RECORDS                      
         LA    RE,1(RE)                                                         
         ST    RE,INCNT                                                         
         LA    R2,IOAREA+SORTKDQ                                                
         LA    R3,IOAREA                                                        
         USING SORTKD,R3                                                        
*                                                                               
         CLC   4(12,R2),=C'"IssueMonth"'                                        
         BNE   SORT3                                                            
         XC    0(SORTKDQ,R3),0(R3)                                              
         B     SORT4                                                            
*                                                                               
SORT3    DS    0H                                                               
         AHI   R2,4                                                             
         XC    0(SORTKDQ,R3),0(R3)                                              
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
         AHI   R2,SORTKDQ                                                       
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
*                                                                               
PAR05    CR    R5,R0               THIS THE FIELD WE LOOKING FOR?               
         BL    PAR10                                                            
PAR06    LA    R2,1(R2)            MOVE PAST FIRST "                            
*                                                                               
PAR07    MVC   0(1,RE),0(R2)                                                    
         LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         LA    R7,1(R7)                                                         
         STC   R7,WORK             KEEP TRACK OF LENGTH                         
*                                                                               
PAR08    CLI   0(R2),C'"'          END OF FIELD YET?                            
         BNE   PAR07                                                            
         B     PARXX                                                            
*                                                                               
PAR10    CLI   0(R2),C','          START OF NEXT FIELD?                         
         BE    PAR15                                                            
PAR12    LA    R2,1(R2)            BUMP THROUGH TILL NEXT FIELD                 
         CLI   0(R2),C','                                                       
         BNE   PAR12                                                            
         LA    R5,1(R5)                                                         
         LA    R2,1(R2)                                                         
         B     PAR05                                                            
*                                                                               
PAR15    LA    R2,1(R2)            POINT TO NEXT STARTING "                     
         LA    R5,1(R5)            BUMP COUNTER                                 
         B     PAR05                                                            
PARXX    XIT1                                                                   
*                                                                               
NDEMS    EQU   40                                                               
* ****************************************************************              
* PRNTREC -    PRINT FIELDS FROM RECD                                           
* ****************************************************************              
PRNTREC  NTR1                                                                   
         LA    R4,P                                                             
         USING PRNTD,R4                                                         
         CLI   MITORIG,C'0'                                                     
         BNE   PRNT20                                                           
         MVC   PITTYPE,MITTYPE                                                  
         MVC   PITCOVG,MITCOVG                                                  
         MVC   PITPRG,MITPRG                                                    
         MVC   PITTRACK,MITTRACK                                                
         MVC   PITBREAK,MITBREAK                                                
         MVC   PITSPC,MITSPC                                                    
         MVC   PITCOMR,MIDCOMR                                                  
         MVC   PITAVG,MITAVG                                                    
         MVC   PITTELC,MITTELC+5                                                
         MVC   PIDPNAME,MIDPNAME                                                
         MVC   PIDTKNAM,MIDTKNAM                                                
         MVC   PIDDWOS,MIDDAYS                                                  
         MVC   PIDHOUR,MITHOUR                                                  
         MVC   PIDDUR,MIDDUR                                                    
         MVC   PIDEPNAM,MIDEPNAM                                                
         MVC   PIDEPNUM,MIDEPNUM  CLT SPECD EPISODE NUMBER                      
         B     PRNT30                                                           
*                                                                               
PRNT20   DS    0H                  DSECT FOR CORRECTION RECDS                   
         USING PRNCRT,R4                                                        
         MVC   PRNORIG,MITORIG                                                  
         MVC   PRNCORDT,MITCORDT+1                                              
         MVC   PRNCORRS,MITCORRS                                                
         MVC   PRNNET,MITTYPE                                                   
         MVC   PRNCOVG,MITCOVG                                                  
         MVC   PRNPRG,MITPRG                                                    
         MVC   PRNTRK,MITTRACK                                                  
         MVC   PRNAVG,MITAVG                                                    
         MVC   PRNTELC,MITTELC+5                                                
         MVC   PRNNAME,MIDPNAME                                                 
         MVC   PRNDAYS,MIDDAYS                                                  
         MVC   PRNSTIM,MITHOUR                                                  
         MVC   PRNST,MITSTART                                                   
         MVC   PRNEND,MITEND                                                    
         MVC   PRNDUR,MIDDUR                                                    
         MVC   PRNEPNAM,MIDEPNAM                                                
         MVC   PRNEPNUM,MIDEPNUM                                                
*                                                                               
PRNT30   DS    0H                                                               
*        GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
         DROP  R4                                                               
*                                                                               
* ****************************************************************              
* SORT50 -                                                                      
* ****************************************************************              
*                                                                               
*                                                                               
SORTCRD  DC    CL80'SORT FIELDS=(1,32,A),FORMAT=BI,WORK=1 '                     
RECCRD   DC    CL80'RECORD TYPE=F,LENGTH=401'                                   
         EJECT                                                                  
FILIN1   DCB   DDNAME=FILIN1,DSORG=PS,RECFM=VB,MACRF=(GM),             X        
               EODAD=SORT10,LRECL=1200,BLKSIZE=3600                             
*                                                                               
FILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=VB,MACRF=(PM),             X        
               LRECL=1200,BLKSIZE=3600                                          
*                                                                               
SORTKTAB DS    0H                                                               
         DC    AL1(6),AL1(3)                                                    
         DC    AL1(1),AL1(2)                                                    
         DC    AL1(2),AL1(4)                                                    
         DC    AL1(4),AL1(4)                                                    
         DC    AL1(3),AL1(1)                                                    
         DC    AL1(8),AL1(2)                                                    
         DC    AL1(7),AL1(2)                                                    
         DC    AL1(5),AL1(1)                                                    
         DC    AL1(9),AL1(6)                                                    
         DC    X'FF'                                                            
*                                                                               
INCNT    DC    F'0'                                                             
OUTCNT   DC    F'0'                                                             
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    CL17                                                             
TMP      DS    CL5                                                              
INFILE   DS    X                                                                
REL      DS    X                                                                
CHAR     DS    C                                                                
AREC     DS    F                                                                
         DS    F                                                                
         SPACE 2                                                                
         LTORG                                                                  
RECLENQ  EQU   5000                                                             
IOAREA   DS    CL(RECLENQ)                                                      
LASTREC  DS    CL(RECLENQ)                                                      
         EJECT                                                                  
*                                                                               
SORTKD   DSECT                                                                  
SORTKY   DS    CL1                                                              
SORTMON  DS    CL2                 MONTH                                        
         DS    CL1                                                              
SORTYR   DS    CL4                 YEAR                                         
         DS    CL1                                                              
SORTQTR  DS    CL1                 QTR                                          
         DS    CL1                                                              
SORTDYR  DS    CL4                 DATA YEAR                                    
         DS    CL1                                                              
SORTLVL  DS    CL1                 CPP LEVEL                                    
         DS    CL1                                                              
SORTMID  DS    CL3                 MARKET ID                                    
         DS    CL1                                                              
SORTTID  DS    CL2                 TARGET ID                                    
         DS    CL1                                                              
SORTDID  DS    CL2                 DAYPART ID                                   
         DS    CL1                                                              
SORTCPP  DS    CL6                 CPP VALUE                                    
SORTKDQ  EQU   *-SORTKY                                                         
*                                                                               
PRNTD    DSECT                                                                  
PITTYPE  DS    CL6       15-20     DATA TYPE CODE                               
PITCOVG  DS    CL6       71-76     COVERAGE SAMPLE ID                           
         DS    C                                                                
PITPRG   DS    CL10      21-30     PRG CODE/CAB NET/STN ID/STN GRP              
         DS    C                                                                
PITTRACK DS    CL3       31-33     TRACKAGE ID                                  
         DS    C                                                                
PITBREAK DS    CL1       35        BREAKOUT IND                                 
PITSPC   DS    CL1       36        SPECIAL IND                                  
PITCOMR  DS    CL1       36        COMMERCIAL STATUS MIDCOMR                    
         DS    C                                                                
PITAVG   DS    CL2       42-43     NUMBER OF DAYS/WEEKS IN AVG                  
         DS    C                                                                
PITTELC  DS    CL5       58-67     TELECAST NUMBER                              
         DS    C                                                                
PIDPNAME DS    CL25      115-139   PROGRAM NAME                                 
         DS    C                                                                
PIDTKNAM DS    CL25      140-164   TRACKAGE NAME                                
         DS    C                                                                
PIDDWOS  DS    CL7                 DAYS OF WEEK BITS                            
         DS    C                                                                
PIDHOUR  DS    CL4                 START TIME                                   
         DS    C                                                                
PIDDUR   DS    CL4                 DURATION                                     
         DS    C                                                                
PIDEPNAM DS    CL10       180-211  EPISODE NAME                                 
         DS    C                                                                
PIDEPNUM DS    CL4        212-215  EPISODE NUMBER                               
         DS    C                                                                
*                                                                               
PRNCRT   DSECT                     DSECT FOR CORRECTION RECDS                   
PRNORIG  DS    CL1                 CORRECTION TYPE                              
         DS    C                                                                
PRNCORDT DS    CL6                 CORRECTION START DATE                        
         DS    C                                                                
PRNCORRS DS    CL3                 CORRECTION REASON                            
         DS    C                                                                
PRNNET   DS    CL6                 NET IF PRESENT                               
         DS    C                                                                
PRNCOVG  DS    CL6                 COVERAGE SAMPLE ID                           
         DS    C                                                                
PRNAVG   DS    CL2                 NUMBER DAYS IN AVG                           
         DS    C                                                                
PRNPRG   DS    CL10                PROGRAM NUMBER                               
         DS    C                                                                
PRNTRK   DS    CL3                 TRACKAGE                                     
         DS    C                                                                
PRNTELC  DS    CL5                 TELECAST NUMBER                              
         DS    C                                                                
PRNST    DS    CL6                 START DATE                                   
         DS    C                                                                
PRNEND   DS    CL6                 END DATE                                     
         DS    C                                                                
PRNSTIM  DS    CL4                 START TIME                                   
         DS    C                                                                
PRNDAYS  DS    CL7                 DAYS                                         
         DS    C                                                                
PRNNAME  DS    CL25                PROGRAM NAME                                 
         DS    C                                                                
PRNDUR   DS    CL4                 DURATION                                     
         DS    C                                                                
PRNEPNAM DS    CL10       180-211  EPISODE NAME                                 
         DS    C                                                                
PRNEPNUM DS    CL4        212-215  EPISODE NUMBER                               
         DS    C                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DEMITD                                                         
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'090SQADSORT  06/02/06'                                      
         END                                                                    
