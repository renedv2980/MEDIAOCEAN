*          DATA SET SCHODSRT   AT LEVEL 088 AS OF 09/22/00                      
*PHASE DEHSDSRA DEHSDSRT                                                        
*INCLUDE REGSAVE                                                                
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
         TITLE 'SORT/COMBINE NHTI AND SPANISH DOMINANT TAPES'                   
DENHSORT CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DENHSORT,VREGSAVE                                              
         USING MITD,R2              R2 = MIT DSECT                              
         GOTO1 =V(STXITER),DMCB,STXTAB                                          
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         LA    R4,MID1                                                          
         USING PRNTD,R4                                                         
         MVC   PITTYPE,=C'NET   '                                               
         MVC   PITCOVG,=C'COVRG '                                               
         MVC   PITPRG,=C'PRG NUMBER'                                            
         MVC   PITTRACK,=C'TRK '                                                
         MVC   PITBREAK,=C'B'                                                   
         MVC   PITSPC,=C'S'                                                     
         MVC   PITCOMR,=C'C'                                                    
         MVC   PITAVG,=C'AV'                                                    
         MVC   PITTELC,=C'TELC#'                                                
         MVC   PIDPNAME,=C'PROGRAM NAME                  '                      
         MVC   PIDTKNAM,=C'TRACKAGE NAME                 '                      
         MVC   PIDEPNAM,=C'EPIS NAME   '                                        
         MVC   PIDEPNUM,=C'EPS#'                                                
         MVC   PIDDWOS,=C'DAY M-S'                                              
         MVC   PIDHOUR,=C'TIME'                                                 
         MVC   PIDDUR,=C'DUR '                                                  
         DROP  R4                                                               
         B     INIT                                                             
*                                                                               
STXTAB   DS    0F                                                               
         DC    A(DENHSORT)                                                      
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
SORT2    GET   FILIN1,IOAREA       REGULAR NHTI                                 
         B     SORT3                                                            
*                                                                               
SORT2A   GET   FILIN2,IOAREA       SPANISH DOMINANT                             
*                                                                               
SORT3    L     RE,INCNT            COUNT THE INPUT RECORDS                      
         LA    RE,1(RE)                                                         
         ST    RE,INCNT                                                         
         LA    R2,IOAREA                                                        
*                                                                               
         CLC   MITSEQ(2),=C'00'                                                 
         BNE   SORT3A                                                           
         CLI   INFILE,2            JUST ONE '00' WILL DO                        
         BE    SORT2A                                                           
         MVC   MI0FILE(40),=C'NHTI AUDIENCE ESTIMATES+SPANISH DOMINANT'         
*                                                                               
SORT3A   MVC   MITKEY+105(1),MITSMPL                                            
         MVI   MITSMPL,X'40'       MOVE SAMPLE DOWN THE KEY                     
         CLI   INFILE,2                                                         
         BNE   SORT4                                                            
*                                  FOR SPANISH DOMINANT ONLY:                   
         MVC   MITMKTBR,=C'515'    CHANGE MARKET BREAK ALL TO 515               
*                                                                               
SORT4    GOTO1 =V(SORTER),DMCB,=C'PUT',IOAREA                                   
*                                                                               
         CLI   INFILE,2            NEXT RECORD                                  
         BNE   SORT2                                                            
         B     SORT2A                                                           
*                                                                               
SORT10   CLOSE (FILIN1,)                                                        
         OPEN  (FILIN2,(INPUT))                                                 
         MVI   INFILE,2                                                         
         B     SORT2A                                                           
*                                                                               
SORT10A  DS    0H                                                               
         CLOSE (FILIN2,)                                                        
*                                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* GET SORTED RECORDS FROM SORTER.  COMBINE 'D'S AND 'H'S WITH 'P'S              
***********************************************************************         
*                                                                               
SORT14   DS    0H                                                               
         OPEN  (FILOUT,(OUTPUT))                                                
         MVI   REL,C'N'                                                         
         L     R0,AREC             CLEAR BUFFER- BLANK PAD                      
         LH    R1,=Y(RECLENQ)      1200 BYTES                                   
         LA    RE,IOAREA           DUMMY ADDRESS                                
         SR    RF,RF               FILL CHARACTER                               
         ICM   RF,8,=C'    '                                                    
         MVCL  R0,RE                                                            
*                                                                               
SORT15   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    DONE                                                             
         L     R2,DMCB+4           ADDRESS OF RECD FROM SORT                    
         BAL   RE,COMB                                                          
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
*COMB -  PROCESS RECORDS                                                        
***********************************************************************         
COMB     NTR1                      COMBINE DEMO RECS W/HEADERS                  
         L     R2,DMCB+4           A(SORT RECD)  --R2 USES MITD--               
*                                                                               
         PUT   FILOUT,(R2)         REGULAR DATA--1ST OUTPUT FILE                
*                                                                               
COMBX    XIT1                                                                   
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
*ORTCRD  DC    CL80'SORT FIELDS=(1,114,A),FORMAT=BI,WORK=1 '                    
SORTCRD  DC    CL80'SORT FIELDS=(1,105,A),FORMAT=BI,WORK=1 '                    
RECCRD   DC    CL80'RECORD TYPE=F,LENGTH=401'                                   
         EJECT                                                                  
FILIN1   DCB   DDNAME=FILIN1,DSORG=PS,RECFM=FB,MACRF=(GM),             X        
               EODAD=SORT10,LRECL=0401,BLKSIZE=16842                            
FILIN2   DCB   DDNAME=FILIN2,DSORG=PS,RECFM=FB,MACRF=(GM),             X        
               EODAD=SORT10A,LRECL=0401,BLKSIZE=16842                           
*                                                                               
FILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=FB,MACRF=(PM),             X        
               LRECL=0401,BLKSIZE=16842                                         
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
**PAN#1  DC    CL21'088SCHODSRT  09/22/00'                                      
         END                                                                    
