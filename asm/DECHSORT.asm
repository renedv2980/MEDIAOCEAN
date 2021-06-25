*          DATA SET DECHSORT   AT LEVEL 066 AS OF 10/10/13                      
*PROCESS USING(WARN(15))                                                        
*PHASE DECHSRTA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'SORT CABLE DEMO TAPE BEFORE RUNNING CONVERSION PRGM'            
DECABSRT CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DECABSRT,=V(REGSAVE)                                           
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
         DC    A(DECABSRT)                                                      
         DC    V(DUMMY)                                                         
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
INIT     OPEN  (FILIN,(INPUT))                                                  
         GOTO1 =V(SORTER),DMCB,SORTCRD,RECCRD                                   
         XC    INCNT,INCNT                                                      
         XC    OUTCNT,OUTCNT                                                    
         LA    RE,IOAREA                                                        
         LH    R1,=Y(RECLENQ)                                                   
         AR    RE,R1                                                            
         ST    RE,AREC             A(SAVED RECORD)                              
*                                                                               
SORT2    GET   FILIN,IOAREA                                                     
         L     RE,INCNT            COUNT THE INPUT RECORDS                      
         LA    RE,1(RE)                                                         
         ST    RE,INCNT                                                         
         LA    R2,IOAREA                                                        
*                                                                               
         CLC   MITSEQ,=C'04'                                                    
         BNE   SORT3                                                            
         CLI   MITREC,C'E'                                                      
         BE    SORT2                                                            
         CLI   MITREC,C'F'                                                      
         BE    SORT2                                                            
         CLC   MITHLFID,=C'00'     DISCARD ALL PRG 1/2HR RECDS                  
         BNE   SORT2                                                            
         CLI   MITPUT,C'1'         DISCARD US PROGRAM PUTS                      
         BE    SORT2                                                            
*                                                                               
SORT3    CLC   MITSEQ,=C'03'                                                    
         BNE   SORT4                                                            
         MVC   MITPRG+4(6),MITCOVG   FOR SORT: NEED CVG HI IN KEY               
*                                                                               
SORT4    CLC   MITSEQ,=C'03'                                                    
         BE    *+10                                                             
         CLC   MITSEQ,=C'05'                                                    
         BNE   SORT4B                                                           
         CLC   MITAVG,=C'05'       GET RID OF AVGS                              
         BE    SORT2                                                            
         CLC   MITAVG,=C'07'                                                    
         BE    SORT2                                                            
*TST                                                                            
SORT4B   GOTO1 =V(SORTER),DMCB,=C'PUT',IOAREA                                   
*                                                                               
         CLI   MITREC,C'H'                                                      
         BNE   SORT2                                                            
         CLC   MITAVG,=C'00'       NO QTR HR AVGS RECD                          
         BNE   SORT2                                                            
         CLC   MITSEQ,=C'03'                                                    
         BE    *+14                                                             
         CLC   MITSEQ,=C'05'                                                    
         BNE   SORT2                                                            
         PACK  DUB,MITHLFID                                                     
         CVB   R3,DUB                                                           
         SLL   R3,1                HLF HR CODE * 2  - 1 = QTR HR CODE           
         BCTR  R3,0                                                             
         MVI   TMP+1,0             OUTPUT 2 QTR HR RECDS                        
SORT5    STC   R3,TMP                                                           
         EDIT  (1,TMP),(2,MITQTRID)                                             
         OC    MITQTRID,=C'00'     PAD WITH 0'S NOT BLANKS                      
         MVC   MIHDUR,=C'0015'     15-MINUTE DURATION                           
         GOTO1 =V(SORTER),DMCB,=C'PUT',IOAREA                                   
         IC    RE,TMP+1                                                         
         LA    RE,1(RE)                                                         
         STC   RE,TMP+1                                                         
         LA    R3,1(R3)                                                         
         CLC   MITMIN,=C'00'                                                    
         BNE   *+10                                                             
         MVC   MITMIN,=C'15'                                                    
         CLC   MITMIN,=C'30'                                                    
         BNE   *+10                                                             
         MVC   MITMIN,=C'45'                                                    
         CLI   TMP+1,2                                                          
         BL    SORT5               OUTPUT 2 RECORDS                             
         B     SORT2               DONE DUPLICATING RECORD                      
*                                                                               
SORT10   CLOSE (FILIN,)                                                         
         B     SORT14                                                           
         EJECT                                                                  
***********************************************************************         
* GET SORTED RECORDS FROM SORTER.  COMBINE 'D'S AND 'H'S WITH 'P'S              
***********************************************************************         
*                                                                               
SORT14   OPEN  (FILOUT,(OUTPUT))                                                
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
         BAS   RE,COMB                                                          
         B     SORT15              GO GET ANOTHER RECD                          
*                                                                               
DONE     DS    0H                  DONE PROCESSING SORTED TAPE RECDS            
         CLI   REL,C'Y'                                                         
         BNE   DONE10                                                           
         L     R3,AREC                                                          
         CLC   =C'99',0(R3)                                                     
         BE    DONE5                                                            
         CLI   MITORIG-MITD(R3),C'0'   ORIG DATA ON 1ST OUTFILE                 
         BNE   DONE5                                                            
*                                                                               
DONE4    PUT   FILOUT,(R3)                                                      
         B     DONE7                                                            
*                                                                               
DONE5    DS    0H                                                               
*                                                                               
DONE7    L     RE,OUTCNT                                                        
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
         CLC   MITSEQ,=C'00'       REPORT DESCR RECD                            
         BE    COMB5                                                            
         CLC   MITSEQ,=C'99'       AUTHORIZATION RECDS                          
         BE    COMB5                                                            
         CLI   MITREC,C'P'         DEMOGRAPHIC PERSONS RECDS                    
         BNE   COMB5                                                            
         L     RF,AREC                                                          
         CLC   MITKEY(MITREC-MITD),0(RF)   CMP TILL RECTYP FLD                  
         BE    COMB_P                                                           
*                                                                               
COMB5    CLI   REL,C'Y'            DO WE NEED TO RELEASE LAST REC?              
         BNE   COMB10              NO                                           
         MVC   TITLE(12),=C'TAPE WEEK: '                                        
         MVC   TITLE+12(6),MITSTART                                             
         MVC   TITLE+18(4),=C'THRU'                                             
         MVC   TITLE+23(6),MITEND                                               
         L     R4,AREC             YES                                          
         CLC   =C'99',0(R4)                                                     
         BE    COMB7                                                            
         CLI   MITORIG-MITD(R4),C'0'   ORIG DATA ON 1ST OUTFILE                 
         BNE   COMB7                                                            
*                                                                               
COMB6    PUT   FILOUT,(R4)         REGULAR DATA--1ST OUTPUT FILE                
         CLC   =C'00',0(R4)                                                     
         BE    *+8                 SEND TO OTHER FILE AS WELL                   
         B     COMB8                                                            
*                                                                               
COMB7    DS    0H                                                               
*                                                                               
COMB8    CLC   MITSEQ,=C'04'                                                    
         BNE   *+8                                                              
         BAS   RE,PRNTREC                                                       
         L     RE,OUTCNT           BUMP OUTPUT RECD COUNTER                     
         LA    RE,1(RE)                                                         
         ST    RE,OUTCNT                                                        
*                                                                               
         L     R0,AREC             CLEAR BUFFER- BLANK PAD                      
         LH    R1,=Y(RECLENQ)      1200 BYTES                                   
         LA    RE,IOAREA           DUMMY ADDRESS                                
         SR    RF,RF               FILL CHARACTER                               
         ICM   RF,8,=C'    '                                                    
         MVCL  R0,RE                                                            
*                                                                               
COMB10   MVI   REL,C'Y'            MOVE NEW RECD INTO BUFFER                    
         L     RF,AREC                                                          
         LA    R1,400              LENGTH OF HEADER RECD                        
         LR    RE,R2                                                            
         MOVE  ((RF),(R1)),(RE)    SAVE RECD IN AREC                            
         B     COMBX                                                            
*                                                                               
COMB_P   DS    0H                                                               
         L     RF,AREC             BUILD COMBINED RECD HERE                     
         LA    R1,400              LENGTH OF HEADER RECD                        
         AR    RF,R1               PT TO 1ST POSTN AFTER HEADER                 
         LA    R1,NDEMS*9          DISP TO PUTS (TOT DEMOS*9BYTE BKTS)          
         CLI   MITPUT,C' '         TREAT A BLANK LIKE A '0'                     
         BE    *+14                NO                                           
         CLI   MITPUT,C'0'         ARE THESE DEMOS PUTS?                        
         BE    *+6                 NO                                           
         AR    RF,R1               YES- PT TO PUT AREA IN AREC                  
         LA    R1,MIPNDEMS*9       DISP TO DEMOS 21-40                          
         CLC   MITDEMG,=C'001'     ARE THESE DEMOS GROUP 21-40?                 
         BE    *+6                 NO                                           
         AR    RF,R1               YES                                          
         LA    RE,MIPDEM1          PT TO DEMOS IN SOURCE RECD                   
         MOVE  ((RF),(R1)),(RE)    SAVE 'P' RECD DEMOS IN AREC (RF)             
*                                                                               
COMBX    XIT1                                                                   
*                                                                               
NDEMS    EQU   40                  TOTAL 40 DEMOS IN THIS RECD                  
*                                                                               
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
         EJECT                                                                  
SORTCRD  DC    CL80'SORT FIELDS=(1,114,A),FORMAT=BI'                            
RECCRD   DC    CL80'RECORD TYPE=F,LENGTH=400'                                   
         EJECT                                                                  
FILIN    DCB   DDNAME=FILIN,DSORG=PS,RECFM=FB,MACRF=(GM),              X        
               EODAD=SORT10,LRECL=0400                                          
FILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=FB,MACRF=(PM),             X        
               LRECL=1120,BLKSIZE=11200                                         
*                                                                               
INCNT    DC    F'0'                                                             
OUTCNT   DC    F'0'                                                             
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    CL17                                                             
TMP      DS    CL5                                                              
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
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE DEMITD                                                         
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'066DECHSORT  10/10/13'                                      
         END                                                                    
