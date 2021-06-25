*          DATA SET DEWBSORT   AT LEVEL 092 AS OF 07/18/11                      
*PHASE DEWBSRTA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE NETWEEK                                                                
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
                                                                                
         TITLE 'SORT TIME WARNER AND MAIN TCAR DATA'                            
DEWBSORT CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DEWBSORT,VREGSAVE                                              
         GOTO1 =V(STXITER),DMCB,STXTAB                                          
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         B     INIT                                                             
*                                                                               
STXTAB   DS    0F                                                               
         DC    A(DEWBSORT)                                                      
         DC    V(PDUMPER)                                                       
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
VREGSAVE DC    V(REGSAVE)                                                       
         EJECT                                                                  
*                                                                               
INIT     OPEN  (FILIN1,(INPUT))                                                 
         GOTO1 =V(SORTER),DMCB,SORTCRD,RECCRD                                   
         LA    RE,IOAREA                                                        
         LH    R1,=Y(RECLENQ)                                                   
         AR    RE,R1                                                            
         ST    RE,AREC             A(SAVED RECORD)                              
         LA    R7,1                                                             
         MVI   DESRECD,NO                                                       
*                                                                               
SORT10   GET   FILIN1,IOAREA       TARGET OR 1ST MKT                            
         L     RE,INCNT            COUNT THE INPUT RECORDS                      
         LA    RE,1(RE)                                                         
         ST    RE,INCNT                                                         
*                                                                               
*CATCH UNMARKED CORRECTIONS.                                                    
*NIELSEN IS SOMETIMES SENDING DATA THAT APPLIES TO PREVIOUS WEEKS BUT           
*IS NOT MARKED AS CORRECTIONS. THIS CODE WILL CHECK TO SEE IF DATA              
*BELONGS TO A PREVIOUS WEEK, AND WILL TURN ON THE CORRECTION FLAG IF            
*THAT IS THE CASE.                                                              
         LA    R2,IOAREA                                                        
         USING MITD,R2                                                          
*                                                                               
         CLC   MITSEQ,=C'00'       DESCRIPTIVE RECORD. SAVE FILE BOOK.          
         BNE   SORT12                                                           
*                                                                               
* CATCH MULTIPLE DESCRIPTIVE RECORDS ON THE SAME FILE.                          
* USUALLY THIS SCENARIO IS ASSOCIATED WITH MORE THAN ONE WEEK OF                
* DATA IN THE SAME FILE. WE ARE TRYING TO CATCH AN ISSUE WHERE NIELSEN          
* HAS BEEN INCORRECTLY INCLUDING DATA FROM PREVIOUS WEEKS IN THE                
* CURRENT WEEK'S FILE, THUS OVERRIDING DATA THAT WAS ALREADY LOADED.            
         CLI   DESRECD,NO                                                       
         BE    *+6                                                              
         DC    H'0'                MULTIPLE RESCRIPTIVE RECORDS FOUND           
         MVI   DESRECD,YES                                                      
*                                                                               
         GOTO1 =V(NETWEEK),DMCB,MITEND,V(GETDAY),V(ADDAY)                       
         MVC   FILEBK,4(R1)        YEAR                                         
         MVC   FILEBK+1(1),8(R1)   WEEK                                         
         B     SORT15                                                           
*                                                                               
SORT12   CLC   MITSEQ,=C'04'       PROGRAM RECORDS. MATCH TO FILE BOOK.         
         BNE   SORT15                                                           
         CLI   MITORIG,C'0'                                                     
         BNE   SORT15              ALREADY MARKED AS CORRECTION                 
         GOTO1 =V(NETWEEK),DMCB,MITEND,V(GETDAY),V(ADDAY)                       
         MVC   HALF,4(R1)          YEAR                                         
         MVC   HALF+1(1),8(R1)     WEEK                                         
         CLC   HALF,FILEBK                                                      
         BE    SORT15              RECD BOOK SAME AS FILE BOOK. THIS IS         
         BL    *+6                  ORIGINAL DATA.                              
         DC    H'0'                RECD BOOK HIGHER THAN FILE BOOK.             
         MVI   MITORIG,C'1'        RECD BOOK BEFORE FILE BOOK MUST BE A         
         DROP  R2                   CORRECTION. FLAG AS CORRECTION.             
*                                                                               
SORT15   GOTO1 =V(SORTER),DMCB,=C'PUT',IOAREA                                   
         B     SORT10                                                           
*                                                                               
SORT20   CLOSE (FILIN1,)                                                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
SORT30   DS    0H                                                               
         OPEN  (FILOUT,(OUTPUT))                                                
         MVI   REL,C'N'                                                         
         L     R0,AREC             CLEAR BUFFER- BLANK PAD                      
         LH    R1,=Y(RECLENQ)      1200 BYTES                                   
         LA    RE,IOAREA           DUMMY ADDRESS                                
         SR    RF,RF               FILL CHARACTER                               
         ICM   RF,8,=C'    '                                                    
         MVCL  R0,RE                                                            
*                                                                               
SORT40   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    DONE                                                             
         L     R2,DMCB+4           ADDRESS OF RECD FROM SORT                    
         PUT   FILOUT,(R2)                                                      
         L     RE,OUTCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,OUTCNT                                                        
         B     SORT40                                                           
*                                                                               
DONE     GOTO1 =V(PRINTER)         BLANK LINE                                   
         MVC   P(23),=C'TOTAL RECORDS OUTPUT = '                                
         EDIT  OUTCNT,(8,P+25)                                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
DONE10   CLOSE FILOUT                                                           
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                               
SORTCRD  DC    CL80'SORT FIELDS=(1,105,A),FORMAT=BI,WORK=1 '                    
RECCRD   DC    CL80'RECORD TYPE=F,LENGTH=400'                                   
         EJECT                                                                  
FILIN1   DCB   DDNAME=FILIN1,DSORG=PS,RECFM=FB,MACRF=(GM),             X        
               EODAD=SORT20,LRECL=0400                                          
FILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=FB,MACRF=(PM),             X        
               LRECL=0400,BLKSIZE=0                                             
*                                                                               
INCNT    DC    F'0'                                                             
OUTCNT   DC    F'0'                                                             
DUB      DS    D                                                                
DMCB     DS    6F                                                               
HALF     DS    H                                                                
FILEBK   DS    H                                                                
WORK     DS    CL17                                                             
TMP      DS    CL5                                                              
INFILE   DS    X                                                                
REL      DS    X                                                                
CHAR     DS    C                                                                
DESRECD  DS    C                                                                
AREC     DS    F                                                                
         DS    F                                                                
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
RECLENQ  EQU   5000                                                             
IOAREA   DS    CL(RECLENQ)                                                      
LASTREC  DS    CL(RECLENQ)                                                      
         EJECT                                                                  
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DEMITD                                                         
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'092DEWBSORT  07/18/11'                                      
         END                                                                    
