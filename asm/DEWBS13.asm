*          DATA SET DEWBS13    AT LEVEL 135 AS OF 11/14/13                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEWBS13A                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE NETWEEK                                                                
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
         TITLE 'SORT TIME WARNER AND MAIN TCAR DATA'                            
***********************************************************************         
* THIS VERSION REPLACES DEWB1SR, ONLY FOR THE COMMERCIAL WARNER DATA,           
* BEGINNING WITH DATA FOR 8/27/12.                                              
* THE CHANGES ARE TO ALLOW THE AA DATA TO BE PASSED ON TO THE                   
* CONVERSION, INSTEAD OF THE GAA DATA.  NIELSEN STOPPED SENDING THE             
* GAA DATA BEGINNING WITH THE 8/27/12 BOOK AND WE ARE NOW USING THE AA          
* DATA INSTEAD.                                                                 
***********************************************************************         
DEWBS13  CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         NBASE 0,DEWBS13,=V(REGSAVE)                                            
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCRD,(X'80',RECCRD),(X'80',0)                 
         GOTO1 =V(DYNALLOC),DMCB,(C'A',=C'FILIN1  ')                            
         CLI   DMCB+4,0            IS FILIN1 ALLOCATED VIA DD STMT?             
         BE    DO_NFS              NO                                           
         MVI   HAVEDATA,C'Y'       YES: WE HAVE SOMETHING TO CONVERT            
         MVI   USINGNFS,C'N'       WE WILL READ VIA NFS                         
         B     OPENFILS                                                         
*                                                                               
* IF WE GET HERE, THEN THERE IS NO FILIN1 DD STATEMENT IN THE JCL.              
* THAT MEANS THAT WE WILL READ ALL THE FILES IN AN NFS-MOUNTED FOLDER           
* TO GET THE DATA. THE FILENAMES IN THE FOLDER WILL BE IN THE DATASET           
* WITH DDNAME NFSFILES, AND IS PRODUCED BY THE "DDNFSNAMES" PROGRAM             
* (WHICH MUST RUN PRIOR TO THIS PROGRAM).                                       
*                                                                               
* FOR EACH FILE IN THE NFS-MOUNTED FOLDER, WE NEED TO:                          
*  1. DYNAMICALLY ALLOCATE THE FILE                                             
*  2. OPEN IT                                                                   
*  3. READ ITS CONTENTS                                                         
*  4. CLOSE IT                                                                  
*  5. DEALLOCATE THE DDNAME                                                     
*                                                                               
DO_NFS   DS    0H                                                               
         MVI   HAVEDATA,C'N'       ASSUME MOUNTED FOLDER IS EMPTY               
         OPEN  NFSFILES            CONTAINS FILENAMES IN MOUNTED FOLDER         
*                                                                               
FILELOOP DS    0H                                                               
         GET   NFSFILES,IOAREA     GET THE FIRST FILENAME                       
         SR    R2,R2                                                            
         ICM   R2,3,IOAREA         RECLEN (FROM RDW)                            
         SHI   R2,4                L'RDW                                        
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'H',=C'FILIN1  '),((R2),IOAREA+4),  +        
               (X'50',0)                                                        
         TM    DMCB+8,X'20'        DYNAMIC ALLOCATION FAILED?                   
         BZ    *+6                                                              
         DC    H'0'                YES                                          
         MVI   HAVEDATA,C'Y'       WE HAVE AT LEAST ONE FILE TO CONVERT         
*                                                                               
OPENFILS DS    0H                                                               
         CLI   HAVEDATA,C'Y'       IS THERE ANY DATA TO CONVERT?                
         BNE   XBASE               NO                                           
*                                                                               
         OPEN  (FILIN1,(INPUT))                                                 
*                                                                               
SORT10   GET   FILIN1,IOAREA+1     TARGET OR 1ST MKT                            
*                                                                               
*CATCH UNMARKED CORRECTIONS.                                                    
*NIELSEN IS SOMETIMES SENDING DATA THAT APPLIES TO PREVIOUS WEEKS BUT           
*IS NOT MARKED AS CORRECTIONS. THIS CODE WILL CHECK TO SEE IF DATA              
*BELONGS TO A PREVIOUS WEEK, AND WILL TURN ON THE CORRECTION FLAG IF            
*THAT IS THE CASE.                                                              
         LA    R2,IOAREA+1                                                      
         USING MITD,R2                                                          
*                                                                               
         MVC   IOAREA(1),MITVCR                                                 
*                                                                               
         CLC   MITSEQ,=C'00'       DESCRIPTIVE RECORD. SAVE FILE BOOK.          
         BNE   SORT12                                                           
         GOTO1 =V(NETWEEK),DMCB,MITEND,V(GETDAY),V(ADDAY)                       
         MVC   FILEBK,4(R1)        YEAR                                         
         MVC   FILEBK+1(1),8(R1)   WEEK                                         
*                                                                               
         CLC   FILEBK,=AL1(YR_2012,WEEK_36)                                     
         BNL   *+6                                                              
         DC    H'0'                FOR DATA PRIOR TO 8/27/12,                   
*                                  USE DEWB1SR                                  
*                                                                               
         CLC   MI0FILE(3),=C'TPA'                                               
         BE    SORT10                                                           
         MVC   MI0FILE(L'MI0FILE-3),MI0FILE+3                                   
         CLI   MI0FILE+14,C'1'                                                  
         BNE   *+8                                                              
         MVI   IOAREA,C'6'                                                      
         CLI   MI0FILE+14,C'2'                                                  
         BNE   *+8                                                              
         MVI   IOAREA,C'8'                                                      
         CLI   MI0FILE+14,C'3'                                                  
         BNE   *+8                                                              
         MVI   IOAREA,C'A'                                                      
         CLI   MI0FILE+14,C'7'                                                  
         BNE   *+8                                                              
         MVI   IOAREA,C'4'                                                      
         CLI   MI0FILE+14,C'S'                                                  
         BNE   *+8                                                              
         MVI   IOAREA,C'2'                                                      
         CLI   MI0FILE+14,C' '                                                  
         BNE   *+8                                                              
         MVI   IOAREA,C' '                                                      
         B     SORT15                                                           
*                                                                               
SORT12   CLC   MITAUDTY,=C'4'                                                   
         BE    *+14                                                             
         CLC   MITAUDTY,=C'6'                                                   
         BNE   SORT10                                                           
*                                                                               
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
*                                                                               
SORT15   GOTO1 =V(SORTER),DMCB,=C'PUT',IOAREA                                   
         B     SORT10                                                           
*                                                                               
SORT20   CLOSE (FILIN1,)                                                        
*                                                                               
         DROP  R2                   CORRECTION. FLAG AS CORRECTION.             
         EJECT                                                                  
***********************************************************************         
*                                                                               
         OPEN  (FILOUT,(OUTPUT))                                                
*                                                                               
SORT40   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,DMCB+4        A(SORT RECORD)                               
         BZ    DONE                TEST EOF                                     
*                                                                               
         LA    R2,1(R2)                                                         
         PUT   FILOUT,(R2)                                                      
         L     RE,OUTCNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,OUTCNT                                                        
         B     SORT40                                                           
*                                                                               
DONE     GOTO1 =V(PRINTER)         BLANK LINE                                   
         MVC   P(23),=C'TOTAL RECORDS OUTPUT = '                                
         EDIT  OUTCNT,(8,P+25),ZERO=NOBLANK                                     
         GOTO1 =V(PRINTER)                                                      
         B     DONE10                                                           
*                                                                               
EOFIN1   DS    0H                                                               
         CLOSE (FILIN1,)                                                        
*                                                                               
         FREEPOOL FILIN1                                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(DYNALLOC),DMCB,(C'U',=C'FILIN1  ')                            
         OC    DMCB+4(4),DMCB+4    SUCCESSFUL DYNAMIC UNALLOCATION?             
         BZ    *+6                                                              
         DC    H'0'                NO                                           
*                                                                               
         CLI   USINGNFS,C'Y'                                                    
         BE    FILELOOP                                                         
*                                                                               
         DS    0H                                                               
DONE10   CLI   HAVEDATA,C'Y'                                                    
         BNE   CLOSNAMS                                                         
         CLOSE (FILOUT)                                                         
*                                                                               
CLOSNAMS DS    0H                                                               
         CLI   USINGNFS,C'Y'                                                    
         BNE   XBASE                                                            
         CLOSE NFSFILES                                                         
*                                                                               
XBASE    XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
*                                                                               
SORTCRD  DC    CL80'SORT FIELDS=(1,106,A),FORMAT=BI '                           
RECCRD   DC    CL80'RECORD TYPE=F,LENGTH=401'                                   
         EJECT                                                                  
FILIN1   DCB   DDNAME=FILIN1,DSORG=PS,RECFM=FB,MACRF=(GM),             X        
               EODAD=EOFIN1,LRECL=0400,BLKSIZE=0                                
FILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=FB,MACRF=(PM),             X        
               LRECL=0400,BLKSIZE=16800                                         
*                                                                               
* DCB FOR DATASET WITH FILENAMES IN NFS-MOUNTED FOLDER                          
NFSFILES DCB   DDNAME=NFSFILES,DSORG=PS,MACRF=GM,LRECL=256,RECFM=VB,   X        
               EODAD=SORT20                                                     
*                                                                               
UTL      DS    0D                                                               
         DC    4X'00',X'0C'        UTL FOR DEMOS SYSTEM                         
*                                                                               
         DS    0D                                                               
         DC    CL8'**SSB***'                                                    
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSOMTIND            SYSTEM DATAMGR FLAGS                         
         DC    AL1(SSOWRTN)        WRITE=NO (DON'T OPEN FOR UPDATE)             
         ORG                                                                    
         SPACE 3                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
OUTCNT   DC    F'0'                                                             
HALF     DS    H                                                                
FILEBK   DS    H                                                                
WORK     DS    CL17                                                             
HAVEDATA DS    C                                                                
USINGNFS DC    C'Y'                ASSUME DATA WILL BE READ VIA NFS             
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
RECLENQ  EQU   5000                                                             
IOAREA   DS    CL(RECLENQ)                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DEMITD                                                         
         EJECT                                                                  
       ++INCLUDE DDMONYREQU                                                     
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'135DEWBS13   11/14/13'                                      
         END                                                                    
