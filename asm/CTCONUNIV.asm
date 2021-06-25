*          DATA SET CTCONUNIV  AT LEVEL 003 AS OF 04/22/09                      
*PHASE CTCONUNA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
***********************************************************************         
*                                                                     *         
* THIS PROGRAM READS CTFILE, AND DUMPS SOME DEMO FORMULA RECORDS TO   *         
* AN OUTPUT DATASET. THIS PROGRAM IS INTENDED TO RUN ONCE A YEAR, SO  *         
* THAT AFTER THE NEW UNIVERSES ARE ENTERED IN TST, WE CAN USE THIS    *         
* PROGRAM TO DUMP THEM TO TAPE. THEN WE USE PROGRAM CTADDDFORM TO ADD *         
* THE RECORDS TO THE LIVE CTFILE IN UPDATIVE MODE.                    *         
*                                                                     *         
* REQUIRED SYSIN CARD:                                                *         
* BOOK=YYWW (YEAR/WEEK, IN THE SAME FORMAT THAT IT WOULD BE ENTERED   *         
*            IN THE DEMO FORMULA MAINTENANCE SCREEN)                  *         
*                                                                     *         
***********************************************************************         
CTCONUNV CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,CTCONUNV,=V(REGSAVE)                                           
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'BOOK=',CARD      BOOK=YYWW                                    
         BE    *+6                                                              
         DC    H'0'                UNKNOWN PARAMETER CARD                       
         MVC   FULL,CARD+5         YYWW (YEAR/WEEK)                             
         NC    FULL,=C'0000'                                                    
         CLC   FULL,=C'0000'                                                    
         BE    *+6                                                              
         DC    H'0'                BOOK IS NOT NUMERIC                          
         CLI   CARD+9,C' '                                                      
         BE    *+6                                                              
         DC    H'0'                YYMM MUST BE FOLLOWED BY A BLANK             
*                                                                               
         MVC   DUB(2),CARD+5       YY (YEAR)                                    
         MVC   DUB+2(4),=C'0101'   ANY MONTH/DAY WILL FOOL DATCON               
         GOTO1 =V(DATCON),DMCB,(0,DUB),(3,FULL)                                 
         MVC   YEAR,FULL           SET YEAR                                     
         PACK  DUB,CARD+7(2)       GET NETWORK WEEK                             
         CVB   R0,DUB                                                           
         LTR   R0,R0               TEST 1 THRU 53                               
         BNZ   *+6                                                              
         DC    H'0'                WEEK CANNOT BE ZERO                          
         CHI   R0,53               HIGHEST ALLOWABLE WEEK                       
         BNH   *+6                                                              
         DC    H'0'                WEEK CANNOT EXCEED 53                        
         STC   R0,WEEK             SET WEEK                                     
         XC    BOOK,=X'FFFF'       FLIP BITS IN BOOK                            
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',IO                                                 
*                                                                               
         OPEN  (TOUT,OUTPUT)       OUTPUT DATASET FOR DUMPED RECORDS            
*                                                                               
         SR    R3,R3               COUNT TOTAL NUMBER OF OUTPUT RECS            
*                                                                               
         USING CTGREC,R6                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,CTGKTEQU        DEMO FORMULA ('G') RECORDS                   
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IO               R6 = A(RECORD)                               
         B     CHECKKEY                                                         
*                                                                               
READNEXT GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRSEQ'),=C'CTFILE',KEY,IO                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHECKKEY CLI   0(R6),CTGKTEQU      DEMO FORMULA RECORD?                         
         BNE   CLOSE               NO: WE'RE DONE                               
*                                                                               
*                                  CHECK FOR PNN                                
         CLI   CTGKFILE,C'P'       PAV?                                         
         BNE   CHECKCNN                                                         
         CLI   CTGKMED,C'N'        NETWORK?                                     
         BNE   CHECKCNN                                                         
         CLI   CTGKSRC,C'N'        NIELSEN?                                     
         BNE   CHECKCNN                                                         
         CLI   CTGKDEMO,C'U'       YES: MODIFIER U?                             
         BNE   READNEXT            NO: SKIP IT                                  
         B     BOOKFILT            YES: CONTINUE                                
*                                                                               
CHECKCNN DS    0H                                                               
*                                  CHECK FOR CNN                                
         CLI   CTGKFILE,C'C'       CABLE?                                       
         BNE   READNEXT                                                         
         CLI   CTGKMED,C'N'        NETWORK?                                     
         BNE   READNEXT                                                         
         CLI   CTGKSRC,C'N'        NIELSEN?                                     
         BNE   READNEXT                                                         
         CLI   CTGKDEMO,C'L'       YES: MODIFIER L?                             
         BNE   READNEXT            NO: SKIP IT                                  
*                                                                               
BOOKFILT DS    0H                                                               
         CLC   CTGKSTRT,BOOK       MATCH ON REQUESTED BOOK?                     
         BNE   READNEXT            NO: SKIP IT                                  
*                                                                               
         SR    RF,RF               COMPUTE RDW AND PUT THE RECORD               
         ICM   RF,3,CTGLEN                                                      
         AHI   RF,4                                                             
         STH   RF,RECLEN                                                        
         PUT   TOUT,RECLEN                                                      
         AHI   R3,1                INCREMENT TOTAL                              
*                                                                               
         CLI   CTGKFILE,C'P'       PRINTABLE FILE CODE                          
         BNE   *+14                                                             
         MVC   PFILE,=C'PAV'                                                    
         B     PRTMORE                                                          
         CLI   CTGKFILE,C'C'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PFILE,=C'CAB'                                                    
*                                                                               
PRTMORE  DS    0H                                                               
         MVC   PMEDIA,CTGKMED      MEDIA                                        
         MVC   PSOURCE,CTGKSRC     SOURCE                                       
         MVC   PMOD,CTGKDEMO       MODIFIER                                     
         EDIT  (B1,CTGKDEMO+1),PCAT,ALIGN=LEFT,ZERO=NOBLANK                     
         GOTO1 =V(PRINTER)                                                      
         DROP  R6                                                               
*                                                                               
         B     READNEXT            GET NEXT RECORD                              
                                                                                
CLOSE    DS    0H                                                               
         CLOSE TOUT                                                             
*                                                                               
         MVI   P,0                                                              
         GOTO1 =V(PRINTER)                                                      
         MVC   P(25),=C'TOTAL NUMBER OF RECORDS: '                              
         EDIT  (R3),(8,P+25),ALIGN=LEFT,ZERO=NOBLANK                            
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XBASE                                                                  
         LTORG                                                                  
         EJECT                                                                  
TOUT     DCB   DDNAME=TOUT,DSORG=PS,MACRF=(PM),                        X        
               RECFM=VB,BLKSIZE=8200,LRECL=2048,BUFNO=2                         
         SPACE 2                                                                
         DS    0D                                                               
         DC    CL8'***SSB**'                                                    
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         SPACE 3                                                                
         DC    CL8'***UTL**'                                                    
UTL      DC    F'0',X'0A'          FOR DATAMGR (CONTROL SYSTEM)                 
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
CARD     DS    CL80                                                             
WORK     DS    CL17                                                             
KEY      DS    CL25                                                             
BOOK     DS    0XL2                                                             
YEAR     DS    X                                                                
WEEK     DS    X                                                                
*                                                                               
DMCB     DS    6F                                                               
         DS    0D                                                               
         DC    C'***IO***'                                                      
RECLEN   DS    H                   REC LEN FOR QSAM PUT                         
         DC    H'0'                                                             
IO       DS    XL2000              IO AREA                                      
         PRINT OFF                                                              
         EJECT                                                                  
* ++INCLUDE CTGENFILE                                                           
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
* ++INCLUDE DDDPRINT                                                            
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
         ORG   P                                                                
PFILE    DS    CL3                 FILE                                         
         DS    C                                                                
PMEDIA   DS    C                   MEDIA                                        
         DS    C                                                                
PSOURCE  DS    C                   SOURCE                                       
         DS    C                                                                
PMOD     DS    C                   DEMO MODIFIER                                
PCAT     DS    CL3                 DEMO CATEGORY                                
         ORG                                                                    
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTCONUNIV 04/22/09'                                      
         END                                                                    
