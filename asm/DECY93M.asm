*          DATA SET DECY93M    AT LEVEL 090 AS OF 07/24/08                      
*PHASE DECY93MA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE DMDMGR                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'SORT/COMBINE COUNTY/STATION/DMA DATA'                           
DECY93M  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DECY93M,VREGSAVE                                               
         GOTO1 =V(STXITER),DMCB,STXTAB                                          
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         LA    R4,IOAREA                                                        
         USING IODSCT,R4                                                        
         LA    R5,IOAREA                                                        
         USING SVDSCT,R5                                                        
         LA    R2,IODATA                                                        
         XC    KEYSAVE,KEYSAVE                                                  
         XC    KEY,KEY                                                          
         GOTO1 =V(SORTER),DMCB,SRTCRD,RECCRD                                    
************************************************************                    
READCTY  OPEN  (CTYIN,(INPUT))                                                  
         MVI   RECTYPE,C'C'                                                     
         B     GETCTY                                                           
*                                                                               
READSTA  CLOSE (CTYIN,)                                                         
         OPEN  (STAIN,(INPUT))                                                  
         MVI   RECTYPE,C'S'                                                     
         B     GETSTA                                                           
*                                                                               
READDMA  CLOSE (STAIN,)                                                         
         OPEN  (DMAIN,(INPUT))                                                  
         MVI   RECTYPE,C'D'                                                     
         B     GETDMA                                                           
*************************************************************                   
GETCTY   GET   CTYIN,IOACTLN                                                    
         BAS   RE,SORTREC                                                       
         B     GETCTY                                                           
*                                                                               
GETSTA   GET   STAIN,IOACTLN                                                    
         BAS   RE,SORTREC                                                       
         B     GETSTA                                                           
*                                                                               
GETDMA   GET   DMAIN,IOACTLN                                                    
         BAS   RE,SORTREC                                                       
         B     GETDMA                                                           
************************************************************                    
SORTREC  NTR1                                                                   
         CLI   0(R2),C'R'          KEEP ALL "R" RECORDS                         
         BE    SORT20                                                           
SORT10   CLI   RECTYPE,C'C'        M/M/S REC, KEEPING FIRST SET ONLY            
         BNE   SORTX                                                            
SORT20   DS    0H                                                               
         ICM   RE,3,IOACTLN                                                     
         AHI   RE,IOSRTLN                                                       
         STCM  RE,3,IOLEN                                                       
         MVC   IOKEY,0(R2)                                                      
         MVC   IORECTYP,RECTYPE                                                 
         GOTO1 =V(SORTER),DMCB,=C'PUT',IOAREA                                   
*                                                                               
SORTX    XIT1                                                                   
************************************************************                    
*&&DO                                                                           
PROCREC  DS    0H                                                               
         OPEN  (FILOUT,(OUTPUT))                                                
PROC10   DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    DONE                                                             
         L     R4,DMCB+4           ADDRESS OF SORTKEY+RECORD                    
*                                                                               
         LA    R2,IODATA           ADDRESS OF THE REAL RECORD                   
         USING DRKEY,R2                                                         
         CLI   DRCODE,DRCODEQU     ONLY MERGIN "R" RECORDS                      
         BE    PROC20                                                           
         PUT   FILOUT,IOACTLN                                                   
         B     PROC10                                                           
*                                                                               
PROC20   DS    0H                                                               
         MVC   KEY,0(R2)                                                        
         CLC   KEY,KEYSAVE         SAME AS LAST SET OF REC?                     
         BNE   PROC30              NO, START FRESH                              
         BAS   RE,EXTRACT          YES, EXTRACT THE SHARES                      
         CLI   IORECTYP,C'S'                                                    
         BNE   PROC10                                                           
         BAS   RE,MERGE            MERGE THE SHARES                             
         LA    R2,SVDATA                                                        
         LH    RE,DRRLEN                                                        
         AHI   RE,4                                                             
         STH   RE,SVACTLN                                                       
         PUT   FILOUT,SVACTLN      MERGED C/S/D REC'S, PUT AND NEXT             
         B     PROC10                                                           
*                                                                               
PROC30   MVC   KEYSAVE,KEY         FIRST TIME WITH NEW KEY                      
         MOVE  (SVDATA,2000),IODATA                                             
         XC    SHRTABS,SHRTABS     CLEAR THE SHARE TABS                         
         XC    SHRTABD,SHRTABD                                                  
         B     PROC10                                                           
*&&                                                                             
************************************************************                    
PROCREC  DS    0H                                                               
         OPEN  (FILOUT,(OUTPUT))                                                
PROC10   DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    DONE                                                             
         L     R4,DMCB+4           ADDRESS OF SORTKEY+RECORD                    
*                                                                               
         LA    R2,IODATA           ADDRESS OF THE REAL RECORD                   
         USING DRKEY,R2                                                         
         CLI   DRCODE,DRCODEQU     ONLY MERGIN "R" RECORDS                      
         BE    PROC20                                                           
         PUT   FILOUT,IOACTLN                                                   
         OC    KEYSAVE,KEYSAVE     IS THERE A SAVED REC?                        
         BZ    PROC10                                                           
         LA    R2,SVDATA           YES, PUT THAT OUT                            
         LH    RE,DRRLEN                                                        
         AHI   RE,4                                                             
         STH   RE,SVACTLN                                                       
         PUT   FILOUT,SVACTLN      MERGED C/S/D REC'S, PUT AND NEXT             
         XC    KEYSAVE,KEYSAVE                                                  
         B     PROC10                                                           
*                                                                               
PROC20   DS    0H                                                               
         MVC   KEY,0(R2)                                                        
         OC    KEYSAVE,KEYSAVE                                                  
         BZ    PROC33                                                           
         CLC   KEY,KEYSAVE         SAME AS LAST SET OF REC?                     
         BNE   PROC30              NO, START FRESH                              
         BAS   RE,EXTRACT          YES, EXTRACT THE SHARES                      
         B     PROC10                                                           
*                                                                               
PROC30   BAS   RE,MERGE            MERGE THE SHARES                             
         LA    R2,SVDATA                                                        
         LH    RE,DRRLEN                                                        
         AHI   RE,4                                                             
         STH   RE,SVACTLN                                                       
         PUT   FILOUT,SVACTLN      MERGED C/S/D REC'S, PUT AND NEXT             
*                                                                               
PROC33   MVC   KEYSAVE,KEY         FIRST TIME WITH NEW KEY                      
         MOVE  (SVDATA,2000),IODATA                                             
         XC    SHRTABS,SHRTABS     CLEAR THE SHARE TABS                         
         XC    SHRTABD,SHRTABD                                                  
         B     PROC10                                                           
************************************************************                    
*R0,R1,RF: UTILITY                                                              
*R2: IODATA                                                                     
*R6: ELEMENT                                                                    
*RE: POINTING TO SHRTABS OR SHRTABD                                             
************************************************************                    
EXTRACT  NTR1                                                                   
         LA    R2,IODATA                                                        
         LA    R6,DRFRSTEL                                                      
         MVI   ELCODE,X'41'                                                     
         BAS   RE,GETELEM                                                       
         CLI   0(R6),0                                                          
         BE    EXTRX                                                            
*                                                                               
         XC    SHRLEN,SHRLEN                                                    
         ZIC   R1,2(R6)                                                         
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         ST    R1,SHRLEN                                                        
         LA    R3,SHRINDEX         FIRST SHARE CELL                             
*                                                                               
         DS    0H                                                               
         CLI   IORECTYP,C'S'                                                    
         BNE   *+12                                                             
         LA    RE,SHRTABS          STATION REC SHARES                           
         B     *+8                                                              
         LA    RE,SHRTABD          DMA REC SHARES                               
*                                                                               
EXTR10   DS    0H                                                               
         XR    R0,R0                                                            
         L     R1,SHRLEN                                                        
         ZIC   RF,0(R3)            CELL INDEX                                   
         MR    R0,RF               X BY LENGTH                                  
         AHI   R1,3                ADD ELEM HEADER 3 BYTES                      
         ZIC   RF,1(R6)            ELEMENT LENGTH                               
         CR    RF,R1               DOES THE CELL EXIST?                         
         BL    EXTRX               NO,EXIT                                      
*                                                                               
         AR    R1,R6                                                            
         L     RF,SHRLEN                                                        
         SR    R1,RF               POING TO BEGINNING OF THE CELL               
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R1)       STORE IT                                     
         LA    RE,1(RF,RE)                                                      
         AHI   R3,1                NEXT INDEX                                   
         CLI   0(R3),X'FF'                                                      
         BNE   EXTR10                                                           
*                                                                               
EXTRX    XIT1                                                                   
************************************************************                    
MERGE    NTR1                                                                   
*                                                                               
MERGE01  OC    SHRTABS,SHRTABS                                                  
         BNZ   MERGE04                                                          
         OC    SHRTABD,SHRTABD                                                  
         BZ    MERGEX                                                           
*                                                                               
MERGE04  LA    R2,SVDATA                                                        
         LA    R6,DRFRSTEL                                                      
         MVI   ELCODE,X'41'                                                     
         BAS   RE,GETELEM                                                       
         CLI   0(R6),0                                                          
         BE    MERGEX                                                           
*                                                                               
         XC    SHRLEN,SHRLEN                                                    
         ZIC   R1,2(R6)                                                         
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         ST    R1,SHRLEN                                                        
         LA    R3,SHRINDEX                                                      
*                                                                               
         XC    ELEM,ELEM                                                        
         ZIC   RE,1(R6)                                                         
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
         MVI   ELEM+1,MAXELEN                                                   
*                                                                               
         LA    RE,ELEM                                                          
         ZIC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         LA    R1,SHRTABS                                                       
MERGE10  L     RF,SHRLEN                                                        
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R1)                                                    
         LA    RE,1(RF,RE)         NEXT SLOT                                    
         LA    R1,1(RF,R1)         NEXT SHARE                                   
         LA    R3,1(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   MERGE10                                                          
         LA    RF,SHRTABD                                                       
         CR    RF,R1                                                            
         BL    MERGE13             MERGED BOTH TABLES                           
         LA    R1,SHRTABD                                                       
         LA    R3,SHRINDEX                                                      
         B     MERGE10                                                          
*                                                                               
MERGE13  SHI   RE,1                ADJUST ELEMENT LENGTH FOR ZERO'S             
         LA    R1,6                                                             
MERGE15  CLI   0(RE),0                                                          
         BNE   MERGE20                                                          
         SHI   RE,1                                                             
         B     MERGE15                                                          
*                                                                               
MERGE20  LA    R1,ELEM                                                          
         SR    RE,R1                                                            
         AHI   RE,1                                                             
         STC   RE,ELEM+1                                                        
*                                                                               
*        GOTO1 =V(PUTEL),DMCB,SVDATA,ELEM                                       
         GOTO1 =V(HELLO),DMCB,(C'D',=C'DEMFIL  '),(X'41',SVDATA),0,0            
         MVC   DRRLEN,DMCB+14                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'DEMFIL  '),SVDATA,ELEM,0                 
         MVC   DRRLEN,DMCB+14                                                   
*        ZIC   RE,DMCB,3                                                        
*        STCM  RE,3,DRRLEN                                                      
*                                                                               
MERGEX   XIT1                                                                   
************************************************************                    
GETELEM  DS    0H                                                               
         GETEL R6,0,ELCODE                                                      
         BR    RE                                                               
         EJECT                                                                  
DONE     CLOSE FILOUT                                                           
         XBASE                                                                  
*                                                                               
         EJECT                                                                  
STXTAB   DS    0F                                                               
         DC    A(DECY93M)                                                       
         DC    V(PDUMPER)                                                       
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
VREGSAVE DC    V(REGSAVE)                                                       
         EJECT                                                                  
* ****************************************************************              
*                                                                               
SRTCRD   DC    CL80'SORT FIELDS=(4,26,A),FORMAT=BI,WORK=1 '                     
RECCRD   DC    CL80'RECORD TYPE=V,LENGTH=2034'                                  
*                                                                               
CTYIN    DCB   DDNAME=CTYIN,DSORG=PS,RECFM=VB,MACRF=(GM),              X        
               EODAD=READSTA,LRECL=2000,BLKSIZE=8200                            
STAIN    DCB   DDNAME=STAIN,DSORG=PS,RECFM=VB,MACRF=(GM),              X        
               EODAD=READDMA,LRECL=2000,BLKSIZE=8200                            
DMAIN    DCB   DDNAME=DMAIN,DSORG=PS,RECFM=VB,MACRF=(GM),              X        
               EODAD=PROCREC,LRECL=2000,BLKSIZE=8200                            
*                                                                               
FILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=VB,MACRF=(PM),             X        
               LRECL=2000,BLKSIZE=8200                                          
*                                                                               
INCNT    DC    F'0'                                                             
OUTCNT   DC    F'0'                                                             
RECTYPE  DS    CL1                                                              
NTIDIR   DC    CL8'NTIDIR'                                                      
COMMAND  DS    CL8                                                              
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    CL17                                                             
TMP      DS    CL5                                                              
INFILE   DS    X                                                                
REL      DS    X                                                                
CHAR     DS    C                                                                
ELCODE   DS    XL1                                                              
ELEM     DS    XL50                                                             
MAXELEN  EQU   29                                                               
MERGEFLG DS    XL1                                                              
AREC     DS    F                                                                
KEY      DS    CL20                                                             
KEYSAVE  DS    CL20                                                             
SHRLEN   DS    F                                                                
SHRINDEX DC    AL1(5)                                                           
         DC    AL1(9)                                                           
         DC    AL1(12)                                                          
         DC    X'FF'                                                            
SHRTABS  DS    XL10                                                             
SHRTABD  DS    XL10                                                             
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
RECLENQ  EQU   5000                                                             
IOAREA   DS    CL(RECLENQ)                                                      
IODSCT   DSECT                                                                  
IOLEN    DS    XL4                                                              
IOKEY    DS    XL20     LENGTH OF MAJOR KEY AND 2 BYTES OF MINOR                
IORECTYP DS    CL1                                                              
         DS    XL5      FOR THE HIGHDAY AND HIGH QH                             
IOSRTLN  EQU   *-IODSCT                                                         
IOACTLN  DS    XL4                                                              
IODATA   DS    0C                                                               
*                                                                               
SVDSCT   DSECT                                                                  
SVACTLN  DS    XL4                                                              
SVDATA   DS    0C                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DEMITD                                                         
       ++INCLUDE DEDEMFILE                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'090DECY93M   07/24/08'                                      
         END                                                                    
