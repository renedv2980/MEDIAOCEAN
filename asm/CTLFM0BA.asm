*          DATA SET CTLFM0BA   AT LEVEL 021 AS OF 05/01/02                      
*PHASE TA020BA,+0                                                               
         TITLE 'CTLFM0B - CONTROL FILE MAINT - LATEST DEMO BOOK'                
CTLFM0B  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFMB**                                                       
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         L     R4,AREC                                                          
         USING CTBREC,R4           R4=A(RECORD)                                 
         EJECT                                                                  
*              BUILD KEY & VALIDATE PASSWORD                                    
*                                                                               
KEYVAL   XC    CTBKEY,CTBKEY                                                    
         MVI   CTBKTYP,C'B'                                                     
*                                                                               
         CLI   ACTN,CHANGE         IF ACTION IS CHANGE                          
         BNE   KEYV2                                                            
         GOTO1 AFVAL,DEMPASSH      PASSWORK MUST BE INPUT                       
         BZ    EXIT                                                             
         CLI   FLDH+5,6                                                         
         BNE   EIIF                                                             
*        GOTO1 ABLDACT             GET C'YYMMDD' IN DUB                         
         GOTO1 VDATCON,DMCB,(5,0),(20,DUB)     YYYYMMDD                         
         LA    R1,DUB+7                                                         
         LA    RE,DUB1                                                          
         LA    RF,6                                                             
         MVC   0(1,RE),0(R1)       REVERSE DATE INTO DUB1                       
         LA    RE,1(RE)                                                         
         BCTR  R1,0                                                             
         BCT   RF,*-12                                                          
         PACK  DUB,DUB1(6)                                                      
         ZAP   DUB1,=P'999999'                                                  
         SP    DUB1,DUB            AND GET 9'S COMPLEMENT                       
         UNPK  DUB(6),DUB1                                                      
         OI    DUB+5,X'F0'                                                      
         CLC   DUB(6),FLD          TEST IF PASSWORD VALID                       
         BNE   EIIF                                                             
*                                                                               
KEYV2    MVC   KEY,CTBKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         LA    R1,DEMPASSH                                                      
         ST    R1,FADR                                                          
*                                                                               
         CLI   ACTN,CHANGE         IF ACTN=CHANGE AND KEY NEQ LKEY              
         BNE   *+18                SET ACTION TO DISPLAY                        
         CLC   KEY,LKEY                                                         
         BE    *+8                                                              
         MVI   ACTN,DISPLAY                                                     
         CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU FOR UPDATABLE ACTIONS                
         GOTO1 AREAD                                                            
         BZ    EXIT                                                             
*                                                                               
         TM    DMCB+8,X'10'        CHECK FOR NOT FOUND                          
         BZ    *+16                                                             
         CLI   ACTN,ADD            ONLY VALID FOR ADD                           
         BE    DATAVAL                                                          
         B     ERNF                                                             
         CLI   ACTN,ADD            FOUND NOT VALID FOR ADD                      
         BE    ERAE                                                             
         TM    DMCB+8,X'02'        DELETED RECORDS MAY ONLY BE DSPLYD           
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY                                                     
         BNE   ERNF                                                             
         CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
         B     DISPREC                                                          
         EJECT                                                                  
*              DISPLAY RECORD                                                   
*                                                                               
DISPREC  TWAXC DEMLIN1H                                                         
         LA    R5,CTBDATA          R5=A(FIRST ELEMENT)                          
         LA    R6,DEMLIN1H         R6=A(DISPLAY LINE)                           
         USING LINED,R6                                                         
*                                  LOOP DOWN RECORD                             
DISP2    CLI   0(R5),0                                                          
         BE    DISPEND                                                          
         CLI   0(R5),CTBKELEQ                                                   
         BE    DISP6                                                            
*                                                                               
DISP4    ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DISP2                                                            
*                                  DISPLAY LATEST BOOK ELEMENT                  
         USING CTBKD,R5                                                         
DISP6    LA    RE,SRCTAB                                                        
DISP6A   CLI   0(RE),0             SOURCE                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   CTBKSRC,0(RE)                                                    
         BE    *+12                                                             
         LA    RE,L'SRCTAB(RE)                                                  
         B     DISP6A                                                           
         CLI   0(RE),C'M'          DIFFERRENTIATE BBR FROM MEDF TV              
         BNE   DISP6AA                                                          
         CLI   CTBKMED,C'R'        IF MEDIA=RADIO THEN SRC=BBR                  
         BE    DISP6AA                                                          
         MVC   LINSRC,=CL8'MFX'                                                 
         B     *+10                                                             
DISP6AA  MVC   LINSRC,1(RE)                                                     
         LA    RE,MEDTAB                                                        
DISP6B   CLI   0(RE),0             MEDIA                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   CTBKMED,0(RE)                                                    
         BE    *+12                                                             
         LA    RE,L'MEDTAB(RE)                                                  
         B     DISP6B                                                           
         MVC   LINMED,1(RE)                                                     
         CLC   LINSRC,=CL8'TVQ'                                                 
         BNE   *+18                                                             
         CLI   CTBKMED,C'C'                                                     
         BNE   *+10                                                             
         MVC   LINMED,=CL8'CABLE'                                               
*                                  LATEST BOOK                                  
         GOTO1 VDATCON,DMCB,(3,CTBKYM),(9,LINBOOK)                              
         LA    R6,LINLEN(R6)                                                    
         B     DISP4                                                            
         DROP  R5                                                               
*                                  SET NEXT ACTION & EXIT                       
DISPEND  MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         LA    R1,DEMPASSH                                                      
         ST    R1,FADR                                                          
         CLI   DDS,0                                                            
         BE    *+8                                                              
         MVI   NACTN,OKCHA                                                      
         B     EXIT                                                             
         EJECT                                                                  
*              ADD/WRITE RECORD                                                 
*                                                                               
DATAVAL  MVI   TEMP,0              BUILD BASIC RECORD                           
         GOTO1 ABLDREC                                                          
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
         LA    R6,DEMLIN1H         R6=A(INPUT LINE)                             
         USING LINED,R6                                                         
         LA    R5,TEMP             R5=A(LATEST BOOK ELEMENT)                    
         USING CTBKD,R5                                                         
*                                                                               
DATAV2   CLI   LINSRCH,9           TEST E-O-T                                   
         BNH   DATAVEND                                                         
*                                  TEST SOURCE INPUT                            
         GOTO1 AFVAL,LINSRCH                                                    
         BNZ   DATAV4                                                           
         CLI   LINMEDH+5,0         NO - MEDIA & BOOK MUST NOT BE ALSO           
         BNE   EXIT                                                             
         CLI   LINBOOKH+5,0                                                     
         BNE   EXIT                                                             
         B     DATAV10                                                          
*                                  VALIDATE SOURCE                              
DATAV4   ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         LA    RE,SRCTAB                                                        
DATAV6   CLI   0(RE),0                                                          
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   1(0,RE),FLD                                                      
         BE    *+12                                                             
         LA    RE,L'SRCTAB(RE)                                                  
         B     DATAV6                                                           
         CLC   LINSRC,1(RE)                                                     
         BE    *+14                                                             
         MVC   LINSRC,1(RE)                                                     
         OI    LINSRCH+6,X'80'                                                  
         MVC   CTBKSRC,0(RE)                                                    
*                                  VALIDATE MEDIA                               
         GOTO1 AFVAL,LINMEDH                                                    
         BZ    EXIT                                                             
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         LA    RE,MEDTAB                                                        
DATAV8   CLI   0(RE),0                                                          
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   1(0,RE),FLD                                                      
         BE    *+12                                                             
         LA    RE,L'SRCTAB(RE)                                                  
         B     DATAV8                                                           
         CLC   LINMED,1(RE)                                                     
         BE    *+14                                                             
         MVC   LINMED,1(RE)        DISPLAY FULL NAME                            
         OI    LINMEDH+6,X'80'                                                  
         MVC   CTBKMED,0(RE)                                                    
*                                  ENSURE SOURCE/MEDIA IS VALID                 
         LA    RE,SMTAB                                                         
DATAV9   CLI   0(RE),0                                                          
         BE    EIIF                                                             
         CLC   CTBKSRC(2),0(RE)                                                 
         BE    *+12                                                             
         LA    RE,L'SMTAB(RE)                                                   
         B     DATAV9                                                           
*                                  VALIDATE LATEST BOOK                         
         GOTO1 AFVAL,LINBOOKH                                                   
         BZ    EXIT                                                             
         GOTO1 VDATVAL,DMCB,(2,FLD),WORK                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    EXIT                                                             
         GOTO1 VDATCON,DMCB,(0,WORK),(3,CTBKYM),0                               
*&&DO                                                                           
         PACK  DUB,WORK(2)         CONVERT TO BINARY YYMM                       
         CVB   R1,DUB                                                           
         STC   R1,CTBKYM                                                        
         PACK  DUB,WORK+2(2)                                                    
         CVB   R1,DUB                                                           
         STC   R1,CTBKYM+1                                                      
*&&                                                                             
         CLC   CTBKYM,CTBDATA+2                                                 
         BH    EIIF                                                             
         MVI   CTBKEL,CTBKELEQ                                                  
         MVI   CTBKLN,CTBKLNEQ                                                  
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                                                               
DATAV10  LA    R6,LINLEN(R6)       BUMP TO NEXT TWA LINE                        
         B     DATAV2                                                           
*                                  SET NEXT ACTION & EXIT                       
DATAVEND LA    R1,DEMPASSH                                                      
         ST    R1,FADR                                                          
         MVI   FERN,X'FF'                                                       
         L     RF,AADD                                                          
         CLI   ACTN,ADD                                                         
         BE    *+8                                                              
         L     RF,AWRITE                                                        
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVI   NACTN,OKCHA                                                      
         LA    R1,BASACTNH         SET CURSOR & EXIT                            
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
*              LITERALS ETC.                                                    
*                                                                               
         LTORG                                                                  
*                                  TABLE OF VALID SOURCE CODES                  
SRCTAB   DS    0CL9                                                             
         DC    C'A',CL8'ARB'                                                    
         DC    C'N',CL8'NSI'                                                    
         DC    C'M',CL8'BBR'                                                    
         DC    C'S',CL8'SRC'                                                    
         DC    C'M',CL8'MFX'       <--MUST APPEAR AFTER BBR                     
         DC    C'Q',CL8'TVQ'                                                    
         DC    C'R',CL8'RAR'                                                    
         DC    X'00'                                                            
*                                  TABLE OF VALID MEDIA CODES                   
MEDTAB   DS    0CL9                                                             
         DC    C'T',CL8'USTV'                                                   
         DC    C'C',CL8'CANTV'                                                  
         DC    C'C',CL8'CABLE'                                                  
         DC    C'R',CL8'RADIO'                                                  
         DC    C'D',CL8'DPT'                                                    
         DC    C'P',CL8'MPA'                                                    
         DC    C'N',CL8'NETWORK'                                                
         DC    C'U',CL8'UPGRADE'                                                
         DC    C'W',CL8'WEEKLY'                                                 
         DC    X'00'                                                            
*                                  TABLE OF VALID SOURCE/MEDIA COMBOS           
SMTAB    DS    0CL2                                                             
         DC    C'NT'                                                            
         DC    C'AT'                                                            
         DC    C'ST'                                                            
         DC    C'NC'                                                            
         DC    C'AC'                                                            
         DC    C'NR'                                                            
         DC    C'AR'                                                            
         DC    C'RR'               RADAR NETWORK RADIO                          
         DC    C'MR'                                                            
         DC    C'ND'                                                            
         DC    C'NN'                                                            
         DC    C'NP'                                                            
         DC    C'AU'                                                            
         DC    C'NU'                                                            
         DC    C'NW'                                                            
         DC    C'MT'                                                            
         DC    C'QN'               TVQ NETWORK FILE                             
         DC    C'QC'               TVQ CABLE FILE                               
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER INPUT LINE                                        
*                                                                               
LINED    DSECT                                                                  
LINSRCH  DS    CL8                                                              
LINSRC   DS    CL8                                                              
LINMEDH  DS    CL8                                                              
LINMED   DS    CL8                                                              
LINBOOKH DS    CL8                                                              
LINBOOK  DS    CL6                                                              
LINLEN   EQU   *-LINED                                                          
         SPACE 1                                                                
* CTLFMACTNS                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTLFMACTNS                                                     
         PRINT ON                                                               
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMF4D                                                                      
       ++INCLUDE CTLFMF4D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021CTLFM0BA  05/01/02'                                      
         END                                                                    
