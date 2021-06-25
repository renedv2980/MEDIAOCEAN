*          DATA SET CTLFM08    AT LEVEL 007 AS OF 02/25/19                      
*PHASE TA0208A                                                                  
*INCLUDE SCINKEY                                                                
         TITLE 'CTLFM08 - CONTROL FILE MAINT - STATION RENAME LISTS'            
CTLFM08  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 010,**LFM8**                                                     
         USING WORKD,RC            RC=A(TEMP W/S)                               
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         L     R4,AREC                                                          
         USING CTSREC,R4           R4=A(RECORD)                                 
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
*                                                                               
KEYVAL   XC    CTSKEY,CTSKEY                                                    
         MVI   CTSKTYP,CTSKTEQU                                                 
*                                  VALIDATE MEDIA CODE                          
         GOTO1 AFVAL,STAMEDH                                                    
         BZ    EXIT                                                             
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         LA    RE,MEDTAB           LOOK-UP MEDIA CODE IN TABLE                  
KEYV2    CLI   0(RE),0                                                          
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(1,RE),FLD                                                      
         BE    *+12                                                             
         LA    RE,L'MEDTAB(RE)                                                  
         B     KEYV2                                                            
         MVC   MEDVALS,0(RE)       SAVE TABLE VALUES                            
         MVC   CTSKMED,MEDCODE                                                  
         CLC   STAMED,MEDNAME      OUTPUT FULL NAME                             
         BE    *+14                                                             
         MVC   STAMED,MEDNAME                                                   
         OI    STAMEDH+6,X'80'                                                  
*                                  VALIDATE SOURCE CODE                         
         GOTO1 AFVAL,STASRCH                                                    
         BZ    EXIT                                                             
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         LA    RE,SRCTAB           LOOK-UP SOURCE IN TABLE                      
KEYV4    CLI   0(RE),0                                                          
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),FLD                                                      
         BE    *+12                                                             
         LA    RE,L'SRCTAB(RE)                                                  
         B     KEYV4                                                            
         MVC   SRCVALS,0(RE)       SAVE TABLE VALUES                            
*                                                                               
         CLI   SRCCODE,C'C'        COMSCORE?                                    
         BNE   *+12                                                             
         CLI   MEDCODE,C'T'        MUST BE USTV                                 
         BNE   EIIF                                                             
*                                                                               
         MVC   CTSKSRC,SRCCODE                                                  
         CLC   STASRC,SRCNAME      OUTPUT FULL NAME                             
         BE    *+14                                                             
         MVC   STASRC,SRCNAME                                                   
         OI    STASRCH+6,X'80'                                                  
*                                  ENSURE MEDIA/SOURCE IS VALID                 
         LA    RE,SMTAB                                                         
KEYV4A   CLI   0(RE),0                                                          
         BE    EIIF                                                             
         CLC   CTSKSRC(2),0(RE)                                                 
         BE    *+12                                                             
         LA    RE,L'SMTAB(RE)                                                   
         B     KEYV4A                                                           
*                                  VALIDATE START BOOK (MMM/YY)                 
         GOTO1 AFVAL,STABOOKH                                                   
         BZ    EXIT                                                             
         GOTO1 VDATVAL,DMCB,(2,FLD),WORK                                        
         OC    0(4,R1),0(R1)       TEST IF DATE VALID                           
         BZ    EIDF                                                             
         GOTO1 VDATCON,DMCB,(0,WORK),(3,DUB),0                                  
         MVC   CTSKBOOK,DUB        MOVE BINARY YYMM INTO KEY                    
         XC    CTSKBOOK,=X'FFFF'                                                
*                                  CHECK ACTION SEQUENCE                        
         MVC   KEY,CTSKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         LA    R1,STAMEDH                                                       
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
DISPREC  TWAXC STAFLD1H            CLEAR DOWN TWA                               
         LA    R5,CTSDATA          R5=A(FIRST ELEMENT)                          
         L     R6,ATIA             R6=A(OUTPUT BLOCK)                           
         SR    R7,R7               R7=N'BLOCK ENTRIES                           
*                                                                               
DISP2    CLI   0(R5),0             TEST E-O-R                                   
         BE    DISP8                                                            
         CLI   0(R5),X'02'         DESCRIPTION ELEMENT                          
         BE    DISP6                                                            
DISP4    ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DISP2                                                            
*                                  ADD ENTRY TO OUTPUT BLOCK                    
DISP6    MVI   0(R6),C' '                                                       
         MVC   1(11,R6),0(R6)                                                   
         MVC   0(5,R6),2(R5)       FORMAT IS AAAAA-BBBBB                        
         LA    R1,4(R6)                                                         
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'-'                                                       
         MVC   2(5,R1),7(R5)                                                    
         LA    R6,12(R6)                                                        
         LA    R7,1(R7)            BUMP BLOCK COUNT                             
         B     DISP4                                                            
*                                  PUT OUTPUT BLOCK TO TWA                      
DISP8    L     R6,ATIA                                                          
         RELOC (RF)                                                             
         A     RF,=V(SCINKEY)                                                   
         GOTO1 (RF),DMCB,(15,STAFLD1H),(12,(R6)),(R7)                           
*                                  SET NEXT ACTION & EXIT                       
         TM    CTSSTAT,X'80'                                                    
         BO    *+12                                                             
         MVI   NACTN,OKDEL+OKCHA                                                
         B     *+8                                                              
         MVI   NACTN,OKRES                                                      
         LA    R1,STAFLD1H                                                      
         ST    R1,FADR                                                          
         MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         B     EXIT                                                             
         EJECT                                                                  
*              ADD/CHANGE RECORD                                                
*                                                                               
DATAVAL  MVI   TEMP,0              BUILD BASIC RECORD                           
         GOTO1 ABLDREC                                                          
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         ZAP   NUMELS,=P'0'                                                     
         LA    R7,STAFLD1H         R7=A(FIRST INPUT FIELD)                      
*                                                                               
DATAV2   CLI   0(R7),9             TEST E-O-T                                   
         BNH   DATAV8                                                           
         LR    R1,R7                                                            
         GOTO1 AFVAL                                                            
         BZ    DATAV6                                                           
         GOTO1 VSCANNER,DMCB,FLDH,ATIA,C',=,-'                                  
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         MVC   NLINES,4(R1)        SAVE NUMBER OF LINES INPUT                   
         MVI   FNDX,1                                                           
         L     R6,ATIA             R6=A(SCAN BLOCK ENTRY)                       
*                                                                               
DATAV4   CLC   0(1,R6),MEDMIN      TEST FIELD LENGTHS                           
         BL    EFTS                                                             
         CLC   0(1,R6),MEDMAX                                                   
         BH    EFTL                                                             
         CLC   1(1,R6),MEDMIN                                                   
         BL    EFTS                                                             
         CLC   1(1,R6),MEDMAX                                                   
         BH    EFTL                                                             
         MVC   TEMP(2),=X'020C'    BUILD AND ADD ELEMENT                        
         MVC   TEMP+2(5),12(R6)                                                 
         MVC   TEMP+7(5),22(R6)                                                 
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         AP    NUMELS,=P'1'                                                     
         LA    R6,32(R6)           BUMP TO NEXT SCAN BLOCK ENTRY                
         ZIC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         CLC   FNDX,NLINES                                                      
         BNH   DATAV4                                                           
*                                                                               
DATAV6   ZIC   R1,0(R7)            BUMP TO NEXT INPUT FIELD                     
         AR    R7,R1                                                            
         TM    1(R7),X'20'         SKIP PROTS                                   
         BO    DATAV6                                                           
         B     DATAV2                                                           
*                                  ADD/WRITE RECORD                             
DATAV8   LA    R1,STAFLD1H                                                      
         ST    R1,FADR                                                          
         MVI   FNDX,0                                                           
         CP    NUMELS,=P'0'        ERROR IF NO DATA INPUT                       
         BE    EMIF                                                             
         LA    R1,STAMEDH                                                       
         ST    R1,FADR                                                          
         MVI   FERN,X'FF'                                                       
         L     RF,AADD                                                          
         CLI   ACTN,ADD                                                         
         BE    *+8                                                              
         L     RF,AWRITE                                                        
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVI   NACTN,OKDEL+OKCHA                                                
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
*                                  TABLE OF VALID MEDIAS                        
MEDTAB   DS    0CL11                                                            
         DC    C'USTV    ',C'T',AL1(4,5)                                        
         DC    C'CANTV   ',C'C',AL1(4,5)                                        
         DC    C'MPA     ',C'P',AL1(4,5)                                        
         DC    C'NETWORK ',C'N',AL1(4,5)                                        
         DC    C'RADIO   ',C'R',AL1(4,5)                                        
         DC    X'00'                                                            
*                                  TABLE OF VALID SOURCES                       
SRCTAB   DS    0CL9                                                             
         DC    C'NSI     ',C'N'                                                 
         DC    C'ARB     ',C'A'                                                 
         DC    C'SRC     ',C'S'                                                 
         DC    C'COM     ',C'C'                                                 
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
         DC    C'NN'                                                            
         DC    C'NP'                                                            
         DC    C'CT'               USTV/COMSCORE                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER TEMP W/S                                          
*                                                                               
WORKD    DSECT                                                                  
MEDVALS  DS    0CL11                                                            
MEDNAME  DS    CL8                 MEDIA NAME                                   
MEDCODE  DS    CL1                 MEDIA CODE                                   
MEDMIN   DS    X                   MIN STATION LENGTH                           
MEDMAX   DS    X                   MAX STATION LENGTH                           
SRCVALS  DS    0CL9                                                             
SRCNAME  DS    CL8                 SOURCE NAME                                  
SRCCODE  DS    CL1                 SOURCE CODE                                  
NUMELS   DS    PL2                                                              
NLINES   DS    X                                                                
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
* CTLFMF7D                                                                      
       ++INCLUDE CTLFMF7D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007CTLFM08   02/25/19'                                      
         END                                                                    
