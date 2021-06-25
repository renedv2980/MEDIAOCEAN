*          DATA SET CTLFM1B    AT LEVEL 010 AS OF 03/11/04                      
*PHASE TA021BA                                                                  
*INCLUDE SCINKEY                                                                
         TITLE 'CTLFM1B - CONTROL FILE MAINT - DEMO CODE RECORDS'               
CTLFM1B  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 100,**LFMY**                                                     
         USING WORKD,RC            RC=A(W/S)                                    
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(TEMP W/S)                               
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         L     R4,AREC                                                          
         USING CTNREC,R4           R4=A(RECORD)                                 
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
*                                                                               
KEYVAL   XC    CTNKEY,CTNKEY                                                    
         MVI   CTNKTYP,CTNKTEQU                                                 
*                                  VALIDATE FILE CODE                           
         GOTO1 AFVAL,DEMFILEH                                                   
         BZ    EXIT                                                             
         LA    RE,FILETAB                                                       
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
*                                  LOOK-UP CODE IN TABLE                        
KEYV2    CLI   0(RE),0                                                          
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),FLD                                                      
         BE    *+12                                                             
         LA    RE,L'FILETAB(RE)                                                 
         B     KEYV2                                                            
         MVC   CTNKFILE,8(RE)                                                   
         MVC   MEDVALS,9(RE)       SAVE VALID SUB-FILE CODES                    
         CLC   DEMFILE(8),0(RE)                                                 
         BE    *+14                                                             
         MVC   DEMFILE(8),0(RE)                                                 
         OI    DEMFILEH+6,X'80'                                                 
*                                  VALIDATE MEDIA (SUB-FILE) CODE               
         GOTO1 AFVAL,DEMSUBFH                                                   
         BZ    EXIT                                                             
         LA    RE,MEDTAB                                                        
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
*                                  LOOK-UP CODE IN TABLE                        
KEYV4    CLI   0(RE),0                                                          
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),FLD                                                      
         BE    *+12                                                             
         LA    RE,L'MEDTAB(RE)                                                  
         B     KEYV4                                                            
         MVC   CTNKMED,8(RE)                                                    
         CLC   DEMSUBF(8),0(RE)                                                 
         BE    *+14                                                             
         MVC   DEMSUBF(8),0(RE)                                                 
         OI    DEMSUBFH+6,X'80'                                                 
*                                  TEST IF VALID SUB-FILE FOR FILE              
         LA    RE,MEDVALS                                                       
*                                                                               
KEYV6    CLI   0(RE),0                                                          
         BE    EIIF                                                             
         CLC   0(1,RE),CTNKMED                                                  
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     KEYV6                                                            
*                                  VALIDATE AGENCY CODE                         
         MVC   CTNKAGY,=X'FFFF'                                                 
         GOTO1 AFVAL,DEMAGYH                                                    
         BZ    KEYV8                                                            
         CLI   FLDH+5,2                                                         
         BNE   EIIF                                                             
         MVC   CTNKAGY,FLD                                                      
*                                  VALIDATE LOOKUP CODE                         
KEYV8    MVI   CTNKCODE,X'FF'                                                   
         GOTO1 AFVAL,DEMCODEH                                                   
         BZ    *+10                                                             
         MVC   CTNKCODE,FLD                                                     
*                                                                               
         MVC   KEY,CTNKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         LA    R1,DEMFILEH                                                      
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
DISPREC  TWAXC DEMMODSH                                                         
         LA    R5,CTNDATA          R5=A(ELEMENT)                                
         LA    R1,SCANTBL                                                       
         ST    R1,DUB              SET VALUES FOR TABLE BUILD                   
         XC    DUB+4(4),DUB+4                                                   
*                                                                               
DISP2    CLI   0(R5),0             TEST E-O-R                                   
         BE    DISP8                                                            
         CLI   0(R5),X'02'         DEMO CODE/NAME ELEMENT                       
         BE    DISP6                                                            
*                                  BUMP TO NEXT ELEMENT                         
DISP4    ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DISP2                                                            
*                                  ADD AN ENTRY TO OUTPUT SCAN TABLE            
DISP6    LM    RE,RF,DUB                                                        
         MVI   0(RE),C' '                                                       
         MVC   1(7,RE),0(RE)                                                    
         MVC   0(1,RE),2(R5)                                                    
         MVI   1(RE),C'='                                                       
         MVC   2(5,RE),3(R5)                                                    
         LA    RE,8(RE)                                                         
         LA    RF,1(RF)                                                         
         STM   RE,RF,DUB                                                        
         B     DISP4                                                            
*                                  FORMAT SCAN TABLE INTO TWA                   
DISP8    L     R0,DUB+4                                                         
         GOTO1 =V(SCINKEY),DMCB,(3,DEMMODSH),(08,SCANTBL),(R0),RR=RB            
*                                                                               
         LA    R1,DEMMODSH                                                      
         ST    R1,FADR                                                          
         TM    CTNSTAT,X'80'                                                    
         BO    *+12                                                             
         MVI   NACTN,OKDEL+OKCHA                                                
         B     *+8                                                              
         MVI   NACTN,OKRES                                                      
         MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         B     EXIT                                                             
         EJECT                                                                  
*              ADD/CHANGE RECORD                                                
*                                                                               
DATAVAL  MVI   TEMP,0              BUILD VIRGIN RECORD & ACTIVITY ELEM.         
         GOTO1 ABLDREC                                                          
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                  VALIDATE CODES/NAMES                         
         LA    R6,DEMMODSH         R6=A(FIRST INPUT LINE)                       
         LA    R7,3                R7=N'INPUT FIELDS                            
         MVI   NLINES,0                                                         
*                                                                               
DATAV2   LR    R1,R6                                                            
         GOTO1 AFVAL                                                            
         BZ    DATAV14                                                          
         GOTO1 VSCANNER,DMCB,FLDH,(20,SCANTBL)                                  
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         MVC   NLINES,4(R1)        SAVE NUMBER OF LINES INPUT                   
         MVI   FNDX,1                                                           
         LA    R8,SCANTBL          R8=A(SCAN TABLE ENTRY)                       
*                                                                               
DATAV4   CLC   FNDX,NLINES                                                      
         BH    DATAV14                                                          
         CLI   0(R8),1             L'LHS                                        
         BNE   EIIF                                                             
         CLI   1(R8),1             L'RHS                                        
         BL    EIIF                                                             
         CLI   1(R8),5             L'RHS                                        
         BH    EFTL                                                             
*                                  BUILD DEMO CODE/NAME ELEMENT                 
         XC    TEMP,TEMP                                                        
         MVI   TEMP,X'02'                                                       
         MVI   TEMP+1,8                                                         
         MVC   TEMP+2(1),12(R8)                                                 
         MVC   TEMP+3(5),22(R8)                                                 
*                                  ENSURE NO DUPLICATE CODES INPUT              
         LA    R5,CTNDATA                                                       
DATAV6   CLI   0(R5),0                                                          
         BE    DATAV12                                                          
         CLI   0(R5),X'02'                                                      
         BE    DATAV10                                                          
DATAV8   ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DATAV6                                                           
DATAV10  CLC   2(1,R5),TEMP+2                                                   
         BE    EDIF                                                             
         B     DATAV8                                                           
*                                  ADD ELEMENT/BUMP TO NEXT SUBFIELD            
DATAV12  GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         ZIC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R8,L'SCANTBL(R8)                                                 
         B     DATAV4                                                           
*                                  BUMP TO NEXT INPUT FIELD                     
DATAV14  ZIC   R1,0(R6)                                                         
         AR    R6,R1                                                            
         TM    1(R6),X'20'         IGNORE PROTS                                 
         BO    DATAV14                                                          
         BCT   R7,DATAV2                                                        
         LA    R1,DEMMODSH                                                      
         ST    R1,FADR                                                          
         CLI   NLINES,0                                                         
         BE    EMIF                                                             
DATAVEND LA    R1,DEMFILEH                                                      
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
*                                  TABLE OF VALID FILES                         
FILETAB  DS    0CL16                                                            
         DC    C'TPT     ',C'T',C'TCPRW',2X'00'                                 
         DC    C'PAV     ',C'P',C'TN',5X'00'                                    
         DC    C'ESTIMATE',C'E',C'V',6X'00'                                     
         DC    C'INV     ',C'I',C'U',6X'00'                                     
         DC    C'DPT     ',C'T',C'D',6X'00'                                     
         DC    C'OPI     ',C'O',C'P',6X'00'                                     
         DC    C'COUNTY  ',C'C',C'U',6X'00'                                     
         DC    X'00'                                                            
*                                  TABLE OF VALID SUB-FILES                     
MEDTAB   DS    0CL9                                                             
         DC    C'USTV    ',C'T'                                                 
         DC    C'CANTV   ',C'C'                                                 
         DC    C'COVERAGE',C'U'                                                 
         DC    C'RADIO   ',C'R'                                                 
         DC    C'WEEKLY  ',C'W'                                                 
         DC    C'DPT     ',C'D'                                                 
         DC    C'NETWORK ',C'N'                                                 
         DC    C'MPA     ',C'P'                                                 
         DC    C'VPH     ',C'V'                                                 
         DC    C'UPGRADE ',C'U'                                                 
         DC    C'PROGRAM ',C'P'                                                 
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER TEMP W/S                                          
*                                                                               
WORKD    DSECT                                                                  
MEDVALS  DS    CL7                                                              
NLINES   DS    X                                                                
SCANTBL  DS    20CL32                                                           
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
* CTLFME4D                                                                      
       ++INCLUDE CTLFME4D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010CTLFM1B   03/11/04'                                      
         END                                                                    
