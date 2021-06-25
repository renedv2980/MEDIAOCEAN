*          DATA SET CTLFM0E    AT LEVEL 008 AS OF 05/01/02                      
*PHASE TA020EA,+0                                                               
         TITLE 'CTLFM0E - CONTROL FILE MAINT - DEMO NAME RECORDS'               
CTLFM0E  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFME**                                                       
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         L     R4,AREC                                                          
         USING CTDREC,R4           R4=A(RECORD)                                 
         EJECT                                                                  
*              VALIDATE KEY FIELDS & READ RECORD                                
*                                                                               
KEYVAL   XC    CTDKEY,CTDKEY                                                    
         MVI   CTDKTYP,CTDKTEQU                                                 
*                                  VALIDATE FILE TYPE                           
         GOTO1 AFVAL,DEMFILEH                                                   
         BZ    EXIT                                                             
         ZIC   R1,FLDH+5           LOOK-UP FIELD IN TABLE                       
         BCTR  R1,0                                                             
         LA    RE,FILETAB                                                       
KEYV2    CLI   0(RE),0                                                          
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   1(0,RE),FLD                                                      
         BE    *+12                                                             
         LA    RE,L'FILETAB(RE)                                                 
         B     KEYV2                                                            
         CLC   FLD(L'DEMFILE),1(RE)                                             
         BE    *+14                                                             
         MVC   DEMFILE,1(RE)       DISPLAY FULL NAME                            
         OI    DEMFILEH+6,X'80'                                                 
         MVC   CTDKFILE,0(RE)                                                   
*                                  VALIDATE MEDIA CODE                          
         GOTO1 AFVAL,DEMSUBFH                                                   
         BZ    EXIT                                                             
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         LA    RE,MEDTAB                                                        
KEYV3    CLI   0(RE),0             TEST E-O-T                                   
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   1(0,RE),FLD                                                      
         BE    *+12                                                             
         LA    RE,L'MEDTAB(RE)                                                  
         B     KEYV3                                                            
         CLC   DEMSUBF,1(RE)                                                    
         BE    *+14                                                             
         MVC   DEMSUBF,1(RE)                                                    
         OI    DEMSUBFH+6,X'80'                                                 
         MVC   CTDKMED,0(RE)                                                    
*                                  ENSURE FILE/MEDIA VALID                      
         LA    RE,FMTAB                                                         
KEYV3A   CLI   0(RE),0                                                          
         BE    EIIF                                                             
         CLC   CTDKFILE(2),0(RE)                                                
         BE    *+12                                                             
         LA    RE,L'FMTAB(RE)                                                   
         B     KEYV3A                                                           
*                                  VALIDATE AGENCY                              
         MVC   CTDKAGY,=X'FFFF'                                                 
         GOTO1 AFVAL,DEMAGYH                                                    
         BZ    KEYV6                                                            
         CLI   DDS,0               TEST IF A USER TERMINAL                      
         BNE   KEYV4                                                            
         MVC   FLD,TWAAGY-TWAD(R2) YES - PRESET AGENCY CODE                     
         MVC   DEMAGY,FLD                                                       
         OI    DEMAGYH+6,X'80'                                                  
KEYV4    MVC   CTDKAGY,FLD                                                      
         B     KEYV8                                                            
KEYV6    CLI   DDS,0               TEST IF A USER TERMINAL                      
         BNE   KEYV8                                                            
         CLI   ACTN,DISPLAY        YES - CAN ONLY DISPLAY DEFAULTS              
         BNE   EIAC                                                             
*                                  VALIDATE LOOKUP CODE                         
KEYV8    MVI   CTDKCODE,X'FF'                                                   
         GOTO1 AFVAL,DEMCODEH                                                   
         BZ    KEYV10                                                           
         ZIC   RF,DEMCODEH+5       L'LOOKUP FIELD                               
         BCTR  RF,0                                                             
         LA    RE,LKUPTAB          RE=A(LOOK UP TABLE)                          
KEYV8A   CLI   0(RE),X'00'         END OF TABLE--INPUT NOT FOUND                
         BE    EIIF                                                             
         TM    FLDH+4,X'08'        NUMERIC INPUT?                               
         BZ    *+18                NO, GO TEST CHAR INPUT                       
         CLC   FLDH+3(1),0(RE)     COMPARE NUMBER                               
         BE    KEYV8C                                                           
         B     KEYV8B              NOT FOUND, TRY NEXT TBL ENTRY                
         EXCLC RF,FLD,1(RE)        COMPARE LOOKUP NAME                          
         BE    KEYV8C                                                           
KEYV8B   LA    RE,L'LKUPTAB(RE)    NEXT ENTRY IN LOOK UP TBL                    
         B     KEYV8A                                                           
*                                                                               
KEYV8C   MVC   CTDKCODE,0(RE)      FOUND, SAVE LOOK UP CODE                     
         XC    DEMCODE,DEMCODE                                                  
         MVC   DEMCODE(L'DEMCODE),1(RE)   MOVE IN DEMO NAME                     
         OI    DEMCODEH+6,X'80'    TRANSMIT                                     
*        MVC   CTDKCODE,FLDH+3                                                  
*                                  VALIDATE DEMO NUMBER                         
KEYV10   GOTO1 AFVAL,DEMDEMOH                                                   
         BZ    EXIT                                                             
         TM    FLDH+4,X'08'        TEST NUMERIC                                 
         BZ    EFNN                                                             
         OC    FLDH(4),FLDH        BETWEEN 1 AND 255                            
         BZ    EIIF                                                             
         OC    FLDH(3),FLDH                                                     
         BNZ   EIIF                                                             
         MVC   CTDKDEMO,FLDH+3                                                  
*                                                                               
         MVC   KEY,CTDKEY          SET KEYS                                     
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
DISPREC  TWAXC DEMFLD1H            CLEAR DOWN TWA                               
         LA    R8,CTDDATA                                                       
         SR    R1,R1                                                            
DISP2    CLI   0(R8),0                                                          
         BE    DISPEND                                                          
         CLI   0(R8),X'02'                                                      
         BE    *+14                                                             
         IC    R1,1(R8)                                                         
         AR    R8,R1                                                            
         B     DISP2                                                            
*                                                                               
         LA    R5,DEMFLD1H         R5=A(FIRST DISPLAY FIELD)                    
         SR    R6,R6               R6=ZERO (FOR IC)                             
         LA    R7,DEMTABH-1        R7=A(LAST DISPLAY FIELD)                     
         LA    R8,2(R8)            R8=A(DATA)                                   
*                                                                               
DISP4    IC    R6,0(R5)            GET TOTAL FIELD LENGTH                       
         TM    1(R5),X'20'         SKIP PROTS                                   
         BO    DISP6                                                            
         SH    R6,=H'9'                                                         
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R5),0(R8)       MOVE DATA TO TWA                             
         LA    R8,1(R6,R8)                                                      
         AH    R6,=H'9'                                                         
DISP6    BXLE  R5,R6,DISP4                                                      
*                                  SET NEXT ACTION & EXIT                       
DISPEND  MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         TM    CTDSTAT,X'80'                                                    
         BZ    *+12                                                             
         MVI   NACTN,OKRES                                                      
         B     EXIT                                                             
         LA    R1,DEMFLD1H                                                      
         ST    R1,FADR                                                          
         MVI   NACTN,OKDEL+OKCHA                                                
         B     EXIT                                                             
         EJECT                                                                  
*              ADD/CHANGE RECORD                                                
*                                                                               
DATAVAL  MVI   TEMP,0              BUILD BASIC RECORD                           
         GOTO1 ABLDREC                                                          
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                                                               
         LA    R8,TEMP                                                          
         XC    TEMP,TEMP                                                        
         MVI   0(R8),X'02'                                                      
         LA    R5,DEMFLD1H         R5=A(FIRST DATA FIELD)                       
         SR    R6,R6               R6=ZERO (FOR IC)                             
         LA    R7,DEMTABH-1        R7=A(LAST DATA FIELD)                        
         LA    R8,2(R8)            R8=A(ELEMENT DATA)                           
*                                                                               
DATAV2   IC    R6,0(R5)            R6=TOTAL FIELD LENGTH                        
         TM    1(R5),X'20'         SKIP PROTS                                   
         BO    DATAV4                                                           
         LR    R1,R5                                                            
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         SH    R6,=H'9'                                                         
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),FLD         MOVE DATA TO ELEMENT                         
         LA    R8,1(R6,R8)                                                      
         AH    R6,=H'9'                                                         
DATAV4   BXLE  R5,R6,DATAV2                                                     
         LA    R1,TEMP             SET ELEMENT LENGTH & ADD ELEMENT             
         SR    R8,R1                                                            
         STC   R8,TEMP+1                                                        
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                  ADD/WRITE RECORD                             
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
         SPACE 1                                                                
*                                  TABLE OF VALID FILES                         
FILETAB  DS    0CL11                                                            
         DC    C'T',CL10'TPT'                                                   
         DC    C'T',CL10'DPT'                                                   
         DC    C'P',CL10'PAV'                                                   
         DC    C'E',CL10'ESTIMATE'                                              
         DC    C'I',CL10'INV'                                                   
         DC    X'FF',CL10'ALL'                                                  
         DC    X'00'                                                            
*                                  TABLE OF VALID SUB-FILES                     
MEDTAB   DS    0CL9                                                             
         DC    C'T',CL8'USTV'                                                   
         DC    C'C',CL8'CANTV'                                                  
         DC    C'R',CL8'RADIO'                                                  
         DC    C'D',CL8'DPT'                                                    
         DC    C'N',CL8'NETWORK'                                                
         DC    C'P',CL8'MPA'                                                    
         DC    C'V',CL8'VPH'                                                    
         DC    C'U',CL8'UPGRADE'                                                
         DC    X'FF',CL8'ALL'                                                   
         DC    X'00'                                                            
*                                  TABLE OF VALID FILE/MEDIA COMBOS             
FMTAB    DS    0CL2                                                             
         DC    C'TT'                                                            
         DC    C'TR'                                                            
         DC    C'TC'                                                            
         DC    C'TD'                                                            
         DC    C'PT'                                                            
         DC    C'PN'                                                            
         DC    C'TP'                                                            
         DC    C'EV'                                                            
         DC    C'IU'                                                            
         DC    X'FFFF'                                                          
         DC    X'00'                                                            
*                                  TABLE OF LOOK UP CODES/NAMES                 
LKUPTAB  DS    0CL15                                                            
         DC    AL1(2),CL14'MISC'                                                
         DC    AL1(3),CL14'WOMEN'                                               
         DC    AL1(4),CL14'MEN'                                                 
         DC    AL1(5),CL14'VIEWERS'                                             
         DC    AL1(6),CL14'LOH'                                                 
         DC    AL1(7),CL14'WORKING WOMEN'                                       
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
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
* CTLFMF1D                                                                      
       ++INCLUDE CTLFMF1D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008CTLFM0E   05/01/02'                                      
         END                                                                    
