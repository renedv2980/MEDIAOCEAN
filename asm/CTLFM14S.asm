*          DATA SET CTLFM14S   AT LEVEL 003 AS OF 05/01/02                      
*PHASE TA0214A                                                                  
*INCLUDE SCINKEY                                                                
         TITLE 'CTLFM14 - CONTROL FILE MAINT - TWX LIST RECORDS'                
CTLFM14  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 350,**LFMK**                                                     
         USING WORKD,RC            RC=A(TEMP W/S)                               
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         L     R4,AREC                                                          
         USING CTAREC,R4           R4=A(RECORD)                                 
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
*                                                                               
KEYVAL   XC    CTAKEY,CTAKEY                                                    
         MVI   CTAKTYP,C'A'                                                     
*                                  EXTRACT USER ID NUMBER                       
         L     R5,AUTL                                                          
         USING UTLD,R5                                                          
         MVC   CTAKUSER,TUSER                                                   
         DROP  R5                                                               
         LA    R1,TWXIDH                                                        
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         CLI   FLDH+5,3                                                         
         BL    EFTS                                                             
         MVC   CTAKID,FLD          MOVE TWX LIST-ID TO KEY                      
*                                                                               
         MVC   KEY,CTAKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         CLI   ACTN,CHANGE         IF ACTN=CHANGE AND KEY NEQ LKEY              
         BNE   KEYV2               SET ACTN=DISPLAY                             
         CLC   KEY,LKEY                                                         
         BE    *+8                                                              
         MVI   ACTN,DISPLAY                                                     
KEYV2    CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET READ-FOR-UPDATE INDIC.                   
         GOTO1 AREAD                                                            
         BZ    EXIT                                                             
         TM    DMCB+8,X'10'        TEST RECORD N/F                              
         BZ    KEYV4                                                            
         CLI   ACTN,ADD            ONLY VALID FOR ADD                           
         BE    DATAVAL                                                          
         B     ERNF                                                             
KEYV4    CLI   ACTN,ADD                                                         
         BE    ERAE                                                             
         TM    DMCB+8,X'02'        TEST RECORD DELETED                          
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY        ONLY VALID FOR DISPLAY                       
         BNE   ERNF                                                             
         CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
         B     DISPREC                                                          
         EJECT                                                                  
*              DISPLAY LIST RECORD                                              
*                                                                               
DISPREC  TWAXC TWXDESCH                                                         
         LA    R5,CTADATA          R5=A(CURRENT ELEMENT)                        
         XC    BLOCK(4),BLOCK                                                   
DISP2    CLI   0(R5),0             E-O-R                                        
         BE    DISP6                                                            
         CLI   0(R5),X'02'         DESCRIPTION                                  
         BE    DISPDESC                                                         
         CLI   0(R5),X'03'         POINTER                                      
         BE    DISPID                                                           
DISP4    ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DISP2                                                            
DISP6    L     R0,BLOCK                                                         
         GOTO1 =V(SCINKEY),DMCB,(10,TWXDST1H),(8,BLOCK+4),(R0),RR=RB            
         TM    CTASTAT,X'80'       SET NACTN SET FADR & EXIT                    
         BO    *+12                                                             
         MVI   NACTN,OKDEL+OKCHA                                                
         B     *+8                                                              
         MVI   NACTN,OKRES                                                      
         LA    R1,TWXDESCH                                                      
         ST    R1,FADR                                                          
         MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         B     EXIT                                                             
         EJECT                                                                  
*              DISPLAY DESCRIPTION ELEMENT                                      
*                                                                               
DISPDESC ZIC   R1,1(R5)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     DISP4                                                            
         MVC   TWXDESC(0),2(R5)                                                 
*                                                                               
*              ADD ENTRY TO OUTPUT DESTINATION BLOCK                            
*                                                                               
         USING CTPASD,R5                                                        
DISPID   CLI   CTPASLEN,4                                                       
         BNH   DISPID2                                                          
         MVC   WORK(2),=C'L='                                                   
         MVC   WORK+2(6),CTPASDTA                                               
         B     DISPID6                                                          
*                                  FOR ID'S READ ID RECORD AND                  
DISPID2  LA    R6,IOAREA2          DISPLAY NAME                                 
         USING CTIREC,R6                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),CTPASDTA                                             
         MVC   KEYSAVE,KEY                                                      
         MVC   KEY,CTIKEY                                                       
         ST    R6,AREC                                                          
         GOTO1 AREAD                                                            
         ST    R4,AREC                                                          
         MVC   KEY,KEYSAVE                                                      
         CLI   DMCB+8,0                                                         
         BNE   DISP4                                                            
         LA    RE,CTIDATA                                                       
         SR    R1,R1                                                            
DISPID4  CLI   0(RE),0                                                          
         BE    DISP4                                                            
         CLI   0(RE),X'02'         FIND ID NAME ELEMENT                         
         BE    *+14                                                             
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     DISPID4                                                          
         MVC   WORK(8),2(RE)                                                    
DISPID6  L     RE,BLOCK            ADD 8 BYTE ENTRY TO OUTPUT BLOCK             
         LA    RF,1(RE)                                                         
         ST    RF,BLOCK                                                         
         SLL   RE,3                                                             
         LA    RE,BLOCK+4(RE)                                                   
         MVC   0(8,RE),WORK                                                     
         B     DISP4                                                            
         DROP  R6                                                               
         EJECT                                                                  
*              CHANGE/ADD LIST RECORD                                           
*                                                                               
DATAVAL  MVI   TEMP,0              BUILD BASIC RECORD                           
         GOTO1 ABLDREC                                                          
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                  BUILD & ADD DESCRIPTION ELEMENT              
         LA    R1,TWXDESCH                                                      
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         LA    RF,TEMP                                                          
         USING CTDSCD,RF                                                        
         XC    TEMP,TEMP                                                        
         MVI   CTDSCEL,X'02'                                                    
         ZIC   R1,FLDH+5                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTDSC(0),FLD                                                     
         LA    R1,2(R1)                                                         
         STC   R1,CTDSCLEN                                                      
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         XC    BLOCK(4),BLOCK                                                   
         LA    R7,TWXDST1H         R7=A(TWA INPUT LINE)                         
         LA    R6,IOAREA2          R6=A(2ND IOAREA)                             
         MVC   KEYSAVE,KEY                                                      
         ST    R6,AREC                                                          
DATAV2   CLI   0(R7),9             END OF TWA                                   
         BE    DATAVM                                                           
         LR    R1,R7                                                            
         GOTO1 AFVAL                                                            
         BZ    DATAVK                                                           
         MVI   FNDX,0                                                           
         GOTO1 VSCANNER,DMCB,FLDH,SCANBLK                                       
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         MVC   NLINES,4(R1)        SAVE NUMBER OF LINES INPUT                   
         MVI   FNDX,1                                                           
         LA    R5,SCANBLK          R5=A(CURRENT SCANBLK ENTRY)                  
DATAV4   CLC   FNDX,NLINES         END OF LINE                                  
         BH    DATAVK                                                           
         CLI   1(R5),0             L'SECOND HALF                                
         BNE   DATAV8                                                           
         CLI   0(R5),3             L'FIRST HALF                                 
         BL    EFTS                                                             
         CLI   0(R5),8             L'FIRST HALF                                 
         BH    EFTL                                                             
*                                  DEAL WITH USER-ID INPUT                      
         USING CTIREC,R6                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,12(R5)                                                    
         MVC   KEY,CTIKEY                                                       
         GOTO1 AREAD               READ ID RECORD                               
         BZ    EXIT                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         LA    RE,CTIDATA                                                       
         SR    R1,R1                                                            
DATAV6   CLI   0(RE),0             FIND ID NUMBER ELEMENT                       
         BE    EIIF                                                             
         CLI   0(RE),X'02'                                                      
         BE    *+14                                                             
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     DATAV6                                                           
*                                                                               
         MVC   WORK(2),2(RE)                                                    
         BAS   RE,DUPCHECK         CHECK FOR DUPLICATES                         
         BE    EDIF                                                             
         LA    RF,TEMP                                                          
         USING CTPASD,RF           BUILD & ADD ID POINTER ELEMENT               
         MVC   CTPASEL(2),=X'0304'                                              
         MVC   CTPASDTA(2),WORK                                                 
         ST    R4,AREC                                                          
         GOTO1 ABLDREC                                                          
         ST    R6,AREC                                                          
         B     DATAVI                                                           
         DROP  RF                                                               
*                                  DEAL WITH INCLUDED LISTS                     
DATAV8   CLI   0(R5),1             L'SECOND HALF                                
         BNE   EIIF                                                             
         CLI   12(R5),C'L'         V'FIRST HALF                                 
         BNE   EIIF                                                             
         CLI   1(R5),3             L'SECOND HALF                                
         BL    EFTS                                                             
         CLI   1(R5),6             L'SECOND HALF                                
         BH    EFTL                                                             
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+CTAKID-CTAREC(L'CTAKID),22(R5)                               
         GOTO1 AREAD               READ LIST RECORD                             
         BZ    EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         LA    RF,IOAREA2+CTADATA-CTAREC                                        
         SR    R1,R1                                                            
DATAVA   CLI   0(RF),0             FIND POINTER ELEMENTS                        
         BE    DATAVG                                                           
         CLI   0(RF),X'03'                                                      
         BE    DATAVE                                                           
DATAVC   IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     DATAVA                                                           
*                                  DEAL WITH POINTER ELEMENTS                   
         USING CTPASD,RF                                                        
DATAVE   CLI   CTPASLEN,4                                                       
         BH    EIRT                DOUBLE NEST LEVELS                           
         MVC   WORK(2),CTPASDTA                                                 
         BAS   RE,DUPCHECK         CHECK FOR DUPLICATES                         
         BE    EDIF                                                             
         B     DATAVC                                                           
*                                  BUILD & ADD LIST POINTER ELEMENT             
DATAVG   LA    RF,TEMP                                                          
         MVC   CTPASEL(2),=X'0308'                                              
         MVC   CTPASDTA(6),22(R5)                                               
         ST    R4,AREC                                                          
         GOTO1 ABLDREC                                                          
         ST    R6,AREC                                                          
         B     DATAVI                                                           
         DROP  RF                                                               
*                                  BUMP SCAN BLOCK POINTERS                     
DATAVI   LA    R5,L'SCANBLK(R5)                                                 
         ZIC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         B     DATAV4                                                           
*                                  BUMP TWA LINE POINTERS                       
DATAVK   LA    R7,L'TWXDST1+8(R7)                                               
         B     DATAV2                                                           
*                                  ADD/WRITE RECORD                             
DATAVM   LA    R1,TWXDST1H                                                      
         ST    R1,FADR                                                          
         MVI   FNDX,0                                                           
         OC    BLOCK(4),BLOCK      CHECK FOR INPUT                              
         BZ    EMIF                                                             
         LA    R1,TWXIDH                                                        
         ST    R1,FADR                                                          
         MVI   FERN,X'FF'                                                       
         L     RF,AADD                                                          
         CLI   ACTN,ADD                                                         
         BE    *+8                                                              
         L     RF,AWRITE                                                        
         MVC   KEY,KEYSAVE         RESTORE KEY/A(RECORD)                        
         ST    R4,AREC                                                          
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVI   NACTN,OKDEL+OKCHA                                                
         LA    R1,BASACTNH         SET CURSOR & EXIT                            
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
*              CHECK FOR DUPLICATE DESTINATIONS                                 
*                                                                               
DUPCHECK NTR1                                                                   
         L     RE,BLOCK                                                         
         LA    R1,BLOCK+4                                                       
         LA    RF,1(RE)                                                         
DUPC2    LTR   RE,RE                                                            
         BZ    DUPC4                                                            
         CLC   WORK(2),0(R1)                                                    
         BE    EXIT                                                             
         LA    R1,2(R1)                                                         
         SH    RE,=H'1'                                                         
         B     DUPC2                                                            
DUPC4    MVC   0(2,R1),WORK        ADD ENTRY TO BLOCK                           
         ST    RF,BLOCK                                                         
         CR    RE,RF                                                            
         B     EXIT                                                             
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
*              LITERALS ETC.                                                    
*                                                                               
         LTORG                                                                  
*                                  DSECT TO COVER TEMP W/S                      
WORKD    DSECT                                                                  
IOAREA2  DS    1000C                                                            
NLINES   DS    X                                                                
SCANBLK  DS    20CL32                                                           
         DS    0F                                                               
BLOCK    DS    1000C                                                            
*                                                                               
* FAUTL/CTLFMACTNS                                                              
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
       ++INCLUDE CTLFMACTNS                                                     
         PRINT ON                                                               
         EJECT                                                                  
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMEBD                                                                      
       ++INCLUDE CTLFMEBD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTLFM14S  05/01/02'                                      
         END                                                                    
