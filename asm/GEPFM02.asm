*          DATA SET GEPFM02    AT LEVEL 001 AS OF 06/13/13                      
*PHASE TF0102A                                                                  
         TITLE 'PFM02 - VALIDATE UPDATE DATA AND ADD/UPDATE RECORD'             
         PRINT NOGEN                                                            
PFM02    CSECT                                                                  
         NMOD1 072,**PF02**                                                     
         USING PFMTEMPD,R9         R9=A(GLOBAL W/S)                             
         USING PFMSAVED,R3         R3=A(TWA)                                    
         XC    FERRS,FERRS                                                      
                                                                                
***********************************************************************         
* READ RECORD TO UPDATE                                               *         
***********************************************************************         
RECREAD  MVI   DISKIOOP,0          SET TO FIRST I/O                             
         CLI   STIP10,0                                                         
         BE    RECR030             NO I/O REQUIRED - NEW REC                    
         GOTO1 ADISKIO                                                          
         CLI   SLRI,0                                                           
         BNE   RECR040                                                          
*                                                                               
RECR010  ZIC   RF,HDRN                                                          
         LA    RF,28(RF)                                                        
         STC   RF,FERN             ERROR DISK/EOF/NOTFOUND                      
*                                                                               
RECR020  LA    RF,PFMHL1HH                                                      
         CLI   DISPDAT,C'H'                                                     
         BE    *+8                                                              
         LA    RF,PFMDL1HH                                                      
         ST    RF,FERRS                                                         
         OI    FIND,X'01'                                                       
         B     EXIT                                                             
*                                                                               
RECR030  MVC   IOAREA(L'SLIOAREA),SLIOAREA                                      
*                                                                               
RECR040  CLI   STIRA,5                                                          
         BNE   VALDATA                                                          
         B     CPYREC                                                           
                                                                                
***********************************************************************         
* COPY KEY TO RECORD                                                  *         
***********************************************************************         
CPYREC   SR    R5,R5               COPY KEY TO RECORD                           
         IC    R5,STIFKL                                                        
         SH    R5,=H'1'                                                         
         BM    COPY010                                                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   IOAREA(0),SLIOAREA                                               
*                                                                               
COPY010  MVI   DISPOP,X'0F'        REDISPLAY RECORD                             
         XC    DISPSLN,DISPSLN                                                  
         MVC   DISPDL,SLDISPDL                                                  
         GOTO1 ADISP                                                            
         B     WRITEREC                                                         
                                                                                
***********************************************************************         
* VALIDATE INPUT                                                      *         
***********************************************************************         
VALDATA  LR    R4,RC               R4=A(UPDATED DATA)                           
         LA    R2,PFMFILEH         POINT TO FILE ENTRY FIELD                    
         LA    R5,PFMHL1HH         R5=A(SCR DISPLAY LINE)                       
         CLI   DISPDAT,C'H'                                                     
         BE    *+8                                                              
         LA    R5,PFMDL1HH                                                      
         USING FLDHDRD,R5                                                       
         LH    R6,DISPDL           # OF BYTES DISPLAYED                         
         SLA   R6,1                R6=HEX CHR RESIDUAL COUNT                    
*                                                                               
VAL010   ST    R5,HEXPOS           HEX HEADER                                   
         ZIC   RF,FLDLEN           LENGTH                                       
         LA    RF,0(RF,R5)                                                      
         ST    RF,CHARPOS          CHAR HEADER                                  
*                                                                               
         ZIC   R7,FLDILEN          # OF HEX CHARACTERS ON LINE                  
         CR    R7,R6               DISPLAY LINE <= WHAT`S LEFT?                 
         BH    VAL070              YES CAN INPUT ANYWHERE                       
*                                                                               
         L     R5,CHARPOS          R5=A(CHAR FLD)                               
         LR    RE,R6               CHECK CHR INPUT LEN                          
         SRA   RE,1                                                             
         ZIC   RF,FLDILEN          NUMBER OF EBCDIC CHARACTERS ON LINE          
         CR    RF,RE               MAKE SURE LENGTH ISN'T EXCEEDED              
         BH    VAL070              INPUT LENGTH TOO LONG                        
*                                                                               
         LR    RE,R7               LOAD LENGTH OF HEX SIDE                      
         SRA   RE,1                IN TERMS OF BYTES                            
         CR    RF,RE               DON'T EXCEED THAT NUMBER                     
         BH    VAL070              OTHERWISE, TOO LONG                          
*                                                                               
         L     R5,HEXPOS           R5=A(HEX)                                    
         GOTO1 AHEXIN,HEXWS,FLDDATA,(R4),(R7) HEX INPUT TO UPDATED DATA         
         OC    12(4,R1),12(R1)     OK?                                          
         BNZ   VAL020              YES                                          
         MVI   FERN,7              ERROR INV HEX INPUT                          
         B     VALERR                                                           
*                                                                               
VAL020   TM    FLDIIND,II1T        HEX INPUT THIS TIME                          
         BZ    VAL030              NO                                           
         LR    R2,R5               POINT TO WHERE LAST CHANGE WAS MADE          
         SRA   R7,1                YES REDISPLAY CHRS                           
         L     R5,CHARPOS          A(CHARS)                                     
         FOUT  (R5),(R4),(R7)                                                   
         L     R8,ADISPLWR         TRANSLATION TABLE                            
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         TR    FLDDATA(0),0(R8)                                                 
         LA    R7,1(R7)                                                         
         SLA   R7,1                                                             
         TM    FLDIIND,II1T        CHR INPUT THIS TIME                          
         BZ    VAL060              NO                                           
*                                                                               
         MVI   FERN,41             ERROR CANT INPUT BOTH                        
         B     VALERR                                                           
*                                                                               
VAL030   L     R5,CHARPOS          R5=A(CHAR)                                   
         TM    FLDIIND,II1T        CHR INPUT THIS TIME                          
         BZ    VAL060              NO                                           
         LR    R2,R5               POINT TO WHERE LAST CHANGE WAS MADE          
         LR    R1,R4               R1=A(NEXT UPDATED CHR)                       
         LR    RF,R7                                                            
         SRA   RF,1                RF=NUM OF CHRS IN SCR LINE                   
         LA    R8,FLDDATA                                                       
VAL040   CLI   0(R8),C'?'          IGNORE SPECIAL CHRS                          
         BE    VAL050                                                           
         CLI   0(R8),0             NULL BYTE?                                   
         BNE   *+8                                                              
         MVI   0(R8),C' '          BECOMES A SPACE                              
         MVC   0(1,R1),0(R8)       MOVE SCR CHR TO UPDATE CHR                   
*                                                                               
VAL050   LA    R1,1(R1)                                                         
         LA    R8,1(R8)                                                         
         BCT   RF,VAL040                                                        
         SRA   R7,1                REDISPLAY HEX                                
         L     R5,HEXPOS           R5=A(HEX)                                    
         GOTO1 AHEXOUT,HEXWS,(R4),FLDDATA,(R7),=C'MIX'                          
         SLA   R7,1                                                             
         FOUT  (R5)                                                             
*                                                                               
VAL060   LR    R8,R7               BUMP UPDATED DATA PTR                        
         SRA   R8,1                                                             
         AR    R4,R8                                                            
         L     R5,HEXPOS                                                        
         LH    R0,HLINLEN          BUMP HEX DISP LINE                           
         CLI   DISPDAT,C'H'                                                     
         BE    *+8                                                              
         LH    R0,DLINLEN          BUMP DEC DISP LINE                           
         AR    R5,R0                                                            
         SR    R6,R7               DOWN RESIDUAL HEX COUNT                      
         BP    VAL010                                                           
         B     VALX                                                             
*                                                                               
VAL070   L     R5,HEXPOS                                                        
         MVI   FERN,28             ERROR INPUT TOO LONG                         
VALERR   ST    R5,FERRS                                                         
         OI    FIND,X'01'                                                       
         B     EXIT                                                             
*                                                                               
VALX     NI    PFMFILEH+6,X'FF'-X'40' DON'T POINT CURSOR HERE                   
         OI    6(R2),X'40'         POINT IT HERE                                
         B     CHKUPD                                                           
                                                                                
***********************************************************************         
* CHECK UPDATES                                                       *         
***********************************************************************         
CHKUPD   CLI   SLEACTN,0           WAS AN ELEMENT DISPLAYED                     
         BNE   CHKEL               YES GO AND CHECK IT                          
         CLI   STIRA,3             WAS A NEW RECORD INPUT                       
         BNE   CHKUPD1             NO                                           
         BAS   RE,SAMEKEY          YES CANT CHANGE KEY IF ANY                   
         CLI   FERN,0                                                           
         BNE   UPDERR                                                           
         B     CHKUPD3                                                          
*                                                                               
CHKUPD1  CLI   STIFT,2             INDX SEQ FILE                                
         BE    CHKUPD2             YES CANT CHANGE KEY                          
         TM    STIFTL,X'01'                                                     
         BO    CHKUPDX             EXIT IF REQUEST TYPE FILE                    
         B     CHKUPD3                                                          
*                                                                               
CHKUPD2  BAS   RE,SAMEKEY                                                       
         CLI   FERN,0                                                           
         BNE   UPDERR                                                           
*                                                                               
CHKUPD3  CLI   STIFRLBN,X'FF'      REC LEN STORED IN REC                        
         BE    CHKUPD4             NO                                           
         SR    R5,R5               YES SET R5=STRT & R6=END                     
         IC    R5,STIFRLBN                                                      
         LA    R7,IOAREA(R5)                                                    
         LA    R6,1(R5)                                                         
         OC    SLNRECL,SLNRECL     NEW RECORD LEN                               
         BZ    *+10                NO                                           
         MVC   0(2,R7),SLNRECL     YES SET IN IOAREA                            
         BAS   RE,SAMEDATA                                                      
         CLI   FERN,0                                                           
         BNE   UPDERR              ERROR CANT CHANGE REC LEN                    
*                                                                               
CHKUPD4  CLI   STIFSL,0            FILE HAVE A SYSTEM AREA                      
         BE    CHKUPD5             NO                                           
         TM    STIFTL,X'80'        SYSTEM AREA A DUMMY                          
         BO    CHKUPD5             YES                                          
         SR    R5,R5               CANT CHANGE SYS                              
         IC    R5,STIFKL                                                        
         SR    R6,R6                                                            
         IC    R6,STIFCL                                                        
         AR    R5,R6               STRT=L'KEY+L'CTRL                            
         SR    R7,R7                                                            
         IC    R7,STIFSL                                                        
         LR    R6,R5                                                            
         AR    R6,R7                                                            
         BCTR  R6,R0               END=L'KEY+L'CTRL+L'SYS-1                     
         BAS   RE,SAMEDATA                                                      
         CLI   FERN,0                                                           
         BNE   UPDERR                                                           
*                                                                               
CHKUPD5  B     CHKUPDX                                                          
*                                                                               
UPDERR   LA    R6,PFMHL1HH                                                      
         CLI   DISPDAT,C'H'                                                     
         BE    *+8                                                              
         LA    R6,PFMDL1HH                                                      
         ST    R6,FERRS                                                         
         OI    FIND,X'01'                                                       
         B     EXIT                                                             
*                                                                               
CHKUPDX  B     MOVEDATA                                                         
                                                                                
***********************************************************************         
* CHECK ELEMENT                                                       *         
***********************************************************************         
CHKEL    LR    R4,RC               R4=A(INPUT ELEMENT)                          
         LH    R2,DISPDL           LOAD # OF BYTES DISPLAYED                    
         LA    R6,IOAREA                                                        
         AH    R6,SLESTRT          R6=A(RECORD ELEMENT)                         
         MVC   DUB(1),SLENL        SET NEW LEN                                  
         CLI   DUB,0                                                            
         BNE   *+10                                                             
CHKELLP  MVC   DUB(1),1(R6)        SET OLD LEN                                  
         ZIC   R0,1(R6)            LOAD LENGTH OF ELEMENT                       
         SR    R2,R0               FIND BYTES LEFT AFTER THIS ELEMENT           
         CLC   DUB(1),1(R4)        CHECK INPUT EL LEN                           
         BE    CHKEL1                                                           
         MVI   FERN,42             ERROR CANT CHANGE LEN                        
         B     UPDERR                                                           
*                                                                               
CHKEL1   CLI   0(R4),0                                                          
         BNE   *+12                                                             
CHKEL2   MVI   FERN,43             ERROR INV EL ID CODE                         
         B     UPDERR                                                           
         CLI   SLEACTN,3                                                        
         BNE   CHKEL3                                                           
         CLI   0(R4),X'FF'                                                      
         BE    CHKEL2              CANT ADD FF ELEMENT                          
         SR    R7,R7                                                            
         IC    R7,1(R4)                                                         
         ST    R7,WORD1            SET +LEN OF NEW EL                           
         B     CHKELA                                                           
*                                                                               
CHKEL3   CLI   0(R4),X'FF'         DELETE ELEMENT CODE                          
         BNE   CHKEL4                                                           
         SR    R7,R7                                                            
         IC    R7,1(R6)                                                         
         LNR   R7,R7                                                            
         ST    R7,WORD1            SET -LEN OF OLD EL                           
         B     CHKELA                                                           
*                                                                               
CHKEL4   SR    R7,R7                                                            
         IC    R7,1(R4)                                                         
         SR    R8,R8                                                            
         IC    R8,1(R6)                                                         
         SR    R7,R8                                                            
         ST    R7,WORD1            SET NEW-OLD VALUE                            
*                                                                               
CHKELA   L     R7,WORD1            RECORD NEEDS INCREASING                      
         LTR   R7,R7                                                            
         BZ    CHKELC                                                           
         BM    CHKELB                                                           
         L     RE,=A(IOAREAX-IOAREA)                                            
         LA    RE,IOAREA(RE)                                                    
         LA    RF,0(RE,R7)                                                      
         LA    R0,1(RE)                                                         
         SR    R0,R6                                                            
         MVC   0(1,RF),0(RE)                                                    
         BCTR  RE,0                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         B     CHKELC                                                           
*                                                                               
CHKELB   LPR   R7,R7               RECORD NEEDS DECREASING                      
         LR    RF,R6                                                            
         LA    RE,0(RF,R7)                                                      
         L     R8,=A(IOAREAX-IOAREA)                                            
         LA    R0,IOAREA(R8)                                                    
         SR    R0,R6                                                            
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,*-14                                                          
         OI    FIND,X'80'          FLAG GOTO                                    
         OI    PFMEACTH+4,X'80'    ASSUME ACTION CHANGED                        
*                                                                               
CHKELC   CLI   0(R4),X'FF'         MOVE NEW EL TO RECORD                        
         BE    CHKELD              BUT NOT IF DELETED                           
         SR    R7,R7                                                            
         IC    R7,1(R4)                                                         
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R4)                                                    
         L     R7,WORD1            CHANGE RECORD?                               
         LTR   R7,R7                                                            
         BNZ   CHKELD              NO                                           
         ZIC   RF,1(R6)            LOAD LENGTH OF THE ELEMENT                   
         AR    RF,R6               POINT TO THE NEXT ELEMENT                    
         CLI   0(RF),0             END OF RECORD?                               
         BE    CHKELD              YES, DON'T GO TO THE NEXT                    
         OI    FIND,X'80'          FLAG GOTO                                    
         OI    PFMEACTH+4,X'80'    ASSUME ACTION CHANGED                        
*                                                                               
CHKELD   L     R7,WORD1            SET NEW REC LEN IN RECORD                    
         LTR   R7,R7                                                            
         BZ    CHKELX              NO CHANGE IN REC LEN                         
         AH    R7,SLRL                                                          
         STH   R7,SLNRECL                                                       
         CLI   STIFRLBN,X'FF'                                                   
         BE    CHKELX              REC LEN NOT STORED IN REC                    
         SR    R8,R8                                                            
         IC    R8,STIFRLBN                                                      
         LA    R8,IOAREA(R8)                                                    
         MVC   0(2,R8),SLNRECL                                                  
CHKELX   B     WRITEREC                                                         
                                                                                
***********************************************************************         
* ROUTINE TO COMPARE KEY (IF ANY) IN IOAREA WITH THAT IN W/S          *         
***********************************************************************         
SAMEKEY  NTR1                                                                   
         MVI   FERN,0              RETURN ZERO IF OK                            
         CLI   STIFKL,0                                                         
         BE    SAMEKEYX            NO KEY IN RECORD                             
         SR    R5,R5                                                            
         SR    R6,R6                                                            
         IC    R6,STIFKL                                                        
         BCTR  R6,R0                                                            
         BAS   RE,SAMEDATA                                                      
         CLI   FERN,0                                                           
         BE    SAMEKEYX                                                         
         MVI   FERN,39             ERROR CANT CHANGE KEY                        
SAMEKEYX XIT1                                                                   
                                                                                
***********************************************************************         
* ROUTINE TO COMPARE DATA IN IOAREA WITH THE UPDATED DATA IN W/S      *         
* R5=INDEX TO FRST BYTE  R6=INDEX TO LAST BYTE                        *         
* FERN SET TO ZERO IF SAME ELSE TO 40 IF DIFFERENT                    *         
***********************************************************************         
SAMEDATA NTR1                                                                   
         LH    R1,STIB             SET R1=LAST BYTE INDEX                       
         AH    R1,DISPDL                                                        
         BCTR  R1,0                                                             
         MVI   FERN,0                                                           
         CH    R6,STIB                                                          
         BL    SAMEDATX            DATA NOT DISPLAYED                           
         CR    R5,R1                                                            
         BH    SAMEDATX            DATA NOT DISPLAYED                           
         CH    R5,STIB                                                          
         BNL   *+8                                                              
         LH    R5,STIB             DATA PART DISPLAYED                          
         CR    R6,R1                                                            
         BNH   *+6                                                              
         LR    R6,R1                                                            
         SR    R6,R5               R6=DISP DATA LEN - 1                         
*                                                                               
         LA    R7,IOAREA(R5)       R7=A(DATA IN IOAREA)                         
         LR    R8,RC                                                            
         SH    R8,STIB                                                          
         AR    R8,R5               R8=A(DATA IN W/S)                            
*                                                                               
         EX    R6,*+8              COMPARE IOAREA WITH W/S                      
         B     *+10                                                             
         CLC   0(0,R7),0(R8)                                                    
         BE    SAMEDATX                                                         
         MVI   FERN,40             ERROR CANT CHANGE DATA                       
*                                                                               
SAMEDATX XIT1                                                                   
                                                                                
***********************************************************************         
* UPDATE FROM W/S TO IOAREA                                           *         
***********************************************************************         
MOVEDATA LA    R4,IOAREA           UPDATE FROM W/S TO IOAREA                    
         AH    R4,STIB                                                          
         LR    R5,RC                                                            
         LH    R6,DISPDL                                                        
MOVELOOP CH    R6,=H'256'                                                       
         BL    MOVEL1                                                           
         MVC   0(256,R4),0(R5)                                                  
         LA    R4,256(R4)                                                       
         LA    R5,256(R5)                                                       
         SH    R6,=H'256'                                                       
         B     MOVELOOP                                                         
MOVEL1   MVC   WORD1+2(L'DISPDL),DISPDL   DON'T GO BACK, STRAIGHT COPY          
         LTR   R6,R6                                                            
         BZ    WRITEREC                                                         
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     WRITEREC                                                         
         MVC   0(0,R4),0(R5)                                                    
                                                                                
***********************************************************************         
* WRITE RECORD                                                        *         
***********************************************************************         
WRITEREC OC    SLNRECL,SLNRECL     SET REC LEN FOR WRITE                        
         BZ    *+10                                                             
         MVC   SLRL,SLNRECL                                                     
WRITER2  MVI   DISKIOOP,1          SET TO SECOND I/O                            
         CLI   STIP11,0                                                         
         BE    WRITEOK             NO SECND I/O REQ ???!!!                      
         GOTO1 ADISKIO                                                          
         CLI   SLRI,0                                                           
         BE    RECR010                                                          
WRITEOK  MVI   HDRN,0                                                           
         MVC   PFMMSG(20),WRK                                                   
         OI    PFMMSGH+6,X'80'                                                  
         L     R7,WORD1            FIND IF A REGULAR CHANGE NOT DELETE          
         LTR   R7,R7                                                            
         BP    EXIT                EXIT IF ADDING, NOT CHANGE OR DELETE         
         BM    WRITEOK1            DELETION ONLY INCREMENT UPDATED PTR          
         ZIC   R1,1(R6)                                                         
         AR    R4,R1                                                            
         AR    R6,R1               BOTH NEW AND OLD HAVE SAME LENGTH            
         B     WRITEOK2            CHECK FOR END OF RECORD                      
WRITEOK1 ZIC   R1,1(R4)            1(R4) IS CORRECT LENGTH                      
         AR    R4,R1               R6 POINTING TO NEXT ONE ALREADY              
WRITEOK2 LTR   R2,R2               NO MORE BYTES LEFT IN DISPLAY?               
         BNP   EXIT                NONE LEFT                                    
         CLI   0(R6),0             END OF RECORD?                               
         BNE   CHKELLP             NO, CONTINUE CHECKING OTHER ELEMS            
         EJECT                                                                  
EXIT     XMOD1 1                                                                
         SPACE 2                                                                
                                                                                
***********************************************************************         
* LITERALS AND DSECTS                                                 *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
DLINLEN  DC    Y(PFMDL2OH-PFMDL1OH)                                             
HLINLEN  DC    Y(PFMHL2OH-PFMHL1OH)                                             
         EJECT                                                                  
* GEPFMSAVE                                                                     
       ++INCLUDE GEPFMSAVE                                                      
         EJECT                                                                  
* GEPFMTEMP                                                                     
       ++INCLUDE GEPFMTEMP                                                      
         EJECT                                                                  
* GEPFMDS                                                                       
       ++INCLUDE GEPFMDS                                                        
         EJECT                                                                  
* DDFLDHDR                                                                      
       ++INCLUDE DDFLDHDR                                                       
* DDFLDIND                                                                      
       ++INCLUDE DDFLDIND                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001GEPFM02   06/13/13'                                      
         END                                                                    
