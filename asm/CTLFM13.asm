*          DATA SET CTLFM13    AT LEVEL 040 AS OF 05/01/02                      
*PHASE TA0213A                                                                  
         TITLE 'CTLFM13 - CONTROL FILE MAINT - VALUE RECORDS'                   
CTLFM13  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKEND-WORKD,**LFMJ**,RR=R8                                     
         USING WORKD,RC            RC=A(TEMP W/S)                               
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         LR    R9,R1                                                            
*                                                                               
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         L     R2,ATWA                                                          
*                                                                               
         USING LFMSAVE,R3          R3=A(TWA SAVE)                               
         LR    R3,R2                                                            
*                                                                               
         USING CTVREC,R4           R4=A(RECORD)                                 
         L     R4,AREC                                                          
         ST    R4,SVREC                                                         
         LA    RF,IO1                                                           
         ST    RF,AIO1                                                          
         ST    R8,RELO                                                          
*                                                                               
         USING TIOBD,R1                                                         
         L     R1,ATIOB                                                         
         MVC   PFKEY,TIOBAID       AID BYTE (0=ENTER, 1-24=PF KEY)              
         B     KEYVAL                                                           
         DROP  R1                                                               
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
*                                                                               
KEYVAL   XC    CTVKEY,CTVKEY                                                    
         MVI   CTVKTYP,C'V'                                                     
*                                                                               
         LA    R1,TYPETBL                                                       
KEYV2    CLI   0(R1),0             LOOK-UP TYPE IN TABLE                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   BASTYPE(1),0(R1)                                                 
         BE    *+12                                                             
         LA    R1,L'TYPETBL(R1)                                                 
         B     KEYV2                                                            
*                                                                               
         MVC   CTVKREC,1(R1)       SET SUB RECORD TYPE                          
         MVC   TYPEACTN,2(R1)                                                   
         MVC   DUB+1(3),3(R1)      SET A(KEY VALIDATION ROUTINE)                
         L     R1,DUB                                                           
         A     R1,RELO                                                          
         ST    R1,AKEYRTN                                                       
*                                  VALIDATE SYSTEM                              
         LA    R1,VALSYSH                                                       
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         ZIC   RE,FLDH+5                                                        
         BCTR  RE,0                                                             
         LA    R1,SYSTBL                                                        
KEYV4    CLI   0(R1),0             LOOK-UP SYSTEM IN TABLE                      
         BE    EIIF                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),FLD                                                      
         BE    *+12                                                             
         LA    R1,L'SYSTBL(R1)                                                  
         B     KEYV4                                                            
*                                                                               
         IC    RE,9(R1)            CHECK SYSTEM VALID FOR SUB-RECORD            
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    TYPEACTN,0                                                       
         BZ    EIIF                                                             
         MVC   CTVKSYS,8(R1)       SET SYSTEM LETTER FROM TABLE                 
         MVC   SYSNUM,10(R1)       SET SYSTEM NUMBER                            
         CLC   0(8,R1),FLD                                                      
         BE    *+14                                                             
         MVC   VALSYS,0(R1)        DISPLAY FULL SYSTEM NAME                     
         OI    VALSYSH+6,X'80'                                                  
         TM    TYPEACTN,X'01'      DOES KEY TYPE REQUIRE ID RECORD              
         BZ    KEYV5                                                            
         MVC   KEYSAVE,CTVKEY      YES - SAVE KEY                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'            BUILD KEY OF ID RECORD                       
*                                                                               
         USING TWAD,R1                                                          
         LR    R1,R2                                                            
         MVC   KEY+23(2),TWAUSRID                                               
         DROP  R1                                                               
*                                                                               
         GOTO1 AREAD               READ ID RECORD                               
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
*                                                                               
         USING CTIREC,R4                                                        
         LA    R1,CTIDATA                                                       
         SR    RE,RE                                                            
KEYV4A   CLI   0(R1),0                                                          
         BE    EIIF                                                             
         CLI   0(R1),X'06'         AGENCY ID ELEMENT                            
         BE    KEYV4C                                                           
         CLI   0(R1),X'21'         SYSTEM ELEMENT                               
         BE    KEYV4D                                                           
KEYV4B   IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     KEYV4A                                                           
*                                                                               
         USING CTAGYD,R1                                                        
KEYV4C   MVC   AGYALPH,CTAGYID     SAVE AGENCY ALPHA-ID                         
         B     KEYV4B                                                           
*                                                                               
         USING CTSYSD,R1                                                        
KEYV4D   CLC   CTSYSNUM,SYSNUM                                                  
         BNE   KEYV4B                                                           
         MVC   AGYNO,CTSYSAGB      SAVE AGENCY KEY ID                           
         DROP  R1                                                               
*                                                                               
         USING CTVREC,R4                                                        
         MVC   CTVKEY,KEYSAVE      RESTORE KEY                                  
*                                                                               
KEYV5    LA    R1,VALKEYH                                                       
         GOTO1 AKEYRTN             GO TO KEY VALIDATION ROUTINE                 
         CLI   FERN,X'FF'          CHECK FOR ERRORS                             
         BNE   EXIT                                                             
*                                                                               
KEYV6    MVC   KEY,CTVKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         XC    IO1(100),IO1                                                     
         CLI   ACTN,CHANGE         IF ACTION=CHANGE AND KEY NEQ LKEY            
         BNE   KEYV8               SET ACTION TO DISPLAY                        
         CLC   KEY,LKEY                                                         
         BE    KEYV8                                                            
         MVI   ACTN,DISPLAY                                                     
*                                                                               
KEYV8    CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET READ-FOR-UPDATE                          
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        CHECK FOR NOT FOUND                          
         BZ    *+16                                                             
         CLI   ACTN,ADD            NOT VALID UNLESS ACTION=ADD                  
         BE    DATAVAL                                                          
         B     ERNF                                                             
*                                                                               
         CLI   ACTN,ADD            CAN'T EXIST FOR ADD                          
         BE    ERAE                                                             
         TM    DMCB+8,X'02'        CHECK FOR DELETE                             
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY        NOT VALID UNLESS DISPLAY                     
         BNE   EIAS                                                             
*                                                                               
         MVC   AREC,AIO1           USE LOCAL IO AREA                            
         MVI   TWOREC,NO                                                        
         MVI   KEY+(CTVKPAGE-CTVREC),1                                          
         GOTO1 AREAD                                                            
         MVC   AREC,SVREC                  RESTORE IO AREA                      
         MVI   KEY+(CTVKPAGE-CTVREC),0     RESET KEY                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        CHECK FOR NOT FOUND                          
         BO    KEYV12              NO  2ND RECORD                               
         MVI   TWOREC,YES          YES 2ND RECORD PRESENT                       
*                                                                               
KEYV12   CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
         B     DISPREC                                                          
         EJECT                                                                  
*              VALIDATE FILTER KEY                                              
*                                                                               
FILTVAL  NTR1                                                                   
         CLI   CTVKSYS,C'M'        MEDIA SYSTEM FILTER                          
         BNE   FILTV4                                                           
         MVC   VALKIS,=CL31'KEY IS MEDIA,FILTER OR FILTER'                      
         OI    VALKISH+6,X'80'                                                  
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         GOTO1 VSCANNER,DMCB,FLDH,TEMP                                          
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         CLI   4(R1),2                                                          
         BH    EIIF                                                             
         MVI   FNDX,1                                                           
         LA    R6,TEMP                                                          
         CLI   4(R1),1                                                          
         BE    FILTV2                                                           
*                                  VALIDATE MEDIA                               
         CLI   0(R6),1             L'FIRST HALF                                 
         BL    EIIF                                                             
         BH    EFTL                                                             
         CLI   1(R6),0             L'SECOND HALF                                
         BNE   EIIF                                                             
         MVC   CTVKKEY+2(1),12(R6) SET MEDIA CODE                               
         LA    R6,32(R6)                                                        
         MVI   FNDX,2                                                           
*                                  VALIDATE FILTER NUMBER                       
FILTV2   CLI   1(R6),0             L'SECOND HALF                                
         BNE   EIIF                                                             
         TM    2(R6),X'80'         T'FIRST HALF                                 
         BZ    EFNN                                                             
         OC    4(4,R6),4(R6)       V'FIRST HALF                                 
         BZ    EFLM                                                             
         OC    4(3,R6),4(R6)                                                    
         BNZ   EFTB                                                             
         MVC   CTVKNUM,7(R6)       SET FILTER NUMBER                            
         MVC   CTVKKEY(2),AGYALPH  SET AGENCY ALPHA-ID                          
         B     KEYVALX                                                          
*                                                                               
FILTV4   CLI   CTVKSYS,C'A'        ACCOUNT SYSTEM FILTER                        
         BNE   FILTV6                                                           
         MVC   VALKIS,=CL31'KEY IS UNIT/LEDGER,FILTER'                          
         OI    VALKISH+6,X'80'                                                  
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         GOTO1 VSCANNER,DMCB,FLDH,TEMP                                          
         CLI   4(R1),2                                                          
         BNE   EIIF                                                             
         MVI   FNDX,1                                                           
         LA    R6,TEMP                                                          
*                                  VALIDATE UNIT/LEDGER                         
         CLI   1(R6),0             L'SECOND HALF                                
         BNE   EIIF                                                             
         CLI   0(R6),2             L'FIRST HALF                                 
         BL    EFTS                                                             
         BH    EFTL                                                             
         MVC   CTVKKEY(1),AGYNO    SET COMPANY CODE                             
         MVC   CTVKKEY+1(2),12(R6) SET UNIT/LEDGER                              
         LA    R6,32(R6)                                                        
         MVI   FNDX,2                                                           
*                                  VALIDATE FILTER NUMBER                       
         CLI   1(R6),0             L'SECOND HALF                                
         BNE   EIIF                                                             
         TM    2(R6),X'80'         T'FIRST HALF                                 
         BZ    EFNN                                                             
         OC    4(4,R6),4(R6)       V'FIRST HALF                                 
         BZ    EFLM                                                             
         OC    4(3,R6),4(R6)                                                    
         BNZ   EFTB                                                             
         MVC   CTVKNUM,7(R6)       SET FILTER NUMBER                            
         B     KEYVALX                                                          
*                                                                               
FILTV6   DC    H'0'                OTHER SYSTEM VALIDATION GOES HERE            
         EJECT                                                                  
*              VALIDATE REQUEST OPTION KEY                                      
*                                                                               
OPTNVAL  NTR1                                                                   
         MVC   VALKIS,=CL31'KEY IS PP,OPTION OR *PP,OPTION'                     
         OI    VALKISH+6,X'80'                                                  
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         GOTO1 VSCANNER,DMCB,FLDH,TEMP                                          
         CLI   4(R1),2                                                          
         BNE   EIIF                                                             
         MVI   FNDX,1                                                           
         LA    R6,TEMP                                                          
*                                  VALIDATE PROGRAM                             
         CLI   1(R6),0             L'SECOND HALF                                
         BNE   EIIF                                                             
         CLI   0(R6),2             L'FIRST HALF                                 
         BL    EFTS                                                             
         BH    OPTNV2                                                           
         MVC   CTVKKEY(2),AGYALPH  SET AGENCY ALPHA-ID                          
         MVC   CTVKKEY+2(2),12(R6) SET PROGRAM                                  
         B     OPTNV4                                                           
*                                  VALIDATE PROGRAM                             
OPTNV2   CLI   0(R6),3                                                          
         BH    EFTL                                                             
         CLI   12(R6),C'*'         *PP MEANS ALL AGENCIES                       
         BNE   EIIF                                                             
         MVC   CTVKKEY+2(2),13(R6) SET PROGRAM                                  
OPTNV4   LA    R6,32(R6)                                                        
         MVI   FNDX,2                                                           
*                                  VALIDATE OPTION NUMBER                       
         CLI   1(R6),0             L'SECOND HALF                                
         BNE   EIIF                                                             
         TM    2(R6),X'80'         T'FIRST HALF                                 
         BZ    EFNN                                                             
         OC    4(4,R6),4(R6)       V'FIRST HALF                                 
         BZ    EFLM                                                             
         OC    4(3,R6),4(R6)                                                    
         BNZ   EFTB                                                             
         MVC   CTVKNUM,7(R6)       SET OPTION NUMBER                            
         B     KEYVALX                                                          
         EJECT                                                                  
*              VALIDATE VALUE KEY                                               
*                                                                               
VALUVAL  NTR1                                                                   
         MVC   VALKIS,=CL31'KEY IS 1-4 CHARACTERS'                              
         OI    VALKISH+6,X'80'                                                  
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         CLI   FLDH+5,4                                                         
         BH    EFTL                                                             
         MVC   CTVKKEY,FLD         SET KEY                                      
         B     KEYVALX                                                          
*                                                                               
KEYVALX  MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         XIT1                                                                   
         EJECT                                                                  
*              DISPLAY A VALUE RECORD                                           
*                                                                               
DISPREC  BAS   RE,DISREC                                                        
         B     EXIT                                                             
         EJECT                                                                  
*              ADD/CHANGE A VALUE RECORD                                        
*                                                                               
DATAVAL  CLI   ACTN,ADD                                                         
         BNE   *+8                                                              
         MVI   VALPAGE,PAGEONE     FORCE PAGE ONE WHEN ADDING                   
         OI    VALPAGEH+6,X'80'    TRANSMIT                                     
         MVI   ADD2ND,NO           ADD 2ND RECORD, SET TO NO                    
         CLI   VALPAGE,PAGEONE     ON PAGE ONE ?                                
         BNE   DATAV010            NO                                           
         MVI   TEMP,0              BUILD BASIC RECORD                           
         GOTO1 ABLDREC                                                          
         CLI   ACTN,ADD            ACTION ADD ?                                 
         BE    DATAV020            YES                                          
*                                  NO, ACTION CHANGE                            
DATAV001 CLI   TWOREC,YES          2ND RECORD EXISTS                            
         BNE   DATAV018            NO                                           
         L     R4,AIO1             YES, MOVE ELEMENTS TO FIRST RECORD           
         LA    R5,CTVDATA          R5=A(ELEMENT)                                
         L     R4,AREC             RESET FOR LATER                              
         SR    R6,R6                                                            
DATAV002 CLI   0(R5),0             EOR ?                                        
         BE    DATAV018            FINISHED 1ST RECORD                          
         IC    R6,1(,R5)           GET LENGTH                                   
         CLI   0(R5),X'A1'         FIND X'A1' ELEMENTS                          
         BNE   DATAV005            NO                                           
         EXMVC R6,TEMP,0(R5)       DON'T SUBTRACT 1 FOR EX                      
         GOTO1 APUTEL              ADD TO 1ST RECORD                            
*                                                                               
DATAV005 AR    R5,R6               GO TO NEXT ELEMENT                           
         B     DATAV002                                                         
*                                                                               
DATAV010 SR    R1,R1               DELETE X'01' & X'A0' FROM 1ST REC            
         LA    R5,CTVDATA          R5=A(ELEMENT)                                
*                                                                               
DATAV012 CLI   0(R5),0             EOR ?                                        
         BE    DATAV015                                                         
         CLI   0(R5),X'01'         ACTIVITY ELEMENT ?                           
         BNE   *+8                                                              
         MVI   0(R5),X'FF'         MARK TO DELETE                               
         CLI   0(R5),X'A0'         MEANING  ELEMENT ?                           
         BNE   *+8                                                              
         MVI   0(R5),X'FF'         MARK TO DELETE                               
         IC    R1,1(,R5)           GET ELEMENT LENGTH                           
         AR    R5,R1                                                            
         B     DATAV012                                                         
*                                                                               
DATAV015 MVI   TEMP,X'FF'                                                       
         GOTO1 ADELEL              DELETE MARKED ELEMENTS                       
*                                                                               
DATAV018 MVC   KEY,CTVREC                                                       
         MVI   KEY+(CTVKPAGE-CTVREC),1                                          
         MVC   AREC,AIO1           CHANGE IO AREA                               
         MVI   TEMP,0              BUILD BASIC RECORD                           
         GOTO1 ABLDREC             BUILD 2ND RECORD                             
         MVC   AREC,SVREC                RESTORE IO AREA                        
         MVI   KEY+(CTVKPAGE-CTVREC),0   RESET   KEY                            
*                                  BUILD & ADD MEANING ELEMENT                  
DATAV020 LA    R1,VALMEANH                                                      
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
*                                                                               
         USING CTVNMD,R5           ADD MEANING ELEMENT                          
         LA    R5,TEMP                                                          
         XC    TEMP,TEMP                                                        
         MVI   CTVNMEL,X'A0'                                                    
         MVC   CTVNMNAM,FLD                                                     
         ZIC   RE,FLDH+5                                                        
         LA    RE,2(RE)                                                         
         STC   RE,CTVNMLEN         ADD ELEMENT TO 1ST RECORD                    
         GOTO1 LPUTEL                                                           
         BZ    EXIT                                                             
         MVC   AREC,AIO1           ADD ELEMENT TO 2ND RECORD TOO                
         GOTO1 APUTEL                                                           
         MVC   AREC,SVREC          RESTORE IO                                   
         BZ    EXIT                                                             
*                                  BUILD & ADD VALUE ELEMENTS                   
         USING LINED,R6                                                         
         USING CTVVLD,R5                                                        
         LA    R6,VALVALH                                                       
DATAV040 CLI   LINCHARH,13         END OF TWA                                   
         BL    DATAV180                                                         
*                                  VALIDATE VALUE                               
         LA    R1,LINCHARH                                                      
         GOTO1 AFVAL                                                            
         BNZ   DATAV060                                                         
         CLI   LINDEFNH+5,0                                                     
         BE    DATAV160            MISSING INPUT FIELD IF DEFINITION            
         B     EMIF                HAS BEEN INPUT                               
*                                                                               
DATAV060 XC    TEMP,TEMP                                                        
         MVI   CTVVLEL,X'A1'                                                    
         MVI   CTVVLTYP,C'A'                                                    
         MVC   CTVVLCHR,FLD                                                     
         CLI   FLDH+5,1            ONE BYTE INPUT DEFAULTS TO CHARACTER         
         BE    DATAV140                                                         
         GOTO1 VSCANNER,DMCB,FLDH,WORK                                          
         CLI   4(R1),1                                                          
         BNE   EIIF                                                             
*                                  ELSE INPUT IS PREFIX=VALUE                   
         CLI   WORK,1                                                           
         BNE   EIIF                                                             
         CLI   WORK+1,0                                                         
         BE    EIIF                                                             
         CLI   WORK+12,C'A'        CHARACTER                                    
         BE    DATAV100                                                         
         CLI   WORK+12,C'N'        NUMERIC                                      
         BE    DATAV110                                                         
         CLI   WORK+12,C'H'        HEX                                          
         BE    DATAV120                                                         
         B     EIIF                                                             
*                                  VALIDATE CHARACTER                           
DATAV100 CLI   WORK+1,1                                                         
         BH    EFTL                                                             
         MVC   CTVVLTYP,WORK+12                                                 
         MVC   CTVVLCHR,WORK+22                                                 
         B     DATAV140                                                         
*                                  VALIDATE NUMBER                              
DATAV110 TM    WORK+3,X'80'                                                     
         BZ    EFNN                                                             
         OC    WORK+8(3),WORK+8                                                 
         BNZ   EFTB                                                             
         MVC   CTVVLTYP,WORK+12                                                 
         MVC   CTVVLCHR,WORK+11                                                 
         B     DATAV140                                                         
*                                  VALIDATE HEX                                 
DATAV120 CLI   WORK+1,2                                                         
         BH    EFTL                                                             
         BL    EFTS                                                             
         TM    WORK+3,X'20'                                                     
         BZ    EFNH                                                             
         GOTO1 VHEXIN,DMCB,WORK+22,CTVVLCHR,2                                   
         MVC   CTVVLTYP,WORK+12                                                 
*                                  VALIDATE DEFINITION                          
DATAV140 LA    R1,LINDEFNH                                                      
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         MVC   CTVVLNAM,FLD                                                     
         ZIC   RE,FLDH+5                                                        
         LA    RE,4(RE)                                                         
         STC   RE,CTVVLLEN                                                      
         GOTO1 LPUTEL              PUT ELEMENT TO RECORD                        
         BZ    EXIT                                                             
*                                  WRITE/ADD RECORD SET NEXT ACTION             
DATAV160 LA    R6,LINLEN(R6)       BUMP TO NEXT INPUT LINE                      
         B     DATAV040                                                         
*                                  WRITE/ADD RECORD SET NEXT ACTION             
DATAV180 LA    R5,CTVDATA          FIRST ELEMENT                                
         SR    R6,R6                                                            
         SR    RF,RF                                                            
*                                                                               
DATAV200 CLI   0(R5),0             EOR                                          
         BE    DATAV300                                                         
         CLI   0(R5),X'A1'         VALUE   ELEMENTS                             
         BNE   DATAV280                                                         
         AHI   R6,1                                                             
         CHI   R6,30                                                            
         BNH   DATAV280            ONLY 30 ALLOWED ON ONE RECORD                
*                                                                               
         SR    RF,RF               MOVE ELEMENT TO RECORD                       
         IC    RF,1(,R5)                                                        
         BCTR  RF,0                                                             
         EXMVC RF,TEMP,0(R5)       MOVE TO 2ND RECORD                           
*                                                                               
         MVC   AREC,AIO1           MOVE ELEMENT TO 2ND RECORD                   
         GOTO1 APUTEL                                                           
         MVC   AREC,SVREC          RESTORE IO                                   
*                                                                               
         MVI   0(R5),X'FF'         MARK DELETED FROM 1ST RECORD                 
         CLI   TWOREC,YES          WAS THERE A 2ND RECORD ON FILE ?             
         BE    DATAV280            YES                                          
         MVI   ADD2ND,YES          NO, SET TO SAY ADD 2ND RECORD                
*                                                                               
DATAV280 SR    RF,RF                                                            
         IC    RF,1(,R5)                                                        
         AR    R5,RF                                                            
         B     DATAV200                                                         
*                                                                               
DATAV300 MVI   TEMP,X'FF'          DELETE ANY UN-WANTED ELEMENTS                
         GOTO1 LDELEL                                                           
         GOTO1 ABLDACT             ADD ACTIVITY ELEMENT                         
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                                                               
         MVC   AREC,AIO1           PUT ACTIVITY ELEMENT ON 2ND RECORD           
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
         MVC   AREC,SVREC          RESTORE ORIGINAL IO                          
         BZ    EXIT                                                             
*                                                                               
         L     RF,AADD             ADD 1ST RECORD                               
         CLI   ACTN,ADD                                                         
         BE    *+8                                                              
         L     RF,AWRITE           CHANGE 1ST RECORD                            
         MVC   KEY,CTVREC                                                       
         MVI   KEY+(CTVKPAGE-CTVREC),0    SET KEY PAGE 1                        
         LA    R1,VALSYSH                                                       
         ST    R1,FADR                                                          
         MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
*                                                                               
         L     RF,AADD             ADD 2ND RECORD                               
         CLI   ADD2ND,YES                                                       
         BE    DATAV400                                                         
         CLI   TWOREC,YES                                                       
         BNE   DATAV450                   NO RECORD TO CHANGE                   
         MVI   KEY+(CTVKPAGE-CTVREC),1    SET KEY PAGE 2                        
         MVI   UPDATE,YES                 READ FOR UPDATE                       
         GOTO1 AREAD               GET 2ND ORIGINAL RECORD FOR UPDATE           
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        FOUND                                        
         BZ    *+6                 YES                                          
         DC    H'00'               NO, SHOULD BE THERE                          
*                                                                               
         SR    RF,RF                                                            
         L     R4,AIO1                                                          
         LA    R5,CTVDATA                                                       
         OI    CTVSTAT,X'80'       MARK DELETED NO MATER WHAT                   
DATAV372 CLI   0(R5),0             EOR                                          
         BE    DATAV380                                                         
         CLI   0(R5),X'A1'         VALUE ELEMENT ?                              
         BE    DATAV378                                                         
         IC    RF,1(,R5)                                                        
         AR    R5,RF                                                            
         B     DATAV372                                                         
*                                                                               
DATAV378 NI    CTVSTAT,TURNOFF-X'80'    RESTORE, HAS VALUE ELEMENT              
DATAV380 L     RF,AWRITE                                                        
         L     R4,AREC                  RESTORE R4                              
*                                                                               
DATAV400 MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         MVI   KEY+(CTVKPAGE-CTVREC),1    SET KEY PAGE 2                        
         MVC   AREC,AIO1                                                        
         BASR  RE,RF                      WRITE OR ADD RECORD                   
         MVC   AREC,SVREC                                                       
         MVI   KEY+(CTVKPAGE-CTVREC),0    RESET KEY PAGE 1                      
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         GOTO1 AREAD               GET ORIGINAL RECORD BACK INTO AREC           
         BZ    EIIO                                                             
         MVI   TWOREC,YES                                                       
*                                                                               
DATAV450 MVI   NACTN,OKDEL+OKCHA                                                
         LA    R1,BASACTNH         SET CURSOR & EXIT                            
         ST    R1,FADR                                                          
         BAS   RE,DISREC                                                        
         B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
DISREC   NTR1                                                                   
         CLI   PFKEY,PFKUP         PAGE UP                                      
         BNE   *+8                                                              
         MVI   VALPAGE,PAGEONE     PAGE 1                                       
         CLI   PFKEY,PFKDOWN       PAGE DOWN                                    
         BNE   *+8                                                              
         MVI   VALPAGE,PAGETWO     PAGE 2                                       
         MVI   PFKEY,0             RESET                                        
*                                                                               
         TWAXC VALVALH,PROT=Y                                                   
         OI    VALPAGEH+6,X'80'    TRANSMIT                                     
         L     R4,AREC                                                          
         CLI   VALPAGE,PAGEONE     PAGE ONE  ?                                  
         BE    DISP1                                                            
         L     R4,AIO1             DISPLAY PAGE 2 (RECORD 2)                    
         CLI   TWOREC,YES          IS THERE A 2ND RECORD                        
         BNE   DISPX               NO, SO NOTHING TO DISPLAY                    
*                                                                               
DISP1    XC    VALMEAN,VALMEAN     CLEAR THE TWA                                
         OI    VALMEANH+6,X'80'                                                 
         LA    R5,CTVDATA          R5=A(ELEMENT)                                
         LA    R6,VALVALH          R6=A(TWA OUTPUT LINE)                        
*                                                                               
         USING LINED,R6                                                         
DISP2    CLI   0(R5),0                                                          
         BE    DISPX                                                            
         CLI   0(R5),X'A0'         MEANING ELEMENT                              
         BE    DISP6                                                            
         CLI   0(R5),X'A1'         VALUE ELEMENT                                
         BE    DISP7                                                            
DISP4    ZIC   RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     DISP2                                                            
*                                  MEANING ELEMENT                              
         USING CTVNMD,R5                                                        
DISP6    ZIC   RE,CTVNMLEN                                                      
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         B     DISP4                                                            
         MVC   VALMEAN(0),CTVNMNAM                                              
*                                  VALUE ELEMENT                                
         USING CTVVLD,R5                                                        
DISP7    ZIC   RE,CTVVLLEN                                                      
         SHI   RE,5                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   LINDEFN(0),CTVVLNAM DEFINITION                                   
         MVC   LINCHAR(1),CTVVLCHR VALUE                                        
         CLI   CTVVLTYP,C'A'       IF A CHARACTER FIELD DON'T SHOW              
         BE    DISP10              PREFIX                                       
         CLI   CTVVLTYP,C'N'                                                    
         BNE   DISP8A                                                           
         MVC   LINCHAR(2),=C'N='                                                
         EDIT  (B1,CTVVLCHR),(3,TEMP),ALIGN=LEFT                                
         OI    TEMP,X'F0'                                                       
         MVC   LINCHAR+2(3),TEMP                                                
         B     DISP10                                                           
*                                                                               
DISP8A   CLI   CTVVLTYP,C'H'                                                    
         BNE   DISP8B                                                           
         MVC   LINCHAR(2),=C'H='                                                
         GOTO1 VHEXOUT,DMCB,CTVVLCHR,LINCHAR+2,1,=C'TOG'                        
         B     DISP10                                                           
*                                                                               
DISP8B   DC    H'0'                                                             
DISP10   LA    R6,LINLEN(R6)       BUMP TO NEXT TWA LINE                        
         B     DISP4                                                            
*                                                                               
DISPX    TM    CTVSTAT,X'80'       SET NEXT ACTION AND EXIT                     
         BO    *+12                                                             
         MVI   NACTN,OKDEL+OKCHA                                                
         B     *+8                                                              
         MVI   NACTN,OKRES                                                      
         LA    R1,VALMEANH                                                      
         ST    R1,FADR                                                          
         MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
LPUTEL   NTR1                      LOCAL PUTEL FOR CTFBIG                       
         L     R5,AREC                                                          
         MVI   FERN,X'FF'                                                       
         GOTO1 VHELLO,DMCB,(C'P',=C'CTFBIG  '),(R5),TEMP                        
         CLI   DMCB+12,0                                                        
         BE    *+8                                                              
         MVI   FERN,68                                                          
         CLI   FERN,68                                                          
         B     EXIT                                                             
         EJECT                                                                  
LDELEL   NTR1                      LOCAL DELEL FOR CTFBIG                       
         L     R5,AREC                                                          
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFBIG  '),(TEMP,(R5)),0                    
         B     EXIT                                                             
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
*              LITERALS ETC.                                                    
*                                                                               
         LTORG                                                                  
*                                  TABLE OF SYSTEMS                             
SYSTBL   DS    0CL11                                                            
*&&US                                                                           
*********DC    C'SPOT    ',C'S',X'E002'                                         
*********DC    C'PRINT   ',C'P',X'E004'                                         
         DC    C'ACCOUNT ',C'A',X'E006'                                         
*********DC    C'REP     ',C'R',X'E008'                                         
*&&                                                                             
*&&UK                                                                           
         DC    C'ACCOUNT ',C'A',X'E006'                                         
         DC    C'MEDIA   ',C'M',X'E004'                                         
*&&                                                                             
         DC    X'00'                                                            
*                                  TABLE OF RECORD TYPES                        
TYPETBL  DS    0CL6                                                             
         DC    C'F',C'F',X'81',AL3(FILTVAL)                                     
         DC    C'O',C'O',X'41',AL3(OPTNVAL)                                     
         DC    C'V',C'V',X'20',AL3(VALUVAL)                                     
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER TEMP W/S                                          
*                                                                               
WORKD    DSECT                                                                  
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
TURNOFF  EQU   X'FF'                                                            
PAGEONE  EQU   C'1'                                                             
PAGETWO  EQU   C'2'                                                             
PFKUP    EQU   7                                                                
PFKDOWN  EQU   8                                                                
*                                                                               
RELO     DS    A                                                                
AKEYRTN  DS    A                                                                
AIO1     DS    A                                                                
SVREC    DS    A                                                                
TYPEACTN DS    X                                                                
SYSNUM   DS    X                                                                
AGYALPH  DS    CL2                                                              
AGYNO    DS    X                                                                
TWOREC   DS    CL1                 TWO RECORDS    (Y/N)                         
ADD2ND   DS    CL1                 ADD 2ND RECORD (Y/N)                         
PFKEY    DS    XL1                 PFKEY OR ENTER = 0                           
IO1      DS    XL2000                                                           
WORKEND  EQU   *                                                                
*                                                                               
*              DSECT TO COVER TWA LINE                                          
*                                                                               
LINED    DSECT                                                                  
LINCHARH DS    CL8                                                              
LINCHAR  DS    CL5                                                              
LINDEFNH DS    CL8                                                              
LINDEFN  DS    CL30                                                             
LINLEN   EQU   *-LINED                                                          
*                                                                               
* FATWA                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATWA                                                          
         PRINT ON                                                               
         EJECT                                                                  
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
* CTLFMACTNS                                                                    
       ++INCLUDE CTLFMACTNS                                                     
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMECD                                                                      
       ++INCLUDE CTLFMECD                                                       
* FATIOB                                                                        
       ++INCLUDE FATIOB                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040CTLFM13   05/01/02'                                      
         END                                                                    
