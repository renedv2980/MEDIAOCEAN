*          DATA SET CTLFM05    AT LEVEL 022 AS OF 08/22/00                      
*PHASE TA0205A                                                                  
*INCLUDE SCINKEY                                                                
         TITLE 'CTLFM05 - CONTROL FILE MAINT - INDINFO RECORD'                  
CTLFM05  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 220,**LFM5**                                                     
         USING WRKD,RC             RC=A(OVERLAY W/S)                            
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         LA    R4,IOAREA                                                        
         USING CTIREC,R4           R4=A(ID RECORD)                              
         EJECT                                                                  
KEYVAL   LA    R1,BASACTNH         ADD NOT AVAILABLE FOR 05                     
         ST    R1,FADR                                                          
         CLI   ACTN,ADD                                                         
         BE    EFNV                                                             
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         LA    R1,IDPIDAH          VALIDATE ID                                  
         GOTO1 AFVAL                                                            
         BZ    EXIT                MISSING ID                                   
         CLI   FLDH+5,3                                                         
         BL    EFTS                ID TOO SHORT                                 
         MVC   CTIKID,FLD                                                       
         MVC   KEY,CTIKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         MVC   KEYSAVE,KEY                                                      
         CLI   ACTN,CHANGE                                                      
         BNE   KEYV2                                                            
         CLC   KEY,LKEY                                                         
         BE    KEYV2                                                            
         MVI   ACTN,DISPLAY        IF KEY CHANGED SET ACTN TO DISPLAY           
*                                                                               
KEYV2    DS    0H                                                               
         CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU                                      
         GOTO1 AREAD               READ ID                                      
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        TEST FOR N/F                                 
         BO    ERNF                                                             
         TM    DMCB+8,X'02'        TEST FOR DELETED                             
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY                                                     
         BNE   ERNF                                                             
         CLI   ACTN,CHANGE         VALIDATE ACTIONS                             
         BE    PROCHA                                                           
         CLI   ACTN,DISPLAY                                                     
         BE    PRODIS                                                           
         B     EIAS                                                             
         EJECT                                                                  
*              BUILD DESTINATION DETAIL ELEMENT                                 
*                                                                               
PROCHA   MVI   TEMP,X'01'          STRIP-OFF ALL ELEMENTS HANDLED               
         GOTO1 ADELEL              BY THIS OVERLAY                              
         MVI   TEMP,X'33'                                                       
         GOTO1 ADELEL                                                           
         MVI   TEMP,X'34'                                                       
         GOTO1 ADELEL                                                           
         MVI   TEMP,X'36'                                                       
         GOTO1 ADELEL                                                           
         MVI   TEMP,X'30'                                                       
         GOTO1 ADELEL                                                           
         MVI   TEMP,X'3A'                                                       
         GOTO1 ADELEL                                                           
         MVI   TEMP,X'42'                                                       
         GOTO1 ADELEL                                                           
         MVI   TEMP,X'4C'                                                       
         GOTO1 ADELEL                                                           
         LA    R5,CTIDATA                                                       
         SR    R1,R1                                                            
         SPACE 2                                                                
PROCH2   CLI   0(R5),0             LOOK FOR ID# ELEMENT                         
         BNE   *+6                                                              
         DC    H'0'                DIE IF N/F                                   
         CLI   0(R5),X'02'                                                      
         BE    PROCH4                                                           
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     PROCH2                                                           
         SPACE 1                                                                
PROCH4   MVC   IDNUM,2(R5)         SAVE ID#                                     
         MVC   IDALP,FLD           AND NAME                                     
         MVI   WORK,0                                                           
         XC    TEMP,TEMP                                                        
         MVI   TEMP,X'30'                                                       
         MVI   TEMP+1,166                                                       
         LA    R7,TEMP                                                          
         USING CTDSTD,R7                                                        
         LA    R1,IDPDNAMH         VALIDATE DEST NAME                           
         LR    R8,R1                                                            
         GOTO1 AFVAL                                                            
         BZ    PROCH6                                                           
         OI    WORK,X'80'                                                       
         MVC   CTDSTNAM,FLD        NOVE NAME TO ELEMENT                         
         SPACE 1                                                                
PROCH6   LA    R1,IDPDADDH         VALIDATE DEST ADDR                           
         GOTO1 AFVAL                                                            
         BZ    PROCH8                                                           
         TM    WORK,X'80'          WAS NAME INPUT                               
         BO    PROCH10                                                          
         ST    R8,FADR             NAME NOT INPUT                               
         B     EMIF                                                             
         SPACE 1                                                                
PROCH8   TM    WORK,X'80'          ADDRESS NOT INPUT                            
         BO    EMIF                                                             
         B     PROCH12                                                          
         SPACE 1                                                                
PROCH10  MVC   CTDSTADD,FLD        MOVE ADDRESS TO ELEMENT                      
         MVC   CTDSTAD2,IDPDAD2                                                 
         MVC   CTDSTAD3,IDPDAD3                                                 
         OC    CTDSTAD2,SPACES                                                  
         OC    CTDSTAD3,SPACES                                                  
         OI    WORK,X'40'                                                       
         SPACE 1                                                                
PROCH12  LA    R1,IDPDLO1H         VALIDATE LOGO 1                              
         GOTO1 AFVAL                                                            
         BZ    PROCH14                                                          
         TM    WORK,X'C0'          WAS NAME/ADDRESS INPUT                       
         BO    PROCH16                                                          
         ST    R8,FADR             NAME NOT INPUT                               
         B     EMIF                                                             
         SPACE 1                                                                
PROCH14  TM    WORK,X'C0'          LOGO 1 NOT INPUT                             
         BO    EMIF                                                             
         B     PROCH18                                                          
         SPACE 1                                                                
PROCH16  MVC   CTDSTLG1,FLD        MOVE LOGO 1 TO ELEMENT                       
         OI    WORK,X'20'                                                       
         SPACE 1                                                                
PROCH18  LA    R1,IDPDLO2H         VALIDATE LOGO2                               
         GOTO1 AFVAL                                                            
         BZ    PROCH20                                                          
         TM    WORK,X'E0'          OTHERS INPUT                                 
         BO    *+12                                                             
         ST    R8,FADR             NAME NOT INPUT                               
         B     EMIF                                                             
         MVC   CTDSTLG2,FLD        MOVE LOGO 2 TO ELEMENT                       
         SPACE 1                                                                
PROCH20  LA    R1,IDPDPWRH         VALIDATE POWER CODE                          
         GOTO1 AFVAL                                                            
         BZ    PROCH22                                                          
         TM    WORK,X'E0'          OTHERS INPUT                                 
         BO    PROCH24                                                          
         ST    R8,FADR             NAME NOT INPUT                               
         B     EMIF                                                             
         SPACE 1                                                                
PROCH22  TM    WORK,X'E0'          POWER CODE NOT INPUT                         
         BO    EMIF                                                             
         B     PROCH26                                                          
         SPACE 1                                                                
PROCH24  MVC   CTDSTPOW,FLD        POWER CODE TO ELEMENT                        
         OI    WORK,X'10'                                                       
         SPACE 1                                                                
PROCH26  CLI   WORK,0                                                           
         BE    PROCH28                                                          
         GOTO1 APUTEL              ADD DEST DETAIL ELEMENT                      
         BZ    EXIT                                                             
         SPACE 1                                                                
PROCH28  LA    R1,IDPDSHAH         BUILD SHIPPING INFO ELEMENT                  
         GOTO1 AFVAL                                                            
         BZ    PROCH30                                                          
         XC    TEMP,TEMP                                                        
         MVC   TEMP(2),=X'4C06'                                                 
         MVI   TEMP+2,C'P'                                                      
         SR    R6,R6                                                            
         IC    R6,FLDH+5                                                        
*&&US                                                                           
         CLI   FLD,C'0'            CHECK FOR NUMERIC SHIPPING UNIT              
         BL    PROCH28A                                                         
         CLI   FLD,C'9'                                                         
         BH    PROCH28A                                                         
         GOTO1 ABLDSHP,DMCB,FLD,TEMP+6,(R6)   CALL SPECIAL ROUTINE              
         CLI   8(R1),0                                                          
         BE    EFTB                                                             
         ZIC   R6,8(R1)            R6 = LENGTH OF ELEMENT DATA                  
         B     PROCH28B                                                         
*&&                                                                             
*                                                                               
PROCH28A MVC   TEMP+6(60),FLD      OTHERWISE MOVE IN AS IS                      
*                                                                               
PROCH28B LA    R1,IDPDSHBH                                                      
         GOTO1 AFVAL                                                            
         BZ    PROCH29                                                          
         MVC   TEMP+66(60),FLD                                                  
         IC    R6,FLDH+5                                                        
         LA    R6,60(R6)                                                        
*                                                                               
PROCH29  LA    R6,6(R6)                                                         
         STC   R6,TEMP+1                                                        
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         EJECT                                                                  
*              BUILD ORIGIN DETAIL ELEMENT                                      
*                                                                               
PROCH30  MVI   WORK,0                                                           
         LA    R7,TEMP                                                          
         USING CTORGD,R7                                                        
         MVC   TEMP(2),=X'3644'                                                 
         LA    R1,IDPONAMH                                                      
         GOTO1 AFVAL                                                            
         BZ    *+14                                                             
         MVC   CTORGNAM,FLD        ORIGIN NAME                                  
         OI    WORK,X'80'                                                       
         LR    R8,R1                                                            
         LA    R1,IDPOADDH                                                      
         GOTO1 AFVAL                                                            
         BZ    PROCH32                                                          
         MVC   CTORGADD,FLD        ORIGIN ADDRESS                               
         TM    WORK,X'80'                                                       
         BO    PROCH34                                                          
         ST    R8,FADR                                                          
         B     EMIF                                                             
         SPACE 1                                                                
PROCH32  TM    WORK,X'80'                                                       
         BO    EMIF                                                             
         SPACE 1                                                                
PROCH34  CLI   WORK,0              DO NOT ADD ELEMENT IF FLDS N/I               
         BE    VALIDS                                                           
         GOTO1 APUTEL                                                           
         BNZ   VALIDS                                                           
         DC    H'0'                DIE IF CANT ADD ELEMENT                      
         EJECT                                                                  
*              VALIDATE & BUILD ID ELEMENTS                                     
*                                                                               
VALIDS   LA    RA,IDPDIDAH                                                      
         LA    R6,3                                                             
         SPACE 1                                                                
VALID2   MVI   FNDX,0                                                           
         LR    R1,RA                                                            
         GOTO1 AFVAL                                                            
         BZ    VALID4                                                           
         GOTO1 VSCANNER,DMCB,FADR,LINES                                         
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         MVC   NLINEST,4(R1)                                                    
         MVI   FNDX,1                                                           
         LA    R5,LINES                                                         
         B     VALID6                                                           
         SPACE 1                                                                
VALID4   ZIC   RE,0(RA)                                                         
         AR    RA,RE                                                            
         BCT   R6,VALID2                                                        
         B     VALPRT                                                           
         SPACE 1                                                                
VALID6   CLC   FNDX,NLINEST                                                     
         BH    VALID4                                                           
         LA    R4,REC                                                           
         LA    R7,TEMP                                                          
         USING CTVALD,R7                                                        
         MVC   TEMP(2),=X'3410'                                                 
         CLI   1(R5),0                                                          
         BNE   VALIDA                                                           
         CLI   0(R5),3                                                          
         BL    EFTS                                                             
         ST    R4,AREC                                                          
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID,12(R5)                                                    
         MVC   KEY,CTIKEY                                                       
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         LA    R1,CTIDATA                                                       
         SR    RE,RE                                                            
         SPACE 1                                                                
VALID8   CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),X'02'                                                      
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     VALID8                                                           
         MVC   CTVALNUM,2(R1)                                                   
         MVC   CTVALDST,12(R5)                                                  
         XC    CTVALSPR,CTVALSPR                                                
         LA    R1,IOAREA                                                        
         ST    R1,AREC                                                          
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         B     VALIDC                                                           
         SPACE 1                                                                
VALIDA   ZIC   R1,0(R5)            VALIDATE LIST-ID                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'LIST'   CHECK KEYWORD                                
         BNE   EIIF                                                             
         CLI   1(R5),6                                                          
         BH    EFTL                                                             
         LA    R8,REC              SWITCH I/O AREAS                             
         ST    R8,AREC                                                          
         USING CTWREC,R8                                                        
         XC    CTWKEY,CTWKEY       BUILD KEY OF LIST RECORD                     
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'I'                                                     
         MVC   CTWKID,22(R5)                                                    
         MVC   KEY,CTWKEY                                                       
         GOTO1 AREAD                                                            
         BZ    EIIO                CHECK RECORD FOUND/OK                        
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         XC    CTVALDST(16),CTVALDST                                            
         MVC   CTVALDST+2,22(R5)                                                
         LA    R1,IOAREA                                                        
         ST    R1,AREC                                                          
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         SPACE 1                                                                
VALIDC   DS    0H                                                               
         LA    R5,L'LINES(R5)                                                   
         SR    R1,R1                                                            
         IC    R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         B     VALID6                                                           
         EJECT                                                                  
*              VALIDATE & BUILD PRINTER ELEMENTS                                
*                                                                               
VALPRT   LA    RA,IDPDPTAH                                                      
         LA    R6,3                                                             
         LA    R4,IOAREA                                                        
         SPACE 1                                                                
VALPRT2  MVI   FNDX,0                                                           
         LR    R1,RA                                                            
         GOTO1 AFVAL                                                            
         BZ    VALPRT4                                                          
         GOTO1 VSCANNER,DMCB,FADR,LINES                                         
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         MVC   NLINEST,4(R1)                                                    
         MVI   FNDX,1                                                           
         LA    R5,LINES                                                         
         B     VALPRT6                                                          
         SPACE 1                                                                
VALPRT4  ZIC   RE,0(RA)                                                         
         AR    RA,RE                                                            
         BCT   R6,VALPRT2                                                       
         B     VALOUT                                                           
         SPACE 1                                                                
VALPRT6  CLC   FNDX,NLINEST                                                     
         BH    VALPRT4                                                          
         LA    R7,TEMP                                                          
         USING CTPRND,R7                                                        
         MVC   TEMP(2),=X'3A0B'                                                 
         TM    2(R5),X'80'                                                      
         BZ    VALPRT20                                                         
         CLC   4(4,R5),=F'255'                                                  
         BH    EFTB                                                             
         MVC   CTPRNNUM,7(R5)                                                   
         CLI   1(R5),4             ALLOW 4 CHR LUIDS                            
         BL    EFTS                                                             
         CLI   1(R5),8                                                          
         BH    EFTL                                                             
         MVC   CTPRNLIN(8),22(R5)                                               
         LA    R1,CTIDATA                                                       
         SR    RE,RE                                                            
         SPACE 1                                                                
VALPRT8  CLI   0(R1),0                                                          
         BE    VALPRT14                                                         
         CLI   0(R1),X'3A'                                                      
         BE    VALPRT12                                                         
         SPACE 1                                                                
VALPRT10 IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     VALPRT8                                                          
         SPACE 1                                                                
VALPRT12 CLC   CTPRNNUM,2(R1)                                                   
         BE    EDIF                                                             
         CLC   CTPRNLIN(8),3(R1)                                                
         BE    EDIF                                                             
         B     VALPRT10                                                         
         SPACE 1                                                                
VALPRT14 GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         LA    R5,L'LINES(R5)                                                   
         SR    R1,R1                                                            
         IC    R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         B     VALPRT6                                                          
         SPACE 1                                                                
VALPRT20 ZIC   R1,0(R5)            VALIDATE LIST-ID                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'LIST'   CHECK KEYWORD                                
         BNE   EFNN                                                             
         CLI   1(R5),6             VALIDATE LIST ENTRY                          
         BH    EFTL                                                             
         LA    R8,REC              SWITCH I/O AREAS                             
         ST    R8,AREC                                                          
         USING CTWREC,R8                                                        
         XC    CTWKEY,CTWKEY       BUILD KEY OF LIST RECORD                     
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'R'                                                     
         MVC   CTWKID,22(R5)                                                    
         MVC   KEY,CTWKEY                                                       
         GOTO1 AREAD                                                            
         BZ    EIIO                CHECK RECORD FOUND/OK                        
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         XC    CTPRNNUM(9),CTPRNNUM                                             
         MVC   CTPRNNUM+2(6),22(R5)                                             
         LA    R1,IOAREA                                                        
         ST    R1,AREC                                                          
         B     VALPRT14                                                         
         EJECT                                                                  
*              VALIDATE & BUILD OUTPUT TYPE ELEMENT                             
*                                                                               
VALOUT   LA    R1,IDPOUTIH                                                      
         GOTO1 AFVAL                                                            
         BZ    VALOTH                                                           
         CLI   FLDH+5,1                                                         
         BL    EFTS                                                             
         LA    R4,REC                                                           
         USING CTOREC,R4           BUILD OUTPUT TYPE KEY                        
         XC    CTOKEY,CTOKEY                                                    
         MVI   CTOKEY,C'O'                                                      
         MVC   CTOKID,FLD          MOVE ID TO KEY                               
         MVC   KEY,CTOKEY                                                       
         ST    R4,AREC                                                          
         GOTO1 AREAD                                                            
         BZ    EIIO                DISK ERROR                                   
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         LA    R7,TEMP                                                          
         XC    TEMP,TEMP                                                        
         MVC   TEMP(2),=X'4210'                                                 
         USING CTOCOD,R7                                                        
         MVC   CTOCODE,FLD                                                      
         LA    R4,IOAREA                                                        
         ST    R4,AREC                                                          
         GOTO1 APUTEL              ADD OUTPUT CODE ELEMENT                      
         BZ    EXIT                                                             
         B     VALOTH                                                           
         EJECT                                                                  
* VALIDATE OTHER ID FIELDS                                                      
*                                                                               
VALOTH   DS    0H                                                               
*&&UK                                                                           
VALUKA   XC    TEMP,TEMP           SET UP UK AGENCY EXTRA DATA ELEMENT          
         MVC   TEMP(2),=X'3309'                                                 
         LA    R7,TEMP                                                          
         USING CTUKAD,R7                                                        
         GOTO1 AFVAL,IDPPLNOH                                                   
         MVC   CTUKALIN,FLD                                                     
         GOTO1 AFVAL,IDPIPAH                                                    
         MVC   CTUKAIPA,FLD                                                     
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
VALUKAX  EQU   *                                                                
*&&                                                                             
*&&US                                                                           
VALUSA   XC    TEMP,TEMP           SET UP US AGENCY EXTRA DATA ELEMENT          
         MVC   TEMP(2),=X'3309'                                                 
         LA    R7,TEMP                                                          
         USING CTUSAD,R7                                                        
VALUSA1  GOTO1 AFVAL,IDPDARPH      VALIDATE DARE PARTNER ID                     
         BZ    VALUSAX             IF NONE, THEN NO NEED FOR ROUTING            
         CLI   FLD,C'A'                                                         
         BL    EIIF                                                             
         CLI   FLD,C'Z'                                                         
         BH    EIIF                                                             
         MVC   CTUSADPI,FLD                                                     
VALUSA2  GOTO1 AFVAL,IDPDARRH      VALIDATE DARE ROUTING CODE                   
         BZ    VALUSA3             STILL NEED TO CHANGE THE RECORD              
         MVC   CTUSADRC,FLD                                                     
VALUSA3  GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
VALUSAX  EQU   *                                                                
*&&                                                                             
VALOTHX  B     VALEND                                                           
         EJECT                                                                  
* WRITE-BACK UPDATED ID RECORDS                                                 
*                                                                               
VALEND   MVC   KEY,KEYSAVE                                                      
         LA    R4,IOAREA                                                        
         ST    R4,AREC                                                          
         USING CTIREC,R4                                                        
         LA    R1,IDPIDAH                                                       
         ST    R1,FADR                                                          
         GOTO1 ABLDACT             BUILD ACTIVITY ELEMENT                       
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVI   TEMP,X'02'                                                       
         GOTO1 ADELEL                                                           
         MVC   TEMP(2),=X'020C'                                                 
         MVC   TEMP+2(10),IDALP                                                 
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTILEN-2(2),IDNUM                                                
         MVC   KEY,CTIKEY                                                       
         LA    R5,REC              DO RFU FOR PASSIVE RECORD                    
         ST    R5,AREC                                                          
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         ST    R4,AREC             RESTORE I/O PARMS                            
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVI   FERN,X'FF'                                                       
         MVI   NACTN,OKCHA+OKDEL                                                
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
*              DISPLAY ID RECORD                                                
*                                                                               
PRODIS   TWAXC IDPDNAMH                                                         
         LA    R5,CTIDATA          R5=A(FIRST ELEMENT)                          
         XC    DUB(24),DUB                                                      
         LA    R1,REC                                                           
         ST    R1,DUB1                                                          
         LA    R1,REC+500                                                       
         ST    R1,DUB2                                                          
         SPACE 1                                                                
DISP2    CLI   0(R5),0             END OF RECORD                                
         BE    DISPEND                                                          
         CLI   0(R5),X'30'         DESTINATION DETAIL                           
         BE    DISP6                                                            
         CLI   0(R5),X'33'         UK AGENCY VALUES/US DARE INFO                
         BE    DISP20                                                           
         CLI   0(R5),X'34'         DESTINATION ID                               
         BE    DISP12                                                           
         CLI   0(R5),X'36'         ORIGIN DETAIL                                
         BE    DISP10                                                           
         CLI   0(R5),X'3A'         PRINTER ID                                   
         BE    DISP14                                                           
         CLI   0(R5),X'42'         OUTPUT TYPE                                  
         BE    DISP16                                                           
         CLI   0(R5),X'4C'         SHIPPING INFO                                
         BE    DISP18                                                           
         SPACE 1                                                                
DISP4    SR    RF,RF                                                            
         IC    RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     DISP2                                                            
         EJECT                                                                  
*              DISPLAY DESTINATION DETAIL ELEMENT                               
*                                                                               
DISP6    LR    R7,R5                                                            
         USING CTDSTD,R7                                                        
         MVC   IDPDNAM,CTDSTNAM                                                 
         MVI   IDPDNAMH+5,33                                                    
         MVI   IDPDNAMH+7,33                                                    
         OI    IDPDNAMH+6,X'80'                                                 
         MVC   IDPDADD,CTDSTADD                                                 
         MVI   IDPDADDH+5,33                                                    
         MVI   IDPDADDH+7,33                                                    
         OI    IDPDADDH+6,X'80'                                                 
         MVC   IDPDAD2,SPACES                                                   
         MVI   IDPDAD2H+5,33                                                    
         MVI   IDPDAD2H+7,33                                                    
         OI    IDPDAD2H+6,X'80'                                                 
         MVC   IDPDAD3,SPACES                                                   
         MVI   IDPDAD3H+5,33                                                    
         MVI   IDPDAD3H+7,33                                                    
         OI    IDPDAD3H+6,X'80'                                                 
         CLI   CTDSTLEN,166                                                     
         BL    DISP7                                                            
         MVC   IDPDAD2,CTDSTAD2                                                 
         MVC   IDPDAD3,CTDSTAD3                                                 
DISP7    MVC   IDPDLO1,CTDSTLG1                                                 
         MVI   IDPDLO1H+5,7                                                     
         MVI   IDPDLO1H+7,7                                                     
         OI    IDPDLO1H+6,X'80'                                                 
         OC    CTDSTLG2,CTDSTLG2                                                
         BZ    DISP8                                                            
         MVC   IDPDLO2,CTDSTLG2                                                 
         MVI   IDPDLO2H+5,7                                                     
         MVI   IDPDLO2H+7,7                                                     
         OI    IDPDLO2H+6,X'80'                                                 
         SPACE 1                                                                
DISP8    MVC   IDPDPWR,CTDSTPOW                                                 
         MVI   IDPDPWRH+5,4                                                     
         MVI   IDPDPWRH+7,4                                                     
         OI    IDPDPWRH+6,X'80'                                                 
         B     DISP4                                                            
*                                                                               
*              DISPLAY ORIGIN DETAIL ELEMENT                                    
*                                                                               
DISP10   LR    R7,R5                                                            
         USING CTORGD,R7                                                        
         MVC   IDPONAM,CTORGNAM                                                 
         MVI   IDPONAMH+5,33                                                    
         MVI   IDPONAMH+7,33                                                    
         OI    IDPONAMH+6,X'80'                                                 
         MVC   IDPOADD,CTORGADD                                                 
         MVI   IDPOADDH+5,33                                                    
         MVI   IDPOADDH+7,33                                                    
         OI    IDPOADDH+6,X'80'                                                 
         B     DISP4                                                            
         EJECT                                                                  
*              ADD ID TO DISPLAY STREAM                                         
*                                                                               
DISP12   LR    R7,R5                                                            
         USING CTVALD,R7                                                        
         LM    RE,RF,DUB1                                                       
         MVC   0(12,RE),=CL12' '                                                
         MVC   0(10,RE),CTVALDST                                                
         OC    0(2,RE),0(RE)                                                    
         BNZ   *+10                                                             
         MVC   0(2,RE),=C'L='                                                   
         LA    RE,12(RE)                                                        
         LA    RF,1(RF)                                                         
         STM   RE,RF,DUB1                                                       
         B     DISP4                                                            
*                                                                               
*              ADD PRINTER TO STREAM                                            
*                                                                               
DISP14   LR    R7,R5                                                            
         USING CTPRND,R7                                                        
         LM    RE,RF,DUB2                                                       
         MVC   0(12,RE),=CL12' '                                                
         OC    CTPRNNUM(2),CTPRNNUM                                             
         BZ    DISP14A                                                          
         EDIT  (B1,CTPRNNUM),(3,0(RE)),ALIGN=LEFT                               
         LR    R6,RE                                                            
         AR    R6,R0                                                            
         MVI   0(R6),C'='                                                       
         MVC   1(8,R6),CTPRNLIN                                                 
DISP14B  LA    RE,12(RE)                                                        
         LA    RF,1(RF)                                                         
         STM   RE,RF,DUB2                                                       
         B     DISP4                                                            
         SPACE 1                                                                
DISP14A  MVC   0(2,RE),=C'L='                                                   
         MVC   2(6,RE),CTPRNLIN+1                                               
         B     DISP14B                                                          
         EJECT                                                                  
*                                                                               
*              DISPLAY OUTPUT TYPE ELEMENT                                      
*                                                                               
DISP16   LR    R7,R5                                                            
         USING CTOCOD,R7                                                        
         MVC   IDPOUTI,CTOCODE                                                  
         MVI   IDPOUTIH+5,10                                                    
         MVI   IDPOUTIH+7,10                                                    
         OI    IDPOUTIH+6,X'80'                                                 
         B     DISP4                                                            
*                                                                               
*              DISPLAY SHIPPING INFO ELEMENT                                    
*                                                                               
DISP18   LR    R7,R5                                                            
         USING CTSHPD,R7                                                        
         SR    R8,R8                                                            
         IC    R8,CTSHPLEN                                                      
         SH    R8,=H'6'                                                         
         SR    R6,R6                                                            
         CH    R8,=H'60'                                                        
         BNH   *+12                                                             
         LR    R6,R8                                                            
         LA    R8,60                                                            
         SR    R6,R8                                                            
*&&US                                                                           
         TM    6(R7),X'80'         TEST SPECIAL NUMERIC SHIPPING UNIT           
         BO    DISP18A                                                          
         GOTO1 ADISPSHP,DMCB,IDPDSHA,6(R7),(R8)   CALL SPECIAL ROUTINE          
         ZIC   R8,8(R1)                                                         
         B     DISP18B                                                          
*&&                                                                             
*                                                                               
DISP18A  BCTR  R8,0                OTHERWISE MOVE IN AS IS                      
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   IDPDSHA(0),6(R7)                                                 
         LA    R8,1(R8)                                                         
*                                                                               
DISP18B  STC   R8,IDPDSHAH+7                                                    
         LTR   R6,R6                                                            
         BZ    DISP4                                                            
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   IDPDSHB(0),66(R7)                                                
         LA    R6,1(R6)                                                         
         STC   R6,IDPDSHBH+7                                                    
         B     DISP4                                                            
*&&UK                                                                           
DISP20   LR    R7,R5               DISPLAY UK AGENCY VALUES                     
         USING CTUKAD,R7                                                        
         MVC   IDPPLNO,CTUKALIN                                                 
         OI    IDPPLNOH+6,X'80'                                                 
         MVC   IDPIPA,CTUKAIPA                                                  
         OI    IDPIPAH+6,X'80'                                                  
         B     DISP4                                                            
*&&                                                                             
*&&US                                                                           
DISP20   LR    R7,R5               DISPLAY US AGENCY VALUES                     
         USING CTUSAD,R7                                                        
         MVC   IDPDARP,CTUSADPI    DARE PARTNER ID                              
         OI    IDPDARPH+6,X'80'                                                 
         MVC   IDPDARR,CTUSADRC    DARE ROUTING CODE                            
         OI    IDPDARRH+6,X'80'                                                 
         B     DISP4                                                            
*&&                                                                             
         EJECT                                                                  
*              DISPLAY PRINTER/ID CHAINS                                        
*                                                                               
DISPEND  L     R6,DUB1+4                                                        
         LTR   R6,R6                                                            
         BZ    DISPEN0                                                          
         GOTO1 =V(SCINKEY),DMCB,(3,IDPDIDAH),(12,REC),(R6),RR=RB                
         SPACE 1                                                                
DISPEN0  L     R6,DUB2+4                                                        
         LTR   R6,R6                                                            
         BZ    DISPEN1                                                          
         GOTO1 =V(SCINKEY),DMCB,(3,IDPDPTAH),(12,REC+500),(R6),RR=RB            
         SPACE 1                                                                
DISPEN1  DS    0H                                                               
         SPACE 1                                                                
         TM    CTISTAT,X'80'       SET NEXT ACTION                              
         BO    DISPEN2                                                          
         LA    R1,IDPDNAMH                                                      
         ST    R1,FADR                                                          
         MVI   NACTN,OKDEL+OKCHA                                                
         B     DISPXX                                                           
         SPACE 1                                                                
DISPEN2  MVI   NACTN,OKRES                                                      
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         SPACE 1                                                                
DISPXX   MVI   FNDX,0                                                           
         MVI   FERN,X'FF'                                                       
         B     EXIT                                                             
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
SPACES   DC    CL80' '                                                          
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER TEMP W/S                                          
*                                                                               
WRKD     DSECT                                                                  
IDNUM    DS    CL2                                                              
IDALP    DS    CL10                                                             
NLINESA  DS    CL1                                                              
NLINESB  DS    CL1                                                              
NLINEST  DS    CL1                                                              
LINES    DS    20CL32                                                           
REC      DS    1024C                                                            
         SPACE 2                                                                
       ++INCLUDE CTLFMACTNS                                                     
         EJECT                                                                  
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMFAD                                                                      
       ++INCLUDE CTLFMFAD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022CTLFM05   08/22/00'                                      
         END                                                                    
