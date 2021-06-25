*          DATA SET CTLFM15    AT LEVEL 004 AS OF 05/01/02                      
*PHASE TA0215A                                                                  
*INCLUDE SCINKEY                                                                
CTLFM15  TITLE '- CONTROL FILE MAINT - SYSTEM LIST RECORDS'                     
CTLFM15  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**LFML**,RR=RE                                       
         USING WORKD,RC            RC=A(TEMP W/S)                               
         ST    RE,RELO                                                          
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         L     R4,AREC                                                          
         USING CTWREC,R4           R4=A(RECORD)                                 
*                                                                               
         L     RF,APARM                                                         
         L     RF,12(RF)                                                        
         MVC   VXSORT,CXSORT-COMFACSD(RF)                                       
         L     RF,=V(SCINKEY)                                                   
         A     RF,RELO                                                          
         ST    RF,VSCINKEY                                                      
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY FIELDS                                                 *         
***********************************************************************         
         SPACE 1                                                                
KEYVAL   XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
*                                                                               
         GOTO1 AFVAL,LSTTYPEH      VALIDATE LIST TYPE                           
         BZ    EXIT                                                             
         SR    R1,R1                                                            
         IC    R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         LA    R8,TYPTAB                                                        
         USING TYPTABD,R8          R8=A(LIST TYPE TABLE)                        
KEYV2    CLI   TYPTABD,TYPTEOTQ    TEST END OF TABLE                            
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         BE    KEYV4                                                            
         CLC   TYPTNAME(0),FLD                                                  
         LA    R8,TYPTABL(R8)                                                   
         B     KEYV2                                                            
*                                                                               
KEYV4    CLC   TYPTNAME,FLD                                                     
         BE    *+14                                                             
         MVC   LSTTYPE,TYPTNAME                                                 
         OI    LSTTYPEH+6,X'80'                                                 
         SR    RE,RE                                                            
         ICM   RE,3,TYPTROUT       RELOCATE ROUTINE ADDRESS                     
         LA    RE,CTLFM15(RE)                                                   
         ST    RE,AVALROUT                                                      
         MVC   CTWKREC,TYPTRTYP    SET RECORD TYPE                              
*                                                                               
         GOTO1 AFVAL,LSTIDH        VALIDATE LIST-ID                             
         BZ    KEYV6                                                            
         CLC   FLDH+5(1),TYPTKMIN                                               
         BL    EFTS                                                             
         CLC   FLDH+5(1),TYPTKMAX                                               
         BH    EFTL                                                             
         MVC   CTWKID,FLD                                                       
*                                                                               
KEYV6    TM    TYPTINDS,TYPTIAGA   TEST AGENCY ALPHA IN KEY                     
         BZ    *+10                                                             
         MVC   CTWKAGY,CTLFMFFD+(TWAAGY-TWAD)                                   
*                                                                               
         MVC   KEY,CTWKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         CLI   ACTN,CHANGE         IF ACTN=CHANGE AND KEY NEQ LKEY              
         BNE   KEYV8               SET ACTN=DISPLAY                             
         CLC   KEY,LKEY                                                         
         BE    *+8                                                              
         MVI   ACTN,DISPLAY                                                     
KEYV8    CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET READ-FOR-UPDATE INDICATOR                
         GOTO1 AREAD                                                            
         BZ    EXIT                                                             
         TM    DMCB+8,X'10'        TEST RECORD N/F                              
         BZ    KEYV10                                                           
         CLI   ACTN,ADD            ONLY VALID FOR ADD                           
         BE    DATAVAL                                                          
         B     ERNF                                                             
KEYV10   CLI   ACTN,ADD                                                         
         BE    ERAE                                                             
         TM    DMCB+8,X'02'        TEST RECORD DELETED                          
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY        ONLY VALID FOR DISPLAY                       
         BNE   ERNF                                                             
         CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
         B     DISPREC                                                          
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST RECORD                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISPREC  TWAXC LSTDESCH                                                         
         LA    R5,CTWDATA          R5=A(ELEMENT)                                
         USING CTLSTD,R5                                                        
*                                                                               
         TM    TYPTINDS,TYPTISRT   IF PRINTER LIST RECORD                       
         BZ    DISP8               SORT ENTRIES BY LOGICAL PRINTER #            
         SR    R0,R0                                                            
DISP4    CLI   CTLSTEL,CTLSTELQ    FIND FIRST LIST ELEMENT                      
         BE    *+14                                                             
         IC    R0,CTLSTLEN                                                      
         AR    R5,R0                                                            
         B     DISP4                                                            
         ST    R5,DMCB             SAVE POINTER TO FIRST LIST ELEMENT           
         LA    RF,1                                                             
DISP6    IC    R0,CTLSTLEN                                                      
         AR    R5,R0                                                            
         CLI   CTLSTEL,CTLSTELQ                                                 
         BNE   *+12                                                             
         LA    RF,1(RF)                                                         
         B     DISP6                                                            
*                                                                               
         ST    RF,DMCB+4                                                        
         L     R5,DMCB                                                          
         SR    RF,RF                                                            
         IC    RF,CTLSTLEN                                                      
         LR    R0,RF                                                            
         BCTR  R0,0                                                             
         GOTO1 VXSORT,DMCB,,,(RF),1,(R0)                                        
         LA    R5,CTWDATA                                                       
*                                                                               
DISP8    L     RE,ATIA                                                          
         ST    RE,ANXTNTRY                                                      
         XC    NUMNTRY,NUMNTRY                                                  
         SR    RE,RE                                                            
         IC    RE,TYPTIMAX         CALCULATE LENGTH OF EACH DISPLAY             
         SR    RF,RF               ENTRY                                        
         IC    RF,TYPTIHEX                                                      
         SLL   RF,1                                                             
         LA    RE,1(RE,RF)                                                      
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         LA    RE,1(RE)                                                         
         CH    RE,=H'8'            MINIMUM LENGTH OF 8                          
         BH    *+8                                                              
         LH    RE,=H'8'                                                         
         STC   RE,LENTRY                                                        
*                                                                               
DISP10   CLI   0(R5),0             END OF RECORD                                
         BE    DISP14                                                           
         CLI   0(R5),CTDSCELQ      DESCRIPTION ELEMENT                          
         BE    DISP16                                                           
         CLI   0(R5),CTLSTELQ      LIST ELEMENT                                 
         BE    DISP18                                                           
         CLI   0(R5),CTLINELQ      LIST INCLUDE ELEMENT                         
         BE    DISP20                                                           
*                                                                               
DISP12   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     DISP10                                                           
*                                                                               
DISP14   GOTO1 VSCINKEY,DMCB,(18,LSTL1H),(LENTRY,ATIA),NUMNTRY                  
         TM    CTWSTAT,X'80'       SET NEXT ACTION & EXIT                       
         BO    *+12                                                             
         MVI   NACTN,OKDEL+OKCHA                                                
         B     *+8                                                              
         MVI   NACTN,OKRES                                                      
         LA    R1,LSTDESCH                                                      
         ST    R1,FADR                                                          
         MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         B     EXIT                                                             
         EJECT                                                                  
         USING CTDSCD,R5                                                        
DISP16   SR    R1,R1               OUTPUT LIST DESCRIPTION                      
         IC    R1,CTDSCLEN                                                      
         SH    R1,=Y(CTDSC+1-CTDSCD)                                            
         EX    R1,*+4                                                           
         MVC   LSTDESC(0),CTDSC                                                 
         B     DISP12                                                           
*                                                                               
         USING CTLSTD,R5                                                        
DISP18   BAS   RE,GETNTRY          HANDLE LIST ENTRY                            
         SR    RE,RE                                                            
         IC    RE,TYPTIMAX                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R6),CTLSTDTA    MOVE ALPHA DATA                              
         SR    R0,R0                                                            
         ICM   R0,1,TYPTIHEX                                                    
         BZ    DISP12                                                           
         LA    RF,CTLSTDTA+1(RE)   AND HEX IF ANY                               
         LA    R6,0(R6,RE)                                                      
         CLI   0(R6),C' '                                                       
         BNE   *+8                                                              
         BCT   R6,*-8                                                           
         MVI   1(R6),C'/'                                                       
         GOTO1 VHEXOUT,DMCB,(RF),2(R6),(R0),=C'TOG'                             
         B     DISP12                                                           
*                                                                               
         USING CTLIND,R5                                                        
DISP20   BAS   RE,GETNTRY          HANDLE INCLUDE ELEMENT                       
         MVC   0(2,R6),=C'L='                                                   
         MVC   2(L'CTLINC,R6),CTLINC                                            
         B     DISP12                                                           
*                                                                               
GETNTRY  L     R6,ANXTNTRY         GET NEXT TIA OUTPUT ENTRY                    
         SR    RF,RF                                                            
         IC    RF,LENTRY                                                        
         LA    RF,0(R6,RF)                                                      
         ST    RF,ANXTNTRY                                                      
         L     RF,NUMNTRY                                                       
         LA    RF,1(RF)                                                         
         ST    RF,NUMNTRY                                                       
         SR    RF,RF                                                            
         IC    RF,LENTRY                                                        
         SH    RF,=H'2'                                                         
         MVI   0(R6),C' '          CLEAR THE ENTRY                              
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   1(0,R6),0(R6)                                                    
         EJECT                                                                  
***********************************************************************         
* CHANGE/ADD LIST RECORD                                              *         
***********************************************************************         
         SPACE 1                                                                
DATAVAL  MVI   TEMP,0              BUILD BASIC RECORD                           
         GOTO1 ABLDREC                                                          
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                                                               
         GOTO1 AFVAL,LSTDESCH      BUILD & ADD DESCRIPTION ELEMENT              
         BZ    EXIT                                                             
         LA    RF,TEMP                                                          
         USING CTDSCD,RF                                                        
         XC    TEMP,TEMP                                                        
         MVI   CTDSCEL,CTDSCELQ                                                 
         SR    R1,R1                                                            
         IC    R1,FLDH+5                                                        
         EX    R1,*+4                                                           
         MVC   CTDSC(0),FLD                                                     
         LA    R1,CTDSC-CTDSCD(R1)                                              
         STC   R1,CTDSCLEN                                                      
         DROP  RF                                                               
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         ZAP   ELS,=P'0'                                                        
         XC    NUMNTRY,NUMNTRY                                                  
         MVC   KEYSAVE,KEY                                                      
         LA    R7,LSTL1H                                                        
DATAV2   CLI   0(R7),0             END OF TWA                                   
         BE    DATAV20                                                          
         GOTO1 AFVAL,(R7)                                                       
         BZ    DATAV18             NOTHING ON THIS LINE                         
         GOTO1 VSCANNER,DMCB,FLDH,SCANBLK,C',=,/'                               
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         MVC   NLINES,4(R1)        SAVE NUMBER OF LINES INPUT                   
         MVI   FNDX,1                                                           
         LA    R6,SCANBLK          R6=A(SCAN BLOCK ENTRY)                       
*                                                                               
DATAV4   CLC   FNDX,NLINES         DONE WITH THIS LINE                          
         BH    DATAV18                                                          
         CLI   0(R6),0             L'FIRST HALF                                 
         BE    EIIF                                                             
         CLI   1(R6),0             L'SECOND HALF                                
         BNE   DATAV6                                                           
         CLC   12(2,R6),=C'L='     CHECK FOR INCLUDE PREFIX                     
         BNE   DATAV6                                                           
*                                                                               
         TM    TYPTINDS,TYPTINON   OPTION TO DISALLOW NESTING                   
         BO    EIRT                                                             
         SR    RE,RE                                                            
         IC    RE,0(R6)                                                         
         SH    RE,=H'2'                                                         
         STC   RE,DUB                                                           
         CLC   DUB(1),TYPTKMIN     CHECK L'INCLUDE KEY                          
         BL    EFTS                                                             
         CLC   DUB(1),TYPTKMAX                                                  
         BH    EFTL                                                             
         BAS   RE,VALLIST          READ INCLUDE RECORD                          
         CLI   FERN,X'FF'                                                       
         BNE   EXIT                                                             
         LA    R5,TEMP                                                          
         USING CTLIND,R5           BUILD INCLUDE ELEMENT                        
         XC    CTLINEL(20),CTLINEL                                              
         MVI   CTLINEL,CTLINELQ                                                 
         MVI   CTLINLEN,CTLINLNQ                                                
         MVC   CTLINC,14(R6)                                                    
         B     DATAV16                                                          
*                                  BUILD LIST ELEMENT                           
DATAV6   LA    R5,TEMP                                                          
         USING CTLSTD,R5                                                        
         XC    CTLSTEL(20),CTLSTEL                                              
         MVI   CTLSTEL,CTLSTELQ                                                 
         SR    RE,RE                                                            
         IC    RE,TYPTIMAX         CALCULATE L'ELEMENT                          
         SR    RF,RF                                                            
         IC    RF,TYPTIHEX                                                      
         SR    R1,R1                                                            
         IC    R1,TYPTICHR                                                      
         LA    RE,CTLSTDTA-CTLSTD(RE,RF)                                        
         AR    RE,R1                                                            
         STC   RE,CTLSTLEN                                                      
*                                                                               
         CLI   0(R6),2             L'FIRST HALF                                 
         BL    EIIF                                                             
         IC    RE,0(R6)                                                         
         LA    RF,12(R6)                                                        
         STC   RE,DUB              SET L'INPUT                                  
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   WORK(0),0(RF)       SET V'INPUT                                  
         CLC   DUB(1),TYPTIMIN     CHECK L'DATA                                 
         BL    EFTS                                                             
         CLC   DUB(1),TYPTIMAX                                                  
         BH    EFTL                                                             
         GOTO1 AVALROUT            VALIDATE DATA                                
         CLI   FERN,X'FF'                                                       
         BNE   EXIT                                                             
         CLI   TYPTIHEX,0          HEX REQD                                     
         BE    DATAV8                                                           
         CLI   1(R6),0             YES - ANY INPUT                              
         BE    EMIF                                                             
         TM    1(R6),1                                                          
         BNZ   EFNH                                                             
         TM    3(R6),X'20'                                                      
         BZ    EFNH                                                             
         SR    RE,RE                                                            
         IC    RE,1(R6)            YES - CHECK VALID HEX                        
         SRL   RE,1                                                             
         STC   RE,DUB                                                           
         CLC   DUB(1),TYPTIHEX     CHECK L'HEX INPUT                            
         BL    EFTS                                                             
         BH    EFTL                                                             
         B     DATAV10                                                          
*                                                                               
DATAV8   CLI   1(R6),0                                                          
         BNE   EIIF                                                             
*                                                                               
DATAV10  SR    RE,RE               MOVE DATA TO ELEMENT                         
         SR    RE,RE                                                            
         IC    RE,TYPTIMAX                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   CTLSTDTA(0),WORK                                                 
         LA    R5,CTLSTDTA+1(RE)                                                
         CLI   TYPTIHEX,0                                                       
         BE    DATAV12                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         GOTO1 VHEXIN,DMCB,22(R6),(R5),(R0)                                     
         SR    RE,RE                                                            
         IC    RE,TYPTIHEX                                                      
         LA    R5,0(RE,R5)                                                      
*                                                                               
DATAV12  SR    RE,RE               MOVE OTHER DATA TO ELEMENT                   
         ICM   RE,1,TYPTICHR                                                    
         BZ    DATAV14                                                          
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R5),OTHRDATA                                                 
*                                                                               
DATAV14  LA    R5,TEMP                                                          
         BAS   RE,DUPCHK           CHECK FOR DUPLICATE LIST ENTRY               
         CLI   FERN,X'FF'                                                       
         BNE   EXIT                                                             
*                                                                               
DATAV16  GOTO1 APUTEL              ADD ELEMENT TO RECORD                        
         BZ    EXIT                                                             
         AP    ELS,=P'1'                                                        
         LA    R6,L'SCANBLK(R6)    BUMP SCAN BLOCK POINTER                      
         IC    RE,FNDX                                                          
         LA    RE,1(RE)                                                         
         STC   RE,FNDX                                                          
         B     DATAV4                                                           
*                                                                               
DATAV18  SR    R0,R0               BUMP TO NEXT TWA INPUT LINE                  
         IC    R0,0(R7)                                                         
         AR    R7,R0                                                            
         B     DATAV2                                                           
*                                                                               
DATAV20  LA    R1,LSTL1H                                                        
         ST    R1,FADR                                                          
         MVI   FNDX,0                                                           
         CP    ELS,=P'0'           CHECK FOR ANY INPUT                          
         BE    EMIF                                                             
         LA    R1,LSTIDH                                                        
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
***********************************************************************         
* VALIDATE LIST CODE FOR NESTED LIST CALL                             *         
***********************************************************************         
         SPACE 1                                                                
VALLIST  NTR1  ,                                                                
         LA    R4,IOAREA2                                                       
         ST    R4,AREC                                                          
         MVC   CTWKEY,KEYSAVE                                                   
         MVC   CTWKID,14(R6)                                                    
         MVC   KEY,CTWKEY                                                       
         GOTO1 AREAD                                                            
         BZ    EIIO                CHECK RECORD FOUND/OK                        
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         LA    R5,CTWDATA                                                       
         SR    R0,R0                                                            
         USING CTLSTD,R5                                                        
VALLIST2 CLI   CTLSTEL,0           TEST E-O-R                                   
         BE    VALLIST6                                                         
         CLI   CTLSTEL,CTLINELQ    NOT MORE THAN 1 NEST LEVEL ALLOWED           
         BE    EIRT                                                             
         CLI   CTLSTEL,CTLSTELQ                                                 
         BNE   VALLIST4                                                         
         BAS   RE,DUPCHK           CHECK FOR DUPLICATE ENTRY                    
         CLI   FERN,X'FF'                                                       
         BNE   EXIT                                                             
VALLIST4 IC    R0,CTLSTLEN                                                      
         AR    R5,R0                                                            
         B     VALLIST2                                                         
VALLIST6 LA    R4,IOAREA                                                        
         ST    R4,AREC                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE USER-ID                                                    *         
***********************************************************************         
         SPACE 1                                                                
VALUSID  NTR1  ,                                                                
         LA    R5,IOAREA2                                                       
         ST    R5,AREC                                                          
         USING CTIKEY,R5                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,WORK                                                      
         MVC   KEY,CTIKEY                                                       
         GOTO1 AREAD                                                            
         BZ    EIIO                CHECK RECORD FOUND OK                        
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         LA    R5,CTIDATA                                                       
         SR    R0,R0                                                            
         USING CTDSCD,R5                                                        
VALUSID2 CLI   CTDSCEL,0           FIND ID NUMBER ELEMENT                       
         BE    EIRT                                                             
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    *+14                                                             
         IC    R0,CTDSCLEN                                                      
         AR    R5,R0                                                            
         B     VALUSID2                                                         
         MVC   OTHRDATA(2),CTDSC                                                
VALUSIDX ST    R4,AREC                                                          
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SYSTEM NAME                                                *         
***********************************************************************         
         SPACE 1                                                                
VALSYSN  STM   RE,R1,12(RD)                                                     
         L     R1,AFACLIST                                                      
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
         CLC   SENAME,WORK                                                      
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     EIIF                                                             
         MVC   OTHRDATA+0(1),SEOVSYS                                            
         MVC   OTHRDATA+1(1),SESYS                                              
         MVC   OTHRDATA+2(1),SEFILSET                                           
VALSYSNX LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         DROP  R1                                                               
         SPACE 2                                                                
VALNOOP  BR    RE                  NO-OP VALIDATION                             
         EJECT                                                                  
***********************************************************************         
* CHECK FOR DUPLICATE LIST ENTRY                                      *         
***********************************************************************         
         SPACE 1                                                                
DUPCHK   NTR1  ,                                                                
         USING CTLSTD,R5                                                        
         L     RE,ATIA             CHECK NEW ELEMENT AGAINST PREVIOUS           
         ICM   RF,15,NUMNTRY       RF=NUMBER OF ELEMENTS SO FAR                 
         BZ    DUPCHK4                                                          
*                                                                               
DUPCHK2  SR    R1,R1                                                            
         IC    R1,TYPTIMAX                                                      
         SR    R0,R0                                                            
         TM    TYPTINDS,TYPTIHIN   TEST INCLUDE HEX LENGTH                      
         BZ    *+8                                                              
         IC    R0,TYPTIHEX                                                      
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BE    EDIF                                                             
         CLC   0(0,RE),CTLSTDTA    COMPARE FIRST OR BOTH PARTS OF DATA          
         LA    RE,1(R1,RE)                                                      
*                                                                               
         TM    TYPTINDS,TYPTICHS   TEST FOR SEPARATE HEX CHECK                  
         BZ    DUPCHK3                                                          
         ST    RF,DUB                                                           
         SR    RF,RF                                                            
         IC    RF,TYPTIMAX                                                      
         LA    RF,CTLSTDTA(RF)                                                  
         SR    R1,R1                                                            
         IC    R1,TYPTIHEX                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         BE    EDIF                                                             
         CLC   0(0,RE),0(RF)       COMPARE SECOND PART OF DATA(HEX)             
         LA    RE,1(R1,RE)                                                      
         L     RF,DUB                                                           
*                                                                               
DUPCHK3  BCT   RF,DUPCHK2                                                       
*                                                                               
DUPCHK4  SR    R1,R1               ADD NEW ELEMENT                              
         IC    R1,TYPTIMAX                                                      
         SR    R0,R0                                                            
         TM    TYPTINDS,TYPTIHIN                                                
         BNZ   *+12                                                             
         TM    TYPTINDS,TYPTICHS                                                
         BZ    *+8                                                              
         IC    R0,TYPTIHEX         INCLUDE HEX PART                             
         AR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RE),CTLSTDTA                                                 
         L     RF,NUMNTRY          INCREMENT ELEMENT COUNTER                    
         LA    RF,1(RF)                                                         
         ST    RF,NUMNTRY                                                       
DUPCHKX  B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
TYPTAB   DS    0X                  ** TYPE TABLE **                             
         DC    C'IDS     ',AL1(0)                                               
         DC    AL2(VALUSID-CTLFM15),AL1(CTWKRUSR)                               
         DC    AL1(1,L'CTWKID,3,10,0,2)                                         
**NOP**  DC    C'IDGROUP ',AL1(TYPTIAGA)                                        
**NOP**  DC    AL2(VALUSID-CTLFM15),AL1(CTWKRIDG)                               
**NOP**  DC    AL1(1,L'CTWKID,3,10,0,2)                                         
         DC    C'SYSTEMS ',AL1(0)                                               
         DC    AL2(VALSYSN-CTLFM15),AL1(CTWKRSYS)                               
         DC    AL1(0,L'CTWKID,3,7,0,3)                                          
         DC    C'PRINTERS',AL1(TYPTINON+TYPTICHS+TYPTISRT)                      
         DC    AL2(VALNOOP-CTLFM15),AL1(CTWKRPRT)                               
         DC    AL1(1,L'CTWKID,8,8,1,0)                                          
TYPTABX  DC    AL1(TYPTEOTQ)                                                    
         SPACE 2                                                                
TYPTABD  DSECT                     ** LIST TYPE TABLE **                        
TYPTEOTQ EQU   0                   END OF TABLE INDICATOR                       
TYPTNAME DS    CL8                 LIST TYPE NAME                               
TYPTINDS DS    XL1                 INDICATORS                                   
TYPTIHIN EQU   X'80'               INCLUDE HEX DATA FOR DUPLICATES              
TYPTINON EQU   X'40'               NESTING NOT ALLOWED                          
TYPTICHS EQU   X'20'               CHECK HEX SEPARATELY FOR DUPLICATES          
TYPTIAGA EQU   X'10'               SET CTWKAGY FROM TWAAGY                      
TYPTISRT EQU   X'08'               SORT DATA ON LOGICAL PRINTER NUMBER          
TYPTROUT DS    AL2                 DISPLACEMENT TO VALIDATION ROUTINE           
TYPTRTYP DS    CL1                 RECORD TYPE                                  
TYPTKMIN DS    AL1                 MINIMUM INPUT KEY LENGTH                     
TYPTKMAX DS    AL1                 MAXIMUM INPUT KEY LENGTH                     
TYPTIMIN DS    AL1                 MINIMUM INPUT DATA LENGTH                    
TYPTIMAX DS    AL1                 MAXIMUM INPUT DATA LENGTH                    
TYPTIHEX DS    AL1                 LENGTH OF HEXADECIMAL INPUT OR ZERO          
TYPTICHR DS    AL1                 LENGTH OF CHARACTER INPUT OR ZERO            
TYPTABL  EQU   *-TYPTABD                                                        
         EJECT                                                                  
WORKD    DSECT                     ** LOCAL WORKING STORAGE **                  
RELO     DS    A                                                                
VXSORT   DS    V                                                                
VSCINKEY DS    V                                                                
AVALROUT DS    A                                                                
ANXTNTRY DS    A                                                                
NUMNTRY  DS    A                                                                
OTHRDATA DS    CL10                                                             
LENTRY   DS    XL1                                                              
ELS      DS    PL3                                                              
IOAREA2  DS    1000C                                                            
NLINES   DS    X                                                                
SCANBLK  DS    20CL32                                                           
WORKX    EQU   *                                                                
         SPACE 1                                                                
* CTLFMACTNS                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTLFMACTNS                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
         SPACE 1                                                                
* CTLFMTWA                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTLFMTWA                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* CTLFMEAD                                                                      
       ++INCLUDE CTLFMEAD                                                       
         SPACE 1                                                                
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004CTLFM15   05/01/02'                                      
         END                                                                    
