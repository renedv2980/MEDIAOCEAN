*          DATA SET CTLFM02US  AT LEVEL 015 AS OF 05/01/02                      
*PHASE TA0202A                                                                  
*INCLUDE GETIDS                                                                 
*INCLUDE SCINKEY                                                                
         TITLE 'CTLFM02 - CONTROL FILE MAINT - TERMINAL RECORDS'                
         PRINT NOGEN                                                            
CTLFM02  CSECT                                                                  
         NMOD1 WRKX-WRKD,**LFM2**,RA,RR=R7                                      
         USING WRKD,RC             RC=A(TEMP W/S)                               
         ST    R7,MYRELO                                                        
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         LA    R4,IOAREA                                                        
         USING CTTREC,R4           R4=A(IO)                                     
         L     R5,PARM+12          R5=A(COMFACS)                                
         USING COMFACSD,R5                                                      
         MVC   ATERMVAL,CTERMVAL                                                
         MVI   TERMINFO,0          SET TERMINAL INFO FLAG                       
         MVI   SYSNUMSC,0          INITIALISE FOR SYSNUMS                       
         EJECT                                                                  
* VALIDATE TERMINAL ID                                                          
*                                                                               
KEYVAL   XC    CTTKEY,CTTKEY       INITIALISE TERMINAL RECORD KEY               
         MVI   CTTKEY,C'T'                                                      
*                                                                               
         LA    R1,TRMIDH           MUST HAVE TERMINAL ID INPUT                  
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         TM    FLDH+4,X'08'        NUMERIC-ASSUME EXISTING UTL NUM              
         BO    KEYV2                                                            
         CLI   FLD,C'#'            #NNNN MEANS TERM OR PSWD NUM INPUT           
         BE    KEYV5                                                            
         SR    R1,R1               READ AND SPACE FILL 8 CHR ID                 
         LA    RF,8                                                             
         IC    R1,FLDH+5                                                        
         SR    RF,R1               VTAM IDS CAN BE < 8 CHRS                     
         BM    EIIF                                                             
         BZ    KEYV1                                                            
         LA    R1,FLD(R1)                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SPACES                                                   
KEYV1    MVC   CTTKTID(8),FLD                                                   
         B     KEYV6                                                            
*                                                                               
KEYV2    XC    DMCB(20),DMCB       VALIDATE TERMINAL NUM                        
         GOTO1 ATERMVAL,DMCB,(X'00',FLDH)                                       
         SR    R5,R5                                                            
         ICM   R5,7,DMCB+5         ERROR IF NUMBER NOT IN UTL                   
         BZ    EIIF                                                             
         SR    RE,RE                                                            
         ICM   RE,7,DMCB+1         POINT TO RETURN TERMINAL ID                  
         MVC   CTTKTID(8),0(RE)                                                 
         B     KEYV6                                                            
*                                                                               
KEYV5    L     RF,=A(VALPSVN)      VALIDATE #NNNNN TERM/PSWD NUMBER             
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
         BNZ   EXIT                                                             
         B     KEYVAL              BACK TO VALIDATE ALPHA FIELDS                
*                                                                               
KEYV6    XC    TRMID,TRMID         ECHO TERMINAL ID BACK TO TWA                 
         MVC   TRMID(8),CTTKTID                                                 
         MVI   TRMIDH+5,8                                                       
         OI    TRMIDH+6,X'80'                                                   
         EJECT                                                                  
* VALIDATE SYSTEM                                                               
*                                                                               
KEYVA    MVI   SYSTEM,0            SET SYSTEM NOT INPUT                         
         LA    R1,TRMSYSTH                                                      
         GOTO1 AFVAL                                                            
         BZ    KEYVD                                                            
         SR    R1,R1                                                            
         IC    R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         L     R5,ASYSTBL                                                       
         USING SYSLSTD,R5          R5=A(VALID SYSTEMS TABLE)                    
*                                                                               
KEYVC    CLI   SYSLNUM,0           END OF LIST                                  
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),SYSLNAME                                                  
         BE    *+12                                                             
         LA    R5,SYSLLEN(R5)                                                   
         B     KEYVC                                                            
         MVC   SYSTEM,SYSLNUM      SET SYSTEM NUMBER FROM LIST                  
         MVC   TRMSYST(7),SYSLNAME DISPLAY FULL SYSTEM NAME                     
         OI    TRMSYSTH+6,X'80'                                                 
         DROP  R5                                                               
         EJECT                                                                  
* VALIDATE PASSWORD                                                             
*                                                                               
KEYVD    MVI   PASSWORD,0          SET PASSWORD NOT INPUT                       
         XC    SVPSWRD,SVPSWRD                                                  
         LA    R1,TRMPWRDH                                                      
         GOTO1 AFVAL               TEST IF PASSWORD INPUT                       
         BZ    KEYVE               NO                                           
         LA    R1,TRMIDH                                                        
         ST    R1,FADR                                                          
         MVC   KEY,CTTKEY                                                       
         GOTO1 AREAD               READ MASTER TERMINAL REC                     
         BZ    EIIO                                                             
         TM    DMCB+8,X'12'        RECORD MUST EXIST                            
         BNZ   ERNF                                                             
         TM    CTTSTAT,X'0D'       RECORD MUST NOT BE PASSIVE OR PRNTR          
         BNZ   EIIF                                                             
         MVC   CTTKPASS,FLD        MOVE PASSWORD TO KEY                         
         MVC   SVPSWRD,FLD                                                      
         MVI   PASSWORD,1                                                       
         OI    TERMINFO,X'80'      SET PASSWORD SPECIFIED                       
         EJECT                                                                  
* VALIDATE THIS/LAST ACTIONS                                                    
*                                                                               
KEYVE    MVC   KEY,CTTKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         MVC   TERMID,CTTKTID      SAVE ORIGINAL TERMINAL ID                    
         LA    R1,TRMIDH                                                        
         ST    R1,FADR                                                          
         CLI   PASSWORD,0                                                       
         BE    KEYVE1                                                           
         LA    R1,TRMPWRDH                                                      
         ST    R1,FADR                                                          
KEYVE1   CLI   ACTN,CHANGE         NORMAL CHANGE                                
         BNE   KEYVG                                                            
         TM    ACTINDS,X'80'                                                    
         BO    KEYVE2                                                           
         MVI   ACTN,DISPLAY                                                     
         CLC   KEY,LKEY            KEY AND SYSTEM MUST NOT CHANGE               
         BNE   KEYVG                                                            
         CLC   SYSTEM,LSYSTEM                                                   
         BNE   KEYVG                                                            
         MVI   ACTN,CHANGE         IF KEYS ARE THE SAME RESET ACTION            
         B     KEYVG                                                            
KEYVE2   CLC   KEY,LKEY            MOVE TYPE CHANGE KEYS MUST DIFFER            
         BE    EIIF                                                             
         CLC   SYSTEM,LSYSTEM                                                   
         BNE   EIIF                                                             
*                                                                               
KEYVG    CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'                                                     
         BZ    KEYVG2                                                           
KEYVG1   CLI   ACTN,ADD            NOT FOUND VALID FOR ADD/COPY                 
         BE    DATAVAL                                                          
         CLI   ACTN,COPY                                                        
         BE    DATAVAL                                                          
         B     ERNF                                                             
KEYVG2   CLI   ACTN,ADD            FOUND NOT VALID FOR ADD/COPY                 
         BE    ERAE                                                             
         CLI   ACTN,COPY                                                        
         BE    ERAE                                                             
         TM    DMCB+8,X'02'                                                     
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY        DELETED REC CAN ONLY BE DISPLAYED            
         BNE   ERNF                                                             
         CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
         TM    CTTSTAT,X'01'       PASSIVE RECORD CANT BE WORKED ON             
         BO    ERNF                                                             
         B     DISPREC                                                          
         EJECT                                                                  
* DISPLAY TERMINAL PRINCIPAL ID / VALID ID LIST / PROGRAM EXCEPTIONS            
* DISPLAY SYSTEM PROGRAM ACCESS LIST (IF SYSTEM INPUT)                          
* DISPLAY TERMINAL NETWORK DATA (IF NO PASSWORD INPUT)                          
*                                                                               
DISPREC  TWAXC TRMPIDH                                                          
         XC    XXCNT(XXCNTL),XXCNT ZEROISE ALL COUNTERS                         
         CLI   SYSTEM,0                                                         
         BE    DISP1                                                            
         L     RF,=A(GETSE)        GET A(SELIST ENTRY) INTO ASE                 
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
DISP1    XC    ASYSEL,ASYSEL       SET A(SYSTEM ELEMENT)                        
         XC    ATRMEL,ATRMEL       SET A(TERMINAL DEFN ELEMENT)                 
         MVI   SYSNUMSC,0          INITIALISE FOR SYSNUMS                       
         LA    R5,CTTDATA                                                       
*                                                                               
DISP2    CLI   0(R5),0                                                          
         BE    DISPEND                                                          
         CLI   0(R5),X'1F'         PRINCIPAL ID                                 
         BE    DISPPID                                                          
         CLI   0(R5),X'20'         ID                                           
         BE    DISPID                                                           
         CLI   0(R5),X'21'         SYSTEM                                       
         BE    DISPSS                                                           
         CLI   0(R5),X'23'         PROGRAM EXCEPTION                            
         BE    DISPEX                                                           
         CLI   0(R5),X'24'         APPLICATION ID                               
         BE    DISPAP                                                           
         CLI   0(R5),X'25'         TERMINAL DEFINITION                          
         BE    DISPTD                                                           
*                                                                               
DISP4    SR    R6,R6               BUMP TO NEXT ELEMENT                         
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     DISP2                                                            
         EJECT                                                                  
* DISPLAY PRINCIPAL ID                                                          
*                                                                               
DISPPID  DS    0H                                                               
         USING CTPIDD,R5                                                        
         MVC   TRMPID,CTPID                                                     
         B     DISP4                                                            
* ADD AN ID ELEMENT TO ID BLOCK (BLOCK1)                                        
*                                                                               
DISPID   SR    R1,R1                                                            
         IC    R1,IDCNT            BUMP BLOCK COUNT                             
         LR    R6,R1                                                            
         LA    R1,1(R1)                                                         
         STC   R1,IDCNT                                                         
         MH    R6,=H'20'                                                        
         LA    R6,BLOCK1(R6)       POINT TO ENTRY IN BLOCK                      
         MVI   0(R6),C' '          AND CLEAR IT                                 
         MVC   1(19,R6),0(R6)                                                   
         IC    R1,1(R5)                                                         
         OC    2(2,R5),2(R5)                                                    
         BNZ   *+10                                                             
         MVC   2(2,R5),=C'L='                                                   
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),2(R5)       MOVE IN ID                                   
         B     DISP4                                                            
* ADD A PROGRAM EXCEPTION ELEMENT TO BLOCK (BLOCK2)                             
*                                                                               
DISPEX   DS    0H                                                               
         USING CTPRGD,R5                                                        
         MVC   PGNAME(4),CTPRGRAM  IF NOT IN SYSTEM MODE DISPLAY TSPP           
         CLI   SYSTEM,0                                                         
         BE    DISPEX2                                                          
         MVC   DUB(4),CTPRGRAM                                                  
         MVI   DUB,C'0'                                                         
         GOTO1 VHEXIN,DMCB,DUB,DUB+4,4                                          
         CLC   SYSTEM,DUB+4        EXCEPTION FOR THIS SYSTEM                    
         BNE   DISP4                                                            
         MVC   PROGRAM,DUB+5       YES - GET PROGRAM NAME                       
         L     RF,=A(GETPRGN)                                                   
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
         CLI   PROGRAM,X'FF'                                                    
         BE    DISP4                                                            
*                                                                               
DISPEX2  DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,EXCNT            BUMP BLOCK COUNT                             
         LR    R6,R1                                                            
         LA    R1,1(R1)                                                         
         STC   R1,EXCNT                                                         
         MH    R6,=H'20'                                                        
         LA    R6,BLOCK2(R6)       POINT TO ENTRY IN BLOCK                      
         MVI   0(R6),C' '          AND CLEAR                                    
         MVC   1(19,R6),0(R6)                                                   
         MVC   0(4,R6),PGNAME      FIRST HALF IS NAME (TSPP OR XXXX)            
         LA    R6,4(R6)                                                         
         CLI   0(R6),C' '                                                       
         BNE   *+8                                                              
         BCT   R6,*-8                                                           
         MVI   1(R6),C'='                                                       
         MVC   2(1,R6),CTPRGTST    MOVE TEST LEVEL                              
         B     DISP4                                                            
         DROP  R5                                                               
* SAVE A(SYSTEM ELEMENT) IF IT'S THE ONE REQUESTED                              
*                                                                               
DISPSS   CLI   SYSTEM,0            IS A SYSTEM BEING DISPLAYED                  
         BE    DISPSS1             NO                                           
         USING CTSYSD,R5                                                        
         CLC   CTSYSNUM,SYSTEM                                                  
         BNE   *+8                                                              
         ST    R5,ASYSEL           SAVE A(SYSTEM ELEMENT)                       
DISPSS1  MVC   SYSNUMS,CTSYSNUM                                                 
         L     RF,=A(GETSEN)       GET SE NAME AND ADD TO LIST                  
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
         B     DISP4                                                            
         DROP  R5                                                               
* ADD A VTAM APPLICATION ID TO AP BLOCK (BLOCK 3)                               
*                                                                               
DISPAP   SR    R1,R1                                                            
         IC    R1,APCNT            BUMP BLOCK COUNT                             
         LR    R6,R1                                                            
         LA    R1,1(R1)                                                         
         STC   R1,APCNT                                                         
         MH    R6,=H'20'                                                        
         LA    R6,BLOCK3(R6)       POINT TO ENTRY IN BLOCK                      
         MVI   0(R6),C' '          AND CLEAR IT                                 
         MVC   1(19,R6),0(R6)                                                   
         IC    R1,1(R5)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),3(R5)       MOVE IN APPLICATION ID                       
         B     DISP4                                                            
* SAVE A(TERMINAL ELEMENT) IF WE ARE DISPLAYING IT                              
*                                                                               
DISPTD   TM    TERMINFO,X'80'      TEST IF PASSWORD INPUT                       
         BO    DISP4                                                            
         ST    R5,ATRMEL                                                        
         B     DISP4                                                            
* PUT BLOCKS TO TWA                                                             
*                                                                               
DISPEND  CLI   IDCNT,0             VALID ID'S                                   
         BE    DISPEND2                                                         
         ZIC   R0,IDCNT                                                         
         GOTO1 =V(SCINKEY),DMCB,(3,TRMID1H),(20,BLOCK1),(R0),RR=MYRELO          
*                                                                               
DISPEND2 CLI   EXCNT,0             PROGRAM EXCEPTIONS                           
         BE    DISPEND4                                                         
         ZIC   R0,EXCNT                                                         
         GOTO1 =V(SCINKEY),DMCB,(2,TRMPE1H),(20,BLOCK2),(R0),RR=MYRELO          
*                                                                               
DISPEND4 OC    ASYSEL,ASYSEL       PROGRAM ACCESS                               
         BZ    DISPEND6                                                         
         L     RF,=A(DISPSYS)                                                   
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
         ZIC   R0,PGCNT                                                         
         GOTO1 =V(SCINKEY),DMCB,(2,TRMPA1H),(20,BLOCK1),(R0),RR=MYRELO          
*                                                                               
DISPEND6 SR    R0,R0               APPLICATION IDS                              
         ICM   R0,1,APCNT                                                       
         BZ    DISPEND7                                                         
         TM    TERMINFO,X'80'      ONLY FOR BASIC TERMINAL DISPLAY              
         BO    DISPEND7                                                         
         GOTO1 =V(SCINKEY),DMCB,(2,TRMAP1H),(20,BLOCK3),(R0),RR=MYRELO          
*                                                                               
DISPEND7 TM    TERMINFO,X'80'      TERMINAL INFO                                
         BO    DISPENDA                                                         
         OC    ATRMEL,ATRMEL                                                    
         BZ    DISPEND8                                                         
         L     RF,=A(DISPTRM)                                                   
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
         SR    R0,R0                                                            
         ICM   R0,1,ATCNT                                                       
         BZ    DISPENDA                                                         
         GOTO1 =V(SCINKEY),DMCB,(2,TRMAT1H),(20,BLOCK4),(R0),RR=MYRELO          
         B     DISPENDA                                                         
DISPEND8 L     RE,=A(DEVTBL)                                                    
         A     RE,MYRELO                                                        
         MVC   TRMDEV(8),0(RE)                                                  
         L     RE,=A(TYPTBL)                                                    
         A     RE,MYRELO                                                        
         MVC   TRMTYP(8),0(RE)                                                  
*                                                                               
DISPENDA MVI   TRMSYH,C'-'         DISPLAY LIST OF SYSTEMS                      
         MVC   TRMSYH+1(L'TRMSYH-1),TRMSYH                                      
         CLI   SYSNUMSC,0                                                       
         BE    DISPENDB                                                         
         SR    RF,RF                                                            
         ICM   RF,1,SYSNAMSL       RF=L'SYSTEM NAMES LIST                       
         BZ    DISPENDB                                                         
         LA    R1,L'TRMSYH                                                      
         SR    R1,RF                                                            
         BNP   DISPENDB                                                         
         SRL   R1,1                                                             
         LA    RE,TRMSYH(R1)                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SYSNAMS                                                  
DISPENDB OI    TRMSYHH+6,X'80'                                                  
*                                                                               
DISPENDD CLI   ACTN,CHANGE         HERE TO DISPLAY CHANGED RECORD               
         BE    DATAX1              YES RETURN                                   
         MVC   LSYSTEM,SYSTEM                                                   
         TM    CTTSTAT,X'80'       OK TO RESTORE IF DELETED                     
         BZ    *+12                                                             
         MVI   NACTN,OKRES                                                      
         B     EXIT                                                             
         LA    R1,TRMPIDH                                                       
         ST    R1,FADR                                                          
         MVI   NACTN,OKCHA+OKDEL                                                
         OI    NACTN,OKCOPY                                                     
         B     EXIT                                                             
         EJECT                                                                  
* CHANGE/ADD TERMINAL RECORD                                                    
*                                                                               
DATAVAL  MVC   KEYSAVE,KEY         SAVE KEY                                     
         MVC   CTTKEY,KEY                                                       
         XC    TERMNUM,TERMNUM     CLEAR PASSIVE DATA SAVE AREAS                
         XC    SVTRMEL,SVTRMEL                                                  
         MVI   SVSTAT,0                                                         
         LA    R1,TRMIDH           POSITION CURSOR TO TERMINAL ID               
         ST    R1,FADR                                                          
*                                                                               
DATAV1   CLI   ACTN,ADD            ADD FUNCTION                                 
         BNE   DATAV2                                                           
         MVI   TEMP,0              BUILD KEY+LEN                                
         GOTO1 ABLDREC                                                          
         B     DATAV8                                                           
*                                                                               
DATAV2   CLI   ACTN,COPY           COPY FUNCTION                                
         BNE   DATAV3                                                           
         MVC   KEY,LKEY            RESTORE LAST KEY & READ RECORD               
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0            MUST BE OK                                   
         BNE   EIIO                                                             
         MVC   TEMP(4),=X'010300'                                               
         GOTO1 ADELMUL             DELETE ACTV-PASS#                            
         NI    CTTSTAT,255-X'02'                                                
         GOTO1 ABLDACT             BUILD & ADD ACTIVITY ELEMENT                 
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         MVC   KEY,KEYSAVE         RESTORE COPY TO KEY                          
         MVC   KEYNEXT,KEY                                                      
         MVC   CTTKEY,KEY                                                       
         B     DATAEND             GO TO ADD LOGIC                              
*                                                                               
DATAV3   MVC   SVSTAT,CTTSTAT      CHANGE FUNCTION - SAVE ORIG STATUS           
         LA    R5,CTTDATA                                                       
         SR    R6,R6                                                            
DATAV3A  CLI   0(R5),0             SEARCH RECORD FOR PASSIVE ELEMENTS           
         BE    DATAV3X                                                          
DATAV3B  CLI   0(R5),X'03'                                                      
         BNE   DATAV3C                                                          
         MVC   TERMNUM,2(R5)       SAVE TERMINAL NUMBER PASSIVE                 
         B     DATAV3W                                                          
DATAV3C  CLI   0(R5),X'25'         SAVE TERMINAL DEFN ELEMENT                   
         BNE   DATAV3W                                                          
         MVC   SVTRMEL,0(R5)                                                    
*                                                                               
DATAV3W  IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     DATAV3A                                                          
DATAV3X  EQU   *                                                                
*                                                                               
DATAV4   TM    ACTINDS,X'80'       TEST MOVE TYPE CHANGE                        
         BZ    DATAV5                                                           
         LA    RE,IOAREAX          RECORD DATA TO BE MOVED IN IOAREAX           
         ST    RE,AREC                                                          
         MVC   KEY,LKEY            RESTORE LAST KEY & READ RECORD               
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0            MUST BE OK                                   
         BNE   EIIO                                                             
         LA    RE,IOAREA                                                        
         ST    RE,AREC                                                          
         MVC   TEMP(4),=X'010300'                                               
         GOTO1 ADELMUL             DELETE ACTV-PASS#                            
         NI    CTTSTAT,255-X'02'                                                
         MVC   TEMP(5),=X'1F20212300'                                           
         GOTO1 ADELMUL             DELETE ALL ELS TO BE COPIED                  
         GOTO1 ACPYEL              COPY PRINID/ID/SYSTEM/PROGEX ELS             
         BZ    EXIT                                                             
         GOTO1 ABLDACT             BUILD AND ADD NEW ACTIVITY EL                
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         TM    SVSTAT,X'04'        REMEMBER IF TERMINAL/PRINTER                 
         BZ    *+8                                                              
         OI    TERMINFO,X'04'                                                   
         MVC   KEY,KEYSAVE         RESTORE COPY TO KEY                          
         MVC   KEYNEXT,KEY                                                      
         MVC   CTTKEY,KEY                                                       
         B     DATAV66                                                          
*                                                                               
DATAV5   L     RF,ADELMUL          NORMAL CHANGE - STRIP DOWN RECORD            
         MVC   TEMP(4),=X'010300'                                               
         BASR  RE,RF               DELETE ACTV-PASS#                            
         NI    CTTSTAT,255-X'02'                                                
         MVC   TEMP(3),=X'1F2000'                                               
         BASR  RE,RF               DELETE PRINCIPLE-ID/ID                       
         GOTO1 VHEXOUT,DMCB,SYSTEM,WORK,1,=C'TOG'                               
         MVI   WORK,C'T'                                                        
         GOTO1 ,DMCB,(C'D',=C'CTFILE '),(X'23',(R4)),(2,WORK)                   
         CLI   SYSTEM,0                                                         
         BNE   *+10                                                             
         XC    DMCB+8(4),DMCB+8                                                 
         GOTO1 VHELLO,DMCB         DELETE PROGRAM EXCEPTION ELEMENTS            
*                                                                               
DATAV7   TM    TERMINFO,X'80'      DISPLAYING TERM NETWORK DATA                 
         BZ    DATAV7A             YES                                          
         TM    SVSTAT,X'04'        NO REMEMBER IF TERMINAL/PRINTER              
         BZ    DATAV8                                                           
         OI    TERMINFO,X'04'                                                   
         B     DATAV8                                                           
DATAV7A  MVC   TEMP(3),=X'242500'                                               
         GOTO1 ADELMUL             DELETE APPLID/TERMDEFN                       
*                                                                               
DATAV8   GOTO1 ABLDACT             BUILD AND ADD ACTIVITY/DESC ELS              
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         EJECT                                                                  
* VALIDATE PRINCIPAL ID                                                         
*                                                                               
DATAV9   MVI   FLAG,0              SET FLAGS FOR WHAT WAS INPUT                 
         XC    SVAGYAP,SVAGYAP                                                  
         XC    SVAGYAI,SVAGYAI                                                  
         GOTO1 AFVAL,TRMPIDH                                                    
         BZ    DATAV10             PRINCIPAL ID NOT INPUT                       
*                                                                               
         CLI   FLDH+5,3                                                         
         BL    EFTS                                                             
         LA    R5,IOAREA2                                                       
         ST    R5,AREC                                                          
         USING CTIREC,R5           READ PRINCIPAL ID RECORD                     
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,FLD                                                       
         MVC   KEY,CTIKEY                                                       
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         CLI   DMCB+8,0            TEST ERRORS                                  
         BNE   ERNF                                                             
         BAS   RE,GETAGYA          GET AGENCY ALPHA ID                          
         MVC   SVAGYAP,0(R1)                                                    
         GOTO1 =V(GETIDS),DMCB,(C'C',IOAREA2),ATIA,VDATAMR,RR=MYRELO            
         CLI   0(R1),0             TEST ANY ID'S COMPATIBLE                     
         BE    EIIF                                                             
         CLI   0(R1),X'FF'                                                      
         BE    ERNF                                                             
         MVI   FLAG,1              SET COMPATIBLE ID LIST BUILT                 
         MVC   KEY,KEYSAVE                                                      
         LA    R4,IOAREA                                                        
         ST    R4,AREC                                                          
         MVC   TEMP(2),=X'1F0C'    BUILD PRINCIPAL ID ELEMENT                   
         MVC   TEMP+2(L'CTPID),FLD                                              
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         EJECT                                                                  
* VALIDATE AND BUILD ID ELEMENTS                                                
*                                                                               
DATAV10  LA    R1,TRMID1H                                                       
         GOTO1 AFVAL                                                            
         BZ    DATAV30                                                          
         CLI   FLDH+5,3                                                         
         BNE   DATAV11                                                          
         CLC   FLD(3),=C'ALL'                                                   
         BNE   DATAV11                                                          
         TM    DDS,X'80'           ALL ONLY VALID FOR DDS TERMS                 
         BZ    EIIF                                                             
         MVC   TEMP(2),=X'200C'                                                 
         MVC   TEMP+2(10),FLD                                                   
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         B     DATAV30                                                          
*                                                                               
DATAV11  LA    R7,TRMID1H                                                       
         LA    R8,3                                                             
*                                                                               
DATAV12  TM    1(R7),X'20'         SKIP PROTECTED FIELDS                        
         BZ    *+12                                                             
         SR    R1,R1                                                            
         IC    R1,0(R7)                                                         
         AR    R7,R1                                                            
         LR    R1,R7                                                            
         GOTO1 AFVAL                                                            
         BZ    DATAV28                                                          
         GOTO1 VSCANNER,DMCB,FLDH,(20,BLOCK1)                                   
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LA    R6,BLOCK1           R6=A(SCAN BLOCK ENTRY)                       
         MVC   FCNT,4(R1)                                                       
         MVI   FNDX,1                                                           
*                                                                               
DATAV14  CLC   FNDX,FCNT                                                        
         BH    DATAV28                                                          
         CLI   1(R6),0                                                          
         BNE   DATAV16                                                          
         CLI   0(R6),3             VALIDATE USER-ID                             
         BL    EFTS                                                             
         CLI   0(R6),10                                                         
         BH    EFTL                                                             
         LA    R5,IOAREA2          SWITCH I/O AREAS                             
         ST    R5,AREC                                                          
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY       BUILD ID KEY                                 
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,12(R6)                                                    
         MVC   KEY,CTIKEY                                                       
         GOTO1 AREAD               CHECK RECORD FOUND/OK                        
         BZ    EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         MVC   KEY,KEYSAVE                                                      
         LA    R4,IOAREA                                                        
         ST    R4,AREC                                                          
         MVC   TEMP(2),=X'200C'    BUILD & ADD ELEMENT                          
         MVC   TEMP+2(10),12(R6)                                                
         B     DATAV18                                                          
*                                                                               
DATAV16  CLI   0(R6),0             VALIDATE LIST-ID                             
         BE    EIIF                                                             
         ZIC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),=C'LIST'   CHECK FOR VALID KEYWORD                      
         BNE   EIIF                                                             
         CLI   1(R6),6                                                          
         BH    EFTL                                                             
         LA    R5,IOAREA2          SWITCH I/O AREAS                             
         ST    R5,AREC                                                          
         USING CTWREC,R5                                                        
         XC    CTWKEY,CTWKEY       BUILD LIST KEY                               
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'I'                                                     
         MVC   CTWKID,22(R6)                                                    
         MVC   KEY,CTWKEY                                                       
         GOTO1 AREAD                                                            
         BZ    EIIO                CHECK RECORD FOUND/OK                        
         CLI   DMCB+8,0                                                         
         BNE   EIIF                                                             
         ST    R4,AREC                                                          
         MVC   KEY,KEYSAVE                                                      
         MVC   TEMP(2),=X'200C'    BUILD & ADD ELEMENT                          
         XC    TEMP+2(2),TEMP+2                                                 
         MVC   TEMP+4(8),22(R6)                                                 
*                                                                               
DATAV18  CLI   FLAG,0              FIRST LIST ENTRY                             
         BNE   DATAV20                                                          
         OC    TEMP+2(2),TEMP+2    MUST BE AN ID                                
         BZ    EIIF                                                             
         BAS   RE,GETAGYA                                                       
         MVC   SVAGYAI,0(R1)                                                    
         GOTO1 =V(GETIDS),DMCB,(C'C',IOAREA2),ATIA,VDATAMR,RR=MYRELO            
         CLI   0(R1),0                                                          
         BE    EIIF                                                             
         CLI   0(R1),X'FF'                                                      
         BE    ERNF                                                             
         MVI   FLAG,1                                                           
         B     DATAV26                                                          
*                                                                               
DATAV20  MVC   WORK(10),TEMP+2     OTHER LIST ENTRIES MUST BE IN FIRST          
         OC    WORK(2),WORK        LIST ENTRY'S COMPATIBLE ID LIST              
         BZ    *+16                                                             
         BAS   RE,VALID                                                         
         BE    EIIF                                                             
         B     DATAV26                                                          
         LA    R5,CTWDATA                                                       
         SR    R1,R1                                                            
*                                                                               
DATAV22  CLI   0(R5),0             IF ENTRY IS A LIST DO FOR ALL                
         BE    DATAV26             ENTRIES IN LIST RECORD                       
         CLI   0(R5),X'A4'                                                      
         BNE   DATAV24                                                          
         MVC   WORK(10),3(R5)                                                   
         BAS   RE,VALID                                                         
         BE    EIIF                                                             
DATAV24  IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DATAV22                                                          
*                                  ADD ID ELEMENT TO TERM REC                   
DATAV26  GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE '),AREC,TEMP,=C'ADD=CODE'            
         CLI   12(R1),0                                                         
         BE    *+12                                                             
         MVI   FERN,68             CHECK RECORD TOO BIG                         
         B     EXIT                                                             
*                                                                               
         ZIC   R1,FNDX             BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R6,32(R6)                                                        
         B     DATAV14                                                          
*                                                                               
DATAV28  ZIC   R1,0(R7)            BUMP TO NEXT TWA LINE                        
         AR    R7,R1                                                            
         BCT   R8,DATAV12                                                       
         B     DATAV30                                                          
         EJECT                                                                  
* VALIDATE AND BUILD PROGRAM EXCEPTION ELEMENTS                                 
*                                                                               
DATAV30  LA    R7,TRMPE1H                                                       
         LA    R8,2                                                             
*                                                                               
DATAV32  TM    1(R7),X'20'         SKIP PROTECTED FIELDS                        
         BZ    *+12                                                             
         SR    R1,R1                                                            
         IC    R1,0(R7)                                                         
         AR    R7,R1                                                            
         LR    R1,R7                                                            
         GOTO1 AFVAL                                                            
         BZ    DATAV39                                                          
         GOTO1 VSCANNER,DMCB,FLDH,(20,BLOCK1)                                   
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LA    R5,BLOCK1           R5=A(SCAN BLOCK ENTRY)                       
         MVC   FCNT,4(R1)                                                       
         MVI   FNDX,1                                                           
*                                                                               
DATAV34  CLC   FNDX,FCNT                                                        
         BH    DATAV39                                                          
         CLI   0(R5),1             L'PART1                                      
         BL    EMIF                                                             
         CLI   0(R5),7             L'PART1                                      
         BH    EFTL                                                             
         CLI   1(R5),1             L'PART2                                      
         BH    EFTL                                                             
         BL    EIIF                                                             
         CLI   0(R5),4             L'PART1                                      
         BNE   DATAV36                                                          
         CLI   12(R5),C'T'         'T' MEANS ONLINE                             
         BNE   DATAV36                                                          
         GOTO1 VHEXIN,DMCB,13(R5),DUB,3,0                                       
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   DATAV38                                                          
*                                                                               
DATAV36  CLI   SYSTEM,0            CAN'T INPUT PGMNAME IF NOT IN                
         BE    EIIF                SYSTEM MODE                                  
         L     RF,=A(GETSE)        GET A(SELIST ENTRY) INTO ASE                 
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
         L     RF,=A(GETPRGX)      GET PROGRAM NUMBER                           
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
         CLI   PROGRAM,X'FF'                                                    
         BE    EIIF                                                             
         MVC   DUB(1),SYSTEM                                                    
         MVC   DUB+1(1),PROGRAM                                                 
         GOTO1 VHEXOUT,DMCB,DUB,12(R5),2,=C'TOG'                                
         MVI   12(R5),C'T'         NAME CONVERTED TO TSPP                       
*                                                                               
DATAV38  CLI   22(R5),C'A'         TEST LEVEL S/B A,B OR C                      
         BL    EIIF                                                             
         CLI   22(R5),C'C'                                                      
         BH    EIIF                                                             
         XC    TEMP,TEMP           BUILD PROGRAM EXCEPTION ELEMENT              
         LA    R6,TEMP                                                          
         USING CTPRGD,R6                                                        
         MVC   CTPRGEL(2),=X'2307'                                              
         MVC   CTPRGRAM,12(R5)                                                  
         MVC   CTPRGTST,22(R5)                                                  
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                                                               
         ZIC   R1,FNDX             BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R5,32(R5)                                                        
         B     DATAV34                                                          
*                                                                               
DATAV39  ZIC   R1,0(R7)            BUMP TO NEXT TWA LINE                        
         AR    R7,R1                                                            
         BCT   R8,DATAV32                                                       
         B     DATAV40                                                          
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE AND BUILD A SYSTEM ELEMENT                                           
*                                                                               
DATAV40  LA    R1,TRMPA1H                                                       
         GOTO1 AFVAL                                                            
         BNZ   DATAV42                                                          
         CLI   SYSTEM,0                                                         
         BE    DATAV66                                                          
         B     EMIF                                                             
*                                                                               
DATAV42  CLI   SYSTEM,0                                                         
         BE    EIIF                                                             
*                                                                               
DATAV44  CLI   FLDH+5,6                                                         
         BNE   DATAV46                                                          
         CLC   FLD(6),=C'DELETE'                                                
         BNE   DATAV46                                                          
         CLI   ACTN,ADD            CANNOT DELETE EL IF ACTN=ADD                 
         BE    EIIF                                                             
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE '),(X'21',(R4)),(1,SYSTEM)           
         B     DATAV66                                                          
*                                                                               
DATAV46  LA    R6,TRMPA1H                                                       
         XC    TEMP,TEMP                                                        
         LA    R5,TEMP                                                          
         USING CTSYSD,R5                                                        
         MVC   CTSYSEL(2),=X'2110'                                              
         MVC   CTSYSNUM,SYSTEM                                                  
         MVC   CTSYSALL,=X'FFFF'   PRESET ALL & PROGRAM VALUES                  
         MVC   CTSYSPGM,=X'FFFF'                                                
         MVC   CTSYSPGM+2(126),CTSYSPGM                                         
         L     RF,=A(GETSE)        GET A(SELIST ENTRY) INTO ASE                 
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
         LA    R8,2                                                             
*                                                                               
DATAV48  TM    1(R6),X'20'         SKIP PROTECTED FIELDS                        
         BZ    *+12                                                             
         SR    R1,R1                                                            
         IC    R1,0(R6)                                                         
         AR    R6,R1                                                            
         LR    R1,R6                                                            
         GOTO1 AFVAL                                                            
         BZ    DATAV59                                                          
         GOTO1 VSCANNER,DMCB,FLDH,(20,BLOCK1)                                   
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LA    R7,BLOCK1           R7=A(SCAN BLOCK ENTRY)                       
         MVC   FCNT,4(R1)                                                       
         MVI   FNDX,1                                                           
*                                                                               
DATAV50  CLC   FNDX,FCNT                                                        
         BH    DATAV59                                                          
         CLI   0(R7),7             L'PART1                                      
         BH    EFTL                                                             
         CLI   0(R7),1             L'PART1                                      
         BL    EFTS                                                             
         CLI   1(R7),4             L'PART2                                      
         BH    EFTL                                                             
         BE    DATAV52                                                          
         CLI   1(R7),1             L'PART2                                      
         BNE   EIIF                                                             
         MVC   DUB(2),=X'000F'     ONE CHR INPUT S/B Y OR N                     
         CLI   22(R7),C'Y'                                                      
         BE    DATAV54                                                          
         MVC   DUB(2),=X'0000'                                                  
         CLI   22(R7),C'N'                                                      
         BE    DATAV54                                                          
         B     EIIF                                                             
*                                                                               
DATAV52  TM    3(R7),X'20'         IF NOT Y OR N MUST BE VALID HEX              
         BZ    EFNH                                                             
         GOTO1 VHEXIN,DMCB,22(R7),DUB,4                                         
         OC    DMCB+12(4),DMCB+12  DOUBLE CHECK FOR VALID HEX                   
         BZ    EFNH                                                             
*                                                                               
DATAV54  CLI   0(R7),3                                                          
         BNE   DATAV56                                                          
         CLC   12(3,R7),=C'ALL'                                                 
         BNE   DATAV56                                                          
         CLC   CTSYSALL,=X'FFFF'   ALL VALUES ALREADY INPUT ?                   
         BNE   EDIF                                                             
         MVC   CTSYSALL,DUB                                                     
         B     DATAV58                                                          
*                                                                               
DATAV56  L     RF,=A(GETPGAX)      GET PROGRAM NUMBER                           
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
         CLI   PROGRAM,X'FF'                                                    
         BE    EIIF                                                             
         SR    R1,R1                                                            
         IC    R1,PROGRAM                                                       
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         LA    R1,CTSYSPGM(R1)     POINT TO PROGRAM AUTH                        
         CLC   0(2,R1),=X'FFFF'    PRGM VALUE PREVIOUSLY INPUT ?                
         BNE   EDIF                                                             
         MVC   0(2,R1),DUB                                                      
*                                                                               
DATAV58  ZIC   R1,FNDX             BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R7,32(R7)                                                        
         B     DATAV50                                                          
*                                                                               
DATAV59  ZIC   R1,0(R6)            BUMP TO NEXT TWA LINE                        
         AR    R6,R1                                                            
         BCT   R8,DATAV48                                                       
DATAV60  CLC   CTSYSALL,=X'FFFF'   SET ALL VALUE TO N IF N/I                    
         BNE   *+10                                                             
         MVC   CTSYSALL,=X'0000'                                                
         LA    R1,CTSYSPGM         SET PRG VALUES TO ALL VALUE IF N/I           
         LA    RE,64                                                            
*                                                                               
DATAV62  CLC   0(2,R1),=X'FFFF'                                                 
         BNE   *+10                                                             
         MVC   0(2,R1),CTSYSALL                                                 
         LA    R1,2(R1)                                                         
         BCT   RE,DATAV62                                                       
         OI    CTSYSEL+5,X'80'                                                  
         CLC   CTSYSALL(128),CTSYSPGM                                           
         BE    DATAV63                                                          
         L     RF,=A(CNVELM21)                                                  
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
         B     DATAV63A                                                         
DATAV63  MVI   CTSYSLEN,16         SET SHORT IF NO PRGM OVERRIDES               
DATAV63A CLI   ACTN,ADD                                                         
         BE    DATAV64                                                          
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE '),(X'21',(R4)),(1,SYSTEM)           
DATAV64  GOTO1 APUTEL              DELETE OLD AND ADD NEW SYSTEM EL             
         BZ    EXIT                                                             
*                                                                               
DATAV66  TM    TERMINFO,X'80'      TEST IF PASSWORD RECORD                      
         BZ    DATAV67                                                          
         B     DATAEND                                                          
*                                                                               
DATAV67  CLI   ACTN,CHANGE         TEST IF MOVE TYPE CHANGE                     
         BNE   DATAV68                                                          
         TM    ACTINDS,X'80'                                                    
         BZ    DATAV68                                                          
         B     DATAEND                                                          
*                                                                               
DATAV68  EQU   *                                                                
         EJECT                                                                  
* VALIDATE AND BUILD VTAM APPLICATION ID ELEMENTS                               
*                                                                               
DVAPL    LA    R1,TRMAP1H                                                       
         GOTO1 AFVAL                                                            
         BZ    DVAPLX                                                           
         CLI   FLDH+5,3                                                         
         BNE   DVAPL1                                                           
         CLC   FLD(3),=C'ALL'                                                   
         BNE   DVAPL1                                                           
         TM    DDS,X'80'           ALL ONLY VALID FOR DDS TERMS                 
         BZ    EIIF                                                             
         MVC   TEMP(2),=X'240B'                                                 
         MVI   TEMP+2,0                                                         
         MVC   TEMP+3(8),FLD                                                    
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         B     DVAPLX                                                           
*                                                                               
DVAPL1   LA    R7,TRMAP1H                                                       
         LA    R8,2                                                             
*                                                                               
DVAPL2   TM    1(R7),X'20'         SKIP PROTECTED FIELDS                        
         BZ    *+12                                                             
         SR    R1,R1                                                            
         IC    R1,0(R7)                                                         
         AR    R7,R1                                                            
         LR    R1,R7                                                            
         GOTO1 AFVAL                                                            
         BZ    DVAPL8                                                           
         GOTO1 VSCANNER,DMCB,FLDH,(20,BLOCK3)                                   
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LA    R6,BLOCK3           R6=A(SCAN BLOCK ENTRY)                       
         MVC   FCNT,4(R1)                                                       
         MVI   FNDX,1                                                           
*                                                                               
DVAPL4   CLC   FNDX,FCNT           TEST LAST FIELD THIS LINE                    
         BH    DVAPL8                                                           
         CLI   1(R6),0                                                          
         BNE   EIIF                                                             
         CLI   0(R6),3             VALIDATE APPLICATION ID                      
         BL    EFTS                                                             
         CLI   0(R6),8                                                          
         BH    EFTL                                                             
         MVC   TEMP(2),=X'240B'    BUILD APPLICATION ID ELEMENT                 
         MVI   TEMP+2,0                                                         
         MVC   TEMP+3(8),12(R6)                                                 
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                                                               
         ZIC   R1,FNDX             BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R6,32(R6)                                                        
         B     DVAPL4                                                           
*                                                                               
DVAPL8   ZIC   R1,0(R7)            BUMP TO NEXT TWA LINE                        
         AR    R7,R1                                                            
         BCT   R8,DVAPL2                                                        
DVAPLX   EQU   *                                                                
         EJECT                                                                  
* VALIDATE AND BUILD A TERMINAL INFO ELEMENT                                    
*                                                                               
DVTRM    LA    R5,TEMP             R5=A(TERMINAL DEFN EL)                       
         USING CTTRMD,R5                                                        
         XC    TEMP,TEMP                                                        
         MVC   CTTRMEL(2),=X'2520'                                              
*                                                                               
DVDEV    L     R8,=A(DEVTBL)       VALIDATE DEVICE                              
         A     R8,MYRELO                                                        
         LA    R1,TRMDEVH                                                       
         GOTO1 AFVAL                                                            
         BZ    DVDEV2                                                           
         SR    R1,R1                                                            
         IC    R1,FLDH+5                                                        
         BCTR  R1,0                                                             
DVDEV1   CLI   0(R8),X'00'         END OF LIST                                  
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),0(R8)                                                     
         BE    *+12                                                             
         LA    R8,DEVTBLL(R8)                                                   
         B     DVDEV1                                                           
DVDEV2   MVC   CTTRMDEV,8(R8)      SET DEVICE                                   
         MVC   TRMDEV(8),0(R8)     DISPLAY FULL DEVICE NAME                     
         OI    TRMDEVH+6,X'80'                                                  
         TM    CTTRMDEV,X'80'                                                   
         BZ    *+8                                                              
         OI    TERMINFO,X'04'      SET DEVICE IS A PRINTER                      
         CLI   CTTRMDEV,X'82'                                                   
         BNE   *+8                                                              
         OI    TERMINFO,X'18'      SET DEVICE IS SHUTTLE/AUTO                   
*                                                                               
DVTYP    L     R8,=A(TYPTBL)       VALIDATE TYPE                                
         A     R8,MYRELO                                                        
         LA    R1,TRMTYPH                                                       
         GOTO1 AFVAL                                                            
         BZ    DVTYP2                                                           
         SR    R1,R1                                                            
         IC    R1,FLDH+5                                                        
         BCTR  R1,0                                                             
DVTYP1   CLI   0(R8),X'00'         END OF LIST                                  
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),0(R8)                                                     
         BE    *+12                                                             
         LA    R8,TYPTBLL(R8)                                                   
         B     DVTYP1                                                           
DVTYP2   MVC   CTTRMTYP,8(R8)      SET TYPE                                     
         MVC   TRMTYP(8),0(R8)     DISPLAY FULL TYPE NAME                       
         OI    TRMTYPH+6,X'80'                                                  
*                                                                               
DVCTY    GOTO1 AFVAL,TRMCTYH       VALIDATE COUNTRY                             
         L     R1,VCTRY                                                         
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1         R1=A(COUNTRY TABLE)                          
         ZIC   R8,FLDH+5                                                        
         SH    R8,=H'1'            R8=L'INPUT-1                                 
         BNM   DVCTY1                                                           
         B     DVCTY2              NO COUNTRY INPUT                             
DVCTY1   EX    R8,*+8                                                           
         B     *+10                                                             
         CLC   CTRYSHR(0),FLD      COMPARE ON SHORT NAME                        
         BE    DVCTY2                                                           
         BXLE  R1,RE,DVCTY1                                                     
         B     EIIF                                                             
DVCTY2   MVC   CTTRMCTY,CTRYCODE   SAVE COUNTRY CODE                            
         MVC   TRMCTY(L'CTRYSHR),CTRYSHR                                        
         OI    TRMCTYH+6,X'80'                                                  
DVCTYX   EQU   *                                                                
         DROP  R1                                                               
*                                                                               
DVAGY    GOTO1 AFVAL,TRMAGYH       VALIDATE AGENCY ALPHA                        
         BNZ   DVAGY1                                                           
         MVC   TRMAGY,SVAGYAP      SET VALUE FROM PRINCIPAL ID                  
         OC    TRMAGY,TRMAGY                                                    
         BNZ   *+10                                                             
         MVC   TRMAGY,SVAGYAI      SET VALUE FROM FIRST USER ID                 
         OC    TRMAGY,TRMAGY                                                    
         BNZ   DVAGYX                                                           
         MVC   TRMAGY,=CL8' '                                                   
         B     DVAGYX                                                           
DVAGY1   CLI   FLDH+5,2            MUST BE TWO CHR LONG                         
         BNE   EFTS                                                             
DVAGYX   MVC   CTTRMAGY,TRMAGY                                                  
         OI    TRMAGYH+6,X'80'                                                  
*                                                                               
DVOFC    GOTO1 AFVAL,TRMOFCH       VALIDATE OFFICE CODE                         
         CLI   FLDH+5,1                                                         
         BH    DVOFC1                                                           
         BNL   *+16                                                             
         MVI   TRMOFC,C' '                                                      
         MVI   CTTRMOFC,C' '       DEFAULT IS SPACE                             
         B     DVOFCX                                                           
         MVC   CTTRMOFC,FLD        INPUT IS SINGLE CHR                          
         CLI   FLD,C'A'                                                         
         BNL   DVOFCX                                                           
         CLI   FLD,C'*'                                                         
         BNE   EIIF                                                             
         OI    CTTRMFL1,X'01'      TREAT * AS *1                                
         B     DVOFCX                                                           
DVOFC1   CLI   FLD,C'*'            ALLOW *1 OR *2 FOR DDS LEVELS                
         BNE   EIIF                                                             
         MVI   CTTRMOFC,C'*'       SET DDS OFFICE CHR                           
         CLI   FLD+1,C'1'                                                       
         BNE   *+12                                                             
         OI    CTTRMFL1,X'01'      SET DDS LEVEL 1 FLAG                         
         B     DVOFCX                                                           
         CLI   FLD+1,C'2'                                                       
         BNE   *+12                                                             
         OI    CTTRMFL1,X'02'      SET DDS LEVEL 2 FLAG                         
         B     DVOFCX                                                           
         B     EIIF                                                             
DVOFCX   OI    TRMOFCH+6,X'80'                                                  
*                                                                               
DVNDE    L     R8,=A(NODETBL)      VALIDATE NODE                                
         A     R8,MYRELO                                                        
         LR    R0,R8                                                            
         GOTO1 AFVAL,TRMNDEH                                                    
         BZ    DVNDE2                                                           
         CLI   FLDH+5,3                                                         
         BNE   EIIF                                                             
         LA    R8,4(R8)                                                         
DVNDE1   CLI   0(R8),C' '          END OF LIST                                  
         BE    EIIF                                                             
         CLC   FLD(3),0(R8)                                                     
         BE    *+12                                                             
         LA    R8,4(R8)                                                         
         B     DVNDE1                                                           
DVNDE2   SR    R8,R0               GET INDEX INTO NODE LIST                     
         SRL   R8,2                                                             
         STC   R8,CTTRMNDE         SET NODE NUMBER                              
*                                                                               
DVLNE    GOTO1 AFVAL,TRMLNEH       VALIDATE LINE ID                             
         BZ    DVLNE1                                                           
         CLI   FLDH+5,4                                                         
         BL    EIIF                                                             
DVLNE1   MVC   CTTRMLNE,FLD                                                     
*                                                                               
DVCU     GOTO1 AFVAL,TRMCUH        VALIDATE CONTROL UNIT ADDRESS                
         BZ    DVCU1                                                            
         CLI   FLDH+5,2                                                         
         BNE   EIIF                                                             
         GOTO1 VHEXIN,DMCB,FLD,CTTRMCU,2                                        
         OC    DMCB+12(4),DMCB+12  EXIT IF INVALID HEX                          
         BZ    EFNH                                                             
DVCU1    EQU   *                                                                
*                                                                               
DVDV     GOTO1 AFVAL,TRMDVH        VALIDATE DEVICE ADDRESS                      
         BZ    DVDV1                                                            
         CLI   FLDH+5,2                                                         
         BNE   EIIF                                                             
         GOTO1 VHEXIN,DMCB,FLD,CTTRMDV,2                                        
         OC    DMCB+12(4),DMCB+12  EXIT IF INVALID HEX                          
         BZ    EFNH                                                             
DVDV1    EQU   *                                                                
         SPACE 2                                                                
DVTATL1  LA    R7,TRMAT1H          TERMINAL ATTRIBUTE LIST                      
         LA    R8,2                                                             
*                                                                               
DVTATL2  TM    1(R7),X'20'         SKIP PROTECTED FIELDS                        
         BZ    *+12                                                             
         SR    R1,R1                                                            
         IC    R1,0(R7)                                                         
         AR    R7,R1                                                            
         LR    R1,R7                                                            
         GOTO1 AFVAL                                                            
         BZ    DVTATL5                                                          
         GOTO1 VSCANNER,DMCB,FLDH,(20,BLOCK4)                                   
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LA    R6,BLOCK4           R6=A(SCAN BLOCK ENTRY)                       
         MVC   FCNT,4(R1)                                                       
         MVI   FNDX,1                                                           
*                                                                               
DVTATL3  CLC   FNDX,FCNT           TEST LAST FIELD THIS LINE                    
         BH    DVTATL5                                                          
         CLI   0(R6),8             MAX PART 1 LEN                               
         BH    EFTL                                                             
         CLI   0(R6),3             MIN PART 1 LEN                               
         BL    EFTS                                                             
         CLI   1(R6),1             MIN PART 2 LEN                               
         BL    EFTS                                                             
         B     DVAT1               GO CHECK KEYWORD=VALUE FIELD                 
*                                                                               
DVTATL4  ZIC   R1,FNDX             BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R6,32(R6)                                                        
         B     DVTATL3                                                          
*                                                                               
DVTATL5  ZIC   R1,0(R7)            BUMP TO NEXT TWA LINE                        
         AR    R7,R1                                                            
         BCT   R8,DVTATL2                                                       
*                                                                               
DVTATLX  GOTO1 APUTEL              ADD TERMINAL DEFINITION EL                   
         BZ    EXIT                                                             
         B     DATAEND                                                          
         SPACE 2                                                                
DVAT1    CLC   12(3,R6),=C'AT1='   AT1=ABCDEFGH                                 
         BNE   DVAT1X                                                           
         BAS   RE,DVALAT           VALIDATE TERMINAL ATTRIBUTES                 
         BNE   EIIF                                                             
         MVC   CTTRMAT1,DUB                                                     
         TM    TERMINFO,X'04'      TEST IF TERMINAL IS A PRINTER                
         BZ    DVTATL4             NO                                           
         TM    CTTRMAT1,X'02'      TEST IF AUTO MODE AT1=G                      
         BZ    DVTATL4             NO                                           
         OI    TERMINFO,X'08'      SET PRINTER AUTO BIT                         
         B     DVTATL4                                                          
DVAT1X   EQU   *                                                                
*                                                                               
DVAT2    CLC   12(3,R6),=C'AT2='   AT2=ABCDEFGH                                 
         BNE   DVAT2X                                                           
         BAS   RE,DVALAT           VALIDATE TERMINAL ATTRIBUTES                 
         BNE   EIIF                                                             
         MVC   CTTRMAT2,DUB                                                     
         B     DVTATL4                                                          
DVAT2X   EQU   *                                                                
*                                                                               
DVESC    CLC   12(3,R6),=C'ESC='    ESCAPE SEQUENCE NUMBER                      
         BNE   DVESCX                                                           
         TM    3(R6),X'80'                                                      
         BZ    EIIF                                                             
         L     R0,8(R6)                                                         
         CH    R0,=H'1'                                                         
         BL    EIIF                                                             
         CH    R0,=H'255'                                                       
         BH    EIIF                                                             
         STCM  R0,1,CTTRMESC                                                    
         B     DVTATL4                                                          
DVESCX   EQU   *                                                                
*                                                                               
DVLTY    CLC   12(3,R6),=C'LTYPE=' LINE TYPE                                    
         BNE   DVLTYX                                                           
         CLI   1(R6),1                                                          
         BNE   EIIF                                                             
         MVC   DUB(1),22(R6)                                                    
         CLI   DUB,C'B'            B=BSC                                        
         BE    DVLTY1                                                           
         CLI   DUB,C'L'            L=LOCAL                                      
         BE    DVLTY1                                                           
         CLI   DUB,C'S'            S=SDLC                                       
         BE    DVLTY1                                                           
         CLI   DUB,C'T'            T=TWX                                        
         BE    DVLTY1                                                           
         B     EIIF                                                             
DVLTY1   MVC   CTTRMLTY,DUB                                                     
         B     DVTATL4                                                          
DVLTYX   EQU   *                                                                
*                                                                               
DVLSP    CLC   12(3,R6),=C'LSPEED=' LINE SPEED IN BAUD                          
         BE    *+14                                                             
         CLC   12(3,R6),=C'BAUD='                                               
         BNE   DVLSPX                                                           
         TM    3(R6),X'80'                                                      
         BZ    EIIF                                                             
         L     R0,8(R6)                                                         
         CH    R0,=H'200'                                                       
         BL    EIIF                                                             
         C     R0,=F'65536'                                                     
         BH    EIIF                                                             
         SRL   R0,3                 CONVERT BAUD TO CPS                         
         STCM  R0,3,CTTRMLSP                                                    
         B     DVTATL4                                                          
DVLSPX   EQU   *                                                                
*                                                                               
DVPSP    CLC   12(3,R6),=C'PSPEED=' PRINTER SPEED                               
         BNE   DVPSPX                                                           
         TM    3(R6),X'80'                                                      
         BZ    EIIF                                                             
         ICM   R0,15,8(R6)                                                      
         BZ    EIIF                                                             
         CH    R0,=H'2000'                                                      
         BH    EIIF                                                             
         TM    CTTRMDEV,X'80'      ONLY FOR PRINTERS                            
         BZ    EIIF                                                             
         STCM  R0,3,CTTRMPSP                                                    
         B     DVTATL4                                                          
DVPSPX   EQU   *                                                                
*                                                                               
DVPST    CLC   12(3,R6),=C'PTYPE=' PRINTER SPEED TYPE                           
         BNE   DVPSTX                                                           
         CLI   1(R6),1                                                          
         BNE   EIIF                                                             
         MVC   DUB(1),22(R6)                                                    
         CLI   DUB,C'B'            B=BIDIRECTIONAL                              
         BE    DVPST1                                                           
         CLI   DUB,C'C'            C=CHARACTER                                  
         BE    DVPST1                                                           
         CLI   DUB,C'L'            L=LINE                                       
         BE    DVPST1                                                           
         CLI   DUB,C'P'            P=PAGE                                       
         BE    DVPST1                                                           
         B     EIIF                                                             
DVPST1   TM    CTTRMDEV,X'80'      ONLY FOR PRINTERS                            
         BZ    EIIF                                                             
         MVC   CTTRMPST,DUB                                                     
         B     DVTATL4                                                          
DVPSTX   EQU   *                                                                
*                                                                               
DVPRQ    CLC   12(3,R6),=C'PQUEUE=' PRINTER QUEUE NUMBER OF ENTRIES             
         BNE   DVPRQX                                                           
         TM    3(R6),X'80'                                                      
         BZ    EIIF                                                             
         L     R0,8(R6)                                                         
         CH    R0,=H'255'                                                       
         BH    EIIF                                                             
         CH    R0,=H'99'                                                        
         BNL   *+8                                                              
         LH    R0,=H'99'                                                        
         TM    CTTRMDEV,X'80'      ONLY FOR PRINTERS                            
         BZ    EIIF                                                             
         STCM  R0,1,CTTRMPRQ                                                    
         B     DVTATL4                                                          
DVPRQX   EQU   *                                                                
*                                                                               
DVPBS    CLC   12(3,R6),=C'PBUFF=' PRINTER BUFFER SIZE                          
         BNE   DVPBSX                                                           
         TM    3(R6),X'80'                                                      
         BZ    EIIF                                                             
         L     R0,8(R6)                                                         
         CH    R0,=H'256'                                                       
         BL    EIIF                                                             
         CH    R0,=H'2048'                                                      
         BH    EIIF                                                             
         TM    CTTRMDEV,X'80'      ONLY FOR PRINTERS                            
         BZ    EIIF                                                             
         STCM  R0,3,CTTRMPBS                                                    
         B     DVTATL4                                                          
DVPBSX   EQU   *                                                                
*                                                                               
         B     EIIF                INVALID TERMINAL KEYWORD FIELD               
         SPACE 2                                                                
DVALAT   XC    DUB,DUB             VALIDATE ATTRIBUTE LIST                      
         CLI   1(R6),8                                                          
         BH    DVALATER                                                         
         ZIC   R0,1(R6)            R0=NUM OF ATTRIBUTES CHRS                    
         LA    R1,22(R6)           R1=A(NEXT ATTRIBUTE CHR)                     
DVALAT1  CLI   0(R1),C'*'                                                       
         BE    DVALAT2             * IS NOP VALUE                               
         CLI   0(R1),C'A'                                                       
         BL    DVALATER                                                         
         CLI   0(R1),C'H'                                                       
         BH    DVALATER                                                         
         MVC   DUB+7(1),0(R1)      CONVERT A-H TO 1-8                           
         NI    DUB+7,X'0F'                                                      
         LH    RF,DUB+6                                                         
         LA    RF,ATTBITS-1(RF)                                                 
         OC    DUB(1),0(RF)                                                     
         B     DVALAT2                                                          
DVALATER LA    R0,255              SET ERROR VALUE                              
         B     *+12                                                             
DVALAT2  LA    R1,1(R1)                                                         
         BCT   R0,DVALAT1                                                       
         LTR   R0,R0                                                            
         BR    RE                  EXIT WITH CC NEQ IF ERROR                    
ATTBITS  DC    X'8040201008040201'                                              
         DROP  R5                                                               
         EJECT                                                                  
* I/O HANDLING FOR ADD/CHANGE                                                   
*                                                                               
DATAEND  LA    R1,TRMIDH           POSN TO 1ST KEY FLD & SET OK                 
         ST    R1,FADR                                                          
         MVI   FNDX,0                                                           
         MVI   FERN,X'FF'                                                       
         TM    TERMINFO,X'04'      TEST IF PRINTER DEVICE                       
         BO    CHGPRT                                                           
         SPACE 2                                                                
CHGTRM   CLI   ACTN,CHANGE         CHANGE TERMINAL RECORD                       
         BNE   ADDTRM                                                           
         TM    SVSTAT,X'04'        WAS IT A TERMINAL BEFORE                     
         BZ    CHGTRM1                                                          
         MVC   DELKEY,KEY          NO DELETE OLD PRINTER PASSIVE                
         MVI   DELKEY+6,C'P'                                                    
         L     RF,=A(DELREC)       DELETE UNWANTED PASSIVE                      
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
         L     RF,=A(GETNUM)       GET NEXT TERMINAL NUM PASSIVE                
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
         BNZ   CHGTRM1                                                          
         DC    H'0'                                                             
*                                                                               
CHGTRM1  NI    CTTSTAT,255-X'0C'   SET RECORD IS A TERMINAL REC                 
         MVC   TEMP(2),=X'0304'    BUILD AND ADD NUMERIC POINTER                
         MVC   TEMP+2(2),TERMNUM                                                
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         MVC   KEYSAVE,KEY                                                      
         GOTO1 AWRITE              WRITE TERMINAL RECORD                        
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVC   KEYSAVE,KEY         SAVE ORIGINAL TERM RECORD                    
         LA    R0,IOAREAX                                                       
         LA    R1,2000                                                          
         LA    RE,IOAREA                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
CHGTRM2  XC    CTTKEY,CTTKEY       UPDATE PASSIVE TERM NUMBER RECORD            
         MVI   CTTKEY,C'T'                                                      
         MVC   CTTKPASS+8(2),TERMNUM                                            
         OI    CTTSTAT,X'01'                                                    
         MVI   TEMP,X'03'                                                       
         GOTO1 ADELEL                                                           
         MVC   TEMP(2),=X'0314'                                                 
         MVC   TEMP+2(18),KEYSAVE+7                                             
         OC    TEMP+10(10),TEMP+10                                              
         BNZ   *+8                                                              
         MVI   TEMP+1,X'0A'                                                     
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         MVC   KEY,CTTKEY                                                       
         TM    SVSTAT,X'04'        WAS IT A TERMINAL BEFORE                     
         BZ    CHGTRM3             YES                                          
         GOTO1 AADD                NO MUST ADD PASSIVE                          
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         B     CHGTRM4                                                          
*                                                                               
CHGTRM3  LA    R5,IOAREA2          READ OLD PASSIVE                             
         ST    R5,AREC                                                          
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         ST    R4,AREC                                                          
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
*                                                                               
CHGTRM4  MVC   KEY,KEYSAVE         RESTORE ORIGINAL TERM RECORD                 
         LA    R0,IOAREA                                                        
         LA    R1,2000                                                          
         LA    RE,IOAREAX                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     DATAX                                                            
         SPACE 2                                                                
ADDTRM   L     RF,=A(GETNUM)       ADD/COPY TERMINAL RECORD                     
         A     RF,MYRELO                                                        
         BASR  RE,RF               GET NEXT TERMINAL NUM PASSIVE                
         BNZ   *+6                                                              
         DC    H'0'                                                             
ADDTRM1  MVC   TEMP(2),=X'0304'    BUILD PASSIVE (NUMBER) ELEMENT               
         MVC   TEMP+2(2),TERMNUM                                                
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         GOTO1 AADD                ADD TERMINAL NUMBER RECORD                   
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVC   KEYSAVE,KEY         SAVE ORIGINAL TERM RECORD                    
         LA    R0,IOAREAX                                                       
         LA    R1,2000                                                          
         LA    RE,IOAREA                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
ADDTRM2  MVI   TEMP,X'03'          REPLACE PASSIVE NUM EL BY ALPHA EL           
         GOTO1 ADELEL                                                           
         MVC   TEMP(2),=X'0314'                                                 
         MVC   TEMP+2(18),KEYSAVE+7                                             
         OC    TEMP+10(10),TEMP+10                                              
         BNZ   *+8                                                              
         MVI   TEMP+1,X'0A'                                                     
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKEY,C'T'                                                      
         MVC   CTTKPASS+8(2),TERMNUM                                            
         OI    CTTSTAT,X'01'       SET IS A PASSIVE COPY                        
         MVC   KEY,CTTKEY                                                       
         GOTO1 AADD                ADD TERMINAL NUMERIC PASSIVE                 
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL TERM RECORD                 
         LA    R0,IOAREA                                                        
         LA    R1,2000                                                          
         LA    RE,IOAREAX                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     DATAX                                                            
         SPACE 2                                                                
CHGPRT   CLI   ACTN,CHANGE         CHANGE PRINTER RECORD                        
         BNE   ADDPRT                                                           
         TM    SVSTAT,X'04'        WAS IT A PRINTER BEFORE                      
         BO    CHGPRT1             YES                                          
         OC    TERMNUM,TERMNUM     NO DELETE PASSIVE TERM NUM POINTER           
         BZ    CHGPRT1                                                          
         XC    DELKEY,DELKEY                                                    
         MVI   DELKEY,C'T'                                                      
         MVC   DELKEY+23(2),TERMNUM                                             
         L     RF,=A(DELREC)       DELETE UNWANTED PASSIVE                      
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
*                                                                               
CHGPRT1  OI    CTTSTAT,X'04'       SET IS A PRINTER                             
         NI    CTTSTAT,255-X'08'                                                
         TM    TERMINFO,X'08'                                                   
         BZ    *+8                                                              
         OI    CTTSTAT,X'08'       SET PRINTER IN AUTO MODE                     
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVC   KEYSAVE,KEY         SAVE ORIGINAL PRINTER REC                    
         LA    R0,IOAREAX                                                       
         LA    R1,2000                                                          
         LA    RE,IOAREA                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
CHGPRT2  MVI   CTTKTID-1,C'P'      UPDATE PASSIVE PRINTER RECORD                
         OI    CTTSTAT,X'01'                                                    
         MVC   KEY,CTTKEY                                                       
         LA    R5,IOAREA2          SEARCH FOR EXISTING 'P' RECORD               
         ST    R5,AREC                                                          
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         ST    R4,AREC             RESTORE CURRENT RECORD POINTER               
         TM    DMCB+8,X'40'        IO ERROR                                     
         BO    EIIO                                                             
         TM    SVSTAT,X'04'        CHANGE FROM PRINT OR TERM TYPE ?             
         BO    CHGPRT3                                                          
         TM    DMCB+8,X'10'        'P' RECORD NOT FOUND                         
         BO    CHGPRT4                                                          
         TM    DMCB+8,X'02'        'P' RECORD FOUND DELETED                     
         BNO   EIIO                                                             
         B     CHGPRT5                                                          
*                                                                               
CHGPRT3  CLI   DMCB+8,0            HERE IF PREVIOUSLY PRINTER TYPE              
         BNE   EIIO                                                             
CHGPRT5  GOTO1 AWRITE                                                           
         B     CHGPRT6                                                          
CHGPRT4  GOTO1 AADD                                                             
CHGPRT6  CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
*                                                                               
CHGPRT7  MVC   KEY,KEYSAVE         RESTORE ORIGINAL PRINTER REC                 
         LA    R0,IOAREA                                                        
         LA    R1,2000                                                          
         LA    RE,IOAREAX                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     DATAX                                                            
         SPACE 2                                                                
ADDPRT   OI    CTTSTAT,X'04'       ADD/COPY PRINTER RECORD                      
         TM    TERMINFO,X'08'                                                   
         BZ    *+8                                                              
         OI    CTTSTAT,X'08'       SET PRINTER IN AUTO MODE                     
         GOTO1 AADD                ADD PRINTER TERMINAL RECORD                  
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVC   KEYSAVE,KEY         SAVE ORIGINAL PRINTER REC                    
         LA    R0,IOAREAX                                                       
         LA    R1,2000                                                          
         LA    RE,IOAREA                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
ADDPRT2  MVI   CTTKTID-1,C'P'      CREATE PRINTER PASSIVE                       
         OI    CTTSTAT,X'01'                                                    
         GOTO1 AADD                ADD PRINTER PRINTER PASSIVE                  
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL PRINTER REC                 
         LA    R0,IOAREA                                                        
         LA    R1,2000                                                          
         LA    RE,IOAREAX                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     DATAX                                                            
         SPACE 2                                                                
*                                                                               
DATAX    MVI   NACTN,OKCHA+OKDEL   UPDATE FILE                                  
         TM    CTTSTAT,X'02'                                                    
         BO    *+8                                                              
         OI    NACTN,OKCOPY        OK TO COPY IF RECORD HAS NO LUID             
         CLI   ACTN,CHANGE                                                      
         BNE   DATAX1                                                           
         TM    ACTINDS,X'80'       IF MOVE TYPE CHANGE THEN REDISPLAY           
         BO    DISPREC                                                          
DATAX1   LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
* CHECK ID IS IN COMPATIBLE ID LIST OF MASTER ID                                
*                                                                               
VALID    L     RF,ATIA                                                          
VALID2   CLC   0(10,RF),WORK                                                    
         BE    VALIDX                                                           
         LA    RF,12(RF)                                                        
         CLI   0(RF),X'FF'                                                      
         BNE   VALID2                                                           
VALIDX   CLI   0(RF),X'FF'         CC=EQ IF NOT IN LIST                         
         BR    RE                                                               
         SPACE 2                                                                
* SET AGENCY ALPHA ID FROM ID RECORD AT R5                                      
*                                                                               
GETAGYA  SR    R0,R0                                                            
         LA    R1,CTIDATA-CTIREC(R5)                                            
GETAGYA1 CLI   0(R1),0                                                          
         BNE   *+12                                                             
         LA    R1,=X'06040000'     POINT TO DUMMY ELEMENT                       
         B     GETAGYA2                                                         
         CLI   0(R1),X'06'         TEST AGENCY ALPHA ID EL                      
         BE    GETAGYA2                                                         
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GETAGYA1                                                         
GETAGYA2 LA    R1,2(R1)            R1=A(TWO CHR AGY ALPHA ID)                   
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
SPACES   DC    40C' '                                                           
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FOLLOWING TABLES/SUBROUTINES ARE ONLY ADDRESSABLE VIA =A(.....)     *         
***********************************************************************         
*                                                                               
         DROP  RB,RA                                                            
*                                                                               
DEVTBL   CSECT                                                                  
DEVTBLL  EQU   9                                                                
         DC    CL8'TERMINAL',X'01'                                              
         DC    CL8'TRM     ',X'01'                                              
         DC    CL8'PRINTER ',X'81'                                              
         DC    CL8'PRT     ',X'81'                                              
         DC    CL8'PC      ',X'02'                                              
         DC    CL8'SHUTTLE ',X'82'                                              
         DC    X'00'                                                            
*                                                                               
TYPTBL   CSECT                                                                  
TYPTBLL  EQU   9                                                                
         DC    CL8'3270    ',X'01'                                              
         DC    CL8'TWX     ',X'02'                                              
         DC    X'00'                                                            
*                                                                               
NODETBL  CSECT                                                                  
NODETBLL EQU   4                                                                
         DC    C'    DDN ATL CHI LAX DDL DAL TOR BLN JWN DET '                  
         DC    C'DDG MAN LEE AGB NYT FFT HAM FRA HOL IBM     '                  
         EJECT                                                                  
* GET PASSWORD/TERMINAL NAME FROM PASSIVE NUMBER #NNNNNN                        
*                                                                               
VALPSVN  CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING VALPSVN,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+VPN'    INSERT NAME                                  
*                                                                               
         SR    R0,R0               R0=ERROR NUMBER (ZERO IS OK)                 
         SR    R1,R1                                                            
         IC    R1,FLDH+5                                                        
         SH    R1,=H'2'            R1=L'NUMBER-1                                
         BNM   *+12                                                             
         LA    R0,3                                                             
         B     VALPSVNX                                                         
         MVC   DUB,=8C'0'          FIELD MUST BE NUMERIC                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),FLD+1                                                     
         CLC   DUB,=8C'0'                                                       
         BE    *+12                                                             
         LA    R0,3                                                             
         B     VALPSVNX                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD+1(0)                                                     
         CVB   R1,DUB                                                           
         LTR   R1,R1               NUMBER CAN'T BE ZERO                         
         BNZ   *+12                                                             
         LA    R0,2                                                             
         B     VALPSVNX                                                         
         STH   R1,DUB              MOVE NUMBER TO KEY                           
         MVC   CTTKPASS+8(2),DUB                                                
         MVC   KEY,CTTKEY                                                       
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BE    *+12                                                             
         LA    R0,13                                                            
         B     VALPSVNX                                                         
         LA    R5,CTTDATA                                                       
         SR    R6,R6                                                            
VALPSVN1 CLI   0(R5),0             SEARCH REC FOR POINTER ELEMENT               
         BNE   *+6                                                              
         DC    H'0'                DIE IF N/F                                   
         CLI   0(R5),X'03'                                                      
         BE    *+14                                                             
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     VALPSVN1                                                         
         USING CTPASD,R5                                                        
         XC    TRMID,TRMID         MOVE TERMINAL ID TO TWA                      
         MVC   TRMID(8),CTPASDTA                                                
         MVI   TRMIDH+5,8                                                       
         NI    TRMIDH+4,X'F1'                                                   
         OI    TRMIDH+6,X'80'                                                   
         XC    TRMPWRD,TRMPWRD     MOVE PASSWORD NAME TO TWA                    
         MVI   TRMPWRDH+5,0                                                     
         NI    TRMPWRDH+4,X'F1'                                                 
         OI    TRMPWRDH+6,X'80'                                                 
         CLI   1(R5),20            TEST IF EL CONTAINS PASSWORD                 
         BNE   VALPSVNX                                                         
         OC    CTPASDTA+8(10),CTPASDTA+8                                        
         BZ    VALPSVNX                                                         
         MVC   TRMPWRD,CTPASDTA+8                                               
         MVI   TRMPWRDH+5,10                                                    
*                                                                               
VALPSVNX LTR   R0,R0               EXIT WITH CC=ZERO IF OK                      
         BZ    *+8                                                              
         STC   R0,FERN                                                          
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* LOCATE SELIST ENTRY FOR SYSTEM AND SAVE AT ASE                                
*                                                                               
GETSE    CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETSE,RB                                                         
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GSE'    INSERT NAME                                  
*                                                                               
         L     RE,AFACLIST                                                      
         USING SYSFACD,RE                                                       
         L     R5,VSELIST                                                       
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
*                                                                               
         CLC   SYSTEM,SEOVSYS                                                   
         BE    *+10                                                             
         BXLE  R5,R6,*-10                                                       
         DC    H'0'                DIE IF N/F                                   
*                                                                               
         ST    R5,ASE              SAVE A(SELIST ENTRY)                         
         MVC   APGM,SEPGMS         AND A(SEPGMS)                                
         XIT1                                                                   
         DROP  RE,R5                                                            
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* LOCATE SELIST ENTRY FOR SYSTEM AT SYSNUMS AND ADD TO LIST OF NAMES            
*                                                                               
GETSEN   CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETSEN,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GSN'    INSERT NAME                                  
*                                                                               
GETSEN1  CLI   SYSNUMSC,0          TEST FIRST CALL - NUMBER OF ITEMS            
         BNE   GETSEN2                                                          
         MVI   SYSNAMS,C' '                                                     
         MVC   SYSNAMS+1(L'SYSNAMS-1),SYSNAMS                                   
         MVC   SYSNAMS(8),=C'TERMINAL'                                          
         CLI   PASSWORD,0                                                       
         BE    *+10                                                             
         MVC   SYSNAMS(8),=C'PASSWORD'                                          
         NC    SYSNAMS+1(7),=8X'BF'                                             
         MVC   SYSNAMS+9(7),=C'SYSTEMS'                                         
         NC    SYSNAMS+10(6),=8X'BF'                                            
         LA    RE,SYSNAMS+17       SET A(NEXT ENTRY)                            
         ST    RE,ASYSNAMS                                                      
*                                                                               
GETSEN2  L     RE,AFACLIST         SEARCH SE LIST FOR SE NUM AT SYSNUMS         
         USING SYSFACD,RE                                                       
         L     R5,VSELIST                                                       
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)            R5=A(SELIST ENTRY)                           
         USING SELISTD,R5                                                       
         CLC   SYSNUMS,SEOVSYS                                                  
         BE    GETSEN3                                                          
         BXLE  R5,R6,*-10                                                       
         LA    R5,=CL7'XXX    '    SET UNKNOWN SYSTEM NAME                      
*                                                                               
GETSEN3  L     RE,ASYSNAMS         MOVE NAME TO LIST                            
         SR    R1,R1                                                            
         ICM   R1,1,SYSNUMSC       TEST COUNT OF ITEMS IN LIST                  
         BZ    *+12                                                             
         MVI   0(RE),C'/'                                                       
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,SYSNUMSC         BUMP ITEM COUNT                              
         LA    RF,SYSNAMS+L'SYSNAMS-3                                           
         CR    RE,RF                                                            
         BH    GETSENX                                                          
         MVC   0(3,RE),SENAME      EXTRACT SE NAME                              
         NC    1(2,RE),=8X'BF'     SET TO LOWER CASE                            
         LA    RE,3(RE)                                                         
         ST    RE,ASYSNAMS                                                      
         LA    R1,SYSNAMS                                                       
         SR    RE,R1                                                            
         STC   RE,SYSNAMSL         SET LIST LENGTH                              
*                                                                               
GETSENX  XIT1                                                                   
         DROP  RE,R5                                                            
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* SET PROGRAM NAME FROM PROGRAM NUMBER                                          
*                                                                               
GETPRGN  CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETPRGN,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GPN'    INSERT NAME                                  
*                                                                               
         L     R5,APGM                                                          
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING PGMLSTD,R5                                                       
*                                                                               
         CLC   PGMNUM,PROGRAM                                                   
         BE    *+16                                                             
         BXLE  R5,R6,*-10                                                       
         MVI   PROGRAM,X'FF'       SET PROGRAM=X'FF' IF N/F                     
         B     *+10                                                             
*                                                                               
         MVC   PGNAME,PGMNAME                                                   
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* SET PROGRAM NUMBER FROM PROGRAM NAME                                          
* R5=A(SCANNER BLOCK ENTRY)                                                     
*                                                                               
GETPRGX  CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETPRGX,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GPX'    INSERT NAME                                  
*                                                                               
         LR    R3,R5                                                            
         SR    R1,R1                                                            
         IC    R1,0(R3)                                                         
         BCTR  R1,0                R1=L'COMPARE                                 
         L     R5,APGM                                                          
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING PGMLSTD,R5                                                       
*                                                                               
GETPRGX2 EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PGMNAME(0),12(R3)                                                
         BE    *+16                                                             
         BXLE  R5,R6,GETPRGX2                                                   
         MVI   PROGRAM,X'FF'       SET PROGRAM=X'FF' IF N/F                     
         B     *+10                                                             
*                                                                               
         MVC   PROGRAM,PGMNUM                                                   
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* SET PROGRAM NAME FROM PROGRAM ACCESS NUMBER                                   
*                                                                               
GETPGAN  CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETPGAN,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GPA'    INSERT NAME                                  
*                                                                               
         L     R5,APGM                                                          
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING PGMLSTD,R5                                                       
*                                                                               
GETPGAN2 CLC   PGMNUM,PROGRAM      MATCH ON PROGRAM NUMBER                      
         BE    GETPGANY                                                         
         CLI   PGMALNUM,0                                                       
         BE    *+14                                                             
         CLC   PGMALNUM,PROGRAM    OR ACCESS OVERRIDE (IF SET)                  
         BE    GETPGANY                                                         
         BXLE  R5,R6,GETPGAN2                                                   
         MVI   PROGRAM,X'FF'       SET PROGRAM=X'FF' IF N/F                     
         B     GETPGANX                                                         
GETPGANY MVC   PGNAME,PGMNAME                                                   
GETPGANX XIT1                                                                   
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* SET PROGRAM NUMBER FROM PROGRAM NAME                                          
* R5=A(SCANNER BLOCK ENTRY)                                                     
*                                                                               
GETPGAX  CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETPGAX,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GPG'    INSERT NAME                                  
*                                                                               
         LR    R3,R7               POINT TO SCANNER BLOCK ENTRY                 
         SR    R1,R1                                                            
         IC    R1,0(R3)                                                         
         BCTR  R1,0                R1=L'COMPARE                                 
         L     R5,APGM                                                          
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING PGMLSTD,R5                                                       
*                                                                               
GETPGAX2 EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PGMNAME(0),12(R3)                                                
         BE    *+16                                                             
         BXLE  R5,R6,GETPGAX2                                                   
         MVI   PROGRAM,X'FF'       SET PROGRAM=X'FF' IF N/F                     
         B     GETPGAXX                                                         
         MVC   PROGRAM,PGMNUM                                                   
         CLI   PGMALNUM,0          USE ACCESS OVERRIDE IF SET                   
         BE    *+10                                                             
         MVC   PROGRAM,PGMALNUM                                                 
GETPGAXX XIT1                                                                   
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* BUILD PROGRAM ACCESS LIST INTO BLOCK1 AND SET PGCNT TO NUMBER OF              
* ENTRIES IN BLOCK.                                                             
*                                                                               
DISPSYS  CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING DISPSYS,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+DSY'    INSERT NAME                                  
*                                                                               
         L     RF,=A(GETSE)        GET A(SELIST ENTRY) INTO ASE                 
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
         L     R5,ASYSEL           GET A(PGMS) INTO APGM                        
         USING CTSYSEL,R5                                                       
         LA    R6,CTSYSPGM         R6=A(AUTHS)                                  
         ZIC   R7,CTSYSLEN         R7=PGM NUMBER                                
         CLI   CTSYSLEN,16         CHECK FOR ALL= VALUE ONLY                    
         BE    DISPSYS8                                                         
*                                                                               
DISPSYS2 CH    R7,=H'16'                                                        
         BNH   DISPSYS8                                                         
         MVC   PROGRAM,0(R6)                                                    
         LA    R6,1(R6)                                                         
         L     RF,=A(GETPGAN)      GET PROGRAM NAME                             
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
         CLI   PROGRAM,X'FF'       SET TO X'FF' IF N/F                          
         BE    DISPSYS6                                                         
*                                                                               
DISPSYS4 SR    R1,R1                                                            
         IC    R1,PGCNT            BUMP BLOCK COUNT                             
         LA    RE,1(R1)                                                         
         STC   RE,PGCNT                                                         
         MH    R1,=H'20'                                                        
         LA    R1,BLOCK1(R1)       GET A(BLOCK ENTRY)                           
         MVI   0(R1),C' '                                                       
         MVC   1(19,R1),0(R1)                                                   
         LR    R8,R1                                                            
         MVC   0(4,R8),PGNAME                                                   
         LA    R8,4(R8)                                                         
         CLI   0(R8),C' '                                                       
         BNE   *+8                                                              
         BCT   R8,*-8                                                           
         MVI   1(R8),C'='                                                       
         MVI   2(R8),C'Y'          AUTH IS Y N OR XXXX                          
         CLC   0(2,R6),=X'000F'                                                 
         BE    DISPSYS6                                                         
         MVI   2(R8),C'N'                                                       
         CLC   0(2,R6),=X'0000'                                                 
         BE    DISPSYS6                                                         
         GOTO1 VHEXOUT,DMCB,(R6),2(R8),2,=C'TOG'                                
*                                                                               
DISPSYS6 LA    R8,CTSYSALL         EXIT IF ALL=VALUE JUST DONE                  
         CR    R8,R6                                                            
         BE    DISPSYSX                                                         
         LA    R6,2(R6)                                                         
         SH    R7,=H'3'                                                         
         B     DISPSYS2                                                         
*                                                                               
DISPSYS8 LA    R6,CTSYSALL                                                      
         MVC   PGNAME,=CL8'ALL'                                                 
         B     DISPSYS4                                                         
*                                                                               
DISPSYSX XIT1                                                                   
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* CONVERT THE X'21' ELEMENT TO THE NEW FORMAT                                   
*                                                                               
CNVELM21 CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING CNVELM21,RB                                                      
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+CNV'    INSERT NAME                                  
*                                                                               
         MVC   BLOCK1(L'TEMP),TEMP COPY THE ELEMENT OVER                        
         LA    R7,BLOCK1           POINT FROM WHERE TO COPY                     
         USING CTSYSD,R7                                                        
         LA    R2,TEMP+CTSYSPGM-CTSYSD  WHERE TO COPY                           
         LA    R3,1                FIRST PROGRAM                                
         LA    R0,16               LENGTH IS HEADER FIRST                       
         LA    R6,CTSYSPGM                                                      
CNV21LP  CLC   0(2,R6),CTSYSALL    DEFAULT?                                     
         BE    CNV21NX             YES, SKIP TO NEXT ONE                        
         STC   R3,0(R2)            STORE THE PROGRAM NUMBER                     
         MVC   1(2,R2),0(R6)       AND ITS AUTHORIZATION CODE                   
         AH    R0,=H'3'            LENGTH IS CHANGED BY 3                       
         LA    R2,3(R2)            NEXT POSTION FOR NEXT PROGRAM                
CNV21NX  LA    R3,1(R3)            NEXT PROGRAM NUMBER                          
         LA    R6,2(R6)            NEXT AUTHORIZATION CODE                      
         CH    R3,=H'64'           DID WE DO ALL 64 PROGRAMS?                   
         BNH   CNV21LP             NO, CONTINUE UNTIL WE'RE DONE                
         LA    R7,TEMP             STORE THE NEW LENGTH OF THE ELEMENT          
         STC   R0,CTSYSLEN                                                      
         XIT1                                                                   
         DROP  R7                                                               
         LTORG                                                                  
         EJECT                                                                  
* BUILD TERMINAL INFO INTO BLOCK4 AND SET ATCNT TO NUMBER OF                    
* ENTRIES IN BLOCK.                                                             
*                                                                               
DISPTRM  CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING DISPTRM,RB                                                       
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+DTM'    INSERT NAME                                  
*                                                                               
         L     R5,ATRMEL           GET A(TERMINAL INFO ELEMENT)                 
         USING CTTRMD,R5                                                        
*                                                                               
         L     RE,=A(DEVTBL)       DISPLAY DEVICE                               
         A     RE,MYRELO                                                        
DTDEV1   CLI   0(RE),X'00'         END OF LIST                                  
         BE    DTDEVX                                                           
         CLC   CTTRMDEV,8(RE)                                                   
         BE    *+12                                                             
         LA    RE,DEVTBLL(RE)                                                   
         B     DTDEV1                                                           
DTDEV2   MVC   TRMDEV(8),0(RE)     DISPLAY DEVICE NAME                          
DTDEVX   EQU   *                                                                
*                                                                               
         L     RE,=A(TYPTBL)       DISPLAY TYPE                                 
         A     RE,MYRELO                                                        
DTTYP1   CLI   0(RE),X'00'         END OF LIST                                  
         BE    DTTYPX                                                           
         CLC   CTTRMTYP,8(RE)                                                   
         BE    *+12                                                             
         LA    RE,TYPTBLL(RE)                                                   
         B     DTTYP1                                                           
DTTYP2   MVC   TRMTYP(8),0(RE)     DISPLAY TYPE NAME                            
DTTYPX   EQU   *                                                                
*                                                                               
DTCTY    L     R1,VCTRY            DISPLAY COUNTRY                              
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1         R1=A(COUNTRY TABLE)                          
         CLC   CTRYCODE,CTTRMCTY                                                
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
         MVC   TRMCTY,CTRYSHR      DISPLAY COUNTRY SHORT NAME                   
         DROP  R1                                                               
*                                                                               
DTAGY    MVC   TRMAGY,CTTRMAGY     DISPLAY AGENCY TWO CHR ALPHA ID              
*                                                                               
DTOFC    MVC   TRMOFC(1),CTTRMOFC  DISPLAY OFFICE CODE CHR                      
         CLI   TRMOFC,C'*'                                                      
         BNE   DTOFCX                                                           
         TM    CTTRMFL1,X'01'      SHOW *1 DDS LEVEL                            
         BZ    *+8                                                              
         MVI   TRMOFC+1,C'1'                                                    
         TM    CTTRMFL1,X'02'      SHOW *2 DDS LEVEL                            
         BZ    *+8                                                              
         MVI   TRMOFC+1,C'2'                                                    
DTOFCX   EQU   *                                                                
*                                                                               
DTNDE    SR    RF,RF               DISPLAY NODE NAME                            
         IC    RF,CTTRMNDE                                                      
         SLL   RF,2                                                             
         L     RE,=A(NODETBL)                                                   
         A     RE,MYRELO                                                        
         AR    RE,RF                                                            
         MVC   TRMNDE(3),0(RE)                                                  
         OI    TRMNDEH+6,X'80'                                                  
*                                                                               
DTLNE    MVC   TRMLNE(4),CTTRMLNE  DISPLAY LINE NAME                            
         OI    TRMLNEH+6,X'80'                                                  
*                                                                               
DTCU     CLI   CTTRMCU,0           DISPLAY HEX CONTROL UNIT                     
         BE    DTCUX                                                            
         GOTO1 VHEXOUT,DMCB,CTTRMCU,TRMCU,1,=C'TOG'                             
         OI    TRMCUH+6,X'80'                                                   
DTCUX    EQU   *                                                                
*                                                                               
DTDV     CLI   CTTRMDV,0           DISPLAY HEX DEVICE ADDRESS                   
         BE    DTDVX                                                            
         GOTO1 VHEXOUT,DMCB,CTTRMDV,TRMDV,1,=C'TOG'                             
         OI    TRMDVH+6,X'80'                                                   
DTDVX    EQU   *                                                                
*                                                                               
DTAT1    MVC   DUB(1),CTTRMAT1     ATTRIBUTE BYTE ONE                           
         CLI   DUB,0                                                            
         BE    DTAT1X                                                           
         BAS   RE,DTNXT            SET R6 TO NEXT BLOCK ENTRY                   
         MVC   0(4,R6),=C'AT1='                                                 
         LA    R6,4(R6)                                                         
         BAS   RE,DTATB            AT1=ABCDEFGH                                 
DTAT1X   EQU   *                                                                
*                                                                               
DTAT2    MVC   DUB(1),CTTRMAT2     ATTRIBUTE BYTE TWO                           
         CLI   DUB,0                                                            
         BE    DTAT2X                                                           
         BAS   RE,DTNXT            SET R6 TO NEXT BLOCK ENTRY                   
         MVC   0(4,R6),=C'AT2='                                                 
         LA    R6,4(R6)                                                         
         BAS   RE,DTATB            AT2=ABCDEFGH                                 
DTAT2X   EQU   *                                                                
*                                                                               
DTESC    SR    R0,R0               ESCAPE SEQUENCE                              
         ICM   R0,1,CTTRMESC                                                    
         BZ    DTESCX                                                           
         BAS   RE,DTNXT            SET R6 TO NEXT BLOCK ENTRY                   
         MVC   0(4,R6),=C'ESC='                                                 
         LA    R6,4(R6)                                                         
         EDIT  (R0),(3,(R6)),ALIGN=LEFT                                         
DTESCX   EQU   *                                                                
*                                                                               
DTLTY    MVC   DUB(1),CTTRMLTY     LINE TYPE                                    
         CLI   DUB,0                                                            
         BE    DTLTYX                                                           
         BAS   RE,DTNXT            SET R6 TO NEXT BLOCK ENTRY                   
         MVC   0(6,R6),=C'LTYPE='                                               
         LA    R6,6(R6)                                                         
         MVC   0(1,R6),DUB                                                      
DTLTYX   EQU   *                                                                
*                                                                               
DTLSP    MVC   DUB(2),CTTRMLSP     LINE SPEED                                   
         SR    R0,R0                                                            
         ICM   R0,3,DUB                                                         
         BZ    DTLSPX                                                           
         SLL   R0,3                CONVERT FROM CPS TO BAUD                     
         BAS   RE,DTNXT            SET R6 TO NEXT BLOCK ENTRY                   
         MVC   0(7,R6),=C'LSPEED='                                              
         LA    R6,7(R6)                                                         
         EDIT  (R0),(5,(R6)),ALIGN=LEFT                                         
DTLSPX   EQU   *                                                                
         SPACE 2                                                                
DISPPRT  TM    CTTRMDEV,X'80'      TEST IF PRINTER DEVICE                       
         BZ    DISPTRMX                                                         
*                                                                               
DTPSP    MVC   DUB(2),CTTRMPSP     PRINTER SPEED                                
         SR    R0,R0                                                            
         ICM   R0,3,DUB                                                         
         BZ    DTPSPX                                                           
         BAS   RE,DTNXT            SET R6 TO NEXT BLOCK ENTRY                   
         MVC   0(7,R6),=C'PSPEED='                                              
         LA    R6,7(R6)                                                         
         EDIT  (R0),(5,(R6)),ALIGN=LEFT                                         
DTPSPX   EQU   *                                                                
*                                                                               
DTPST    MVC   DUB(1),CTTRMPST     PRINTER TYPE                                 
         CLI   DUB,0                                                            
         BE    DTPSTX                                                           
         BAS   RE,DTNXT            SET R6 TO NEXT BLOCK ENTRY                   
         MVC   0(6,R6),=C'PTYPE='                                               
         LA    R6,6(R6)                                                         
         MVC   0(1,R6),DUB                                                      
DTPSTX   EQU   *                                                                
*                                                                               
DTPRQ    MVC   DUB(1),CTTRMPRQ     PRINTER NUMBER OF QUEUE ENTRYS               
         SR    R0,R0                                                            
         ICM   R0,1,DUB                                                         
         BZ    DTPRQX                                                           
         BAS   RE,DTNXT            SET R6 TO NEXT BLOCK ENTRY                   
         MVC   0(7,R6),=C'PQUEUE='                                              
         LA    R6,7(R6)                                                         
         EDIT  (R0),(3,(R6)),ALIGN=LEFT                                         
DTPRQX   EQU   *                                                                
*                                                                               
DTPBS    MVC   DUB(2),CTTRMPBS     PRINTER BUFFER SIZE                          
         SR    R0,R0                                                            
         ICM   R0,3,DUB                                                         
         BZ    DTPBSX                                                           
         BAS   RE,DTNXT            SET R6 TO NEXT BLOCK ENTRY                   
         MVC   0(6,R6),=C'PBUFF='                                               
         LA    R6,6(R6)                                                         
         EDIT  (R0),(5,(R6)),ALIGN=LEFT                                         
DTPBSX   EQU   *                                                                
*                                                                               
         B     DISPTRMX                                                         
         SPACE 2                                                                
DTNXT    SR    R1,R1               BUMP TO NEXT OUTPUT BLOCK                    
         IC    R1,ATCNT                                                         
         LR    R6,R1                                                            
         LA    R1,1(R1)                                                         
         STC   R1,ATCNT                                                         
         MH    R6,=H'20'                                                        
         LA    R6,BLOCK4(R6)       R6=A(NEXT BLOCK ENTRY)                       
         MVI   0(R6),C' '                                                       
         MVC   1(19,R6),0(R6) CLEAR BLOCK                                       
         BR    RE                                                               
         SPACE 2                                                                
DTATB    LA    R1,DTATBITS         BYTE IN DUB TO ATT A/B/C/D/E/F/G/H           
         LA    R0,8                                                             
DTATB1   MVC   DUB+7(1),DUB                                                     
         NC    DUB+7(1),0(R1)                                                   
         BZ    *+14                                                             
         MVC   0(1,R6),8(R1)                                                    
         LA    R6,1(R6)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,DTATB1                                                        
         BR    RE                                                               
DTATBITS DC    X'8040201008040201',C'ABCDEFGH'                                  
*                                                                               
DISPTRMX XIT1                                                                   
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* SUBROUTINE TO DELETE A RECORD WITH KEY=DELKEY                                 
*                                                                               
DELREC   CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING DELREC,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+DRE'    INSERT NAME                                  
*                                                                               
         LA    R4,IOAREA2          DELETE RECORD WITH KEY DELKEY                
         ST    R4,AREC                                                          
         MVC   DELSAVE,KEY                                                      
         MVC   KEY,DELKEY                                                       
         MVC   0(25,R4),DELKEY                                                  
         MVI   UPDATE,C'Y'         SET RFU                                      
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BNE   DELRECX                                                          
         OI    27(R4),X'80'        SET DELETE FLAG                              
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   DELRECX                                                          
DELRECX  MVC   KEY,DELSAVE                                                      
         LA    R4,IOAREA                                                        
         ST    R4,AREC                                                          
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* SUBROUTINE TO GET NEXT PASSIVE TERMINAL NUMBER                                
*                                                                               
GETNUM   CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETNUM,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GNU'    INSERT NAME                                  
*                                                                               
         LA    R4,IOAREA2                                                       
         ST    R4,AREC                                                          
         MVC   DELSAVE,KEY                                                      
         XC    TERMNUM,TERMNUM                                                  
         XC    CTTKEY,CTTKEY       READ MASTER TERM REC INTO I/O2               
         MVI   CTTKEY,C'T'                                                      
         MVC   KEY,CTTKEY                                                       
         MVI   UPDATE,C'Y'         SET RFU                                      
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BNE   GETNUMX                                                          
         LA    R5,CTTDATA                                                       
         SR    R6,R6                                                            
GETNUM2  CLI   0(R5),0             SEARCH FOR NUMBER ELEMENT                    
         BNE   *+6                                                              
         DC    H'0'                DIE IF N/F                                   
         CLI   0(R5),X'03'                                                      
         BE    *+14                                                             
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     GETNUM2                                                          
         MVC   TERMNUM,2(R5)       UPDATE POINTER NUMBER                        
         LH    R6,TERMNUM                                                       
         LA    R6,1(R6)                                                         
         STH   R6,TERMNUM                                                       
         MVI   TEMP,0              RE-BUILD MASTER REC & WRITE BACK             
         GOTO1 ABLDREC                                                          
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
         BZ    GETNUMX                                                          
         MVC   TEMP(2),=X'0304'                                                 
         MVC   TEMP+2(2),TERMNUM                                                
         GOTO1 APUTEL                                                           
         BZ    GETNUMX                                                          
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   GETNUMX                                                          
GETNUMX  LA    R4,IOAREA           RESTORE I/O PARMS                            
         ST    R4,AREC                                                          
         MVC   KEY,DELSAVE                                                      
         OC    TERMNUM,TERMNUM     EXIT WITH CC EQL IF ERROR                    
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* DSECT TO COVER TEMP W/S                                                       
*                                                                               
WRKD     DSECT                                                                  
MYRELO   DS    A                                                                
ATERMVAL DS    A                                                                
ASYSEL   DS    A                                                                
ATRMEL   DS    A                                                                
ASE      DS    A                                                                
APGM     DS    A                                                                
*                                                                               
TERMID   DS    CL8                                                              
*                                                                               
SVTRMEL  DS    CL32                                                             
SVAGYAP  DS    CL2                                                              
SVAGYAI  DS    CL2                                                              
SVSTAT   DS    X                                                                
TERMINFO DS    X                                                                
PASSWORD DS    C                                                                
SVPSWRD  DS    CL10                                                             
SYSTEM   DS    C                                                                
PROGRAM  DS    C                                                                
PGNAME   DS    CL8                                                              
*                                                                               
XXCNT    DS    0C                                                               
IDCNT    DS    C                                                                
EXCNT    DS    C                                                                
PGCNT    DS    C                                                                
APCNT    DS    C                                                                
ATCNT    DS    C                                                                
XXCNTL   EQU   *-XXCNT                                                          
*                                                                               
ASYSNAMS DS    A                                                                
SYSNAMS  DS    CL78                                                             
SYSNAMSL DS    X                                                                
SYSNUMS  DS    X                                                                
SYSNUMSC DS    X                                                                
         DS    X                                                                
TERMNUM  DS    H                                                                
MASTERID DS    CL10                                                             
*                                                                               
FCNT     DS    C                                                                
FLAG     DS    X                                                                
*                                                                               
BLOCK1   DS    20CL32                                                           
BLOCK2   DS    20CL32                                                           
BLOCK3   DS    20CL32                                                           
BLOCK4   DS    20CL32                                                           
*                                                                               
DELKEY   DS    CL25                                                             
DELSAVE  DS    CL25                                                             
IOAREA2  DS    2000C                                                            
WRKX     EQU   *                                                                
         EJECT                                                                  
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
LSYSTEM  DS    C                                                                
         SPACE 1                                                                
* CTLFMACTNS                                                                    
       ++INCLUDE CTLFMACTNS                                                     
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMFDD                                                                      
       ++INCLUDE CTLFMFDD                                                       
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015CTLFM02US 05/01/02'                                      
         END                                                                    
