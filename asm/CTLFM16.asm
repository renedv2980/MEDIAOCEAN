*          DATA SET CTLFM16    AT LEVEL 005 AS OF 05/01/02                      
*PHASE TA0216A                                                                  
         TITLE 'CTLFM16 - CONTROL FILE MAINT - PRINTER RECORDS'                 
CTLFM16  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**LFMM**,RA,RR=R7                                    
         USING WORKD,RC            RC=A(W/S)                                    
         ST    R7,MYRELO                                                        
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         LA    R4,IOAREA                                                        
         USING CTTREC,R4           R4=A(RECORD)                                 
         L     R5,PARM+12          R5=A(COMFACS)                                
         USING COMFACSD,R5                                                      
         MVC   ATERMVAL,CTERMVAL                                                
         MVI   TERMINFO,0          SET TERMINAL INFO FLAG                       
         EJECT                                                                  
* VALIDATE KEY FIELDS AND BUILD KEY                                             
*                                                                               
KEYVAL   XC    CTTKEY,CTTKEY       INITIALISE RECORD KEY                        
         MVI   CTTKEY,C'T'                                                      
*                                                                               
         LA    R1,PRTIDH           MUST HAVE TERMINAL ID INPUT                  
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         TM    FLDH+4,X'08'        NUMERIC-ASSUME EXISTING UTL NUM              
         BO    KEYV2                                                            
         CLI   FLDH+5,3                                                         
         BL    EFTS                                                             
         CLC   FLD(2),=C'V='       V=XXXXXXXX MEANS VTAM LUID INPUT             
         BE    KEYV4                                                            
         CLI   FLD,C'#'            #NNNN MEANS TERM CTFILE NUM INPUT            
         BE    KEYV5                                                            
         B     KEYV3                                                            
         B     EIIF                                                             
*                                                                               
KEYV2    XC    DMCB(20),DMCB       VALIDATE TERMINAL NUM                        
         GOTO1 ATERMVAL,DMCB,(X'00',FLDH)                                       
         SR    R5,R5                                                            
         ICM   R5,7,DMCB+5         ERROR IF NUMBER NOT IN UTL                   
         BZ    EIIF                                                             
         SR    RE,RE                                                            
         ICM   RE,7,DMCB+1         POINT TO RETURN TERMINAL ID                  
         MVC   CTTKLINE(8),0(RE)                                                
         B     KEYV6                                                            
*                                                                               
KEYV3    XC    DMCB(20),DMCB       VALIDATE TERMINAL ID                         
         GOTO1 ATERMVAL,DMCB,(X'40',FLDH)                                       
         TM    DMCB,X'C0'                                                       
         BNZ   EIIF                EXIT IF INVALID FORMAT                       
         SR    RE,RE                                                            
         ICM   RE,7,DMCB+1         POINT TO RETURN TERMINAL ID                  
         MVC   CTTKLINE(8),0(RE)                                                
         B     KEYV6                                                            
*                                                                               
KEYV4    L     RF,=A(VALVTLU)      VALIDATE V=VTAMLUID                          
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
         BNZ   EXIT                                                             
         B     KEYV6               BTAM ID HAS BEEN SET IN KEY                  
*                                                                               
KEYV5    L     RF,=A(VALPSVN)      VALIDATE #NNNNN TERM/PSWD NUMBER             
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
         BNZ   EXIT                                                             
         B     KEYVAL              BACK TO VALIDATE ALPHA FIELDS                
*                                                                               
KEYV6    XC    PRTID,PRTID         ECHO TERMINAL ID BACK TO TWA                 
         MVC   PRTID(8),CTTKLINE                                                
         MVI   PRTIDH+5,8                                                       
         OI    PRTIDH+6,X'80'                                                   
         EJECT                                                                  
* VALIDATE PAGE NUMBER - TREATED AS PASSWORD FOR PRINTERS                       
*                                                                               
KEYVD    MVI   PAGENUM,0           SET PAGENUM NOT INPUT                        
         XC    MASTLUID,MASTLUID                                                
         XC    SVPAGENO,SVPAGENO                                                
         GOTO1 AFVAL,PRTPAGEH                                                   
         BNZ   KEYVD1                                                           
         CLI   ACTN,ADD            CANT ADD/COPY BASIC RECORD                   
         BE    BADFUN                                                           
         CLI   ACTN,COPY                                                        
         BE    BADFUN                                                           
         B     KEYVE                                                            
KEYVD1   CLI   FLD,C'1'            PAGE NUMBER MUST BE 1 THRU 9                 
         BL    EIIF                                                             
         CLI   FLD,C'9'                                                         
         BH    EIIF                                                             
         LA    R1,PRTIDH                                                        
         ST    R1,FADR                                                          
         MVC   KEY,CTTKEY                                                       
         GOTO1 AREAD               READ MASTER TERMINAL REC                     
         BZ    EIIO                                                             
         TM    DMCB+8,X'12'        RECORD MUST EXIST                            
         BNZ   ERNF                                                             
         TM    CTTSTAT,X'01'       RECORD MUST NOT BE PASSIVE                   
         BNZ   EIIF                                                             
         LA    R5,CTTDATA                                                       
         SR    R6,R6                                                            
KEYVD2   CLI   0(R5),0             SEARCH FOR VTAMLUID IN MASTER REC            
         BE    KEYVD3                                                           
         CLI   0(R5),X'25'                                                      
         BNE   KEYVD2A                                                          
         TM    CTTRMDEV-CTTRMD(R5),X'80'                                        
         BZ    EIIF                DEVICE MUST BE PRINTER                       
         OI    TERMINFO,X'04'                                                   
         CLI   CTTRMDEV-CTTRMD(R5),X'82'                                        
         BNE   *+12                                                             
         OI    TERMINFO,X'18'      SET DEVICE IS SHUTTLE/AUTO                   
         B     KEYVD2A                                                          
         TM    CTTRMAT1-CTTRMD(R5),X'02'                                        
         BZ    *+8                                                              
         OI    TERMINFO,X'08'      SET DEVICE IS AUTO                           
KEYVD2A  CLI   0(R5),X'26'                                                      
         BE    *+14                                                             
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     KEYVD2                                                           
         USING CTLUID,R5                                                        
         MVC   MASTLUID,CTLUIID    SAVE LUID OF MASTER TERM RECORD              
KEYVD3   MVC   CTTKPASS,FLD        MOVE PAGENUM TO PASSWORD FIELD               
         MVC   SVPAGENO,FLD                                                     
         MVI   PAGENUM,1                                                        
         OI    TERMINFO,X'80'      SET PAGENUM SPECIFIED                        
         DROP  R5                                                               
         EJECT                                                                  
* VALIDATE THIS/LAST ACTIONS AND READ RECORD                                    
*                                                                               
KEYVE    MVC   KEY,CTTKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         MVC   TERMID,CTTKTID      SAVE ORIGIONAL TERMINAL ID                   
         LA    R1,PRTIDH           POSN CURSOR TO FRST KEY FIELD                
         ST    R1,FADR                                                          
         CLI   PAGENUM,0                                                        
         BE    *+12                                                             
         LA    R1,PRTPAGEH         POSN CURSOR TO SCND KEY FIELD                
         ST    R1,FADR                                                          
         CLI   ACTN,CHANGE                                                      
         BNE   KEYVG                                                            
         CLC   KEY,LKEY            CHECK KEY SEQUENCE FOR CHANGE                
         BE    *+8                                                              
         MVI   ACTN,DISPLAY                                                     
*                                                                               
KEYVG    CLI   ACTN,DISPLAY                                                     
*                                  DISPLAY FUNCTION ONLY WITH                   
         BNE   BADFUN                NEW PQ ELEMENTS MAINTAINED                 
*                                    BY GEN PROGRAM                             
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
         B     DISREC                                                           
*                                                                               
BADFUN   LA    R1,BASACTNH         HERE IF ACTION NOT DISPLAY                   
         ST    R1,FADR                                                          
         B     EFNV                FUNCTION NOT AVAILABLE                       
         EJECT                                                                  
* DISPLAY PRINTER DETAILS                                                       
*                                                                               
DISREC   TWAXC PRTPRIDH                                                         
         LA    R5,CTTDATA          R5=A(ELEMENT)                                
         LA    R6,PRTL1H           R6=A(FIRST QUEUE DISPLAY LINE)               
         USING LINED,R6                                                         
*                                                                               
DIS2     CLI   0(R5),0             E-O-R                                        
         BE    DIS6                                                             
         CLI   0(R5),X'28'         PRINTER ID ELEMENT                           
         BE    DISID                                                            
         CLI   0(R5),X'29'         PRINTER QUEUE ELEMENT                        
         BE    DISPQ                                                            
*                                                                               
DIS4     ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DIS2                                                             
*                                                                               
DIS6     MVI   NACTN,0             SET NEXT ACTION & EXIT                       
         TM    CTTSTAT,X'80'                                                    
         BZ    DIS7                                                             
         CLI   PAGENUM,0           TEST IF PAGE NUM INPUT                       
         BE    *+8                                                              
         OI    NACTN,OKRES         YES OK TO RESTORE                            
         B     EXIT                                                             
DIS7     LA    R1,PRTPRIDH                                                      
         ST    R1,FADR                                                          
         MVI   NACTN,OKCHA                                                      
         CLI   PAGENUM,0           TEST IF PAGE NUM INPUT                       
         BE    *+8                                                              
         OI    NACTN,OKDEL         YES OK TO DELETE                             
         TM    CTTSTAT,X'02'                                                    
         BO    *+8                                                              
         OI    NACTN,OKCOPY        OK TO COPY IF RECORD HAS NO LUID             
         B     EXIT                                                             
         EJECT                                                                  
* DISPLAY PRINTER ID ELEMENT                                                    
*                                                                               
         USING CTPRTD,R5                                                        
DISID    MVC   IDNUM,CTPRTID                                                    
         BAS   RE,GETIDNUM                                                      
         CLI   FERN,X'FF'                                                       
         BE    DISID2                                                           
         EDIT  (B2,IDNUM),(5,PRTPRID),ALIGN=LEFT                                
         MVI   FERN,X'FF'                                                       
         B     DISID4                                                           
DISID2   MVC   PRTPRID(L'IDALPH),IDALPH                                         
DISID4   LA    R7,PRTPRID+L'IDALPH-1                                            
         CLI   0(R7),C' '                                                       
         BNE   *+8                                                              
         BCT   R7,*-8                                                           
         MVI   1(R7),C','                                                       
         EDIT  (B1,CTPRTNUM),(3,2(R7)),ALIGN=LEFT                               
         B     DIS4                                                             
         SPACE 2                                                                
* DISPLAY PRINTER QUEUE ELEMENTS                                                
*                                                                               
         USING CTPRQD,R5                                                        
DISPQ    MVC   PRLINE,CTPRQDTA                                                  
         MVC   IDNUM,PRSRCID                                                    
         BAS   RE,GETIDNUM                                                      
         CLI   FERN,X'FF'                                                       
         BE    DISPQ2                                                           
         EDIT  (B2,IDNUM),(5,LINSRCID),ALIGN=LEFT                               
         MVI   FERN,X'FF'                                                       
DISPQ2   MVC   LINSRCID,IDALPH                                                  
         MVC   LINSUBID,PRSUBID                                                 
         CLI   CTPRQLEN,X'08'      CHECK IF NEW SHORT ELEMENT                   
         BNE   *+14                                                             
         MVC   BYTE,PRNEWCLS                                                    
         B     *+10                                                             
         MVC   BYTE,PRCLASS                                                     
         CLI   BYTE,0                                                           
         BNE   *+14                                                             
         MVC   LINCLASS,=C'ALL'                                                 
         B     DISPQ4                                                           
         MVC   LINCLASS(1),BYTE                                                 
         TM    BYTE,X'40'                                                       
         BO    DISPQ4                                                           
         MVI   LINCLASS,C'-'                                                    
         MVC   LINCLASS+1(1),BYTE                                               
         OI    LINCLASS+1,X'40'                                                 
DISPQ4   CLI   CTPRQLEN,X'08'      CHECK IF NEW SHORT ELEMENT                   
         BE    DISPQ6                IF SO GET NEXT                             
         LA    R7,LINSEQ             ELSE DISPLAY SEQUENCE #                    
         SR    R0,R0                                                            
         ICM   R0,3,PRSEQL                                                      
         EDIT  (R0),(5,0(R7)),ALIGN=LEFT                                        
         AR    R7,R0                                                            
         MVI   0(R7),C','                                                       
         SR    R0,R0                                                            
         ICM   R0,3,PRSEQH                                                      
         EDIT  (R0),(5,1(R7)),ALIGN=LEFT                                        
DISPQ6   LA    R6,LINNEXT(R6)                                                   
         LA    RE,PRTLAST          CHECK END OF SCREEN AS NEW RECORD            
         LA    RF,LINNEXT            STRUCTURE CAN HOLD MORE ELEMENTS           
         SR    RE,RF                                                            
         CR    R6,RE                                                            
         BNL   DIS6                                                             
         B     DIS4                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
* CHANGE PRINTER ELEMENTS OR ADD PRINTER/PAGE RECORD                            
*                                                                               
DATAVAL  MVC   KEYSAVE,KEY         SAVE KEY                                     
         MVC   CTTKEY,KEY                                                       
         XC    TERMNUM,TERMNUM     CLEAR PASSIVE DATA SAVE AREAS                
         XC    SVTRMEL,SVTRMEL                                                  
         MVC   SVTRMEL(4),=X'25208101'                                          
         XC    SVLUID,SVLUID                                                    
         MVI   SVSTAT,0                                                         
         MVI   INLUIDF,0           SET NO LUID INPUT                            
         XC    INLUID,INLUID                                                    
         XC    INLUIDX,INLUIDX                                                  
         LA    R1,PRTIDH           POSITION CURSOR TO TERMINAL ID               
         ST    R1,FADR                                                          
*                                                                               
DATAV1   CLI   ACTN,ADD            ADD FUNCTION                                 
         BNE   DATAV2                                                           
         CLI   PAGENUM,0           CANT ADD UNLESS PAGE NUMBER INPUT            
         BNE   *+16                                                             
         LA    R1,BASACTNH         POINT TO ACTION FIELD                        
         ST    R1,FADR                                                          
         B     EIAC                                                             
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
         TM    CTTSTAT,X'02'       CANT COPY IF HAS VTAM LUID                   
         BZ    *+16                                                             
         LA    R1,BASACTNH         POINT TO ACTION FIELD                        
         ST    R1,FADR                                                          
         B     EIAC                                                             
         TM    CTTSTAT,X'04'       CANT COPY UNLESS PRINTER RECORD              
         BO    *+16                                                             
         LA    R1,BASACTNH         POINT TO ACTION FIELD                        
         ST    R1,FADR                                                          
         B     EIAC                                                             
         MVC   TEMP(4),=X'01032600'                                             
         GOTO1 ADELMUL             DELETE ACTV/PASS#/VTAMLUID                   
         NI    CTTSTAT,255-X'02'                                                
         MVC   TEMP(10),=X'021F2021232425262700'                                
         GOTO1 ADELMUL             DELETE EVERYTHING BUT 28/29                  
         GOTO1 ABLDACT             BUILD & ADD ACTIVITY ELEMENT                 
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         MVC   KEY,KEYSAVE         RESTORE COPY TO KEY                          
         MVC   KEYNEXT,KEY                                                      
         MVC   CTTKEY,KEY                                                       
         B     DATAEND             GO TO ADD LOGIC                              
*                                                                               
DATAV3   MVC   SVSTAT,CTTSTAT      CHANGE FUNCTION - SAVE ORIG STATUS           
         TM    SVSTAT,X'04'                                                     
         BZ    *+8                                                              
         OI    TERMINFO,X'04'      SET TERM IS A PRINTER                        
         TM    SVSTAT,X'08'                                                     
         BZ    *+8                                                              
         OI    TERMINFO,X'08'      SET TERM IN AUTO MODE                        
         LA    R5,CTTDATA                                                       
         SR    R6,R6                                                            
DATAV3A  CLI   0(R5),0             SEARCH RECORD FOR PASSIVE ELEMENTS           
         BE    DATAV3X                                                          
DATAV3B  CLI   0(R5),X'03'                                                      
         BNE   DATAV3C                                                          
         MVC   TERMNUM,2(R5)       SAVE TERMINAL NUMBER PASSIVE                 
         B     DATAV3W                                                          
DATAV3C  CLI   0(R5),X'25'         SAVE TERMINAL DEFN ELEMENT                   
         BNE   DATAV3D                                                          
         MVC   SVTRMEL,0(R5)                                                    
         TM    CTTRMDEV-CTTRMD(R5),X'80'                                        
         BZ    DATAV3W                                                          
         OI    TERMINFO,X'04'      SET DEVICE IS PRINTER                        
         CLI   CTTRMDEV-CTTRMD(R5),X'82'                                        
         BNE   *+12                                                             
         OI    TERMINFO,X'18'      SET DEVICE IS SHUTTLE/AUTO                   
         B     DATAV3W                                                          
         TM    CTTRMAT1-CTTRMD(R5),X'02'                                        
         BZ    *+8                                                              
         OI    TERMINFO,X'08'      SET DEVICE IS AUTO                           
         B     DATAV3W                                                          
DATAV3D  CLI   0(R5),X'26'         SAVE LUID ID PASSIVE                         
         BNE   DATAV3W                                                          
         MVC   SVLUID,3(R5)                                                     
         B     DATAV3X                                                          
DATAV3W  IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     DATAV3A                                                          
DATAV3X  EQU   *                                                                
*                                                                               
DATAV4   TM    ACTINDS,X'80'       TEST MOVE TYPE CHANGE                        
         BZ    DATAV5                                                           
         LA    R4,IOAREAX          RECORD DATA TO BE MOVED IN IOAREAX           
         ST    R4,AREC                                                          
         MVC   KEY,LKEY            RESTORE LAST KEY & READ RECORD               
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0            MUST BE OK                                   
         BNE   EIIO                                                             
         TM    CTTSTAT,X'04'       CANT MOVE UNLESS PRINTER RECORD              
         BO    *+16                                                             
         LA    R1,BASACTNH         POINT TO ACTION FIELD                        
         ST    R1,FADR                                                          
         B     EIAC                                                             
         LA    R4,IOAREA                                                        
         ST    R4,AREC                                                          
         MVC   TEMP(4),=X'01032600'                                             
         GOTO1 ADELMUL             DELETE ACTV/PASS#/VTAMLUID                   
         NI    CTTSTAT,255-X'02'                                                
         MVC   TEMP(3),=X'282900'                                               
         GOTO1 ADELMUL             DELETE ALL ELS TO BE COPIED                  
         GOTO1 ACPYEL              COPY PRINTERID/PRINTERQ                      
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
         B     DVLUID                                                           
*                                                                               
DATAV5   L     RF,ADELMUL          NORMAL CHANGE - STRIP DOWN RECORD            
         MVC   TEMP(4),=X'01032600'                                             
         BASR  RE,RF               DELETE ACTV/PASS#/VTAMLUID                   
         NI    CTTSTAT,255-X'02'                                                
         MVC   TEMP(3),=X'282900'                                               
         BASR  RE,RF               DELETE PRINTERID/PRINTERQ                    
         TM    SVSTAT,X'04'                                                     
         BZ    DATAV8                                                           
         OI    TERMINFO,X'04'                                                   
         B     DATAV8                                                           
*                                                                               
DATAV8   GOTO1 ABLDACT             BUILD AND ADD ACTIVITY/DESC ELS              
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         EJECT                                                                  
* VALIDATE PRINTER ID                                                           
*                                                                               
CHA1     GOTO1 AFVAL,PRTPRIDH      PRINTER ID MUST BE INPUT                     
         BZ    EXIT                                                             
         CLC   FLD(6),=C'DELETE'   USE DELETE TO REMOVE OR IGNORE               
         BE    CHA2                                                             
         GOTO1 VSCANNER,DMCB,FLDH,(2,SCANBLK)                                   
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         MVI   FNDX,2                                                           
         CLI   4(R1),2                                                          
         BL    EMIF                                                             
         LA    R7,SCANBLK                                                       
         MVI   FNDX,1                                                           
*                                  VALIDATE USER-ID                             
         CLI   0(R7),0                                                          
         BE    EIIF                                                             
         CLI   1(R7),0                                                          
         BNE   EIIF                                                             
         MVC   FLDH+5(1),0(R7)                                                  
         MVC   FLD(10),12(R7)                                                   
         BAS   RE,GETIDALP                                                      
         CLI   FERN,X'FF'                                                       
         BNE   EXIT                                                             
         LA    R7,32(R7)                                                        
         MVI   FNDX,2                                                           
*                                  VALIDATE PRINTER NUMBER                      
         CLI   0(R7),0                                                          
         BE    EIIF                                                             
         CLI   1(R7),0                                                          
         BNE   EIIF                                                             
         TM    2(R7),X'80'                                                      
         BZ    EFNN                                                             
         OC    4(4,R7),4(R7)                                                    
         BZ    EFLM                                                             
         OC    4(3,R7),4(R7)                                                    
         BNZ   EFTB                                                             
         MVC   DUB(1),7(R7)                                                     
*                                  BUILD & ADD PRINTER ID ELEMENT               
         LA    R5,TEMP                                                          
         USING CTPRTD,R5                                                        
         XC    CTPRTEL(20),CTPRTEL                                              
         MVC   CTPRTEL(2),=X'2806'                                              
         MVC   CTPRTID,IDNUM                                                    
         MVC   CTPRTNUM,DUB                                                     
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         EJECT                                                                  
* VALIDATE PRINTER QUEUE FIELDS                                                 
*                                                                               
CHA2     LA    R6,PRTL1H            R6=A(PRINTER QUEUE FIELD)                   
         USING LINED,R6                                                         
*                                                                               
CHA3     CLI   0(R6),9                                                          
         BE    DVLUID                                                           
         XC    PRLINE,PRLINE                                                    
         MVI   FNDX,0                                                           
         GOTO1 AFVAL,LINSRCH       VALIDATE USER-ID                             
         BZ    CHAK                                                             
         CLC   FLD(6),=C'DELETE'                                                
         BE    CHAK                                                             
         BAS   RE,GETIDALP                                                      
         CLI   FERN,X'FF'                                                       
         BNE   EXIT                                                             
         MVC   PRSRCID,IDNUM                                                    
CHA4     GOTO1 AFVAL,LINSUBH       VALIDATE REPORT SUB-ID                       
         BNZ   CHA6                                                             
         MVC   LINSUBID,=C'ALL'                                                 
         MVI   LINSUBH+4,0                                                      
         MVI   LINSUBH+5,3                                                      
         OI    LINSUBH+6,X'80'                                                  
         B     CHA4                                                             
CHA6     CLI   FLDH+5,3                                                         
         BL    *+14                                                             
         MVC   PRSUBID,FLD                                                      
         B     CHA8                                                             
         CLI   FLDH+5,2                                                         
         BL    EFTS                                                             
         CLI   FLD+1,C'*'                                                       
         BNE   EIIF                                                             
         MVC   PRSUBID,FLD                                                      
CHA8     GOTO1 AFVAL,LINCLSH       VALIDATE REPORT CLASS                        
         BNZ   CHAA                                                             
         MVC   LINCLASS,=C'ALL'                                                 
         MVI   LINCLSH+4,0                                                      
         MVI   LINCLSH+5,3                                                      
         OI    LINCLSH+6,X'80'                                                  
         B     CHA8                                                             
CHAA     CLI   FLDH+5,1                                                         
         BH    *+14                                                             
         MVC   PRCLASS,FLD                                                      
         B     CHAC                                                             
         CLI   FLDH+5,3                                                         
         BL    *+18                                                             
         CLC   FLD(3),=C'ALL'                                                   
         BNE   EIIF                                                             
         B     CHAC                                                             
         CLI   FLD,C'-'                                                         
         BNE   EIIF                                                             
         MVC   PRCLASS,FLD+1                                                    
         NI    PRCLASS,X'BF'                                                    
CHAC     GOTO1 AFVAL,LINSEQH       VALIDATE REPORT SEQUENCE NUMBERS             
         BNZ   CHAE                                                             
         MVC   LINSEQ(7),=C'1,65000'                                            
         MVI   LINSEQH+4,0                                                      
         MVI   LINSEQH+5,7                                                      
         OI    LINSEQH+6,X'80'                                                  
         B     CHAC                                                             
CHAE     GOTO1 VSCANNER,DMCB,FLDH,(2,SCANBLK)                                   
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         MVI   FNDX,2                                                           
         CLI   4(R1),2                                                          
         BL    EMIF                                                             
         LA    R7,SCANBLK                                                       
         MVI   FNDX,1                                                           
CHAG     CLI   0(R7),0                                                          
         BE    EIIF                                                             
         CLI   1(R7),0                                                          
         BNE   EIIF                                                             
         TM    2(R7),X'80'                                                      
         BZ    EFNN                                                             
         OC    4(4,R7),4(R7)                                                    
         BZ    EFLM                                                             
         CLC   4(4,R7),=F'65000'                                                
         BH    EFTB                                                             
         CLI   FNDX,1                                                           
         BNE   CHAI                                                             
         MVC   PRSEQL,6(R7)                                                     
         MVI   FNDX,2                                                           
         LA    R7,32(R7)                                                        
         B     CHAG                                                             
CHAI     MVC   PRSEQH,6(R7)                                                     
         CLC   PRSEQH,PRSEQL                                                    
         BL    EFLM                                                             
*                                  BUILD & ADD PRINTER QUEUE ELEMENT            
         LA    R5,TEMP                                                          
         USING CTPRQD,R5                                                        
         XC    CTPRQEL(20),CTPRQEL                                              
         MVC   CTPRQEL(2),=X'290C'                                              
         MVC   CTPRQDTA,PRLINE                                                  
         GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE '),AREC,TEMP,=C'ADD=CODE'            
         CLI   12(R1),0            TEST FOR ERRORS                              
         BE    CHAK                                                             
         MVI   FERN,68                                                          
         B     EXIT                                                             
*                                  BUMP TO NEXT TWA LINE                        
CHAK     LA    R6,LINNEXT(R6)                                                   
         B     CHA3                                                             
         EJECT                                                                  
* TEST TO SEE IF RECORD NEEDS LUID PROCESSING                                   
*                                                                               
DVLUID   XC    DUMLUIDH,DUMLUIDH   SET NO LUID DUMMY INPUT FIELD                
         MVI   DUMLUIDH+4,X'80'                                                 
         MVI   DUMLUIDH+5,0                                                     
         XC    DUMLUID,DUMLUID                                                  
*                                                                               
DVLUID0A TM    TERMINFO,X'80'      TEST IF TERM/PAGENUM RECORD                  
         BZ    DVLUID0B            NO                                           
         OC    MASTLUID,MASTLUID   TEST IF MASTER TERM HAD LUID                 
         BZ    DVLUID0X            NO                                           
         MVI   DUMLUIDH+5,8        YES SIMULATE INPUT OF LUID                   
         MVC   DUMLUID,MASTLUID                                                 
         B     DVLUID0X                                                         
*                                                                               
DVLUID0B CLI   ACTN,CHANGE         TEST IF CHANGE ACTION                        
         BNE   DVLUID0X                                                         
         OC    SVLUID,SVLUID       TEST IF IT HAD ONE ANYWAY                    
         BZ    DVLUID0X            NO                                           
         MVI   DUMLUIDH+5,8        YES SIMULATE INPUT OF LUID                   
         MVC   DUMLUID,SVLUID                                                   
         B     DVLUID0X                                                         
*                                                                               
DVLUID0X GOTO1 AFVAL,DUMLUIDH      TEST IF LOGICAL UNIT ID INPUT                
         LA    RE,PRTIDH                                                        
         ST    RE,FADR                                                          
         CLI   FERN,1              TEST IF LUID INPUT                           
         BNZ   DVLUID2             YES                                          
*                                                                               
DVLUID1  OC    SVLUID,SVLUID       WAS THERE AN EXISTIING LUID                  
         BZ    DVLUIDX             NO                                           
         OI    INLUIDF,X'80'       YES MUST DELETE OLD LUID                     
         B     DVLUIDX                                                          
*                                                                               
DVLUID2  XC    DMCB(20),DMCB       VALIDATE AND READ LUID                       
         GOTO1 ATERMVAL,DMCB,(X'40',FLDH)                                       
         TM    DMCB,X'C0'                                                       
         BNZ   EIIF                ERROR IF INVALID FORMAT                      
         SR    R6,R6                                                            
         ICM   R6,7,DMCB+1         R6=A(LUID CL8 STANDARD FORMAT)               
         MVC   INLUID,0(R6)        SAVE INPUT VALUE                             
         CLC   INLUID,TERMID                                                    
         BE    EIIF                LUID CANT BE SAME AS TERMINAL ID             
         LA    R5,IOAREA2                                                       
         ST    R5,AREC                                                          
         MVC   KEY,CTTKEY                                                       
         MVC   KEY+7(8),INLUID                                                  
         GOTO1 AREAD               READ LUID RECORD                             
         ST    R4,AREC                                                          
         MVC   KEY,CTTKEY                                                       
         TM    DMCB+8,X'10'        TEST NOT FOUND                               
         BO    DVLUID3                                                          
         TM    CTTSTAT-CTTREC(R5),X'03'                                         
         BO    DVLUID5                                                          
         B     EIIF                ERROR IF NOT PASSIVE LUID REC                
*                                                                               
DVLUID3  CLI   ACTN,ADD            LUID RECORD N/F - OK FOR ADD                 
         BNE   DVLUID4                                                          
         OI    INLUIDF,X'01'       SET MUST ADD LUID PASSIVE                    
         B     DVLUIDW                                                          
*                                                                               
DVLUID4  CLI   SVLUID,0            LUID RECORD N/F - CHANGE                     
         BE    *+8                                                              
         OI    INLUIDF,X'80'       SET MUST DELETE OLD LUID                     
         OI    INLUIDF,X'01'       SET MUST ADD LUID PASSIVE                    
         B     DVLUIDW                                                          
*                                                                               
DVLUID5  LA    RE,CTTDATA-CTTREC(R5)                                            
         SR    RF,RF                                                            
DVLUID5A CLI   0(RE),0             SEARCH FOR LUID ELEMENT IN LUID REC          
         BNE   *+6                                                              
         DC    H'0'                DIE IF N/F                                   
         CLI   0(RE),X'26'                                                      
         BE    *+14                                                             
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     DVLUID5A                                                         
         ST    RE,AINLUIDX         SAVE A(LUID EL) IN LUID REC                  
         MVC   INLUIDX,CTLUIID-CTLUID(RE)                                       
DVLUID5B CLC   CTTKTID,INLUIDX                                                  
         BNE   DVLUID5C                                                         
         CLC   INLUID,SVLUID                                                    
         BNE   DVLUID5C                                                         
         OI    INLUIDF,X'02'       SET MUST CHANGE LUID PASSIVE                 
         B     DVLUIDW                                                          
DVLUID5C B     EIIF                                                             
*                                                                               
DVLUIDW  LA    R5,TEMP             R5=A(TERMINAL LUID EL)                       
         USING CTLUID,R5                                                        
         XC    TEMP,TEMP                                                        
         MVC   CTLUIEL(2),=X'260B'                                              
         MVC   CTLUIID,INLUID      MOVE LUID TO ELEMENT                         
         GOTO1 APUTEL                                                           
         OI    CTTSTAT,X'02'       SET STATUS TO SHOW LUID EL                   
*                                                                               
DVLUIDX  B     DATAEND                                                          
         DROP  R5                                                               
         EJECT                                                                  
* I/O HANDLING FOR ADD/CHANGE                                                   
*                                                                               
DATAEND  LA    R1,PRTIDH           POSN TO 1ST KEY FLD & SET OK                 
         ST    R1,FADR                                                          
         MVI   FNDX,0                                                           
         MVI   FERN,X'FF'                                                       
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
         TM    SVSTAT,X'04'        WAS IT A PRINTER BEFORE                      
         BO    CHGPRT3             YES                                          
         GOTO1 AADD                NO MUST ADD PASSIVE                          
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         B     CHGPRT4                                                          
*                                                                               
CHGPRT3  LA    R5,IOAREA2          READ OLD PRINTER PASSIVE                     
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
CHGPRT4  MVC   KEY,KEYSAVE         RESTORE ORIGINAL PRINTER REC                 
         LA    R0,IOAREA                                                        
         LA    R1,2000                                                          
         LA    RE,IOAREAX                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     DATALU                                                           
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
         B     DATALU                                                           
         SPACE 2                                                                
DATALU   CLI   INLUIDF,0           PROCESS LOGICAL UNIT PASSIVE                 
         BE    DATAX                                                            
DATALU1  TM    INLUIDF,X'80'       TEST TO DELETE OLD LUID (SVLUID)             
         BZ    DATALU2                                                          
         XC    DELKEY,DELKEY       READ OLD LUID REC INTO I/O2                  
         MVI   DELKEY,C'T'                                                      
         MVC   DELKEY+CTTKTID-CTTREC(8),SVLUID                                  
         MVC   DELKEY+CTTKPASS-CTTREC(10),SVPAGENO                              
         L     RF,=A(DELREC)       DELETE UNWANTED PASSIVE                      
         A     RF,MYRELO                                                        
         BASR  RE,RF                                                            
*                                                                               
DATALU2  TM    INLUIDF,X'01'       TEST TO ADD NEW LUID (INLUID)                
         BZ    DATALU3                                                          
         MVC   CTTKTID,INLUID                                                   
         NI    CTTSTAT,X'7F'                                                    
         OI    CTTSTAT,X'03'       SET PASSIVE/LUID                             
         LA    R5,CTTDATA                                                       
         SR    R6,R6                                                            
DATALU2A CLI   0(R5),0             SEARCH FOR LUID ELEMENT                      
         BNE   *+6                                                              
         DC    H'0'                DIE IF N/F                                   
         CLI   0(R5),X'26'                                                      
         BE    *+14                                                             
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     DATALU2A                                                         
         OI    2(R5),X'80'         SET LUID ELEMENT POINTS BACK                 
         MVC   3(8,R5),TERMID      SET NAME OF ORIGINAL RECORD                  
         GOTO1 AADD                                                             
         B     DATAX                                                            
*                                                                               
DATALU3  TM    INLUIDF,X'02'       TEST TO CHANGE OLD LUID (SVLUID)             
         BZ    DATAX                                                            
         LA    R4,IOAREA2          SET TO READ OLD LUID REC                     
         ST    R4,AREC                                                          
         XC    CTTKEY,CTTKEY       READ MASTER TERM REC INTO I/O2               
         MVI   CTTKEY,C'T'                                                      
         MVC   CTTKTID,SVLUID                                                   
         MVC   CTTKPASS,SVPAGENO                                                
         MVC   KEYSAVE,KEY                                                      
         MVC   KEY,CTTKEY                                                       
         MVI   UPDATE,C'Y'         SET RFU                                      
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BNE   DATAX                                                            
         LA    R4,IOAREA                                                        
         ST    R4,AREC                                                          
         MVC   CTTKTID,SVLUID                                                   
         MVC   CTTKPASS,SVPAGENO                                                
         NI    CTTSTAT,X'7F'                                                    
         OI    CTTSTAT,X'03'       SET PASSIVE/LUID                             
         LA    R5,CTTDATA                                                       
         SR    R6,R6                                                            
DATALU3A CLI   0(R5),0             SEARCH FOR LUID ELEMENT                      
         BNE   *+6                                                              
         DC    H'0'                DIE IF N/F                                   
         CLI   0(R5),X'26'                                                      
         BE    *+14                                                             
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     DATALU3A                                                         
         OI    2(R5),X'80'         SET LUID ELEMENT POINTS BACK                 
         MVC   3(8,R5),TERMID      SET NAME OF ORIGINAL RECORD                  
         GOTO1 AWRITE                                                           
         B     DATAX                                                            
*                                                                               
DATAX    MVI   NACTN,OKCHA         OK TO CHANGE                                 
         CLI   PAGENUM,0                                                        
         BE    *+8                                                              
         OI    NACTN,OKDEL         OK TO DELETE IF PAGENUM DEFINED              
         TM    CTTSTAT,X'02'                                                    
         BO    *+8                                                              
         OI    NACTN,OKCOPY        OK TO COPY IF RECORD HAS NO LUID             
DATAX1   LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
* CONVERT ID NUMBER TO USER-ID                                                  
*                                                                               
GETIDNUM NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         LA    R6,IOAREA2                                                       
         USING CTIREC,R6                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),IDNUM                                                
         NI    CTIKID+8,255-X'80'  TURN-OFF GENERIC ID BIT                      
         ST    R6,AREC                                                          
         MVC   KEY,CTIKEY                                                       
         GOTO1 AREAD                                                            
         MVC   KEY,KEYSAVE                                                      
         ST    R4,AREC                                                          
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'                                                     
         BO    ERNF                                                             
         TM    DMCB+8,X'02'                                                     
         BO    ERID                                                             
         LA    R1,CTIDATA                                                       
         SR    RE,RE                                                            
GETIDN2  CLI   0(R1),0                                                          
         BE    EIRT                                                             
         CLI   0(R1),X'02'                                                      
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     GETIDN2                                                          
         MVC   IDALPH,2(R1)                                                     
         B     EXIT                                                             
         EJECT                                                                  
* CONVERT USER-ID TO ID NUMBER                                                  
*                                                                               
GETIDALP NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         LA    R6,IOAREA2                                                       
         USING CTIREC,R6                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,FLD                                                       
         ST    R6,AREC                                                          
         MVC   KEY,CTIKEY                                                       
         GOTO1 AREAD                                                            
         MVC   KEY,KEYSAVE                                                      
         ST    R4,AREC                                                          
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'                                                     
         BO    ERNF                                                             
         TM    DMCB+8,X'02'                                                     
         BO    ERID                                                             
         XC    IDNUM,IDNUM                                                      
         LA    R1,CTIDATA                                                       
         SR    RE,RE                                                            
GETIDA2  CLI   0(R1),0                                                          
         BE    GETIDAX                                                          
         CLI   0(R1),X'02'         ID NUMBER ELEMENT                            
         BE    GETIDA6                                                          
         CLI   0(R1),X'07'         ID OPTIONS ELEMENT                           
         BE    GETIDA8                                                          
GETIDA4  IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     GETIDA2                                                          
GETIDA6  MVC   IDNUM,2(R1)                                                      
         B     GETIDA4                                                          
GETIDA8  OC    IDNUM,IDNUM         TEST ID NUMBER SET                           
         BZ    GETIDA4                                                          
         TM    2(R1),X'40'         YES - TEST IF A GENERIC USER-ID              
         BZ    *+8                                                              
         OI    IDNUM,X'80'         YES - SET GENERIC ID FLAG                    
         B     GETIDA4                                                          
GETIDAX  OC    IDNUM,IDNUM                                                      
         BZ    EIRT                                                             
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FOLLOWING TABLES/SUBROUTINES ARE ONLY ADDRESSABLE VIA =A(.....)     *         
***********************************************************************         
*                                                                               
         DROP  RB,RA                                                            
         SPACE 2                                                                
* GET BTAM TERMINAL ID FROM VTAM LUID INPUT AS V=XXXXXXXX                       
*                                                                               
VALVTLU  CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING VALVTLU,RB                                                       
         SR    R0,R0              R0=ERROR NUMBER (ZERO IS OK)                  
         MVC   DUB(8),FLD+2                                                     
         SR    R1,R1                                                            
         IC    R1,FLDH+5                                                        
         SH    R1,=H'2'                                                         
         BP    *+12                                                             
         LA    R0,2                                                             
         B     VALVTLUX                                                         
         STC   R1,FLDH+5                                                        
         MVC   FLD+0(8),DUB                                                     
         XC    FLD+8(2),FLD+8                                                   
         XC    DMCB(20),DMCB       VALIDATE AND READ VTAMLUID                   
         GOTO1 ATERMVAL,DMCB,(X'60',FLDH)                                       
         TM    DMCB,X'C0'                                                       
         BZ    *+12                                                             
         LA    R0,2                                                             
         B     VALVTLUX            EXIT IF INVALID FORMAT                       
         SR    R5,R5                                                            
         ICM   R5,7,DMCB+9                                                      
         BNZ   *+12                                                             
         LA    R0,13                                                            
         B     VALVTLUX            EXIT IF RECORD NOT FOUND                     
         TM    CTTSTAT-CTTREC(R5),X'03'                                         
         BO    *+12                                                             
         LA    R0,2                                                             
         B     VALVTLUX            EXIT IF NOT PASSIVE VTAMLUID REC             
         LA    R5,CTTDATA-CTTREC(R5)                                            
         SR    R6,R6                                                            
VALVTLU1 CLI   0(R5),0             SEARCH FOR LUID ELEMENT                      
         BNE   *+6                                                              
         DC    H'0'                DIE IF N/F                                   
         CLI   0(R5),X'26'                                                      
         BE    *+14                                                             
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     VALVTLU1                                                         
         USING CTLUID,R5                                                        
         MVC   CTTKLINE(8),CTLUIID                                              
*                                                                               
VALVTLUX LTR   R0,R0               EXIT WITH CC=EQL IF OK                       
         BZ    *+8                                                              
         STC   R0,FERN                                                          
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* GET TERMINAL NAME FROM PASSIVE NUMBER #NNNNNN                                 
*                                                                               
VALPSVN  CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING VALPSVN,RB                                                       
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
         XC    PRTID,PRTID         MOVE TERMINAL ID TO TWA                      
         MVC   PRTID(8),CTPASDTA                                                
         MVI   PRTIDH+5,8                                                       
         NI    PRTIDH+4,X'F1'                                                   
         OI    PRTIDH+6,X'80'                                                   
*                                                                               
VALPSVNX LTR   R0,R0               EXIT WITH CC=ZERO IF OK                      
         BZ    *+8                                                              
         STC   R0,FERN                                                          
         XIT1                                                                   
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
         EJECT                                                                  
* DSECT TO COVER TEMP W/S                                                       
*                                                                               
WORKD    DSECT                                                                  
MYRELO   DS    A                                                                
ATERMVAL DS    A                                                                
ATRMEL   DS    A                                                                
*                                                                               
AINLUIDX DS    A                                                                
TERMID   DS    CL8                                                              
SVLUID   DS    CL8                                                              
INLUID   DS    CL8                                                              
INLUIDX  DS    CL8                                                              
INLUIDF  DS    X                                                                
*                                                                               
BYTE     DS    C                                                                
*                                                                               
SVTRMEL  DS    CL32                                                             
SVSTAT   DS    X                                                                
TERMNUM  DS    XL2                                                              
TERMINFO DS    X                                                                
PAGENUM  DS    C                                                                
MASTLUID DS    CL8                                                              
SVPAGENO DS    CL10                                                             
DUMLUIDH DS    XL8                                                              
DUMLUID  DS    CL8                                                              
*                                                                               
PRLINE   DS    0CL16                                                            
PRSRCID  DS    XL2                                                              
PRSUBID  DS    CL3                                                              
PRNEWCLS DS    C                   NEW SHORT PQ ELEMENT STRUCTURE               
         ORG   PRNEWCLS                                                         
PRSEQL   DS    XL2                                                              
PRCLASS  DS    C                                                                
PRSEQH   DS    XL2                                                              
         DS    XL6                                                              
*                                                                               
IDNUM    DS    H                                                                
IDALPH   DS    CL10                                                             
SCANBLK  DS    3CL32                                                            
*                                                                               
DELKEY   DS    CL25                                                             
DELSAVE  DS    CL25                                                             
IOAREA2  DS    2000C                                                            
WORKX    EQU   *                                                                
         SPACE 1                                                                
* DSECT TO COVER PRINTER QUEUE INPUT LINE                                       
*                                                                               
LINED    DSECT                                                                  
*                                                                               
LINSRCH  DS    CL8                                                              
LINSRCID DS    CL9                                                              
LINSUBH  DS    CL8                                                              
LINSUBID DS    CL3                                                              
LINCLSH  DS    CL8                                                              
LINCLASS DS    CL3                                                              
LINSEQH  DS    CL8                                                              
LINSEQ   DS    CL11                                                             
LINNEXT  EQU   *-LINED                                                          
         SPACE 1                                                                
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
* CTLFMACTNS                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTLFMACTNS                                                     
         PRINT ON                                                               
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFME9D                                                                      
       ++INCLUDE CTLFME9D                                                       
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005CTLFM16   05/01/02'                                      
         END                                                                    
