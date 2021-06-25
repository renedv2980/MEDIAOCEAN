*          DATA SET CTFUS00S   AT LEVEL 033 AS OF 05/01/02                      
*PHASE TA0800A                                                                  
*INCLUDE EXPRESS                                                                
*INCLUDE GETIDS                                                                 
*INCLUDE HEXIN                                                                  
         TITLE 'CTFUS00 - FAST UPDATE SYSTEM PROFILES'                          
FUSPROFS CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**FUS**,RA,R8,RR=RE                                  
         USING WORKD,RC            RC=A(W/S)                                    
         ST    RE,RELO                                                          
         MVC   SVPARMS,0(R1)       SAVE S/R PARM LIST                           
         L     R9,SVTWA                                                         
         USING CTFUSFFD,R9        R9=A(TWA)                                     
*                                  BUILD ADDRESS DIRECTORY                      
         L     RE,=V(EXPRESS)                                                   
         A     RE,RELO                                                          
         ST    RE,PEXPRESS                                                      
         L     RE,=V(GETIDS)                                                    
         A     RE,RELO                                                          
         ST    RE,PGETIDS                                                       
         L     RE,SVCFACS                                                       
         USING COMFACSD,RE                                                      
         MVC   PDATAMGR,CDATAMGR                                                
         MVC   PGETMSG,CGETMSG                                                  
         MVC   PHELLO,CHELLO                                                    
         MVC   PHEXIN,CHEXIN                                                    
         MVC   PHEXOUT,CHEXOUT                                                  
         MVC   PSCANNER,CSCANNER                                                
         MVC   PSWITCH,CSWITCH                                                  
         MVC   PDATCON,CDATCON                                                  
         MVC   PGETTXT,CGETTXT                                                  
         MVC   PDICTATE,CDICTATE                                                
         MVC   PPERVAL,CPERVAL                                                  
         L     RF,CGETFACT                                                      
         DROP  RE                                                               
*                                  GET A(SYSTEMS LIST)                          
         GOTO1 (RF),DMCB,0                                                      
         L     R1,0(R1)                                                         
         L     R1,FASYSLST-FACTSD(R1)                                           
         LA    R1,6(R1)                                                         
         LA    R1,SYSLLEN(R1)      BUMP PAST SERVICE TABLE ENTRY                
         ST    R1,ASYSTAB                                                       
*                                  SET VALUES FROM UTL                          
         L     RE,SVUTL                                                         
         USING UTLD,RE                                                          
         MVC   LANG,TLANG          SAVE CONNECTED LANGUAGE CODE                 
         XC    PASSWD,PASSWD                                                    
         TM    TFLAG,TFLAGSEC      TEST CONNECTED TO PASSWORD                   
         BZ    *+10                                                             
         MVC   PASSWD,TPASSWD      SAVE PASSWORD #                              
         MVC   USERID,TUSER                                                     
         MVI   INDICS,0                                                         
         CLI   TOFFICE+3,C'*'                                                   
         BNE   *+8                                                              
         OI    INDICS,X'80'                                                     
         DROP  RE                                                               
*                                                                               
         GOTO1 PDICTATE,DMCB,C'LU  ',DDDCLST,DDDSLST                            
*                                                                               
         CLI   INITFLAG,0          CHECK FIRST TIME INITIALISATION FLAG         
         BNE   FUSP030               IF NOT CONTINUE                            
         MVI   INITFLAG,1            ELSE SET UP USERID INPUT FIELD             
         TM    INDICS,X'80'          IF AUTHORISED                              
         BO    FUSP010                                                          
         TM    TWAAUTH-TWAD(R9),X'40'  OR IF AUTH=4000                          
         BNO   FUSP020                                                          
*                                                                               
FUSP010  MVCDD FUSUHD,CT#USRID     SET UP USERID INPUT FIELD                    
         GOTO1 PDICTATE,DMCB,C'SL  ',FUSUHD                                     
         OI    FUSUHDH+6,X'80'                                                  
         NI    FUSUIDH+1,X'FF'-X'20'                                            
         OI    FUSUIDH+6,X'80'                                                  
         MVI   INITFLAG,2          FLAG USERID FIELD SET UP                     
*                                                                               
FUSP020  B     EXIT                EXIT FIRST TIME PASS                         
*                                                                               
FUSP030  XC    MSG,MSG                                                          
         MVI   UPDFLAG,0                                                        
         GOTO1 PDATCON,DMCB,(5,0),(3,TODAYB)                                    
         LA    R1,FVAL                                                          
         ST    R1,AFVAL                                                         
         MVC   ERRMSGN,NOERROR     SET NO ERROR                                 
         MVI   KUIDFLAG,0                                                       
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         B     VALACTN                                                          
         EJECT                                                                  
***********************************************************************         
*        VALIDATE ACTION                                              *         
***********************************************************************         
         SPACE 1                                                                
VALACTN  MVI   ACTION,DISPLAY      DEFAULT IS DISPLAY IF N/I                    
         LA    R3,ACTNTAB                                                       
         GOTO1 AFVAL,FUSACTNH                                                   
         BE    *+12                                                             
         LA    RF,CT@DSP                                                        
         B     VALACT4                                                          
         SR    R1,R1                                                            
         IC    R1,FLDH+5                                                        
         CLI   FLDH+5,3             ONLY FIRST 3 BYTES OF INPUT                 
         BNH   *+8                                                              
         LA    R1,3                                                             
         BCTR  R1,0                R1=L'COMPARE                                 
*                                                                               
VALACT2  CLI   0(R3),X'FF'         END OF LIST                                  
         BE    EIIF                                                             
         EX    0,0(R3)             FIND KEYWORD                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),0(RF)       MATCH TABLE WITH INPUT                        
         BE    VALACT4                                                          
         LA    R3,L'ACTNTAB(R3)    BUMP TO NEXT ENTRY                           
         B     VALACT2                                                          
*                                                                               
VALACT4  MVC   FUSACTN,0(RF)       RE-DISPLAY ACTION                            
         OI    FUSACTNH+6,X'80'                                                 
         TM    5(R3),X'80'         X'80' MEANS DDS ONLY                         
         BZ    VALACT6                                                          
         TM    TWAAUTH-TWAD(R9),X'80'  ALLOW IF AUTH=8000                       
         BO    VALACT6                                                          
         TM    INDICS,X'80'                                                     
         BNO   EIIF                                                             
VALACT6  MVC   ACTION,4(R3)         SET ACTION FROM TABLE                       
         B     VALSYS                                                           
         EJECT                                                                  
***********************************************************************         
*        VALIDATE SYSTEM                                              *         
***********************************************************************         
         SPACE 1                                                                
VALSYS   GOTO1 AFVAL,FUSSYSH                                                    
         BNE   EMIF                                                             
         MVI   SYSSENUM,0                                                       
         L     R1,ASYSTAB                                                       
         USING SYSLSTD,R1          R1=A(SYSTEMS TABLE)                          
         ZIC   RE,FLDH+5                                                        
         BCTR  RE,0                RE=L'INPUT-1                                 
*                                                                               
VALSYS2  CLI   SYSLNUM,0           TEST EOT                                     
         BE    EIIF                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),SYSLNAME                                                  
         BE    *+12                                                             
         LA    R1,SYSLLEN(R1)                                                   
         B     VALSYS2                                                          
         MVC   SYSNAME(L'SYSLNAME),SYSLNAME                                     
         MVC   SYSEQU,SYSLUSLT                                                  
         MVC   SYSNUM,SYSLNUM                                                   
         MVC   SYSINDS,SYSLIND1                                                 
         DROP  R1                                                               
         CLC   FLD,SYSNAME                                                      
         BE    VALSYS3                                                          
         MVC   FUSSYS,SPACES                                                    
         MVC   FUSSYS(7),SYSNAME                                                
         OI    FUSSYSH+6,X'80'                                                  
*                                  READ USER ID RECORD                          
VALSYS3  LA    R4,IO                                                            
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),USERID                                               
         MVC   KEY,CTIKEY                                                       
         GOTO1 CTIO,DMCB,DMREAD,(R4)                                            
         BNE   EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
*                                                                               
         LA    R5,CTIDATA          FIND AGENCY/SYSTEM ELEMENTS                  
         SR    R1,R1                                                            
         XC    AGYVALS,AGYVALS                                                  
         MVC   USERIDA,SPACES                                                   
*                                                                               
VALSYS4  CLI   0(R5),0             TEST EOR                                     
         BE    VALSYSC                                                          
         CLI   0(R5),X'02'         USERID ALPHA                                 
         BE    VALSYS7                                                          
         CLI   0(R5),X'06'         AGENCY ID                                    
         BE    VALSYS8                                                          
         CLI   0(R5),X'21'         SYSTEM ELEMENT                               
         BE    VALSYSA                                                          
*                                                                               
VALSYS6  IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     VALSYS4                                                          
*                                                                               
         USING CTDSCD,R5                                                        
VALSYS7  MVC   USERIDA,CTDSC       SET CONNECT USERID ALPHA                     
         B     VALSYS6                                                          
*                                                                               
         USING CTAGYD,R5                                                        
VALSYS8  MVC   AGYALPH,CTAGYID     SET AGENCY ALPHA                             
         B     VALSYS6                                                          
*                                                                               
         USING CTSYSD,R5                                                        
VALSYSA  CLC   CTSYSNUM,SYSNUM                                                  
         BNE   VALSYS6                                                          
         MVC   AGYBINY,CTSYSAGB    SET AGENCY BINARY                            
         MVC   SYSSENUM,CTSYSSE    SET SYSTEM SE NUMBER                         
         B     VALSYS6                                                          
*                                                                               
VALSYSC  OC    AGYALPH,AGYALPH                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   SYSSENUM,0          TEST ID VALID FOR THIS SYSTEM                
         BE    EIIF                                                             
*                                                                               
VALSYSX  B     VALPRG                                                           
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PROGRAM                                             *         
***********************************************************************         
         SPACE 1                                                                
VALPRG   GOTO1 AFVAL,FUSPRGH                                                    
         BNE   EMIF                                                             
         CLI   FLDH+5,2                                                         
         BL    EIIF                                                             
         MVI   PRGNUM,0                                                         
         MVC   PRGNUM+1(2),FLD     2 CHARS MEANS OFFLINE PROG NUM               
         BE    VALPRG1                                                          
         MVC   PRGNUM,FLD          3 CHARS MEANS ONLINE PROG NAME               
*                                  READ FIELD DEFINITION RECORD                 
VALPRG1  LA    R4,IO2                                                           
         USING CTUREC,R4                                                        
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKTYP,C'U'                                                     
         MVC   CTUKSYS,SYSEQU                                                   
         MVC   CTUKPROG,PRGNUM                                                  
         MVC   CTUKLANG,LANG       SET CONNECTED LANGUAGE CODE                  
         MVC   KEY,CTUKEY                                                       
         GOTO1 CTIO,DMCB,DMREAD,(R4)                                            
         BNE   EIIO                                                             
         CLI   DMCB+8,0                                                         
         BE    VALPRG1A            DEFINITION RECORD FOUND                      
*                                    ELSE DEFAULT LANGUAGE CODE NULL            
         MVI   KEY+CTUKLANG-CTUKEY,0                                            
         GOTO1 CTIO,DMCB,DMREAD,(R4)                                            
         BNE   EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                DEFINITION RECORD NOT FOUND                  
*                                                                               
VALPRG1A CLC   SYSEQU,LSYSEQU                                                   
         BNE   *+14                                                             
         CLC   PRGNUM,LPRGNUM                                                   
         BE    VALPRG4                                                          
         OI    INDICS,X'40'        SET GLOBAL KEY CHANGE FLAG                   
         LA    R5,CTUDATA          FIND DESCRIPTION ELEMENT                     
         SR    R1,R1                                                            
         XC    FUSPRGD,FUSPRGD                                                  
         OI    FUSPRGDH+6,X'80'                                                 
*                                                                               
VALPRG2  CLI   0(R5),0                                                          
         BE    VALPRG4                                                          
         CLI   0(R5),X'02'                                                      
         BE    *+14                                                             
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     VALPRG2                                                          
         IC    R1,1(R5)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FUSPRGD(0),2(R5)    MOVE PROGRAM DESCRIPTION TO TWA              
*                                                                               
VALPRG4  B     GETPID                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        GET PRINCIPLE ID FROM AGENCY ACCESS RECORD                   *         
***********************************************************************         
         SPACE 1                                                                
GETPID   LA    R4,IO                                                            
         USING CT5REC,R4                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KEY,C'5'                                                      
         MVC   CT5KALPH,AGYALPH                                                 
         MVC   KEY,CT5KEY                                                       
*                                  READ ACCESS RECORD                           
         GOTO1 CTIO,DMCB,DMREAD,(R4)                                            
         BNE   EIIO                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,CT5DATA                                                       
         SR    R1,R1                                                            
         XC    PUSERIDN,PUSERIDN                                                
*                                                                               
GPID010  CLI   0(R3),0             EXTRACT ELEMENT DATA                         
         BE    VALUID                                                           
         CLI   0(R3),X'02'         PID NUMBER ELEMENT                           
         BE    GPIDPID                                                          
*                                                                               
GPID020  IC    R1,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R1                                                            
         B     GPID010                                                          
*                                                                               
GPIDPID  MVC   PUSERIDN,2(R3)                                                   
         B     VALUID                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE USER-ID AS INPUT                                    *         
***********************************************************************         
         SPACE 1                                                                
VALUID   XC    INUNUM,INUNUM                                                    
         MVI   LISTFLAG,0                                                       
         MVC   INUALPH,SPACES                                                   
         CLI   INITFLAG,2          CHECK IF USERID FIELD SET UP                 
         BNE   VALUIDX             ELSE CONTINUE WITHOUT UID CHECK              
         GOTO1 AFVAL,FUSUIDH                                                    
         BE    VUID004                                                          
*                                  HERE FOR NO USERID INPUT                     
         XC    FUSUID,FUSUID                                                    
         OI    FUSUIDH+6,X'80'                                                  
         MVI   LISTFLAG,1          FLAG FOR FULL AGENCY/USERID LIST             
*        MVC   INUNUM,=XL2'0001'   REMOVE THIS - DDS ID GETS LOST               
         XC    INUNUM,INUNUM       ID VALUE FOR FIRST KEY                       
         B     VALUIDX                                                          
*                                  HERE FOR USERID INPUT                        
VUID004  SR    RF,RF                                                            
         IC    RF,FLDH+5                                                        
         BCTR  RF,0                                                             
         CLI   FLDH+5,4                                                         
         BH    VUID008                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),CT@ALL       REQUEST FOR AGENCY LEVEL                     
         BNE   VUID008                                                          
         MVC   FUSUID(4),CT@ALL                                                 
         OI    FUSUIDH+6,X'80'     REDISPLAY USER-ID                            
*                                  CHECK SECURITY AGENCY LEVEL                  
VUID006  TM    INDICS,X'80'        OK IF DDS                                    
         BO    VALUIDX                                                          
         CLC   PUSERIDN,USERID     OK IF PRINCIPLE ID                           
         BE    VALUIDX                                                          
         B     EISL                  ELSE EXIT ERROR                            
*                                                                               
VUID008  EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   INUALPH(0),FLD                                                   
         LA    R4,IO                                                            
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID,INUALPH                                                   
         MVC   KEY,CTIKEY                                                       
*                                  READ ID RECORD                               
         GOTO1 CTIO,DMCB,DMREAD,(R4)                                            
         BNE   EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         XC    PROGAUTH,PROGAUTH                                                
         LA    R3,CTIDATA                                                       
*                                  EXTRACT ELEMENT DATA                         
VUID010  CLI   0(R3),0                                                          
         BE    VUID100                                                          
         CLI   0(R3),X'02'         USERID# POINTER                              
         BE    VUID030                                                          
         CLI   0(R3),X'06'         AGENCY ID                                    
         BE    VUID040                                                          
         CLI   0(R3),X'21'         SYSTEM                                       
         BE    VUID050                                                          
VUID020  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VUID010                                                          
*                                                                               
VUID030  MVC   INUNUM,2(R3)                                                     
         B     VUID020                                                          
*                                                                               
VUID040  MVC   INUAGY,2(R3)                                                     
         B     VUID020                                                          
*                                                                               
         USING CTSYSD,R3                                                        
VUID050  CLI   CTSYSNUM,X'0A'      SAVE CONTROL/PRO AUTH CODE                   
         BNE   VUID020                                                          
         MVC   PROGAUTH,CTSYSALL                                                
         LA    R1,CTSYSPGM                                                      
         ZIC   RE,CTSYSLEN                                                      
*                                                                               
VUID052  CH    RE,=H'16'                                                        
         BNH   VUID020                                                          
         L     RF,SVUTL            FIND =FUS PROGRAM ENTRY                      
         CLC   0(1,R1),TPRG-UTLD(RF)                                            
         BE    VUID054                                                          
         LA    R1,3(R1)            GET NEXT PROGAM CODE                         
         SH    RE,=H'3'                                                         
         B     VUID052                                                          
*                                                                               
VUID054  MVC   PROGAUTH,1(R1)      SAVE ACCESS CODE                             
         B     VUID020                                                          
         DROP  R3                                                               
*                                  CHECK VALID USERID ACCESS                    
VUID100  TM    INDICS,X'80'        OK IF DDS                                    
         BO    VUID200                                                          
         CLC   USERID,INUNUM       OK IF SAME AS CONNECT USERID                 
         BE    VUID200                                                          
         CLC   INUAGY,AGYALPH      NOT OK IF NOT SAME AGENCY                    
         BNE   EISL                                                             
         TM    PROGAUTH,X'40'      OK IF =FUS AUTH=4000                         
         BNO   EUAU                                                             
         CLC   USERID,PUSERIDN     OK IF PRINCIPLE ID                           
         BE    VUID200               FOR THE SAME AGENCY                        
*                                  BUILD COMPATIBLE ID LIST IN TIA              
*                                    FROM PASSWORD AUTH RECORD                  
         USING CT0REC,R4                                                        
VUID110  OC    PASSWD,PASSWD       UNLESS NOT CONNECTED TO A PASSWORD           
         BZ    EISL                                                             
         LA    R4,IO                                                            
         XC    CT0KEY,CT0KEY                                                    
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,AGYALPH                                                  
         MVC   CT0KNUM,PASSWD                                                   
         MVC   KEY,CT0KEY                                                       
*                                  READ AUTH RECORD                             
         GOTO1 CTIO,DMCB,DMREAD,(R4)                                            
         BNE   EIIO                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 PGETIDS,DMCB,(C'C',CT0REC),SVTIA,PDATAMGR                        
         CLI   0(R1),X'FF'                                                      
         BE    EIIO                                                             
*                                  IN CONNECT ID COMPATIBLE LIST?               
         CLI   0(R1),0             CHECK NULL LIST                              
         BE    EISL                                                             
         L     RF,SVTIA            SEARCH DOWN LIST                             
VUID120  CLC   0(10,RF),INUALPH                                                 
         BE    VUID200             MATCH FOUND                                  
         LA    RF,12(RF)           GET NEXT LIST ENTRY                          
         CLI   0(RF),X'FF'           UPTO END                                   
         BNE   VUID120                                                          
         B     EISL                                                             
*                                  USERID IS VALID                              
VUID200  MVC   FUSUID(8),INUALPH                                                
         OI    FUSUIDH+6,X'80'     REDISPLAY USER-ID                            
*                                                                               
VALUIDX  B     VALFLD                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE FIELD NUMBERS                                       *         
***********************************************************************         
         SPACE 1                                                                
VALFLD   LA    R2,FUSFLD1H                                                      
         USING FLDLINED,R2         R2=A(INPUT LINE)                             
         MVI   FLAG,0                                                           
         LA    R4,IO2                                                           
         USING CTUREC,R4           R4=A(DEFINITION RECORD)                      
         XC    FLDDEFS(L'FLDDEFS*MAXFLDS),FLDDEFS                               
         XC    FLDNUMS,FLDNUMS                                                  
         MVI   FLAG2,0                                                          
*                                                                               
VALFLD2  ZIC   R1,FLAG             BUMP FIELD COUNT                             
         LA    R1,1(R1)                                                         
         STC   R1,FLAG                                                          
         CLI   FLAG,MAXFLDS        TEST IF DONE                                 
         BH    VALFLDE                                                          
         GOTO1 AFVAL,FLDNUMH                                                    
         BE    VALFLD4                                                          
         CLI   FLAG,1              FIRST FIELD MUST BE INPUT                    
         BE    EMIF                                                             
         OC    FLDDESC,FLDDESC     CLEAR FIELD DESCRIPTION                      
         BZ    *+14                                                             
         XC    FLDDESC,FLDDESC                                                  
         OI    FLDDESCH+6,X'80'                                                 
         OI    FLAG2,X'80'         SET FIELD NOT INPUT FLAG                     
         LA    R2,FLDNEXT                                                       
         B     VALFLD2                                                          
*                                                                               
VALFLD4  CLI   FLAG2,0             PREVIOUS FIELD MUST HAVE BEEN INPUT          
         BNE   EIIF                                                             
         TM    FLDH+4,X'08'                                                     
         BZ    EFNN                                                             
         OC    FLDH(4),FLDH        CHECK FIELD NUMBER                           
         BZ    EIIF                                                             
         CLC   FLDH(4),=F'16'                                                   
         BH    EIIF                                                             
         LA    R5,CTUDATA          FIND FIELD DEFINITION ELEMENT                
         SR    R1,R1                                                            
*                                                                               
VALFLD6  CLI   0(R5),0                                                          
         BE    EIIF                                                             
         CLI   0(R5),X'70'                                                      
         BE    VALFLDA                                                          
*                                                                               
VALFLD8  IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     VALFLD6                                                          
*                                                                               
         USING CTFDD,R5                                                         
VALFLDA  CLC   CTFDNUM,FLDH+3      TEST FIELD NUMBER                            
         BNE   VALFLD8                                                          
         MVC   PFLDNUM,CTFDNUM     SAVE PROFILE FIELD NUMBER                    
         TM    CTFDOTHR,X'80'      IF THIS IS A DDS ONLY OPTION                 
         BZ    VALFLDC                                                          
         TM    INDICS,X'80'        AND THIS IS NOT A DDS TERMINAL               
         BO    VALFLDC                                                          
         BAS   RE,DDSAUTH          CHECK DDS LEVEL AUTHORISATION                
         BE    VALFLDC                                                          
         CLI   ACTION,CHANGE       AND THE ACTION IS CHANGE - ERROR             
         LA    R1,FUSACTNH                                                      
         ST    R1,FADR                                                          
         BE    EISL                                                             
*                                                                               
VALFLDC  XC    WORK,WORK           BUILD FIELD DESCRIPTION IN WORK              
         IC    R1,CTFDLEN                                                       
         SH    R1,=H'27'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),CTFDDESC                                                 
         MVCDD WORK+31(6),CT#VALUS                                              
         GOTO1 PDICTATE,DMCB,C'SL  ',WORK+31                                    
         MVI   WORK+37,C'='                                                     
         MVC   WORK+38(L'CTFDLIST),CTFDLIST                                     
         CLC   FLDDESC,WORK        OUTPUT DESCRIPTION IF CHANGED                
         BE    *+14                                                             
         MVC   FLDDESC,WORK                                                     
         OI    FLDDESCH+6,X'80'                                                 
*                                                                               
         ZIC   RE,FLAG             MOVE ELEMENT TO W/S                          
         LA    RF,L'FLDDEFS                                                     
         MR    RE,RE                                                            
         LA    RF,FLDDEFS-L'FLDDEFS(RF)                                         
         MVC   0(L'FLDDEFS,RF),CTFDD                                            
         ZIC   RE,FLAG             ADD FIELD NUMBER TO LIST                     
         LA    RE,FLDNUMS-1(RE)                                                 
         MVC   0(1,RE),CTFDNUM                                                  
         LA    R2,FLDNEXT                                                       
         MVC   NUMFLDS,FLAG        SET NUMBER OF FIELDS INPUT                   
         B     VALFLD2                                                          
*                                                                               
VALFLDE  CLC   FLDNUMS,LFLDNUMS                                                 
         BE    *+8                                                              
         OI    INDICS,X'40'        SET GLOBAL KEY CHANGE FLAG                   
*                                                                               
VALFLDX  B     VALFLT                                                           
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
*        VALIDATE FILTERS                                             *         
***********************************************************************         
         SPACE 1                                                                
VALFLT   XC    FILTERS,FILTERS     CLEAR FILTER VALUES                          
         XC    FLTDATE,FLTDATE     CLEAR FILTER VALUES                          
         XC    FILTUSED,FILTUSED   CLEAR USED FILTER FLAGS                      
         MVI   FLTAGY,0                                                         
         GOTO1 AFVAL,FUSFILTH                                                   
         BNE   VALFLT8                                                          
         GOTO1 PSCANNER,DMCB,FLDH,(8,SCANBLK)                                   
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         MVC   NFLDS,4(R1)         SET NUMBER OF FIELDS INPUT                   
         MVI   FNDX,0                                                           
         LA    R2,SCANBLK-L'SCANBLK                                             
*                                                                               
VALFLT2  LA    R2,L'SCANBLK(R2)    BUMP TO NEXT SCAN BLOCK ENTRY                
         ZIC   R1,FNDX             BUMP FIELD INDEX                             
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         CLC   FNDX,NFLDS          TEST IF DONE                                 
         BH    VALFLT8                                                          
         CLI   0(R2),0             TEST L'FIRST                                 
         BE    EIIF                                                             
         CLI   1(R2),0             TEST L'SECOND                                
         BE    EIIF                                                             
         LA    R1,FLTTAB           R1=A(FILTER TABLE)                           
         ZIC   RE,0(R2)                                                         
         BCTR  RE,0                RE=L'INPUT-1                                 
*                                                                               
VALFLT4  CLI   0(R1),X'FF'         TEST EOT                                     
         BE    EIIF                                                             
         EX    0,0(R1)             GET KEYWORD                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R2),0(RF)                                                   
         BE    *+12                                                             
         LA    R1,L'FLTTAB(R1)                                                  
         B     VALFLT4                                                          
         MVC   FLTVALS,4(R1)       SET FILTER VALUES FROM TABLE                 
         TM    FLTINDS,X'80'       TEST IF DDS ONLY FILTER                      
         BZ    *+12                                                             
         TM    INDICS,X'80'                                                     
         BZ    EIIF                                                             
         SR    RE,RE                                                            
         ICM   RE,1,FLTSYS                                                      
         BZ    VALFLT6                                                          
         EX    RE,*+8              TEST FILTER VALID FOR SYSTEM                 
         B     *+8                                                              
         TM    SYSINDS,0                                                        
         BZ    EIIF                                                             
*                                                                               
VALFLT6  ZIC   RE,FLTNUM           TEST FILTER NOT PREVIOUSLY INPUT             
         LA    RE,FILTUSED-1(RE)                                                
         CLI   0(RE),0                                                          
         BNE   EDIF                                                             
         MVI   0(RE),X'FF'                                                      
         CLC   1(1,R2),FLTMIN      CHECK L'VALUE                                
         BL    EFTS                                                             
         CLC   1(1,R2),FLTMAX                                                   
         BH    EFTL                                                             
*                                                                               
         MVC   FLAG,FLTINDS        BUILD PARM BLOCK FROM SCAN BLOCK             
         MVC   WORK(1),1(R2)                                                    
         MVC   WORK+1(1),3(R2)                                                  
         MVC   WORK+2(4),8(R2)                                                  
         MVC   WORK+6(10),22(R2)                                                
         ICM   RF,7,FLTROUT                                                     
         LA    RF,0(RF)                                                         
         A     RF,RELO                                                          
         MVC   ERRMSGN,NOERROR                                                  
         XC    DUB,DUB                                                          
         BASR  RE,RF               GO TO VALIDATION ROUTINE                     
         BNE   EXIT                                                             
         SR    RE,RE               MOVE RETURN VALUE TO W/S                     
         ICM   RE,3,FLTDSP                                                      
         LA    RE,WORKD(RE)                                                     
         ZIC   RF,FLTLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     VALFLT2                                                          
         MVC   0(0,RE),DUB                                                      
*                                                                               
VALFLT8  CLC   FILTERS,LFILTERS                                                 
         BE    *+8                                                              
         OI    INDICS,X'40'        SET GLOBAL KEY CHANGE FLAG                   
         CLC   FLTDATE,LFLTDATE                                                 
         BE    *+8                                                              
         OI    INDICS,X'40'        SET GLOBAL KEY CHANGE FLAG                   
*                                                                               
VALFLTX  B     VALOPT                                                           
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OPTIONS                                             *         
***********************************************************************         
         SPACE 1                                                                
VALOPT   GOTO1 AFVAL,FUSOPTNH                                                   
         LA    R2,FUSOPTNH                                                      
         MVI   OPTNFLAG,0          CLEAR FLAG                                   
                                                                                
         ZICM  RE,5(R2)                                                         
         BZ    VALOPTX                                                          
         BCTR  RE,0                RE=L'INPUT-1                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),CT@DATE                                                  
         BNE   EIIF                                                             
                                                                                
         OI    OPTNFLAG,DISPACTV   THERE IS A VALID DATE FILTER                 
VALOPTX  CLC   OPTNFLAG,LOPTFLAG   OPTION FLAG CHANGED                          
         BE    *+8                                                              
         OI    INDICS,X'40'        SET GLOBAL KEY CHANGE FLAG                   
         B     VALEND                                                           
         EJECT                                                                  
***********************************************************************         
*        DECIDE WHAT TO DO NOW                                        *         
***********************************************************************         
         SPACE 1                                                                
VALEND   CLC   AGYVALS,LAGYVALS                                                 
         BNE   VALEND10                                                         
         CLC   INUNUM,LINUNUM      CHECK NEW USERID INPUT                       
         BE    *+8                                                              
VALEND10 OI    INDICS,X'40'        SET GLOBAL KEY CHANGE FLAG                   
         LA    R4,KEY              BUILD NEW VIRGIN KEY                         
         USING CTUREC,R4                                                        
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKTYP,C'U'                                                     
         MVC   CTUKSYS,SYSEQU                                                   
         MVC   CTUKPROG,PRGNUM                                                  
*                                                                               
         CLI   LISTFLAG,1          FULL USERID/AGENCY LISTING                   
         BNE   VALEND12                                                         
         MVC   CTUKUID,INUNUM                                                   
         B     VALEND30                                                         
*                                                                               
VALEND12 OC    INUNUM,INUNUM       SPECIFIC USERID LISTING                      
         BZ    VALEND20                                                         
         MVC   CTUKUID,INUNUM                                                   
         B     VALEND30                                                         
*                                  'ALL' AGENCY LEVEL LISTING                   
VALEND20 MVC   CTUKAGY(1),AGYBINY                                               
         TM    SYSINDS,X'80'                                                    
         BO    *+10                                                             
         MVC   CTUKAGY,AGYALPH                                                  
*                                                                               
VALEND30 TM    INDICS,X'40'                                                     
         BNO   *+12                                                             
         MVI   FKEYFLAG,0                                                       
         B     VALEND2                                                          
         CLI   ACTION,CHANGE                                                    
         BNE   VALEND40                                                         
         CLI   FKEYFLAG,0                                                       
         BNE   VALEND32                                                         
         MVI   FKEYFLAG,1                                                       
         MVC   KEY,FKEYSAVE                                                     
         B     DSPREC                                                           
VALEND32 CLI   LACTION,DISPLAY                                                  
         BE    CHAREC                                                           
         B     VALEND44                                                         
*                                                                               
VALEND40 CLI   FKEYFLAG,0                                                       
         BE    VALEND44                                                         
         MVI   FKEYFLAG,0                                                       
         MVC   KEY,FKEYSAVE                                                     
         B     DSPREC                                                           
VALEND44 CLI   MODE,NEXT                                                        
         BNE   VALEND2                                                          
         MVC   KEY,LKEY                                                         
         B     DSPREC                                                           
*                                  KEY HAS CHANGED                              
VALEND2  MVC   LSYSEQU,SYSEQU      SAVE THIS TIME KEY VALUES                    
         MVC   LPRGNUM,PRGNUM                                                   
         MVC   LFLDNUMS,FLDNUMS                                                 
         MVC   LAGYVALS,AGYVALS                                                 
         MVC   LFILTERS,FILTERS                                                 
         MVC   LFLTDATE,FLTDATE    FILTER DATE                                  
         MVC   LOPTFLAG,OPTNFLAG   OPTION FLAG                                  
         MVC   LINUNUM,INUNUM      SAVE LAST INPUT USERID                       
         XC    LKEY,LKEY                                                        
         MVI   MODE,FIRST                                                       
         B     DSPREC              GO TO DISPLAY                                
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY SYSTEM PROFILES                                      *         
***********************************************************************         
         SPACE 1                                                                
DSPREC   TWAXC FUSLIN1H,PROT=Y                                                  
         MVC   FUSLHD1,CT@FUSH1                                                 
         MVC   FUSLHD2,CT@FUSH2                                                 
         TM    OPTNFLAG,DISPACTV            DISPLAY ACTIVITY DATE?              
         BO    *+14                                                             
         OC    FLTDATE,FLTDATE                                                  
         BZ    *+16                                                             
         MVC   FUSLHD1,CT@FUSH3                                                 
         MVC   FUSLHD2,CT@FUSH4                                                 
*                                                                               
         OI    FUSLHD1H+6,X'80'    TRANSMIT                                     
         OI    FUSLHD2H+6,X'80'    TRANSMIT                                     
*                                                                               
         LA    R2,FUSLIN1H                                                      
         MVC   FKEYSAVE,KEY                                                     
*                                                                               
         USING INPLINED,R2         R2=A(OUTPUT LINE)                            
DSPREC1  GOTO1 CTIO,DMCB,(X'80',DMRDHI),IO                                      
         BNE   EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         B     DSPREC4                                                          
*                                                                               
DSPREC2  GOTO1 CTIO,DMCB,(X'80',DMRSEQ),IO                                      
         BNE   EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
*                                                                               
DSPREC4  LA    R4,IO                                                            
         USING CTUREC,R4           R4=A(PROFILE RECORD)                         
         CLI   CTUKTYP,C'U'        TEST END OF DATA                             
         BNE   DSPEND                                                           
         CLC   CTUKSYS,SYSEQU                                                   
         BNE   DSPEND                                                           
         CLC   CTUKPROG,PRGNUM                                                  
         BNE   DSPEND                                                           
*                                                                               
         CLI   LISTFLAG,1          CHECK IF FULL USER ID/AGENCY LIST            
         BNE   DSPREC46                                                         
         CLI   CTUKUID,X'40'       CHECK NEXT RECORD AGENCY LEVEL               
         BNL   DSPREC44                                                         
         CLC   CTUKUID,INUNUM      CHECK NEXT RECORD SAME UID AS LAST           
         BNE   DSPREC40                                                         
*                                                                               
         CLI   KUIDFLAG,0          IF VALID LIST USER ID                        
         BE    DSPREC48              DISPLAY IT                                 
         B     DSPREC2               ELSE GET NEXT                              
*                                                                               
DSPREC40 MVC   INUNUM,CTUKUID      VALIDATE NEW USER ID                         
         MVC   KEY,IO                                                           
         GOTO1 =A(VALKUID),DMCB,(RC),RR=RELO                                    
         CLI   KUIDFLAG,0                                                       
         BE    DSPREC42                                                         
*                                  HERE IF INVALID USERID                       
         GOTO1 CTIO,DMCB,(X'80',DMRDHI),IO                                      
         BNE   EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         B     DSPREC2             GET NEXT RECORD                              
*                                  HERE IF VALID USER ID                        
DSPREC42 GOTO1 CTIO,DMCB,(X'80',DMRDHI),IO                                      
         BNE   EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         B     DSPREC48            DISPLAY RECORD                               
*                                  DISPLAY AGENCY LEVEL RECORD                  
DSPREC44 MVI   LISTFLAG,0                                                       
         XC    INUNUM,INUNUM                                                    
         LA    R4,KEY                                                           
         CLC   CTUKAGY,AGYALPH                                                  
         BE    DSPREC43                                                         
         MVC   CTUKAGY,AGYALPH                                                  
         XC    CTUKMED(CTULEN-CTUKMED),CTUKMED                                  
*                                  CHECK SECURITY AGENCY LEVEL                  
DSPREC43 TM    INDICS,X'80'        OK IF DDS                                    
         BO    DSPREC1                                                          
         CLC   PUSERIDN,USERID     OK IF PRINCIPLE ID                           
         BE    DSPREC1                                                          
         B     DSPEND                ELSE EXIT                                  
*                                                                               
*                                  HERE FOR AGENCY LEVEL ONLY LIST              
*                                                                               
DSPREC46 OC    INUNUM,INUNUM       CHECK IF USERID INPUT                        
         BZ    DSPREC4A                                                         
         CLC   CTUKUID,INUNUM                                                   
         BNE   DSPEND                                                           
*                                                                               
DSPREC48 CLI   SYSNUM,6                                                         
         BE    DSPACC              ACCOUNT SYSTEM AFTER CONVERSION              
         B     DSPREC8                                                          
*                                                                               
DSPREC4A TM    SYSINDS,X'80'                                                    
         BO    DSPREC6                                                          
         CLC   CTUKAGY,AGYALPH                                                  
         BNE   DSPEND                                                           
         CLI   SYSNUM,6                                                         
         BE    DSPACC              ACCOUNT SYSTEM AFTER CONVERSION              
         B     DSPREC8                                                          
*                                                                               
DSPREC6  CLC   CTUKAGY(1),AGYBINY                                               
         BNE   DSPEND                                                           
         EJECT                                                                  
*                                  APPLY UNIT FILTER                            
DSPREC8  CLI   FILTUUNT,0                                                       
         BE    DSPRECC                                                          
         CLI   FLTUNT,0                                                         
         BNE   DSPRECA                                                          
         CLI   CTUKAGY+1,0                                                      
         BE    DSPRECC                                                          
         B     DSPREC2                                                          
*                                                                               
DSPRECA  CLC   FLTUNT,CTUKAGY+1                                                 
         BNE   DSPREC2                                                          
*                                  APPLY MEDIA/LEDGER FILTER                    
DSPRECC  CLI   FILTUMED,0                                                       
         BE    DSPRECG                                                          
         CLI   FLTMED,0                                                         
         BNE   DSPRECE                                                          
         CLI   CTUKMED,0                                                        
         BE    DSPRECG                                                          
         B     DSPREC2                                                          
*                                                                               
DSPRECE  CLC   FLTMED,CTUKMED                                                   
         BNE   DSPREC2                                                          
*                                  APPLY CLIENT/ACCOUNT FILTER                  
DSPRECG  CLI   FILTUCLT,0                                                       
         BE    DSPRECK                                                          
         OC    FLTCLI,FLTCLI                                                    
         BNZ   DSPRECI                                                          
         OC    CTUKCLT,CTUKCLT                                                  
         BZ    DSPRECK                                                          
         B     DSPREC2                                                          
*                                                                               
DSPRECI  TM    FLTCLI,X'40'        TEST SPECIAL CLIENT FILTER VALUE             
         BNZ   DSPRECJ                                                          
         MVC   WORK(L'FLTCLI),FLTCLI                                            
         OI    WORK,X'40'                                                       
         CLC   CTUKCLT,WORK        YES - ONLY INCLUDE CLIENT CODES              
         BL    DSPREC2             GREATER THAN OR EQUAL TO FILTER              
         B     DSPRECK                                                          
*                                                                               
DSPRECJ  CLC   FLTCLI,CTUKCLT                                                   
         BNE   DSPREC2                                                          
         B     DSPRECK                                                          
*                                                                               
**             ACCOUNT SYSTEM AFTER CONVERSION                                  
*                                                                               
DSPACC   CLI   FILTUUNT,0                                                       
         BE    DSPACC4                                                          
         CLI   FLTUNT,0                                                         
         BNE   DSPACC3                                                          
         CLI   CTUKUNT,0                                                        
         BE    DSPACC4                                                          
         B     DSPREC2                                                          
DSPACC3  CLC   FLTUNT,CTUKUNT                                                   
         BNE   DSPREC2                                                          
*                                                                               
DSPACC4  CLI   FILTUMED,0                                                       
         BE    DSPACC6                                                          
         CLI   FLTMED,0                                                         
         BNE   DSPACC5                                                          
         CLI   CTUKLDG,0                                                        
         BE    DSPACC6                                                          
         B     DSPREC2                                                          
DSPACC5  CLC   FLTMED,CTUKLDG                                                   
         BNE   DSPREC2                                                          
*                                                                               
DSPACC6  CLI   FILTUCLT,0                                                       
         BE    DSPACC8                                                          
         OC    FLTCLI,FLTCLI                                                    
         BNZ   DSPACC7                                                          
         OC    CTUKACT,CTUKACT                                                  
         BZ    DSPACC8                                                          
         B     DSPREC2                                                          
DSPACC7  TM    FLTCLI,X'40'        TEST SPECIAL CLIENT FILTER VALUE             
         BNZ   DSPACC7A                                                         
         MVC   WORK(L'FLTCLI),FLTCLI                                            
         OI    WORK,X'40'                                                       
         CLC   CTUKACT,WORK        YES - ONLY INCLUDE CLIENT CODES              
         BL    DSPREC2             GREATER THAN OR EQUAL TO FILTER              
         B     DSPRECK                                                          
DSPACC7A CLC   FLTCLI,CTUKACT                                                   
         BNE   DSPREC2                                                          
DSPACC8  DS    0H                                                               
*                                                                               
DSPRECK  LA    R5,CTUDATA          FIND VALUES ELEMENT                          
         SR    R1,R1                                                            
*                                                                               
DSPRECM  CLI   0(R5),0                                                          
         BE    DSPREC2                                                          
         CLI   0(R5),X'72'                                                      
         BE    *+14                                                             
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DSPRECM                                                          
         USING CTPVD,R5            R5=A(FIELD VALUES ELEMENT)                   
         LA    R1,FILTUVL1         BUILD LIST OF VALUES AND APPLY               
         LA    RE,FLTVL1           VALUES FILTERS TO ELEMENT                    
         LA    RF,FLDNUMS                                                       
         LA    R6,VALUES                                                        
         XC    VALUES,VALUES                                                    
         LA    R7,FLDDEFS                                                       
         USING CTFDD,R7                                                         
         LA    R0,MAXFLDS                                                       
*                                                                               
DSPRECO  CLI   0(RF),0                                                          
         BE    DSPRECR                                                          
         ZIC   R3,0(RF)                                                         
         LA    R3,CTPVALUE-1(R3)   R3=A(CURRENT FIELD VALUE)                    
         MVC   0(1,R6),0(R3)                                                    
         MVC   1(1,R6),CTFDTYPE                                                 
         CLI   0(R1),0             TEST IF FILTERING ON THIS VALUE              
         BE    DSPRECR                                                          
         CLC   0(1,R3),0(RE)                                                    
         BNE   DSPREC2             GET NEXT RECORD IF FILTER VALID              
*                                                                               
DSPRECR  LA    R1,1(R1)            BUMP TO NEXT FIELD                           
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         LA    R6,2(R6)                                                         
         LA    R7,L'FLDDEFS(R7)                                                 
         BCT   R0,DSPRECO                                                       
         MVC   LKEY,CTUKEY                                                      
         CLI   INPKEYH,L'INPKEYH+L'INPKEY                                       
         BL    DSPEND2                                                          
         LA    RE,INPKEY           BUILD KEY OF RECORD IN TWA                   
         OC    INUNUM,INUNUM       CHECK IF USERID INPUT                        
         BZ    DSPRECR2                                                         
         LA    RF,INUALPH                                                       
         LA    R0,L'INUALPH                                                     
DSPRECR0 CLI   0(RF),C' '                                                       
         BE    DSPRECR1                                                         
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,DSPRECR0                                                      
DSPRECR1 MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         B     DSPRECR3                                                         
DSPRECR2 MVC   0(3,RE),CT@ALL                                                   
         MVI   3(RE),C','                                                       
         LA    RE,4(RE)                                                         
         TM    SYSINDS,X'80'        (AND IF NOT OLD SYSTEM)                     
         BO    DSPRECX                                                          
DSPRECR3 CLI   SYSNUM,6            ACCOUNT SYSTEM ?                             
         BNE   DSPRECS                                                          
*                                                                               
*                           ACCOUNT SYSTEM AFTER CONVERSION                     
         CLI   CTUKUNT,0                                                        
         BNE   DSPACC10                                                         
         MVC   0(3,RE),CT@ALL                                                   
         LA    RE,3(RE)                                                         
         B     DSPACC13                                                         
DSPACC10 MVC   0(2,RE),CTUKUNT     UNIT AND LEDGER                              
*&&US                                                                           
         CLI   CTUKUNT,C'T'        SPECIAL FOR UNIT T                           
         BNE   DSPACC11                                                         
         LR    R6,RE                                                            
         GOTO1 PHEXOUT,DMCB,CTUKLDG,1(R6),1,=C'TOG'                             
         LA    RE,1(R6)                                                         
*&&                                                                             
DSPACC11 LA    RE,2(RE)                                                         
*                                  IF ALL ACCOUNTS DISPLAY BLANK                
DSPACC13 OC    CTUKACT,CTUKACT                                                  
         BZ    DSPREC10              (ALLOWS FOR CLIENT VALUE OF 'ALL')         
         MVI   0(RE),C','                                                       
         MVC   1(3,RE),CTUKACT                                                  
         TM    CTUKACT+1,X'C0'     TEST IF 5 CHR CLIENT CODE                    
         BNZ   DSPREC10                                                         
         SR    R1,R1                                                            
         ICM   R1,3,CTUKACT+1                                                   
         EDIT  (R1),(4,2(RE)),FILL=0                                            
         B     DSPREC10                                                         
*                                                                               
*                                                                               
DSPRECS  CLI   CTUKMED,0                                                        
         BNE   DSPRECT                                                          
         MVC   0(3,RE),CT@ALL                                                   
         LA    RE,3(RE)                                                         
         B     DSPRECV                                                          
*                                                                               
DSPRECT  MVC   0(1,RE),CTUKMED                                                  
         LA    RE,1(RE)                                                         
*                                                                               
DSPRECV  OC    CTUKCLT,CTUKCLT                                                  
*                                  IF ALL CLIENTS DISPLAY BLANK                 
         BZ    DSPREC10              (ALLOWS FOR CLIENT VALUE OF 'ALL')         
         MVI   0(RE),C','          MOVE DELIMITER                               
         MVC   1(3,RE),CTUKCLT                                                  
         TM    CTUKCLT+1,X'C0'     TEST IF 5 CHR CLIENT CODE                    
         BNZ   DSPREC10                                                         
         SR    R1,R1                                                            
         ICM   R1,3,CTUKCLT+1                                                   
         EDIT  (R1),(4,2(RE)),FILL=0                                            
         B     DSPREC10                                                         
*                                                                               
DSPRECX  OC    CTUKAGY+1(2),CTUKAGY+1                                           
         BNZ   DSPRECZ                                                          
         MVC   0(3,RE),CT@ALL                                                   
         LA    RE,3(RE)                                                         
         B     DSPRECV                                                          
*                                                                               
DSPRECZ  MVC   0(2,RE),CTUKAGY+1                                                
*&&US                                                                           
         CLI   CTUKAGY+1,C'T'      SPECIAL FOR UNIT T                           
         BNE   DSPRECZ2                                                         
         LR    R6,RE                                                            
         GOTO1 PHEXOUT,DMCB,CTUKAGY+2,1(R6),1,=C'TOG'                           
         LA    RE,1(R6)                                                         
*&&                                                                             
DSPRECZ2 LA    RE,2(RE)                                                         
         B     DSPRECV                                                          
*                                  EDIT VALUES INTO TWA                         
DSPREC10 LA    R3,VALUES                                                        
         LA    R6,INPVALS                                                       
         LA    R7,MAXFLDS                                                       
*                                                                               
DSPREC12 OC    0(2,R3),0(R3)                                                    
         BZ    DSPREC16                                                         
         MVC   0(1,R6),0(R3)                                                    
         CLI   1(R3),C'C'                                                       
         BE    DSPREC14                                                         
         GOTO1 PHEXOUT,DMCB,0(R3),0(R6),1,=C'TOG'                               
         CLI   1(R3),C'X'                                                       
         BE    DSPREC14                                                         
         EDIT  (B1,0(R3)),(3,0(R6)),ALIGN=LEFT                                  
         OI    0(R6),X'F0'                                                      
*                                                                               
DSPREC14 LA    R6,1(R6)            FIND END OF OUTPUT VALUE                     
         CLI   0(R6),C' '                                                       
         BH    DSPREC14                                                         
         MVI   0(R6),C','          SET DELIMITER                                
         LA    R6,1(R6)                                                         
         LA    R3,2(R3)                                                         
         BCT   R7,DSPREC12                                                      
*                                                                               
DSPREC16 BCTR  R6,0                REMOVE LAST DELIMITER                        
         MVI   0(R6),0                                                          
*                                                                               
         DROP  R5                                                               
         TM    OPTNFLAG,DISPACTV            DISPLAY ACTIVITY DATE?              
         BO    *+14                                                             
         OC    FLTDATE,FLTDATE                                                  
         BZ    DSPREC30                                                         
         LR    R1,R2                                                            
         LA    R2,INPNEXT                                                       
         LA    R5,CTUDATA                                                       
         CLI   0(R5),CTACTELQ      ACTIVITY ELEMENT                             
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTACTD,R5                                                        
*                                                                               
         OC    FLTDATE,FLTDATE     FILTER FOR A SPECIFIC DATE?                  
         BZ    DSPREC20                                                         
         CLC   FLTDATE,CTACTDT     YES, COMPARE DATE                            
         BNH   DSPREC20                                                         
         LR    R2,R1                                                            
         XC    INPKEY(L'INPKEY),INPKEY                                          
         XC    INPVALS(L'INPVALS),INPVALS                                       
         B     DSPREC2                                                          
*                                                                               
DSPREC20 GOTO1 PDATCON,DMCB,(3,CTACTDT),(13,INPKEY)                             
*                                                                               
DSPREC30 LA    R2,INPNEXT                                                       
         B     DSPREC2                                                          
         DROP  R2,R4,R5,R7                                                      
         EJECT                                                                  
***********************************************************************         
*        OUTPUT MESSAGE AND EXIT                                      *         
***********************************************************************         
         SPACE 1                                                                
DSPEND   MVI   MODE,LAST                                                        
         B     DSPEND4                                                          
*                                                                               
DSPEND2  MVI   MODE,NEXT                                                        
*                                                                               
DSPEND4  LA    R1,FUSLIN1H                                                      
         CR    R1,R2               TEST IF ANYTHING DISPLAYED                   
         BNE   DSPEND6                                                          
         MVI   MODE,FIRST                                                       
         LA    RF,CI#NOREC                                                      
         LA    R2,FUSACTNH                                                      
         BAS   RE,INFOMSG                                                       
         B     EXIT                                                             
*                                                                               
DSPEND6  MVI   LACTION,DISPLAY                                                  
         CLI   ACTION,DISPLAY                                                   
         BNE   DSPENDA                                                          
         CLI   MODE,LAST           TEST IF ALL RECORD DISPLAYED                 
         BNE   DSPEND8                                                          
         LA    RF,CI#RECSD                                                      
         LA    R2,FUSACTNH                                                      
         BAS   RE,INFOMSG                                                       
         B     EXIT                                                             
*                                                                               
DSPEND8  LA    RF,CI#RSDEN                                                      
         LA    R2,FUSTAB1H                                                      
         OI    FUSACTNH+6,X'80'                                                 
         BAS   RE,INFOMSG                                                       
         B     EXIT                                                             
*                                                                               
DSPENDA  LA    RF,CI#RSDEC                                                      
         LA    R2,FUSINP1H                                                      
         BAS   RE,INFOMSG                                                       
         MVI   FKEYFLAG,1                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        CHANGE SYSTEM PROFILES (AFTER DISPLAY)                       *         
***********************************************************************         
         SPACE 1                                                                
CHAREC   LA    R2,FUSLIN1H                                                      
         USING INPLINED,R2         R2=A(INPUT LINE)                             
         MVC   KEYSAVE,KEY                                                      
         CLI   MODE,FIRST          CHECK ACTION SEQUENCE                        
         BNE   CHAREC2                                                          
         LA    R1,FUSACTNH                                                      
         ST    R1,FADR                                                          
         B     EIAS                                                             
*                                                                               
CHAREC2  CLI   INPKEYH,L'INPKEYH+L'INPKEY                                       
         BL    CHAEND                                                           
         OC    INPKEY,INPKEY       NO MORE RECORDS                              
         BZ    CHAEND                                                           
         GOTO1 AFVAL,INPVALSH                                                   
         BNE   CHARECM             IGNORE FIELD IF NO INPUT                     
         GOTO1 PSCANNER,DMCB,FLDH,(8,SCANBLK)                                   
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         CLC   4(1,R1),NUMFLDS     ALL FIELDS MUST BE INPUT                     
         BNE   EIIF                                                             
         MVC   NFLDS,4(R1)         SET NUMBER OF FIELDS INPUT                   
         MVI   FNDX,0                                                           
         LA    R3,SCANBLK-L'SCANBLK                                             
         XC    VALUES,VALUES       CLEAR NEW FIELD VALUES                       
*                                                                               
CHAREC4  LA    R3,L'SCANBLK(R3)    BUMP TO NEXT SCAN BLOCK ENTRY                
         ZIC   R1,FNDX             BUMP FIELD INDEX                             
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         CLC   FNDX,NFLDS          TEST IF DONE                                 
         BH    CHAREC6                                                          
         MVC   WORK(1),0(R3)       BUILD PARM LIST FOR FIELD VALIDATION         
         MVC   WORK+1(1),2(R3)                                                  
         MVC   WORK+2(4),4(R3)                                                  
         MVC   WORK+6(10),12(R3)                                                
         MVC   FLAG,FNDX                                                        
         BAS   RE,VALVAL                                                        
         BNE   EXIT                EXIT IF FIELD INVALID                        
         ZIC   RE,FNDX                                                          
         LA    RE,VALUES-1(RE)                                                  
         MVC   0(1,RE),DUB         MOVE NEW FIELD VALUE TO LIST                 
         B     CHAREC4                                                          
*                                                                               
CHAREC6  LA    RE,INPKEY+L'INPKEY-1                                             
         LA    R1,L'INPKEY                                                      
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
         STC   R1,INPKEYH+5        SET L'KEY FIELD                              
         GOTO1 PSCANNER,DMCB,INPKEYH,(3,SCANBLK)                                
*                                                                               
         CLI   4(R1),3             CAN BE 2 OR 3 FIELD COMPONENTS               
         BNH   *+6                                                              
         DC    H'0'                                                             
         CLI   4(R1),2                                                          
         BNL   *+6                                                              
         DC    H'0'                                                             
         MVC   SCANFLDS,4(R1)                                                   
*                                                                               
         LA    R4,KEY                                                           
         USING CTUREC,R4                                                        
         MVC   KEY,KEYSAVE         MOVE VIRGIN KEY TO KEY                       
         LA    R3,SCANBLK                                                       
         CLC   12(3,R3),CT@ALL     CHECK AGENCY LEVEL                           
         BE    CHAREC6A                                                         
         MVC   INUALPH,12(R3)        ELSE USERID LEVEL                          
         BAS   RE,GETIDN                                                        
         MVC   CTUKAGY,INUNUM      MOVE IN USERID NUMBER                        
         B     CHAREC6B                                                         
*                                                                               
CHAREC6A MVC   CTUKAGY,AGYALPH     MOVE IN AGENCY ALPHA                         
*                                                                               
CHAREC6B LA    R3,L'SCANBLK(R3)                                                 
         CLC   12(3,R3),CT@ALL     CHECK AGENCY LEVEL                           
         BE    CHAREC8                                                          
         CLI   0(R3),1                                                          
         BNE   *+14                                                             
         MVC   CTUKMED,12(R3)      MOVE IN MEDIA                                
         B     CHAREC8                                                          
         OC    INUNUM,INUNUM       CHECK IF USERID INPUT                        
         BNZ   CHAREC6C                                                         
         LA    R6,CTUKAGY+1                                                     
         CLI   SYSNUM,6                                                         
         BNE   CHAREC6D                                                         
         TM    SYSINDS,X'80'                                                    
         BO    CHAREC6D                                                         
CHAREC6C LA    R6,CTUKUNT          ACCOUNT FILE AFTER CONVERSION                
CHAREC6D MVC   0(2,R6),12(R3)      MOVE IN UNIT/LEDGER                          
*&&US                                                                           
         CLI   12(R3),C'T'         IF UNIT IS T                                 
         BNE   CHAREC8                                                          
         LA    R6,CTUKAGY+2                                                     
         TM    SYSINDS,X'80'                                                    
         BO    *+8                                                              
         LA    R6,CTUKLDG                                                       
         GOTO1 PHEXIN,DMCB,13(R3),0(R6),2 THEN LEDGER MUST BE HEX               
         OC    DMCB+12(4),DMCB+12                                               
         BZ    EIIF                                                             
*&&                                                                             
CHAREC8  LA    R3,L'SCANBLK(R3)                                                 
*                                  THIRD FIELD BLANK IS ALL CLIENTS             
         CLI   SCANFLDS,2                                                       
         BE    CHAREC9                                                          
         MVC   WORK(1),0(R3)       BUILD PARM LIST FOR CLIENT VALIDATN          
         MVC   WORK+1(1),2(R3)                                                  
         MVC   WORK+2(4),4(R3)                                                  
         MVC   WORK+6(10),12(R3)                                                
*                                  SPACE FILL 1 CHAR CLIENT CODE                
         CLI   WORK,1                                                           
         BNE   CHAREC82                                                         
         MVI   WORK,3                                                           
         MVI   WORK+7,C' '                                                      
         MVI   WORK+8,C' '                                                      
         B     CHAREC84                                                         
*                                  SPACE FILL 2 CHAR CLIENT CODE                
CHAREC82 CLI   WORK,2                                                           
         BNE   CHAREC84                                                         
         MVI   WORK,3                                                           
         MVI   WORK+8,C' '                                                      
*                                                                               
CHAREC84 BAS   RE,VALCLI                                                        
         BE    *+6                                                              
         DC    H'0'                MUST BE VALID                                
         LA    R6,CTUKCLT                                                       
         CLI   SYSNUM,6                                                         
         BNE   CHAREC8B                                                         
         OC    INUNUM,INUNUM       CHECK IF USERID INPUT                        
         BNZ   CHAREC8A                                                         
         TM    SYSINDS,X'80'                                                    
         BO    CHAREC8B                                                         
CHAREC8A LA    R6,CTUKACT          ACCOUNT FILE AFTER CONVERSION                
CHAREC8B MVC   0(3,R6),DUB                                                      
*                                  UPDATE FIELD VALUE RECORD                    
CHAREC9  GOTO1 CTIO,DMCB,(X'80',DMREAD),IO                                      
         BNE   EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   CHARECM                                                          
         LA    R4,IO                                                            
         LA    R5,CTUDATA                                                       
*                                                                               
CHARECA  CLI   0(R5),0                                                          
         BE    CHARECK                                                          
         CLI   0(R5),X'01'                                                      
         BE    CHARECE                                                          
         CLI   0(R5),X'72'                                                      
         BE    CHARECG                                                          
*                                                                               
CHARECC  ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     CHARECA                                                          
*                                                                               
         USING CTACTD,R5                                                        
CHARECE  MVC   CTACTDT,TODAYB      SET TODAY'S ACTIVITY DATE                    
         B     CHARECC                                                          
*                                                                               
         USING CTPVD,R5                                                         
CHARECG  MVC   WORK(L'CTPVALUE),CTPVALUE                                        
         LA    RE,VALUES                                                        
         LA    RF,FLDNUMS                                                       
         ZIC   R0,NUMFLDS                                                       
*                                                                               
CHARECI  ZIC   R1,0(RF)                                                         
         LA    R1,CTPVALUE-1(R1)                                                
         MVC   0(1,R1),0(RE)       MOVE VALUE TO FIELD VALUES ELEMENT           
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,CHARECI                                                       
         CLC   CTPVALUE,WORK       NO UPDATE IF RECORD NOT CHANGED              
         BE    CHARECM                                                          
         B     CHARECC                                                          
*                                  WRITE BACK FIELD VALUES RECORD               
CHARECK  GOTO1 CTIO,DMCB,DMWRT,IO                                               
         BNE   EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         OI    UPDFLAG,X'80'                                                    
*                                                                               
CHARECM  LA    R2,INPNEXT          BUMP TO NEXT INPUT FIELD                     
         B     CHAREC2                                                          
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
*        OUTPUT CHANGE MESSAGE & EXIT                                 *         
***********************************************************************         
         SPACE 1                                                                
CHAEND   MVI   LACTION,CHANGE                                                   
*                                  GENERATE TURNAROUND REQUEST                  
         TM    UPDFLAG,X'80'                                                    
         BZ    CHAEND1                                                          
         CLI   FILTUCMP,0          NO REQUEST FOR DDS CHANGING AN               
         BNE   CHAEND1             AGENCY PROFILE                               
*&&US                                                                           
* SWITCH TO APPROPRIATE SYSTEM                                                  
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SYSSENUM                                                 
         GOTO1 PSWITCH,DMCB                                                     
         CLI   4(R1),0                                                          
         BNE   SWERR                                                            
*&&                                                                             
         LA    R5,IO                                                            
         USING REQD,R5                                                          
         XC    REQHDR,REQHDR                                                    
         MVC   REQUEST,SPACES                                                   
         MVI   REQNUMB,74                                                       
         MVC   REQPROG,=C'74'                                                   
         MVI   REQPROG+2,C'*'      SET T/A FLAG                                 
         MVC   REQAGYA,AGYALPH                                                  
*&&US                                                                           
*                                  FORMAT UUSSPP IN CC60 OF REQUEST             
         L     R1,SVFACS                                                        
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)            R1=A(SELIST)                                 
         CLC   SYSSENUM,SESYS-SELISTD(R1)                                       
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
         MVC   REQUEST+59(1),AGYBINY                                            
         TM    SYSINDS,X'80'                                                    
         BO    *+10                                                             
         MVC   REQUEST+59(2),AGYALPH                                            
         MVC   REQUEST+61(1),0(R1) SYSTEM/SUB-SYSTEM                            
*                                                                               
         CLI   SEOVSYS-SELISTD(R1),X'0D'   TEST SPOT TRAFFIC SYSTEM             
         BNE   *+8                                                              
         MVI   REQUEST+61,C'F'                                                  
*                                                                               
         LA    R1,L'SENAME-1(R1)                                                
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         MVC   REQUEST+62(1),0(R1)                                              
         MVC   REQUEST+63(3),PRGNUM                                             
         CLI   PRGNUM,0                                                         
         BNE   *+8                                                              
         MVI   REQUEST+63,C' '                                                  
         MVC   REQSTOR,=C'FILE CONTROL'                                         
         GOTO1 PDATAMGR,DMCB,=C'DMADD',=C'REQUEST',(R5),(R5)                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF CAN'T ADD REQUEST                     
*                                                                               
         GOTO1 PSWITCH,DMCB,=C'CON',0                                           
*&&                                                                             
         DROP  R5                                                               
*                                                                               
CHAEND1  CLI   MODE,LAST                                                        
         BNE   CHAEND2                                                          
         MVI   MODE,FIRST                                                       
         LA    RF,CI#RECSC                                                      
         LA    R2,FUSACTNH                                                      
         BAS   RE,INFOMSG                                                       
         B     EXIT                                                             
*                                                                               
CHAEND2  LA    RF,CI#RSCEN                                                      
         LA    R2,FUSACTNH                                                      
         OI    FUSACTNH+6,X'80'                                                 
         BAS   RE,INFOMSG                                                       
         B     EXIT                                                             
         EJECT                                                                  
*        VALIDATE COMPANY                                             *         
         SPACE 1                                                                
VALCMP   NTR1                                                                   
         CLI   WORK,2                                                           
         BE    *+14                                                             
         MVC   DUB(1),WORK+6                                                    
         B     VALEXT                                                           
         TM    WORK+1,X'20'                                                     
         BZ    EFNH                                                             
         GOTO1 PHEXIN,DMCB,WORK+6,DUB,2                                         
         B     VALEXT                                                           
         SPACE 1                                                                
* VALIDATE AGENCY ALPHA                                                         
         SPACE 1                                                                
VALAGY   NTR1                                                                   
         MVC   DUB(2),WORK+6                                                    
         MVI   FLTAGY,1                                                         
         B     VALEXT                                                           
         SPACE 1                                                                
* VALIDATE UNIT                                                                 
         SPACE 1                                                                
VALUNT   NTR1                                                                   
         CLI   WORK,3                                                           
         BNE   VALUNT2                                                          
         CLC   WORK+6(3),CT@ALL                                                 
         BNE   EIIF                                                             
         B     VALEXT                                                           
VALUNT2  CLI   WORK,1                                                           
         BNE   EIIF                                                             
         MVC   DUB(1),WORK+6                                                    
         B     VALEXT                                                           
         SPACE 1                                                                
* VALIDATE LEDGER/MEDIA                                                         
         SPACE 1                                                                
VALLDG   DS    0H                                                               
VALMED   NTR1                                                                   
         CLI   WORK,3                                                           
         BNE   VALMED2                                                          
         CLC   WORK+6(3),CT@ALL                                                 
         BNE   EIIF                                                             
         B     VALEXT                                                           
VALMED2  CLI   WORK,1                                                           
         BNE   EIIF                                                             
         MVC   DUB(1),WORK+6                                                    
         B     VALEXT                                                           
         SPACE 1                                                                
* VALIDATE CLIENT CODE                                                          
         SPACE 1                                                                
VALACC   DS    0H                                                               
VALCLI   NTR1                                                                   
         MVC   DUB(3),=CL3'   '                                                 
         SR    R0,R0                                                            
         SR    RE,RE                                                            
         IC    RE,WORK                                                          
         LA    RF,WORK+6-1(RE)                                                  
*&&UK*&& CLI   0(RF),C'*'                                                       
*&&UK*&& BE    VALCLI0                                                          
         CLI   0(RF),C'+'                                                       
         BE    *+12                                                             
         CLI   0(RF),C'&&'                                                      
         BNE   VALCLI1                                                          
VALCLI0  BCTR  RE,0                                                             
         STC   RE,WORK                                                          
         LA    R0,1                                                             
         XC    DUB,DUB                                                          
VALCLI1  EQU   *                                                                
         CLI   WORK,3                                                           
         BH    VALCLI2                                                          
         CLI   WORK,0                                                           
         BE    EIIF                                                             
         CLC   WORK+6(3),CT@ALL                                                 
         BE    VALEXT                                                           
         SR    RF,RF                                                            
         IC    RF,WORK                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),WORK+6                                                    
         B     VALCLI3                                                          
VALCLI2  CLI   WORK,5                                                           
         BNE   EIIF                                                             
         MVC   DUB(4),=C'0000'                                                  
         MVZ   DUB(4),WORK+7                                                    
         CLC   DUB(4),=C'0000'                                                  
         BNE   EIIF                                                             
         PACK  DUB,WORK+7(4)                                                    
         CVB   R1,DUB                                                           
         MVC   DUB(1),WORK+6                                                    
         STCM  R1,3,DUB+1                                                       
*                                                                               
VALCLI3  LTR   R0,R0               TEST CODE SUFFIXED WITH AN AMPERSAND         
         BZ    VALEXT                                                           
         NI    DUB,X'FF'-X'40'     YES - MAKE FIRST CHAR LOWER CASE             
         B     VALEXT                                                           
         SPACE 1                                                                
* VALIDATE FIELD VALUE                                                          
         SPACE 1                                                                
VALVAL   NTR1                                                                   
         NI    FLAG,X'0F'                                                       
         ZIC   R1,FLAG                                                          
         LA    R0,L'FLDDEFS                                                     
         MR    R0,R0                                                            
         LA    R7,FLDDEFS-L'FLDDEFS(R1)                                         
         USING CTFDD,R7                                                         
         GOTO1 PEXPRESS,DMCB,CTFDTYPE,CTFDLIST,WORK+6,PSCANNER,0                
         MVC   DUB(1),4(R1)                                                     
         CLI   0(R1),0                                                          
         BE    VALEXT                                                           
         CLI   0(R1),3                                                          
         BE    EIIF                                                             
         B     EPVF                EXPRESS PARAMETER ERROR                      
         DROP  R7                                                               
*                                                                               
*        VALIDATE DATE                                                          
*                                                                               
VALDATE  NTR1                                                                   
         LA    RE,WORK+6           ADDRESS OF DATE FOR FILTER                   
         ST    RE,DMCB                                                          
         ZIC   R1,WORK             LENGTH OF DATE                               
         STC   R1,DMCB                                                          
*                                                                               
         GOTO1 PPERVAL,DMCB,,PERVBLK                                            
         TM    4(R1),PVRCINV1      DATE VALID?                                  
         BO    EIIF                                                             
         LA    R4,PERVBLK                                                       
         USING PERVALD,R4                                                       
         MVC   DUB(L'PVALBSTA),PVALBSTA                                         
         DROP  R4                                                               
*                                                                               
VALEXT   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        GET USERID NUMBER FROM USERID ALPHA                          *         
***********************************************************************         
         SPACE 1                                                                
GETIDN   NTR1                                                                   
         LA    R4,IO2                                                           
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID,INUALPH                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   KEY,CTIKEY                                                       
*                                  READ ID RECORD                               
         GOTO1 CTIO,DMCB,DMREAD,(R4)                                            
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   KEY,KEYSAVE                                                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,CTIDATA                                                       
         SR    R1,R1                                                            
*                                  EXTRACT ELEMENT DATA                         
GIDN010  CLI   0(R3),0                                                          
         BE    GIDNX                                                            
         CLI   0(R3),X'02'         USERID # POINTER                             
         BE    GIDN030                                                          
GIDN020  IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     GIDN010                                                          
*                                                                               
GIDN030  MVC   INUNUM,2(R3)                                                     
         B     GIDNX                                                            
*                                                                               
GIDNX    EQU   *                                                                
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK DDS LEVEL AUTHORISATION FOR FIELD ACCESS               *         
***********************************************************************         
         SPACE 1                                                                
DDSAUTH  TM    TWAAUTH-TWAD(R9),X'20'                                           
         BNO   DAUTNO                                                           
*&&US*&& B     DAUTOK                                                           
*                                  NOT WITH M00 AND M00A PROFILES IN UK         
         CLI   SYSEQU,C'M'                                                      
         BNE   DAUTOK                                                           
         CLC   PRGNUM(3),=CL3'00A'                                              
         BE    DAUTNO                                                           
         CLI   PRGNUM,X'00'                                                     
         BNE   DAUTOK                                                           
         CLC   PRGNUM+1(2),=CL2'00'                                             
         BNE   DAUTOK                                                           
         CLI   PFLDNUM,4                                                        
         BE    DAUTOK                                                           
         CLI   PFLDNUM,6                                                        
         BE    DAUTOK                                                           
         CLI   PFLDNUM,7                                                        
         BE    DAUTOK                                                           
         CLI   PFLDNUM,8                                                        
         BE    DAUTOK                                                           
         CLI   PFLDNUM,9                                                        
         BE    DAUTOK                                                           
         CLI   PFLDNUM,10                                                       
         BE    DAUTOK                                                           
         CLI   PFLDNUM,11                                                       
         BE    DAUTOK                                                           
         CLI   PFLDNUM,12                                                       
         BE    DAUTOK                                                           
         CLI   PFLDNUM,13                                                       
         BE    DAUTOK                                                           
         CLI   PFLDNUM,14                                                       
         BE    DAUTOK                                                           
         CLI   PFLDNUM,16                                                       
         BE    DAUTOK                                                           
         B     DAUTNO                                                           
*                                                                               
DAUTNO   SR    RF,RF                                                            
         B     DAUTX                                                            
DAUTOK   LR    RF,RE                                                            
DAUTX    CR    RF,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
* EXTRACT AND PRE-VALIDATE AN INPUT FIELD.                                      
*                                                                               
* ADDRESS OF FIELD HEADER IS PASSED IN R1. RETURN WITH:-                        
*                                                                               
*              FADR     = A(INPUT FIELD HEADER)                                 
*              ERRMSGN  = MISSING INPUT FIELD MESSAGE# IF NO INPUT              
*              FNDX     = ZERO                                                  
*              FLDH     = INPUT FIELD HEADER (FLDH(4) = BINARY VALUE            
*                                             FOR NUMERIC FIELD)                
*              FLD      = EXTRACTED & SPACE FILLED INPUT FIELD                  
*                                                                               
* RETURN WITH CC=NEQ IF NO INPUT IN FIELD                                       
*                                                                               
         SPACE 1                                                                
FVAL     NTR1                                                                   
         MVI   FNDX,0                                                           
         LA    RF,CE#MISIF                                                      
         STH   RF,ERRMSGN                                                       
         ST    R1,FADR                                                          
         XC    FLDH,FLDH                                                        
         MVC   FLDH+4(2),4(R1)                                                  
         MVC   FLD,SPACES                                                       
         ZIC   RE,FLDH+5                                                        
         SH    RE,=H'1'                                                         
         BM    FVALX                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),8(R1)                                                     
         MVC   ERRMSGN,NOERROR                                                  
         TM    FLDH+4,X'08'                                                     
         BZ    FVALX                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   RE,DUB                                                           
         ST    RE,FLDH                                                          
FVALX    B     EXIT                                                             
         SPACE 1                                                                
* CONTROL FILE I/O. ENTER WITH DMCB(4)=A(COMMAND), DMCB+4(4)=A(IO)              
* EXIT WITH CC=NEQ ON ERROR.                                                    
*                                                                               
         SPACE 1                                                                
CTIO     NTR1                                                                   
         L     R0,4(R1)                                                         
         GOTO1 PDATAMGR,(R1),,=C'CTFILE',KEY,(R0),0                             
         MVC   ERRMSGN,NOERROR                                                  
         TM    8(R1),X'ED'                                                      
         BZ    EXIT                                                             
         XC    ERRMSGN,ERRMSGN     INDICATE IO ERROR                            
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*                  INFO MESSAGES                            *                   
*************************************************************                   
         SPACE 1                                                                
INFOMSG  NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         GOTO1 PGETTXT,DMCB,(RF),0,(C'I',0)                                     
         OI    6(R2),X'40'                                                      
         OI    FUSMSGH+6,X'80'                                                  
         B     EXIT                                                             
         SPACE 2                                                                
*************************************************************                   
*                       ERROR MESSAGES                      *                   
*************************************************************                   
         SPACE 1                                                                
EIIO     LA    RF,0                                                             
         B     ERRX                                                             
EMIF     LA    RF,CE#MISIF                                                      
         B     ERRX                                                             
EIIF     LA    RF,CE#INVIF                                                      
         B     ERRX                                                             
EFNN     LA    RF,CE#FNOTN                                                      
         B     ERRX                                                             
EFNH     LA    RF,CE#FNOTX                                                      
         B     ERRX                                                             
EFTS     LA    RF,CE#FLMIN                                                      
         B     ERRX                                                             
EFTL     LA    RF,CE#FLMAX                                                      
         B     ERRX                                                             
EIAC     LA    RF,CE#INVAC                                                      
         B     ERRX                                                             
EIAS     LA    RF,CE#INVAS                                                      
         B     ERRX                                                             
ERNF     LA    RF,CE#RECNF                                                      
         B     ERRX                                                             
ERAE     LA    RF,CE#RECAE                                                      
         B     ERRX                                                             
ERAD     LA    RF,CE#NDREC                                                      
         B     ERRX                                                             
ERND     LA    RF,CE#NRREC                                                      
         B     ERRX                                                             
EDIF     LA    RF,CE#DUPIF                                                      
         B     ERRX                                                             
ERID     LA    RF,CE#RECDE                                                      
         B     ERRX                                                             
EISL     LA    RF,CE#SECLO                                                      
         B     ERRX                                                             
EUAU     LA    RF,CE#UIDAU                                                      
         B     ERRX                                                             
EPVF     LA    RF,CE#INPVF                                                      
         B     ERRX                                                             
EUID     LA    RF,CE#INUID                                                      
         B     ERRX                                                             
SWERR    DS    0H                                                               
         GOTO1 PSWITCH,DMCB,=C'CON',0  SWITCH BACK TO CONTROL                   
         LA    RF,56                                                            
         B     ERRX                                                             
*                                  READ MSG FROM CTFILE                         
ERRX     STH   RF,ERRMSGN          SAVE ERROR MESSAGE NUMBER                    
         MVC   DMCB2(24),DMCB      SAVE DMCB FOR IO ERROR ANALYSIS              
         LA    R0,DMCB2                                                         
         XC    DMCB(24),DMCB                                                    
         GOTO1 PGETTXT,DMCB,(FNDX,(RF)),0,(C'E',(R0))                           
         OI    FUSMSGH+6,X'80'                                                  
         L     R1,FADR                                                          
         OI    6(R1),X'40'                                                      
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
EXIT     CLC   ERRMSGN,NOERROR                                                  
         XIT1                                                                   
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
*                                  I/O COMMAND LIST                             
DMRDHI   DC    C'DMRDHI '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMWRT    DC    C'DMWRT  '                                                       
*                                                                               
NOERROR  DC    XL2'FFFF'                                                        
         EJECT                                                                  
*                                                                               
* TABLE OF VALID ACTIONS                                                        
*        LA    RF,S(TEXT),ACTNUM,FLAGS                                          
*                                                                               
         DS    0F                                                               
ACTNTAB  DS    0CL6                                                             
         DC    X'41F0',SL2(CT@DSP),AL1(0),X'00'                                 
         DC    X'41F0',SL2(CT@ENQ),AL1(0),X'00'                                 
         DC    X'41F0',SL2(CT@INQ),AL1(0),X'00'                                 
         DC    X'41F0',SL2(CT@CHG),AL1(1),X'00'                                 
         DC    X'41F0',SL2(CT@AMEND),AL1(1),X'00'                               
         DC    X'41F0',SL2(CT@ALTER),AL1(1),X'00'                               
         DC    X'FFFF'                                                          
         SPACE 1                                                                
*                                  TABLE OF VALID FILTERS                       
FLTTAB   DS    0CL16                                                            
         DC    X'41F0',SL2(CT@CPY),X'8080',AL1(1,2),AL3(VALCMP)                 
         DC    AL1(1),AL2(AGYBINY-WORKD),AL1(L'AGYBINY),X'00'                   
         DC    X'41F0',SL2(CT@AGY),X'4080',AL1(2,2),AL3(VALAGY)                 
         DC    AL1(1),AL2(AGYALPH-WORKD),AL1(L'AGYALPH),X'00'                   
         DC    X'41F0',SL2(CT@UNIT),X'8000',AL1(1,3),AL3(VALUNT)                
         DC    AL1(2),AL2(FLTUNT-WORKD),AL1(L'FLTUNT),X'00'                     
         DC    X'41F0',SL2(CT@LED),X'8000',AL1(1,3),AL3(VALLDG)                 
         DC    AL1(3),AL2(FLTLDG-WORKD),AL1(L'FLTLDG),X'00'                     
         DC    X'41F0',SL2(CT@MED),X'4000',AL1(1,3),AL3(VALMED)                 
         DC    AL1(3),AL2(FLTMED-WORKD),AL1(L'FLTMED),X'00'                     
         DC    X'41F0',SL2(CT@ACC),X'8000',AL1(1,3),AL3(VALACC)                 
         DC    AL1(4),AL2(FLTACC-WORKD),AL1(L'FLTACC),X'00'                     
         DC    X'41F0',SL2(CT@CLI),X'4000',AL1(2,6),AL3(VALCLI)                 
         DC    AL1(4),AL2(FLTCLI-WORKD),AL1(L'FLTCLI),X'00'                     
         DC    X'41F0',SL2(DC@V1),X'0001',AL1(1,3),AL3(VALVAL)                  
         DC    AL1(5),AL2(FLTVL1-WORKD),AL1(L'FLTVL1),X'00'                     
         DC    X'41F0',SL2(DC@V2),X'0002',AL1(1,3),AL3(VALVAL)                  
         DC    AL1(6),AL2(FLTVL2-WORKD),AL1(L'FLTVL2),X'00'                     
         DC    X'41F0',SL2(DC@V3),X'0003',AL1(1,3),AL3(VALVAL)                  
         DC    AL1(7),AL2(FLTVL3-WORKD),AL1(L'FLTVL3),X'00'                     
         DC    X'41F0',SL2(CT@DATE),X'0000',AL1(1,8),AL3(VALDATE)               
         DC    AL1(8),AL2(FLTDATE-WORKD),AL1(L'FLTDATE),X'00'                   
         DC    X'FF'                                                            
*                                                                               
DDDCLST  DS    0C                                                               
*                                                                               
         DCDDL CT#DSP,8,L                                                       
         DCDDL CT#ENQ,8,L                                                       
         DCDDL CT#INQ,8,L                                                       
         DCDDL CT#CHG,8,L                                                       
         DCDDL CT#ALTER,8,L                                                     
         DCDDL CT#AMEND,8,L                                                     
         DCDDL CT#ADD,8,L                                                       
         DCDDL CT#NEW,8,L                                                       
         DCDDL CT#DEL,8,L                                                       
         DCDDL CT#RSR,8,L                                                       
*                                                                               
         DCDDL CT#CPY,8,L                                                       
         DCDDL CT#AGY,8,L                                                       
         DCDDL CT#UNIT,8,L                                                      
         DCDDL CT#LED,8,L                                                       
         DCDDL CT#MED,8,L                                                       
         DCDDL CT#ACC,8,L                                                       
         DCDDL CT#CLI,8,L                                                       
*                                                                               
         DCDDL CT#YES,3,L                                                       
         DCDDL CT#ALL,4,L                                                       
         DCDDL CT#DATE,5,L                                                      
         DCDDL CT#FUSH1,79,L                                                    
         DCDDL CT#FUSH2,79,L                                                    
         DCDDL CT#FUSH3,79,L                                                    
         DCDDL CT#FUSH4,79,L                                                    
*                                                                               
DC@V1    DC    CL8'V1      '                                                    
DC@V2    DC    CL8'V2      '                                                    
DC@V3    DC    CL8'V3      '                                                    
         EJECT                                                                  
***********************************************************************         
*        VALIDATE USER-ID NUMBER IN RECORD KEY FOR FULL LISTING       *         
***********************************************************************         
         SPACE 1                                                                
VALKUID  NMOD1 0,FUS/VKUI                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,SVTWA                                                         
*                                                                               
         MVI   KUIDFLAG,0                                                       
         LA    R4,IO2                                                           
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,INUNUM                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   KEY,CTIKEY                                                       
*                                  READ ID RECORD                               
         GOTO1 CTIO,DMCB,DMREAD,(R4)                                            
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   KEY,KEYSAVE                                                      
         CLI   DMCB+8,0                                                         
         BNE   VKIDNO                                                           
         LA    R3,CTIDATA                                                       
         SR    R1,R1                                                            
*                                  EXTRACT ELEMENT DATA                         
VKID010  CLI   0(R3),0                                                          
         BE    VKID100                                                          
         CLI   0(R3),X'02'         USERID ALPHA POINTER                         
         BE    VKID030                                                          
         CLI   0(R3),X'06'         AGENCY ID                                    
         BE    VKID040                                                          
VKID020  IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     VKID010                                                          
                                                                                
VKID030  MVC   INUALPH,2(R3)                                                    
         B     VKID020                                                          
*                                                                               
VKID040  MVC   INUAGY,2(R3)                                                     
         B     VKID020                                                          
*                                  CHECK VALID USERID ACCESS                    
VKID100  CLI   FLTAGY,1            CHECK AGENCY OVERIDE FILTER                  
         BE    VKID104                                                          
         CLC   USERID,INUNUM       OK IF SAME AS CONNECT USERID                 
         BE    VKIDOK                                                           
VKID104  CLC   INUAGY,AGYALPH      NOT OK IF NOT CONNECTED AGENCY               
         BNE   VKIDNO                                                           
         TM    INDICS,X'80'        OK IF DDS                                    
         BO    VKIDOK                OR                                         
         CLC   USERID,PUSERIDN       IF PRINCIPLE ID                            
         BE    VKIDOK                                                           
*                                  BUILD COMPATIBLE ID LIST IN TIA              
*                                    FROM PASSWORD AUTH RECORD                  
         USING CT0REC,R4                                                        
VKID110  OC    PASSWD,PASSWD       UNLESS NOT CONNECTED TO A PASSWORD           
         BZ    VKIDNO                                                           
         LA    R4,IO2                                                           
         XC    CT0KEY,CT0KEY                                                    
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,AGYALPH                                                  
         MVC   CT0KNUM,PASSWD                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   KEY,CT0KEY                                                       
*                                  READ AUTH RECORD                             
         GOTO1 CTIO,DMCB,DMREAD,(R4)                                            
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   KEY,KEYSAVE                                                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 PGETIDS,DMCB,(C'C',CT0REC),SVTIA,PDATAMGR                        
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                  IN CONNECT ID COMPATIBLE LIST?               
         CLI   0(R1),0             CHECK NULL LIST                              
         BE    VKIDNO                                                           
         L     RF,SVTIA            SEARCH DOWN LIST                             
VKID120  CLC   0(10,RF),INUALPH                                                 
         BE    VKIDOK              MATCH FOUND                                  
         LA    RF,12(RF)           GET NEXT LIST ENTRY                          
         CLI   0(RF),X'FF'           UPTO END                                   
         BNE   VKID120                                                          
         B     VKIDNO                                                           
*                                                                               
VKIDNO   MVI   KUIDFLAG,1                                                       
VKIDOK   CLI   KUIDFLAG,0                                                       
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
* EQUATES                                                                       
*                                                                               
DISPLAY  EQU   0                                                                
CHANGE   EQU   1                                                                
FIRST    EQU   0                                                                
NEXT     EQU   1                                                                
LAST     EQU   2                                                                
MAXFLDS  EQU   3                                                                
         SPACE 1                                                                
         EJECT                                                                  
* DSECT TO COVER W/S                                                            
*                                                                               
WORKD    DSECT                                                                  
RELO     DS    A                                                                
DUB      DS    D                                                                
DUB2     DS    D                                                                
DMCB     DS    6F                                                               
DMCB2    DS    6F                                                               
WORK     DS    CL80                                                             
SPACES   DS    CL80                                                             
FLAG     DS    XL1                                                              
FLAG2    DS    XL1                                                              
OPTNFLAG DS    XL1                 FLAG FOR OPTIONS FIELD                       
DISPACTV EQU   X'80'               DISPLAY ALL ACTIVITY DATES                   
*                                                                               
LISTFLAG DS    XL1                 FLAG FOR FULL AGENCY/USERID LIST             
KUIDFLAG DS    XL1                 FLAG FOR VALID LIST KEY USERID               
*                                                                               
LANG     DS    XL1                 CONNECTED LANGUAGE CODE                      
INDICS   DS    XL1                                                              
TODAYB   DS    CL3                                                              
SCANFLDS DS    XL1                                                              
*                                                                               
FLDH     DS    D                                                                
FLD      DS    CL80                                                             
FADR     DS    A                                                                
ERRMSGN  DS    XL2                 GETTXT ERROR MESSAGE NUMBER                  
FNDX     DS    XL1                                                              
MSG      DS    CL60                                                             
*                                                                               
PDATAMGR DS    V                                                                
PDATCON  DS    V                                                                
PEXPRESS DS    V                                                                
PGETMSG  DS    V                                                                
PHELLO   DS    V                                                                
PHEXIN   DS    V                                                                
PHEXOUT  DS    V                                                                
PSCANNER DS    V                                                                
PSWITCH  DS    V                                                                
PGETIDS  DS    V                                                                
PGETTXT  DS    V                                                                
PGETHELP DS    V                                                                
PDICTATE DS    V                                                                
PPERVAL  DS    V                                                                
AFVAL    DS    A                                                                
ASYSTAB  DS    A                                                                
USERID   DS    H                                                                
USERIDA  DS    CL10                CONNECTED USERID ALPHA                       
PASSWD   DS    H                   CONNECTED PASSWORD #                         
FLDNUMS  DS    XL3                                                              
FLDDEFS  DS    3CL26                                                            
NUMFLDS  DS    XL1                                                              
UPDFLAG  DS    XL1                                                              
VALUES   DS    CL6                                                              
*                                                                               
INUNUM   DS    XL2                 INPUT USERID NUMBER                          
INUALPH  DS    CL10                INPUT USERID ALPHA                           
INUAGY   DS    CL2                 INPUT USERID AGENCY ID                       
NUMCID   DS    XL1                 NUMBER OF COMPATIBLE GETIDS                  
*                                                                               
         DS    0F                                                               
SVPARMS  DS    0CL32                                                            
SVFACS   DS    A                                                                
SVTIA    DS    A                                                                
SVUTL    DS    A                                                                
SVCFACS  DS    A                                                                
SVSE     DS    A                                                                
SVTWA    DS    A                                                                
         DS    A                                                                
SVTIOB   DS    A                                                                
*                                                                               
ACTION   DS    XL1                                                              
*                                                                               
SYSVALS  DS    0CL11                                                            
SYSNAME  DS    CL8                                                              
SYSEQU   DS    CL1                                                              
SYSNUM   DS    XL1                                                              
SYSINDS  DS    XL1                 X'80'=ACCOUNT, X'40'=ALL OTHER               
SYSSENUM DS    XL1                                                              
PRGNUM   DS    CL3                                                              
PFLDNUM  DS    XL1                 PROFILE FIELD NUMBER                         
*                                                                               
AGYVALS  DS    0CL3                                                             
AGYALPH  DS    CL2                                                              
AGYBINY  DS    XL1                                                              
*                                                                               
PUSERIDN DS    XL2                 AGENCY PRINCIPLE ID NUMBER                   
*                                                                               
PROGAUTH DS    H                   USER ID =FUS PROGRAM AUTH CODE               
*                                                                               
FLTVALS  DS    0CL12                                                            
FLTSYS   DS    XL1                                                              
FLTINDS  DS    XL1                                                              
FLTMIN   DS    XL1                                                              
FLTMAX   DS    XL1                                                              
FLTROUT  DS    AL3                                                              
FLTNUM   DS    XL1                                                              
FLTDSP   DS    AL2                                                              
FLTLEN   DS    XL1                                                              
         DS    XL1                 SPARE                                        
FILTUSED DS    0CL8                                                             
FILTUCMP DS    XL1                                                              
FILTUUNT DS    XL1                                                              
FILTUMED DS    XL1                                                              
FILTUCLT DS    XL1                                                              
FILTUVL1 DS    XL1                                                              
FILTUVL2 DS    XL1                                                              
FILTUVL3 DS    XL1                                                              
FILTUDAT DS    XL1                                                              
*                                                                               
FILTERS  DS    0CL8                                                             
FLTUNT   DS    CL1                                                              
FLTLDG   DS    CL1                                                              
FLTACC   DS    CL3                                                              
         ORG   FLTLDG                                                           
FLTMED   DS    CL1                                                              
FLTCLI   DS    CL3                                                              
FLTVL1   DS    XL1                                                              
FLTVL2   DS    XL1                                                              
FLTVL3   DS    XL1                                                              
FLTDATE  DS    XL3                 ACTIVITY DATE                                
FLTAGY   DS    XL1                                                              
SCANBLK  DS    8CL32                                                            
PERVBLK  DS    XL56                                                             
NFLDS    DS    XL1                                                              
KEYSAVE  DS    CL25                                                             
KEY      DS    CL25                                                             
IO       DS    1000C                                                            
IO2      DS    1000C                                                            
*                                                                               
DDDSLST  DS    0C                                                               
         DSDDL                                                                  
WORKX    EQU   *                                                                
         EJECT                                                                  
* DSECT TO COVER FIELD DEFINITION LINE                                          
*                                                                               
FLDLINED DSECT                                                                  
FLDLITH  DS    CL8                                                              
FLDLIT   DS    CL7                                                              
FLDNUMH  DS    CL8                                                              
FLDNUM   DS    CL3                                                              
FLDNUMX  DS    CL8                                                              
FLDDESCH DS    CL8                                                              
FLDDESC  DS    CL62                                                             
FLDNEXT  EQU   *                                                                
         SPACE 1                                                                
* DSECT TO COVER KEY AND VALUES FIELDS                                          
*                                                                               
INPLINED DSECT                                                                  
INPKEYH  DS    CL8                                                              
INPKEY   DS    CL18                                                             
INPVALSH DS    CL8                                                              
INPVALS  DS    CL11                                                             
INPNEXT  EQU   *                                                                
         SPACE 1                                                                
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTLFMREQ                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTLFMREQ                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* CTMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTMSGEQUS                                                      
         PRINT ON                                                               
         EJECT                                                                  
* DSECT TO COVER TWA                                                            
*                                                                               
CTFUSFFD DSECT                                                                  
         DS    CL16                                                             
INITFLAG DS    X                   FIRST TIME PASS FLAG                         
*                                  1=NO USERID FIELD, 2=USERID FIELD            
MODE     DS    X                                                                
LSYSEQU  DS    C                                                                
LPRGNUM  DS    CL3                                                              
LFLDNUMS DS    XL3                                                              
LFILTERS DS    CL8                                                              
LAGYVALS DS    XL3                                                              
LINUNUM  DS    XL2                 LAST INPUT USER ID NUMBER                    
LKEY     DS    CL25                                                             
LACTION  DS    X                                                                
* CTFUSFFD                                                                      
       ++INCLUDE CTFUSFFD                                                       
* ADDITIONAL SAVE AREA                                                          
FKEYFLAG DS    XL1                 DISPLAY FROM CHANGE SEQUENCE FLAG            
FKEYSAVE DS    CL25                FIRST KEY IN CURRENT DISPLAY                 
LFLTDATE DS    XL3                                                              
LOPTFLAG DS    X                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033CTFUS00S  05/01/02'                                      
         END                                                                    
