*          DATA SET ACREPTC02  AT LEVEL 006 AS OF 11/16/04                      
***********************************************************************         
* DO NOT DELETE THIS BOOK IT IS IN USE. SEE MOSHE DAHARI              *         
***********************************************************************         
*PHASE ACTC02A,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'ACTC - 1099 TAX RECORD CREATOR'                                 
***********************************************************************         
*              OPT1:'Y'=WRITE ENABLED                                 *         
*              OPT2:'Y'=DO NON-PREVIOUS YEAR                          *         
*              OPT3:'Y'=DISPLAY RECORDS ADDED ON REPORT               *         
*              OPT7:'Y'=PRINTABLE                                     *         
***********************************************************************         
         SPACE 1                                                                
ACTC02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACTC**,R8,R7                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACTCD,RC                                                         
         L     R9,VBIGPRNT                                                      
         USING BIGPRNTD,R9                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                INITIALIZATIONS FOR THE RUN.                 
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXITL    MVI   BYTE,0                                                           
         B     EXITCC                                                           
EXITH    MVI   BYTE,2                                                           
         B     EXITCC                                                           
EXITE    MVI   BYTE,1                                                           
EXITCC   CLI   BYTE,1                                                           
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
*                                                                               
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     RF,MCUTL                                                         
         ST    RF,AUTL                                                          
*                                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
*                                                                               
         L     R2,=A(BOXRC)        SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     R4,MCBXAREA                                                      
         ST    R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
*                                                                               
         USING BIND,RE                                                          
         L     RE,AIDTAB                                                        
         XC    BININ,BININ         INITALIZE TABLE                              
*                                                                               
         ZAP   TOTCPY,=P'0'        TOTAL COMPANY COUNTER                        
         ZAP   TOTYEAR,=P'0'       TOTAL CURRENT YEAR COUNTER                   
         ZAP   TOTNYEAR,=P'0'      TOTAL NEW YEAR COUNTER                       
*                                                                               
         B     EXIT                                                             
         DROP  R2,R4,RE                                                         
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVI   RCREQREP,C'N'       NO REQUEST DETAILS                           
         USING IDTABD,R6           ID TABLE                                     
         LA    R6,IDWRK                                                         
*                                                                               
         XC    LSTALPHA,LSTALPHA                                                
*                                                                               
         CLI   QOPT2,C'Y'                                                       
         BE    REQF10              YES, SO DO NON-PREVIOUS YEAR                 
         GOTO1 DATCON,DMCB,(5,0),(0,DATE)       GET TODAY'S DATE                
         GOTO1 ADDAY,DMCB,(C'Y',DATE),(0,DATE),-1  SUB YEAR BY 1                
         GOTO1 DATCON,DMCB,(0,DATE),(20,CDATE) CONVERT TO 20 STYLE              
         B     REQF20                                                           
*                                                                               
REQF10   CLC   QEND,SPACES                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(0,QEND),(20,CDATE)                                  
*                                                                               
REQF20   GOTO1 DATCON,DMCB,(9,CDATE),(0,DATE)       CONVERT TO 0 STYLE          
         GOTO1 ADDAY,DMCB,(C'Y',DATE),(0,DATE),1    ADD YEAR BY 1               
         GOTO1 DATCON,DMCB,(0,DATE),(20,ADATE)      CONVERT TO 20 STYLE         
         MVC   NEWYEAR,ADATE       SAVING NEW YEAR                              
*                                                                               
         USING T99RECD,R2                                                       
         LA    R2,SVKEY                                                         
         XC    SVKEY,SVKEY                                                      
         MVI   T99KTYP,T99KTYPQ    X'3E' - 1099 TAX INFO TYPE                   
         MVI   T99KSUB,T99KSUBQ    X'15' - 1099 TAX INFO SUB TYPE               
         MVC   T99KCPY,RCCOMPFL    COMPANY                                      
         MVC   T99KYEAR,CDATE      MOVE YEAR INTO KEY                           
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         B     REQF40                                                           
*                                                                               
REQF30   GOTO1 =A(DMSEQDR),DMCB,(RC)                                            
REQF40   CLC   SVKEY(T99KOID-T99KEY),IOKEY   SAME ACCT?                         
         BNE   REQFX                                                            
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
*                                                                               
         MVI   FLAG,0              INIATILIZE FLAG                              
         LA    RE,IDWRK            RE=A(WORK AREA)                              
         LA    RF,L'IDWRK          RF=(LENGTH OF WORK AREA)                     
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0               SPACE OUT WORK AREA                          
*                                                                               
         OC    LSTALPHA,LSTALPHA                                                
         BZ    REQF50                                                           
         CLC   LSTALPHA,ALPHAID                                                 
         BE    REQF60                                                           
REQF50   AP    TOTCPY,=P'1'        INCREMENT TOTAL # OF COMPANIES               
         MVC   LSTALPHA,ALPHAID                                                 
*                                                                               
REQF60   AP    TOTYEAR,=P'1'       INCREMENT TOTAL # OF RECORDS                 
*                                                                               
         LA    R2,IO               SET R2 TO POINT TO THE RECORD                
         MVC   IDALPHA,ALPHAID                                                  
         MVC   IDCC,T99KCPY        SAVING COMPANY CODE                          
         MVC   IDYEAR,T99KYEAR     SAVING YEAR                                  
         MVC   YEAR,T99KYEAR                                                    
         MVC   IDORIGIN,T99KOID    SAVING ORIGIN ID                             
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,T99RLEN        LENGTH OF RECORD                             
         MVC   MSG,=CL10'ALL'                                                   
         GOTO1 ADUMP,DMCB,(RC),IO,(R4)                                          
*                                                                               
         LA    RE,IO2              MAKE COPY OF IO TO IO2                       
         LA    RF,L'IO2                                                         
         LA    R0,IO                                                            
         LA    R1,L'IO                                                          
         MVCL  RE,R0                                                            
*                                                                               
         BAS   RE,GETUSER          GET USER ID                                  
         BNE   REQF30              READ NEXT RECORD IF NO ORIGIN ID             
         MVC   IDUSER,USER         MOVE IN USER NAME                            
*                                                                               
         LA    R2,IO2              RECORD IS IO2                                
         LA    R3,T99RFST                                                       
REQF70   CLI   0(R3),0             DONE WITH ELEMENTS                           
         BE    REQF140             YES                                          
*                                                                               
         CLI   0(R3),FFTELQ        FREE FORM ELEMENT?                           
         BE    REQF90              YES                                          
         CLI   0(R3),NAMELQ        NAME ELEMENT                                 
         BE    REQF120                                                          
         CLI   0(R3),ADRELQ        ADDRESS ELEMENT                              
         BE    REQF130                                                          
*                                                                               
REQF80   ZIC   R1,1(R3)            GETTING LENGTH                               
         AR    R3,R1                                                            
         B     REQF70                                                           
*                                                                               
         USING FFTELD,R3                                                        
REQF90   CLI   FFTTYPE,FFTTTNNI    IS IT TYPE 76?                               
         BE    REQF100                                                          
         CLI   FFTTYPE,FFTTCNAM    IS IT TYPE 75?                               
         BE    REQF110                                                          
*                                                                               
REQF100  MVC   IDDWNL,FFTTDWNL     SAVE DOWNLOAD INFO                           
         MVC   IDFORMS,FFTTNOF     SAVING # OF FORMS                            
         MVC   IDTCC,FFTTTCC       TRANSMISSION CONTROL CODE                    
         MVC   IDTIN,FFTTTIN       TIN/EIN TAX IDENTIFICATION #                 
         B     REQF80                                                           
*                                                                               
REQF110  MVC   IDCON,FFTTNAME      SAVING CONTACT NAME                          
         MVC   IDPHON,FFTTPHON     SAVING CONTACT PHONE #                       
         ZIC   R1,FFTDLEN                                                       
         SHI   R1,FFTT75LN                                                      
         CHI   R1,31               LENGTH OF EMAIL                              
         BNL   REQF115             LENGTH >= 31?                                
         LTR   R1,R1               LENGTH = 0?                                  
         BZ    REQF80              YES                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IDEMAIL(0),FFTTCEM  SAVING EMAIL                                 
         B     REQF80                                                           
REQF115  MVC   IDEMAIL,FFTTCEM     SAVING EMAIL                                 
         B     REQF80                                                           
         DROP  R3                                                               
*                                                                               
         USING NAMELD,R3                                                        
REQF120  ZIC   R1,NAMLN            GET LENGTH                                   
         SHI   R1,NAMLN1Q+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IDCNAME(0),NAMEREC  GET NAME                                     
         B     REQF80                                                           
         DROP  R3                                                               
*                                  DSECT TO COVER ADRR ELEM                     
         USING ADRELD,R3                                                        
REQF130  CLI   ADRLN,ADRLNQ                                                     
         BNE   REQF135                                                          
         MVC   IDADD,ADRLINE1      ADD ADDRESS LINE 1 TO PRINT LINE             
         MVC   IDADD2,ADRLINE2     ADD ADDRESS LINE 2 TO PRINT LINE             
         MVC   IDADD3,ADRCSZZR     ADD ADDRESS LINE 3 TO TABLE ENTRY            
         B     REQF80                                                           
*                                                                               
         USING OADRELD,R3                                                       
REQF135  MVC   IDADD(L'OADRLN1),OADRLN1                                         
         MVC   IDADD2(L'OADRLN2),OADRLN2                                        
         MVC   IDADD3(L'OADRCSZP),OADRCSZP                                      
         B     REQF80                                                           
         DROP  R3                                                               
*                                                                               
REQF140  CLC   IDFORMS,=H'0'       CHECK FOR 0                                  
         BNE   *+12                NO                                           
         MVI   IDFLAG,IDRECNA                                                   
         B     REQF220                                                          
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,T99RLEN        LENGTH OF RECORD                             
         MVC   MSG,=CL10'BFORE CHGE'                                            
         GOTO1 ADUMP,DMCB,(RC),IO2,(R4)                                         
*                                                                               
         MVC   T99KYEAR,NEWYEAR    STORING IN NEW RECORD                        
*                                                                               
REQF150  LA    R3,T99RFST                                                       
REQF160  CLI   0(R3),0             DONE?                                        
         BE    REQF210             YES, NO MORE ELEMENTS                        
*                                                                               
         CLI   0(R3),FFTELQ        FREE FORM ELEMENT?                           
         BE    REQF180             YES                                          
         CLI   0(R3),OFLELQ        OFFICE LIST ELEMENT                          
         BE    REQF200             YES                                          
         CLI   0(R3),RACELQ        RECORD ACTIVITY ELEMENT?                     
         BE    REQF200                                                          
*                                                                               
REQF170  ZIC   R1,1(R3)            GETTING LENGTH                               
         AR    R3,R1                                                            
         B     REQF160                                                          
*                                                                               
         USING FFTELD,R3                                                        
REQF180  CLI   FFTTYPE,FFTTTNNI    IS IT TYPE 76?                               
         BE    REQF190             YES.                                         
         CLI   FFTTYPE,FFTTCNAM    IS IT TYPE 75?                               
         BE    REQF200             YES.                                         
*                                                                               
REQF190  MVC   FFTTLAOC,SPACES     SPACE OUT THESE FIELDS                       
         XC    FFTTDLLR,FFTTDLLR                                                
         MVC   FFTTLNOF,IDFORMS    MOVING CURRENT FORMS INTO LAST YEARS         
         XC    FFTTNOF,FFTTNOF     CLEARING TO 0                                
         B     REQF170                                                          
         DROP  R3                                                               
*                                                                               
REQF200  MVI   0(R3),DELELQ        ELEMENT TO BE DELETED                        
         GOTO1 HELLO,DMCB,(C'D',ACCMST),('DELELQ',(R2)),0,0                     
         B     REQF150                                                          
*                                                                               
REQF210  BAS   RE,BLDACT           BUILD ACTIVITY ELEM                          
*                                                                               
REQF220  GOTO1 ABINADD,DMCB,(RC),(R6),AIDTAB                                    
*                                                                               
         CLC   IDFORMS,=H'0'       IS # OF FORMS 0                              
         BE    REQF30              YES,GET NEXT RECORD                          
*                                                                               
         BAS   RE,CHKNEWYR         CHECK NEW RECORD                             
*                                                                               
         CLI   IDFLAG,IDRECX                                                    
         BE    REQF230                                                          
                                                                                
         CLI   QOPT3,C'Y'          SHOW NEW RECORDS?                            
         BNE   REQF240             NO.                                          
*                                                                               
REQF230  GOTO1 ABINADD,DMCB,(RC),(R6),AIDTAB                                    
*                                                                               
REQF240  CLI   FLAG,1              DOES RECORD EXIST ALREADY                    
         BE    REQF30              YES, SO GET NEXT RECORD                      
*                                                                               
         MVC   SVKEY,IOKEY         SAVING KEY                                   
         LA    RE,IO               PREPARING FOR ADDREC                         
         LA    RF,L'IO                                                          
         LA    R0,IO2                                                           
         LA    R1,L'IO2                                                         
         MVCL  RE,R0                                                            
*                                                                               
         LA    R2,IO                                                            
         SR    R4,R4                                                            
         ICM   R4,3,T99RLEN       LENGTH OF RECORD                              
         MVC   MSG,=CL10'AFTER CHGE'                                            
         GOTO1 ADUMP,DMCB,(RC),IO,(R4)                                          
         AP    TOTNYEAR,=P'1'      INCREMENT COUNTER                            
*                                                                               
         CLI   QOPT1,C'Y'         WRITE OPTION ON?                              
         BNE   REQF30             NO, GET NEXT RECORD                           
         CLI   RCWRITE,C'Y'       WRITE CARD PRESENT?                           
         BNE   REQF30             YES,SO GET NEXT RECORD                        
*                                                                               
         GOTO1 =A(DMADDREC),DMCB,(RC)  ADD RECORD TO MASTER                     
*                                                                               
         SR    R4,R4                                                            
         LA    R4,L'SVDA                                                        
         MVC   MSG,=CL10'DISK ADDR'                                             
         GOTO1 ADUMP,DMCB,(RC),SVDA,(R4)                                        
*                                                                               
         MVC   T99KDA,SVDA             MOVING IN DISK ADDRESS                   
         GOTO1 =A(DMWRTDR),DMCB,(RC)   WRITE RECORD TO DIR                      
         GOTO1 =A(DMREADDR),DMCB,(RC)  FIX SEQUENCE                             
         B     REQF30                  GET NEXT RECORD                          
*                                                                               
REQFX    B     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
**********************************************************************          
* RUN LAST                                                           *          
**********************************************************************          
         SPACE 1                                                                
RUNL     DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         XC    LSTALPHA,LSTALPHA                                                
*                                                                               
         USING BIND,RE                                                          
         L     RE,AIDTAB                                                        
         ICM   R3,15,BININ                                                      
         BZ    RUNLX                                                            
         USING IDTABD,R2                                                        
         LA    R2,BINTABLE                                                      
         USING PLINED,R4                                                        
         LA    R4,XP                                                            
RUNL10   MVC   XP,XSPACES                                                       
         MVC   XPSECOND,XSPACES                                                 
         MVC   XPTHIRD,XSPACES                                                  
         MVC   XPFOURTH,XSPACES                                                 
*                                                                               
         OC    LSTALPHA,LSTALPHA                                                
         BZ    RUNL20                                                           
         CLC   LSTALPHA,IDALPHA                                                 
         BE    RUNL30                                                           
*                                                                               
RUNL20   MVC   LSTALPHA,IDALPHA                                                 
         MVC   PRTALPH,IDALPHA                                                  
         GOTO1 HEXOUT,DMCB,IDCC,HCPYCODE,L'IDCC                                 
         MVC   PRTCC,HCPYCODE                                                   
RUNL30   MVC   PRTYEAR,IDYEAR      MOVE YEAR TO PRINT LINE                      
         EDIT  IDORIGIN,PRTORIG,ALIGN=LEFT                                      
         MVC   PRTUSER,IDUSER                                                   
         MVC   PRTNMADD,IDCNAME                                                 
         MVC   PRTEMAIL,IDEMAIL                                                 
         MVC   PRTTCC,IDTCC                                                     
         MVC   PRTTIN,IDTIN                                                     
         MVC   PRTCON,IDCON                                                     
         MVC   PRTPHON,IDPHON                                                   
         MVC   PRTDWNL,IDDWNL                                                   
         EDIT  IDFORMS,PRTFORMS,ZERO=NOBLANK                                    
*                                                                               
         LA    R1,MSGTAB                                                        
RUNL40   CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                ENTRY NOT FOUND SO DIE                       
         CLC   IDFLAG,0(R1)                                                     
         BE    *+12                                                             
         LA    R1,MSGNTRY(R1)      GO TO NEXT ENTRY                             
         B     RUNL40              LOOP                                         
         LA    R1,1(R1)                                                         
         MVC   PRTMSG,0(R1)        MOVE MESSAGE TO PRINT LINE                   
*                                                                               
         LA    R4,XPSECOND                                                      
         MVC   PRTNMADD,IDADD                                                   
         LA    R4,XPTHIRD                                                       
         MVC   PRTNMADD,IDADD2                                                  
         LA    R4,XPFOURTH                                                      
         MVC   PRTNMADD,IDADD3                                                  
         LA    R4,XP                                                            
*                                                                               
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         LA    R2,IDLNQ(R2)        BUMP TO NEXT ENTRY                           
         BCT   R3,RUNL10                                                        
*                                                                               
         GOTO1 ACREPORT                                                         
         BAS   RE,CLOSEBX                                                       
         MVI   RCSUBPRG,4                                                       
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         USING BOXD,R5                                                          
         L     R5,ADBOX                                                         
         MVI   BOXYORN,C'N'                                                     
         DROP  R5                                                               
*                                                                               
         LA    R4,XP                                                            
         MVC   0(32,R4),=C'TOTAL # OF COMPANIES USING 1099 '                    
         MVC   32(3,R4),=C' = '                                                 
         EDIT  TOTCPY,(10,45(R4)),ZERO=NOBLANK                                  
*                                                                               
         LA    R4,XPSECOND                                                      
         MVC   0(37,R4),=C'TOTAL # OF RECORDS READ FOR THE YEAR '               
         MVC   37(L'YEAR,R4),YEAR                                               
         MVC   41(3,R4),=C' = '                                                 
         EDIT  TOTYEAR,(10,45(R4)),ZERO=NOBLANK                                 
*                                                                               
         LA    R4,XPTHIRD                                                       
         MVC   0(38,R4),=C'TOTAL # OF RECORDS ADDED FOR THE YEAR '              
         MVC   38(L'NEWYEAR,R4),NEWYEAR                                         
         MVC   42(3,R4),=C' = '                                                 
         EDIT  TOTNYEAR,(10,45(R4)),ZERO=NOBLANK                                
*                                                                               
         GOTO1 ACREPORT                                                         
*                                                                               
RUNLX    B     EXIT                                                             
         DROP  R2,R4,RE                                                         
         EJECT                                                                  
**********************************************************************          
* GETUSER - GETS THE USER ID FROM CONTROL FILE                       *          
**********************************************************************          
         SPACE 1                                                                
GETUSER  NTR1                                                                   
         USING CTIREC,R4                                                        
         LA    R4,SVKEY            BUILIDING KEY                                
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ    C'I'                                         
*                                                                               
         USING T99RECD,R2                                                       
         LA    R2,IOKEY2                                                        
         MVC   CTIKNUM,T99KOID     LOGIN ORIGIN ID TO CONTROL KEY               
         DROP  R2                                                               
*                                                                               
         GOTO1 =A(DMCTFRD),DMCB,(RC)                                            
         CLC   SVKEY(CTILEN-CTIKEY),IOKEY   CORRECT RECORD?                     
         BNE   EXITL               EXIT WITH CC IF ORIGIN ID NOT THERE          
*                                                                               
         LA    R4,IOKEY                                                         
         LA    R3,CTIDATA          FIND ELEMENT X'20'                           
GETU10   CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   0(R3),CTDSCELQ      FOUND ELEMENT                                
         BE    GETU20              YES                                          
         ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     GETU10                                                           
*                                                                               
         USING CTDSCD,R3                                                        
GETU20   MVC   USER,CTDSC          SAVE USER NAME                               
         DROP  R3                                                               
                                                                                
*                                                                               
*FIX SEQUENCE BY DOING READ OF OUR ORIGINAL RECORD                              
*                                                                               
         MVC    SVKEY,IOKEY2                                                    
         GOTO1 =A(DMREADDR),DMCB,(RC)                                           
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* CHKNEWYR - CHECKS FOR THE EXISTENCE OF NEW RECORD ON FILE           *         
*            AND ALSO FILLS IN ID TABLE FROM NEW RECORD               *         
***********************************************************************         
         SPACE 1                                                                
CHKNEWYR NTR1                                                                   
         USING IDTABD,R6           ID TABLE                                     
         LA    R6,IDWRK                                                         
*                                                                               
         LA    RE,IDWRK            RE=A(WORK AREA)                              
         LA    RF,L'IDWRK          RF=(LENGTH OF WORK AREA)                     
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0               SPACE OUT WORK AREA                          
*                                                                               
         MVC   SVKEY2,IOKEY                                                     
         MVC   SVKEY,IOKEY2        KEY OF NEW RECORD                            
         GOTO1 =A(DMREADDR),DMCB,(RC)                                           
*                                                                               
         MVI   IDFLAG,IDRECA       DEFAULT FLAG                                 
         LA    R1,DMCB                                                          
         CLI   8(R1),X'10'         CHECK IF RECORD EXISTS                       
         BE    CHKYR10             NO                                           
         MVI   FLAG,1              TURN FLAG ON                                 
         MVI   IDFLAG,IDRECX                                                    
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
*                                                                               
         LA    RE,IO2              PREPARING TO GET DATA                        
         LA    RF,L'IO2                                                         
         LA    R0,IO                                                            
         LA    R1,L'IO                                                          
         MVCL  RE,R0                                                            
*                                                                               
         USING T99RECD,R2                                                       
CHKYR10  LA    R2,IO2                                                           
*                                                                               
         MVC   IDALPHA,ALPHAID                                                  
         MVC   IDCC,T99KCPY        SAVING COMPANY CODE                          
         MVC   IDYEAR,T99KYEAR     SAVING YEAR                                  
         MVC   IDORIGIN,T99KOID    SAVING ORIGIN ID                             
*                                                                               
         BAS   RE,GETUSER          GET USER ID                                  
         MVC   IDUSER,USER         MOVE IN USER NAME                            
*                                                                               
         LA    R3,T99RFST                                                       
CHKYR20  CLI   0(R3),0             DONE WITH ELEMENTS                           
         BE    CHKYR90             YES                                          
*                                                                               
         CLI   0(R3),FFTELQ        FREE FORM ELEMENT?                           
         BE    CHKYR40             YES                                          
         CLI   0(R3),NAMELQ        NAME ELEMENT                                 
         BE    CHKYR70                                                          
         CLI   0(R3),ADRELQ        ADDRESS ELEMENT                              
         BE    CHKYR80                                                          
*                                                                               
CHKYR30  ZIC   R1,1(R3)            GETTING LENGTH                               
         AR    R3,R1                                                            
         B     CHKYR20                                                          
*                                                                               
         USING FFTELD,R3                                                        
CHKYR40  CLI   FFTTYPE,FFTTTNNI    IS IT TYPE 76?                               
         BE    CHKYR50                                                          
         CLI   FFTTYPE,FFTTCNAM    IS IT TYPE 75?                               
         BE    CHKYR60                                                          
*                                                                               
CHKYR50  MVC   IDDWNL,FFTTDWNL     SAVE DOWNLOAD INFO                           
         MVC   IDFORMS,FFTTNOF     SAVING # OF FORMS                            
         MVC   IDTCC,FFTTTCC       TRANSMISSION CONTROL CODE                    
         MVC   IDTIN,FFTTTIN       TIN/EIN TAX IDENTIFICATION #                 
         B     CHKYR30                                                          
*                                                                               
CHKYR60  MVC   IDCON,FFTTNAME      SAVING CONTACT NAME                          
         MVC   IDPHON,FFTTPHON     SAVING CONTACT PHONE #                       
         B     CHKYR30                                                          
*                                                                               
         USING NAMELD,R3                                                        
CHKYR70  ZIC   R1,NAMLN            GET LENGTH                                   
         SHI   R1,NAMLN1Q+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IDCNAME(0),NAMEREC  GET NAME                                     
         B     CHKYR30                                                          
         DROP  R3                                                               
*                                                                               
         USING ADRELD,R3                                                        
CHKYR80  CLI   ADRLN,ADRLNQ                                                     
         BNE   CHKYR85                                                          
         MVC   IDADD,ADRLINE1      ADD ADDRESS LINE 1 TO PRINT LINE             
         MVC   IDADD2,ADRLINE2     ADD ADDRESS LINE 2 TO PRINT LINE             
         MVC   IDADD3,ADRCSZZR     ADD ADDRESS LINE 3 TO TABLE ENTRY            
         B     CHKYR30                                                          
*                                                                               
         USING OADRELD,R3                                                       
CHKYR85  MVC   IDADD(L'OADRLN1),OADRLN1                                         
         MVC   IDADD2(L'OADRLN2),OADRLN2                                        
         MVC   IDADD3(L'OADRCSZP),OADRCSZP                                      
         B     CHKYR30                                                          
         DROP  R3                                                               
*                                                                               
*FIX SEQUENCE                                                                   
*                                                                               
CHKYR90  MVC   SVKEY,SVKEY2                                                     
         GOTO1 =A(DMREADDR),DMCB,(RC)                                           
*                                                                               
         B     EXIT                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* CLOSE BOX                                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING BOXD,R4                                                          
CLOSEBX  NTR1                                                                   
         L     R4,ADBOX                                                         
         MVI   BOXREQ,C'C'          CLOSE BOX                                   
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* BUILD RECORD ACTIVITY ELEMENT                                      *          
**********************************************************************          
         SPACE 1                                                                
BLDACT   NTR1                                                                   
*                                                                               
         USING RACELD,R5                                                        
         LA    R5,ELEM             ADDING NEW ELEMENT                           
         XC    ELEM,ELEM                                                        
         MVI   RACEL,RACELQ        X'F9'                                        
         MVI   RACLN,RACLNQ        LENGTH                                       
         MVI   RACTYPE,RACTADD     TYPE 1                                       
         MVC   RACUSER,=X'0A4B'    USER ID DDS                                  
         GOTO1 DATCON,DMCB,(9,ADATE),(1,SVPDATE)                                
         MVC   RACDATE,SVPDATE     MOVING COMPRESSED DATE                       
         BAS   RE,ADDL                                                          
*                                                                               
BLDACTX  B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* ADD ELEMENT                                                        *          
**********************************************************************          
         SPACE 1                                                                
ADDL     NTR1                                                                   
         LA    R3,IO2                                                           
         LA    R5,ELEM                                                          
         GOTO1 HELLO,DMCB,(C'P',=C'ACCMST'),(R3),(R5)                           
ADDX     B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GETEL                                                              *          
**********************************************************************          
         SPACE 1                                                                
         GETEL R2,DATADISP,ELCODE                                               
         EJECT                                                                  
**********************************************************************          
* TAPE NAMES & ADDRESS CONSTANTS                                     *          
**********************************************************************          
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    V(PRNTBL)                                                        
         DC    A(DUMP)             DUMP ROUTINE                                 
         DC    V(HELLO)                                                         
         DC    A(IDTAB)            ID TABLE                                     
         DC    A(BINADD)                                                        
*                                                                               
ACCMST   DC    CL8'ACCMST'               CHANGE RECORD IN THIS FILE             
DELELQ   EQU   X'FF'                     DELETE ELEMENT FF                      
                                                                                
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMCTFRD  NMOD1 0,CTR               READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',SVKEY,IOKEY                  
         B     DMX                                                              
*                                                                               
DMCTFSQ  NMOD1 0,CTS               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'CTFILE ',SVKEY,IOKEY                  
         B     DMX                                                              
*                                                                               
DMWRTDR  NMOD1 0,WRT               WRITE BACK TO DIR                            
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMWRT'),=C'ACCDIR',IOKEY,IOKEY            
         B     DMX                                                              
*                                                                               
DMADDDR  NMOD1 0,ADD               ADD KEY TO DIR                               
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMADD'),=C'ACCDIR',IOKEY,IOKEY            
         B     DMX                                                              
*                                                                               
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRSEQ),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMREAD),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMADDREC NMOD1 0,AREC              ADD RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'ACCMST ',SVDA,IO,DMWORK               
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         MVC   SVDA,ACCKDA         SAVE OFF DISK ADDRESS                        
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',SVDA,IO,DMWORK                    
         B     DMX                                                              
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',SVDA,IO,DMWORK              
*                                                                               
DMX      XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
         SPACE 1                                                                
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
*                                                                               
         USING BOXD,R4                                                          
         L     R4,ADBOX                                                         
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+6,C'M'                                                   
         MVI   BOXCOLS,C'L'                                                     
*                                                                               
         MVI   BOXCOLS+(PRTCC-PRTLNE-1),C'C'                                    
         MVI   BOXCOLS+(PRTYEAR-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(PRTORIG-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(PRTUSER-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(PRTNMADD-PRTLNE-1),C'C'                                 
         MVI   BOXCOLS+(PRTEMAIL-PRTLNE-1),C'C'                                 
         MVI   BOXCOLS+(PRTTCC-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PRTTIN-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PRTCON-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PRTPHON-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(PRTDWNL-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(PRTFORMS-PRTLNE-1),C'C'                                 
         MVI   BOXCOLS+(PRTMSG-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+PRLNQ,C'R'                                               
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
         XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
        SPACE 1                                                                 
*                                                                               
* MESSAGE TABLE                                                                 
*                                                                               
                                                                                
MSGTAB  DS     0H                                                               
        DC     AL1(IDRECX)                                                      
        DC     CL16'RECORD EXISTS'                                              
MSGNTRY EQU    *-MSGTAB            LENGTH OF EACH ENTRY                         
        DC     AL1(IDRECA)                                                      
        DC     CL16'RECORD ADDED'                                               
        DC     AL1(IDRECNA)                                                     
        DC     CL16'RECORD NOT ADDED'                                           
        DC     AL1(IDRECD)                                                      
        DC     CL21' '                                                          
        DC     X'FF'                                                            
MSGLEN  EQU    *-MSGTAB                                                         
*                                                                               
* BINTABLE 1 - ID TABLE                                                         
*                                                                               
         DC    C'**IDTAB***'                                                    
IDTAB    DS    0D                  BINTABLE CONSTANTS FOR TABLE 6               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(IDLNQ)              LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(IDKLNQ)             KEY LENGTH                               
         DC    AL4(IDMAX)              MAX IN TABLE                             
         DC    AL1(IDBKCT)             NUMBER OF BUCKETS - NO BUCKETS           
         DC    AL1(IDBKT-IDTABD)       DISPLACEMENT TO FIRST BUCKET             
         DS    (IDMAX*IDLNQ)XL1        TABLE                                    
*                                                                               
IDMAX    EQU   10000                                                            
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   DS    0D                                                               
         NMOD1 0,**BINA**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R5,8(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTABLE         A(TABLE)                                     
         L     R3,4(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R3)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINXIT                                                           
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R0,R0                                                            
         ICM   R0,1,BINNUM         NUMBER OF BUCKETS                            
         BZ    BINXIT                NO BUCKETS - EXIT                          
         DC    H'0'                NO BUCKETS                                   
*                                                                               
BINXIT   XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
         SPACE 1                                                                
ACTCD    DSECT                                                                  
ADBOX    DS    A                                                                
AUTL     DS    A                                                                
*                                                                               
VTYPES   DS    0A                                                               
PRNTBL   DS    A                                                                
ADUMP    DS    A                                                                
HELLO    DS    V                                                                
AIDTAB   DS    A                                                                
ABINADD  DS    A                                                                
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
TOTCPY   DS    PL6                 TOTAL # OF COMPANIES USING 1099              
TOTYEAR  DS    PL6                 TOTAL RECORD FOR CURRENT YEAR                
TOTNYEAR DS    PL6                 TOTAL RECORD FOR NEW YEAR                    
*                                                                               
MSG      DS    CL10                MESSAGE FOR PRINTABLE                        
ELCODE   DS    CL1                                                              
FLAG     DS    XL1                 FLAG                                         
*                                                                               
SVPDATE  DS    PL3                 DATE COMPRESSED                              
LSTALPHA DS    XL2                 LAST ALPHA ID                                
HCPYCODE DS    CL2                 HEX OUT CPY CODE                             
YEAR     DS    CL4                 CURRENT YEAR TAX RECORD                      
NEWYEAR  DS    CL4                 NEW YEAR TAX RECORD                          
USER     DS    CL9                 USER ID                                      
*                                                                               
APDUB    DS    D                                                                
*                                                                               
SVDA     DS    CL4                                                              
SVACT    DS    CL12                                                             
SVKEY    DS    CL42                                                             
SVKEY2   DS    CL42                                                             
*                                                                               
DATE     DS    0CL6                TODAY'S DATE                                 
DTYR     DS    CL2                                                              
DTMM     DS    CL2                                                              
DTDD     DS    CL2                                                              
                                                                                
ADATE    DS    0CL8                NEW DATE FOR RECORD BUILD                    
ADTYR    DS    CL4                                                              
ADTMM    DS    CL2                                                              
ADTDD    DS    CL2                                                              
                                                                                
CDATE    DS    0CL8                CONVERTED DATE                               
CTDYR    DS    CL4                                                              
CTDMM    DS    CL2                                                              
CTDDD    DS    CL2                                                              
*                                                                               
ELEM     DS    XL255                                                            
IDWRK    DS    CL(IDLNQ)                                                        
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
                                                                                
IO2      DS    0CL2042                                                          
IOKEY2   DS    CL42                KEY                                          
IODATA2  DS    CL2000              DATA                                         
IO2LNQ   EQU   *-IO2               LENGTH                                       
                                                                                
         EJECT                                                                  
**********************************************************************          
* PRINT DSECT                                                        *          
**********************************************************************          
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLNE   DS    0C                                                               
         DS    CL1                                                              
PRTALPH  DS    CL2                                                              
         DS    CL4                                                              
PRTCC    DS    CL2                                                              
         DS    CL2                                                              
PRTYEAR  DS    CL4                                                              
         DS    CL1                                                              
PRTORIG  DS    CL5                                                              
         DS    CL1                                                              
PRTUSER  DS    CL9                                                              
         DS    CL1                                                              
PRTNMADD DS    CL40                                                             
         DS    CL1                                                              
PRTEMAIL DS    CL31                                                             
         DS    CL1                                                              
PRTTCC   DS    CL5                                                              
         DS    CL1                                                              
PRTTIN   DS    CL9                                                              
         DS    CL1                                                              
PRTCON   DS    CL33                                                             
         DS    CL1                                                              
PRTPHON  DS    CL15                                                             
         DS    CL1                                                              
PRTDWNL  DS    CL1                                                              
         DS    CL3                                                              
PRTFORMS DS    CL5                                                              
         DS    CL1                                                              
PRTMSG   DS    CL16                                                             
PRLNQ    EQU   *-PRTLNE                                                         
         EJECT                                                                  
**********************************************************************          
* ID TABLE DSECT                                                     *          
**********************************************************************          
         SPACE 1                                                                
IDTABD   DSECT                                                                  
IDALPHA  DS    CL2                 ALPHA ID                                     
IDCC     DS    CL1                 COMPANY CODE                                 
IDORIGIN DS    CL2                 ORIGIN ID                                    
IDYEAR   DS    CL4                 YEAR                                         
IDFORMS  DS    CL2                 FORMS                                        
IDKLNQ   EQU   *-IDTABD                                                         
IDUSER   DS    CL9                 USER NAME                                    
IDCNAME  DS    CL40                COMPANY NAME                                 
IDADD    DS    CL40                ADDRESS 1                                    
IDADD2   DS    CL40                ADDRESS 2                                    
IDADD3   DS    CL40                ADDRESS 3                                    
IDEMAIL  DS    CL31                EMAIL                                        
IDTCC    DS    CL5                 TCC CODE                                     
IDTIN    DS    CL9                 TIN NUMBER                                   
IDCON    DS    CL40                CONTACT NAME                                 
IDPHON   DS    CL15                PHONE NUMBER                                 
IDDWNL   DS    CL1                 DOWNLOAD (Y/N)                               
IDFLAG   DS    CL1                 FLAG                                         
IDRECX   EQU   X'80'               RECORD ALREADY EXISTS                        
IDRECD   EQU   X'40'               DEFAULT                                      
IDRECA   EQU   X'20'               RECORD ADDED                                 
IDRECNA  EQU   X'10'               RECORD NOT ADDED                             
IDBKT    DS    0PL8                BUCKET                                       
IDBKLN   EQU   *-IDBKT             BUCKET LENGTH                                
IDBKCT   EQU   (*-IDBKT)/IDBKLN    NUMBER OF BUCKETS                            
IDLNQ    EQU   *-IDTABD                                                         
         EJECT                                                                  
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLN   EQU   *-BIND                                                           
BINNUM   DS    CL1                 NUMBER OF BUCKETS                            
BINFST   DS    CL1                 DISPLACEMENT TO FIRST BUCKET                 
BINTABLE DS    0C                  THE TABLE                                    
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER OLD ADDRESS ELEMENT ADRELD, WITH CITY, STATE ZIP     *         
***********************************************************************         
OADRELD  DSECT                                                                  
OADREL   DS    XL1                                                              
OADRELQ  EQU   X'22'                                                            
OADRLN   DS    XL1                                                              
OADRSTAT DS    XL1                 STATUS                                       
OADRCSZ  EQU   X'80'                                                            
OADRLN1  DS    CL33                ADDRESS LINE 1                               
OADRLN2  DS    CL33                ADDRESS LINE 2                               
OADRCSZP DS    0CL33               CITY, STATE, ZIP, ZIP ROUTE                  
OADRCITY DS    CL22                                                             
OADRST   DS    CL2                 STATE                                        
OADRZIP  DS    CL5                                                              
OADRZRN  DS    CL4                 ZIP ROUTING NO.                              
OADRLNQ  EQU   *-OADRELD           LENGTH OF THIS ELEMENT                       
         EJECT                                                                  
***********************************************************************         
*               ++INCLUDES                                            *         
***********************************************************************         
*                                                                               
*ACREPWORKD                                                                     
*ACGENBOTH                                                                      
*ACGENMODES                                                                     
*DDLOGOD                                                                        
*ACMASTD                                                                        
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENRAC                                                       
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREPTC02 11/16/04'                                      
         END                                                                    
