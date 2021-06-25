*          DATA SET SEACS02    AT LEVEL 010 AS OF 08/11/00                      
*PHASE TA0D02A                                                                  
ACS02    TITLE '- SECURITY ACCESS - PROGRAM RECORDS'                            
ACS02    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACS2**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY            R2=A(RECORD KEY)                             
         USING SAPGREC,R2                                                       
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         XR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY              01 - APMVALK                                 
         B     VALREC              02 - APMVALR                                 
         B     DISKEY              03 - APMDISK                                 
         B     DISREC              04 - APMDISR                                 
         B     DELRES              05 - APMDELR                                 
         B     DELRES              06 - APMRESR                                 
         B     VALSEL              07 - APMVALP                                 
         B     GETSEL              08 - APMGETS                                 
         B     DISSEL              09 - APMDISS                                 
         B     XIT                 10 - APMVALS                                 
         B     FSTLST              11 - APMFLST                                 
         B     XIT                 12 - APMPROC                                 
         B     XIT                 13 - APMFSCR                                 
         B     LSTSCR              14 - APMLSCR                                 
         B     XIT                 15 - APMVALQ                                 
         B     XIT                 16 - APMREPP                                 
         B     XIT                 17 - APMSETT                                 
         B     PUTKEY              18 - APMPUTK                                 
         B     XIT                 19 - APMNEWK                                 
         B     XIT                 20 - APMFRP                                  
         B     XIT                 21 - APMDISS2                                
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF PROGRAM RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY            INITIALIZE KEY                               
         XC    SAPGKEY,SAPGKEY                                                  
         MVI   SAPGTYP,SAPGTYPQ                                                 
         MVI   SAPGSUB,SAPGSUBQ                                                 
*                                                                               
         MVI   FVMINL,1            VALIDATE SYSTEM NAME                         
         GOTO1 AVALSYS,DSPSYSH                                                  
         BNE   VALKEYX                                                          
         MVC   SAPGOVS,APWORK                                                   
*                                                                               
         MVI   FVMINL,1            VALIDATE PROGRAM NAME                        
         GOTO1 AVALPGM,PARM,(SAPGOVS,DSPPGMH)                                   
         BNE   VALKEYX                                                          
         MVC   APGMLST,APPARM        SAVE A(PROGRAM LIST ENTRY)                 
         XC    DSPPCD,DSPPCD                                                    
         OI    DSPPCDH+FHOID,FHOITR                                             
         MVC   SAPGPGM,APWORK                                                   
         MVC   APRECKEY(L'SAPGKEY),SAPGKEY                                      
*                                                                               
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)       IF NOT DISPLAY READ FOR UPDATE               
         GOTO1 AIO                                                              
         BE    VKEY2                                                            
         BL    VALKEYX             IF I/O ERROR EXIT                            
         MVI   APINDS,APIOKADD                                                  
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    *+8                                                              
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         B     VALKEYY                                                          
*                                                                               
VKEY2    MVI   APINDS,APIOKDIS+APIOKCHA                                         
         XC    APELEM,APELEM       FIND PROGRAM ELEMENT                         
         L     R3,AIOAREA1                                                      
         LA    R3,SAPGDATA-SAPGREC(R3)                                          
         XR    RF,RF                                                            
         IC    RF,1(RF)                                                         
         CLI   0(R3),SAPGMELQ                                                   
         BE    *+8                                                              
         BXH   R3,RF,*-12                                                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   APELEM(0),0(R3)                                                  
         PUSH  USING                                                            
         USING SAPGMD,APELEM                                                    
         OC    SAPGMACT,SAPGMACT    PROGRAM RECORD CAN ONLY BE DELETED          
         BNZ   *+8                    IF NO ACTIONS ARE DEFINED FOR IT          
         OI    APINDS,APIOKDEL                                                  
         POP   USING                                                            
*                                                                               
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEYX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A PROGRAM RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
*                                                                               
         CLI   APACTN,ACTADD       TEST ACTION IS ADD                           
         BNE   VREC02                                                           
         MVC   SAPGKEY,APRECKEY    SET UP ELEMENTLESS RECORD                    
         LA    R0,SAPGDATA-SAPGREC+1                                            
         STCM  R0,3,SAPGLEN                                                     
         MVI   SAPGSTAT,0                                                       
         MVI   SAPGDATA,0                                                       
*                                                                               
         LA    R3,APELEM           ADD PROGRAM ELEMENT                          
         USING SAPGMD,R3                                                        
         XC    SAPGMD(SAPGMLNQ),SAPGMD                                          
         MVI   SAPGMEL,SAPGMELQ                                                 
         MVI   SAPGMLN,SAPGMLNQ                                                 
         GOTO1 AADDELS,SAPGREC                                                  
*                                                                               
VREC02   LA    R3,SAPGDATA                                                      
         USING SAPGMD,R3           R3=A(PROGRAM ELEMENT)                        
         XR    RF,RF                                                            
         CLI   SAPGMEL,SAPGMELQ                                                 
         BE    *+12                                                             
         IC    RF,SAPGMLN                                                       
         BXH   R3,RF,*-12                                                       
*                                                                               
         NI    SAPGMIND,FF-SAPGMIPC                                             
         L     RF,APGMLST                                                       
         TM    PGMIND3-PGMLSTD(RF),PGMIPC                                       
         BZ    VREC04                                                           
         OI    SAPGMIND,SAPGMIPC                                                
         MVC   DSPPCD,PCPROG                                                    
*                                                                               
VREC04   GOTO1 AVALTXT,PARM,(SAPGOVS,DSPDSCNH)                                  
         BNE   VALRECX             VALIDATE TEXT DESCRIPTION NUMBER             
         MVC   SAPGMDSC,APHALF                                                  
         MVC   DSPDSCA,APWORK                                                   
         OI    DSPDSCAH+FHOID,FHOITR                                            
*                                                                               
         GOTO1 HIGHFMT,PARM,(DSPRCDF,DSPRCDFH)                                  
         GOTO1 VALFMT,DSPRCDFH     VALIDATE RECORD FORMAT                       
         BNE   VALRECX                                                          
         MVC   SAPGMRIF,FVIFLD                                                  
*                                                                               
         GOTO1 HIGHFMT,PARM,(DSPACTF,DSPACTFH)                                  
         GOTO1 VALFMT,DSPACTFH     VALIDATE ACTION FORMAT                       
         BNE   VALRECX                                                          
         MVC   SAPGMAIF,FVIFLD                                                  
         DROP  R3                                                               
*                                                                               
         GOTO1 ASUBDIC,0                                                        
         PUSH  USING                                                            
         XC    APELEM,APELEM                                                    
         USING SADICD,APELEM                                                    
         MVI   SADICEL,SADICELQ                                                 
         MVI   SADICLN,SADICLNQ                                                 
         GOTO1 VHELLO,PARM,(C'D',CTFILE),('SADICELQ',SAPGREC),0                 
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VREC06   NI    INDICS,FF-INDIDIC                                                
*                                                                               
         MVC   DSPARCW,AC@RECTY    'RECORD TYPE' DICTIONARY NUMBER              
         OI    DSPARCWH+FHOID,FHOITR                                            
         CLI   DSPARCQH+FHILD,0                                                 
         BE    VREC07                                                           
         OI    INDICS,INDIDIC                                                   
         GOTO1 AVALDIC,PARM,(SAPGOVS,L'DSPARCW),(C'L',DSPARCQH)                 
         BNE   VALRECX                                                          
         MVC   DSPARCW,APWORK                                                   
         MVC   SADICARC,APHALF                                                  
*                                                                               
VREC07   XC    DSPARLW,DSPARLW     'RECORD LIST' TEXT NUMBER                    
         OI    DSPARLWH+FHOID,FHOITR                                            
         CLI   DSPARLQH+FHILD,0                                                 
         BE    VREC08                                                           
         GOTO1 AVALTXT,PARM,(SAPGOVS,DSPARLQH)                                  
         BNE   VALRECX                                                          
         MVC   DSPARLW,APWORK                                                   
         MVC   SADICARL,APHALF                                                  
*                                                                               
VREC08   MVC   DSPAACW,AC@ACT      'ACTION' DICTIONARY NUMBER                   
         OI    DSPAACWH+FHOID,FHOITR                                            
         CLI   DSPAACQH+FHILD,0                                                 
         BE    VREC09                                                           
         OI    INDICS,INDIDIC                                                   
         GOTO1 AVALDIC,PARM,(SAPGOVS,L'DSPAACW),(C'L',DSPAACQH)                 
         BNE   VALRECX                                                          
         MVC   DSPAACW,APWORK                                                   
         MVC   SADICAAC,APHALF                                                  
*                                                                               
VREC09   XC    DSPAALW,DSPAALW     'ACTION LIST' TEXT NUMBER                    
         OI    DSPAALWH+FHOID,FHOITR                                            
         CLI   DSPAALQH+FHILD,0                                                 
         BE    VREC10                                                           
         GOTO1 AVALTXT,PARM,(SAPGOVS,DSPAALQH)                                  
         BNE   VALRECX                                                          
         MVC   DSPAALW,APWORK                                                   
         MVC   SADICAAL,APHALF                                                  
*                                                                               
VREC10   MVC   DSPFWRW,FC@WRITE    'WRITE' DICTIONARY NUMBER                    
         OI    DSPFWRWH+FHOID,FHOITR                                            
         CLI   DSPFWRQH+FHILD,0                                                 
         BE    VREC12                                                           
         OI    INDICS,INDIDIC                                                   
         GOTO1 AVALDIC,PARM,(SAPGOVS,L'DSPFWRW),(C'L',DSPFWRQH)                 
         BNE   VALRECX                                                          
         MVC   DSPFWRW,APWORK                                                   
         MVC   SADICFWR,APHALF                                                  
*                                                                               
VREC12   MVC   DSPFRDW,FC@READ     'READ' DICTIONARY NUMBER                     
         OI    DSPFRDWH+FHOID,FHOITR                                            
         CLI   DSPFRDQH+FHILD,0                                                 
         BE    VREC14                                                           
         OI    INDICS,INDIDIC                                                   
         GOTO1 AVALDIC,PARM,(SAPGOVS,L'DSPFRDW),(C'L',DSPFRDQH)                 
         BNE   VALRECX                                                          
         MVC   DSPFRDW,APWORK                                                   
         MVC   SADICFRD,APHALF                                                  
*                                                                               
VREC14   MVC   DSPFNOW,FC@NO       'NO' DICTIONARY NUMBER                       
         OI    DSPFNOWH+FHOID,FHOITR                                            
         CLI   DSPFNOQH+FHILD,0                                                 
         BE    VREC20                                                           
         OI    INDICS,INDIDIC                                                   
         GOTO1 AVALDIC,PARM,(SAPGOVS,L'DSPFNOW),(C'L',DSPFNOQH)                 
         BNE   VALRECX                                                          
         MVC   DSPFNOW,APWORK                                                   
         MVC   SADICFNO,APHALF                                                  
*                                                                               
VREC20   TM    INDICS,INDIDIC                                                   
         BZ    VREC22                                                           
         GOTO1 VHELLO,PARM,(C'P',CTFILE),SAPGREC,SADICD                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*                                                                               
         GOTO1 ASUBDIC,SAPGREC                                                  
         MVC   DSPARLW,AC@RECLD                                                 
         MVC   DSPAALW,AC@ACTLD                                                 
*                                                                               
VREC22   GOTO1 ASETACT,SAPGREC     DEFINE ACTIVITY ELEMENT                      
*                                                                               
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         CLI   APACTN,ACTADD                                                    
         BNE   *+8                                                              
         LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                 ADD/WRITE RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALRECX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF PROGRAM RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         GOTO1 ADISSYS,SAPGOVS                                                  
         MVC   DSPSYS,APWORK       SYSTEM                                       
*                                                                               
         GOTO1 ADISPGM,PARM,(SAPGOVS,SAPGPGM)                                   
         MVC   DSPPGM,APWORK       PROGRAM                                      
*                                                                               
DISKEYX  B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DISPLAY PROGRAM RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         LA    R3,SAPGDATA                                                      
         USING SAPGMD,R3           R3=A(PROGRAM ELEMENT)                        
         XR    RF,RF                                                            
         CLI   SAPGMEL,SAPGMELQ                                                 
         BE    *+12                                                             
         IC    RF,SAPGMLN                                                       
         BXH   R3,RF,*-12                                                       
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),SAPGMDSC                                               
         EDIT  (B4,FULL),(5,DSPDSCN),ALIGN=LEFT                                 
         OI    DSPDSCNH+FHOID,FHOITR                                            
         GOTO1 ADISTXT,PARM,(SAPGOVS,SAPGMDSC)                                  
         MVC   DSPDSCA,APWORK      DISPLAY DESCRIPTION                          
         OI    DSPDSCAH+FHOID,FHOITR                                            
*                                                                               
         GOTO1 HIGHFMT,PARM,(SAPGMRIF,DSPRCDFH)                                 
         GOTO1 HIGHFMT,PARM,(SAPGMAIF,DSPACTFH)                                 
         DROP  R3                                                               
*                                                                               
         GOTO1 ASUBDIC,SAPGREC                                                  
         PUSH  USING                                                            
         XC    APELEM,APELEM                                                    
         USING SADICD,APELEM                                                    
         LA    R1,SAPGDATA                                                      
         XR    RF,RF                                                            
DREC02   CLI   0(R1),0                                                          
         BE    DREC04                                                           
         IC    RF,1(R1)                                                         
         CLI   0(R1),SADICELQ                                                   
         BE    *+8                                                              
         BXH   R1,RF,DREC02                                                     
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SADICD(0),0(R1)                                                  
*                                                                               
DREC04   MVC   APBYTE,SAPGOVS                                                   
         OI    APBYTE,X'80'                                                     
*                                                                               
         XC    DSPARCQ,DSPARCQ                                                  
         OI    DSPARCQH+FHOID,FHOITR                                            
         MVC   DSPARCW,AC@RECTY                                                 
         OI    DSPARCWH+FHOID,FHOITR                                            
         OC    SADICARC,SADICARC                                                
         BZ    DREC05                                                           
         GOTO1 ADISDIC,PARM,(APBYTE,8),(C'L',SADICARC)                          
         MVC   DSPARCQ,APWORK+64                                                
*                                                                               
DREC05   XC    DSPARLQ,DSPARLQ                                                  
         OI    DSPARLQH+FHOID,FHOITR                                            
         MVC   DSPARLW,AC@RECLD                                                 
         OI    DSPARLWH+FHOID,FHOITR                                            
         OC    SADICARL,SADICARL                                                
         BZ    DREC06                                                           
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),SADICARL                                               
         EDIT  (B4,FULL),(5,DSPARLQ),ALIGN=LEFT                                 
*                                                                               
DREC06   XC    DSPAACQ,DSPAACQ                                                  
         OI    DSPAACQH+FHOID,FHOITR                                            
         MVC   DSPAACW,AC@ACT                                                   
         OI    DSPAACWH+FHOID,FHOITR                                            
         OC    SADICAAC,SADICAAC                                                
         BZ    DREC07                                                           
         GOTO1 ADISDIC,PARM,(APBYTE,8),(C'L',SADICAAC)                          
         MVC   DSPAACQ,APWORK+64                                                
*                                                                               
DREC07   XC    DSPAALQ,DSPAALQ                                                  
         OI    DSPAALQH+FHOID,FHOITR                                            
         MVC   DSPAALW,AC@ACTLD                                                 
         OI    DSPAALWH+FHOID,FHOITR                                            
         OC    SADICAAL,SADICAAL                                                
         BZ    DREC08                                                           
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),SADICAAL                                               
         EDIT  (B4,FULL),(5,DSPAALQ),ALIGN=LEFT                                 
*                                                                               
DREC08   XC    DSPFWRQ,DSPFWRQ                                                  
         OI    DSPFWRQH+FHOID,FHOITR                                            
         MVC   DSPFWRW,FC@WRITE                                                 
         OI    DSPFWRWH+FHOID,FHOITR                                            
         OC    SADICFWR,SADICFWR                                                
         BZ    DREC10                                                           
         GOTO1 ADISDIC,PARM,(APBYTE,8),(C'L',SADICFWR)                          
         MVC   DSPFWRQ,APWORK+64                                                
*                                                                               
DREC10   XC    DSPFRDQ,DSPFRDQ                                                  
         OI    DSPFRDQH+FHOID,FHOITR                                            
         MVC   DSPFRDW,FC@READ                                                  
         OI    DSPFRDWH+FHOID,FHOITR                                            
         OC    SADICFRD,SADICFRD                                                
         BZ    DREC12                                                           
         GOTO1 ADISDIC,PARM,(APBYTE,8),(C'L',SADICFRD)                          
         MVC   DSPFRDQ,APWORK+64                                                
*                                                                               
DREC12   XC    DSPFNOQ,DSPFNOQ                                                  
         OI    DSPFNOQH+FHOID,FHOITR                                            
         MVC   DSPFNOW,FC@NO                                                    
         OI    DSPFNOWH+FHOID,FHOITR                                            
         OC    SADICFNO,SADICFNO                                                
         BZ    DREC20                                                           
         GOTO1 ADISDIC,PARM,(APBYTE,8),(C'L',SADICFNO)                          
         MVC   DSPFNOQ,APWORK+64                                                
*                                                                               
         POP   USING                                                            
*                                                                               
DREC20   GOTO1 ADISACT,SAPGREC     DISPLAY ACTIVITY DATE                        
*                                                                               
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO SET SCREEN TO MODIFIED               
*                                                                               
DISRECX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE OR RESTORE A PROGRAM RECORD                       *         
***********************************************************************         
         SPACE 1                                                                
DELRES   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,SAPGREC                                                  
         XI    SAPGSTAT,X'80'      CHANGE DELETE FLAG IN RECORD                 
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
DELRESX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY         SET UP RECORD KEY                            
         XC    SAPGKEY,SAPGKEY                                                  
         MVI   SAPGTYP,SAPGTYPQ                                                 
         MVI   SAPGSUB,SAPGSUBQ                                                 
*                                                                               
         CLI   LSTSYSH+FHILD,0     TEST SYSTEM NAME ENTERED                     
         BE    VSEL2                                                            
         GOTO1 AVALSYS,LSTSYSH     VALIDATE SYSTEM NAME                         
         BNE   VALSELX                                                          
         MVC   SAPGOVS,APWORK                                                   
*                                                                               
VSEL2    MVC   SELOVS,SAPGOVS                                                   
         LA    R0,LSTACTH          SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM                                                        
         MVI   APPARM+4,LSTLINEN   SET NO. OF LIST LINES                        
         LA    R0,LSTLINEL                                                      
         STCM  R0,3,APPARM+6       SET LENGTH OF LIST LINE                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO                                      
*                                                                               
VALSELX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   L     R2,AIOAREA1                                                      
         MVC   IOKEY(L'SAPGKEY),APRECKEY                                        
*                                                                               
         TM    APINDS,APILRERD     TEST SEQUENTIAL READ BROKEN                  
         BZ    GSEL2                                                            
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BNE   GETSELN                                                          
         B     GSEL4                                                            
*                                                                               
GSEL2    TM    APINDS,APILNSEQ     TEST START OF NEW SCREEN                     
         BO    GSEL4                                                            
         GOTO1 AIO,IOCONFIL+IOHI+IO1                                            
         B     GSEL6                                                            
*                                                                               
GSEL4    GOTO1 AIO,IOCONFIL+IOSQ+IO1                                            
GSEL6    BNE   GETSELN                                                          
         CLI   SAPGTYP,SAPGTYPQ    TEST RECORD TYPE                             
         BNE   GETSELN                                                          
         CLI   SAPGSUB,SAPGSUBQ                                                 
         BNE   GETSELN                                                          
*                                                                               
         CLI   SELOVS,0            TEST SYSTEM FILTER                           
         BE    *+14                                                             
         CLC   SAPGOVS,SELOVS                                                   
         BNE   GETSELN                                                          
*                                                                               
         MVC   APRECKEY(L'SAPGKEY),SAPGKEY                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   L     R2,AIOAREA1                                                      
         LA    R3,SAPGDATA                                                      
         USING SAPGMD,R3           R3=A(PROGRAM ELEMENT)                        
         XR    RF,RF                                                            
         CLI   SAPGMEL,SAPGMELQ                                                 
         BE    *+12                                                             
         IC    RF,SAPGMLN                                                       
         BXH   R3,RF,*-12                                                       
*                                                                               
         L     R4,APPARM                                                        
         USING LSTACTH,R4          R4=A(LIST/SELECT LINE)                       
*                                                                               
         GOTO1 ADISSYS,SAPGOVS                                                  
         MVC   LSTLSYS,APWORK      SYSTEM NAME                                  
*                                                                               
         GOTO1 ADISPGM,PARM,(SAPGOVS,SAPGPGM)                                   
         MVC   LSTLPGM,APWORK      PROGRAM NAME                                 
*                                                                               
         GOTO1 ADISTXT,PARM,(SAPGOVS,SAPGMDSC)                                  
         MVC   LSTLTXT,APWORK      PROGRAM DESCRIPTION                          
*                                                                               
DISSELX  B     XIT                                                              
         DROP  R3,R4                                                            
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE FIRST FOR LIST (SET SCREEN TO MODIFIED)           *         
***********************************************************************         
         SPACE 1                                                                
FSTLST   OI    ACSSRVH+FHOID,FHOITR+FHOIMO                                      
         B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT KEY OF RECORD TO SAVED KEY TABLE                     *         
***********************************************************************         
         SPACE 1                                                                
PUTKEY   LA    R2,APRECKEY                                                      
         LA    R3,APELEM                                                        
         XR    R0,R0                                                            
*                                                                               
         MVI   0(R3),KEYSYS        ADD SYSTEM NAME ELEMENT                      
         MVI   1(R3),L'SYSLNAME+2                                               
         GOTO1 ADISSYS,SAPGOVS                                                  
         MVC   2(L'SYSLNAME,R3),APWORK                                          
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
         MVI   0(R3),KEYPGM        ADD PROGRAM NAME ELEMENT                     
         MVI   1(R3),L'PGMNAME+2                                                
         GOTO1 ADISPGM,PARM,(SAPGOVS,SAPGPGM)                                   
         MVC   2(L'PGMNAME,R3),APWORK                                           
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
         MVI   0(R3),0                                                          
         GOTO1 APUTKEY                                                          
*                                                                               
PUTKEYX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE RECORD/ACTION FORMAT                            *         
*                                                                     *         
* NTRY: R1=A(FIELD HEADER)                                            *         
***********************************************************************         
         SPACE 1                                                                
VALFMT   NTR1  ,                                                                
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL                                                            
         BNE   XIT                                                              
*                                                                               
         LA    RF,FMTLIST          SEARCH FORMAT LIST FOR CHARACTER             
         LA    R0,FMTLISTN                                                      
         CLC   FVIFLD(1),0(RF)                                                  
         BE    XIT                                                              
         LA    RF,1(RF)                                                         
         BCT   R0,*-14                                                          
*                                                                               
         MVC   FVMSGNO,=AL2(FVFNOTV) INVALID CHARACTER                          
         LTR   RB,RB               CC=NOT EQUAL                                 
         B     XIT                                                              
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO HIGHLIGHT RECORD/ACTION FORMAT                           *         
*                                                                     *         
* NTRY: P1=(FORMAT CODE, A(FIELD HEADER FOR CODE))                    *         
***********************************************************************         
         SPACE 1                                                                
HIGHFMT  NTR1  ,                                                                
         L     R3,0(R1)                                                         
         USING FHD,R3              R3=A(FIELD HEADER)                           
         MVC   FHDA(1),0(R1)                                                    
         OI    FHOI,FHOITR                                                      
         LA    R0,FMTLISTN                                                      
         LA    RF,FMTLIST                                                       
         XR    RE,RE                                                            
*                                                                               
HIGHFMT2 IC    RE,FHLN                                                          
         AR    R3,RE               BUMP R3 TO NEXT FIELD                        
         OI    FHOI,FHOITR         TRANSMIT                                     
         NI    FHAT,FF-FHATHI                                                   
         CLC   0(1,R1),0(RF)       IF MATCH FOUND                               
         BNE   *+8                   HIGHLIGHT FIELD                            
         OI    FHAT,FHATHI                                                      
         LA    RF,1(RF)            BUMP RF TO NEXT FORMAT                       
         BCT   R0,HIGHFMT2                                                      
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
FF       EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
CTFILE   DC    C'CTFILE '                                                       
PCPROG   DC    CL(L'DSPPCD)'PC program'                                         
FMTLIST  DS    0AL1                LIST OF FORMATS                              
         DC    AL1(SAPGMIFX)       - HEXADECIMAL                                
         DC    AL1(SAPGMIFA)       - ALPHAMERIC                                 
         DC    AL1(SAPGMIFN)       - NUMERICAL                                  
FMTLISTN EQU   *-FMTLIST                                                        
         EJECT                                                                  
* SEACSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SEACSWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSFDD                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSDDD                                                       
         ORG   LSTLIN              * LIST LINE LAYOUT *                         
LSTLSYS  DS    CL7                 SYSTEM                                       
         DS    CL2                                                              
LSTLPGM  DS    CL7                 PROGRAM                                      
         DS    CL2                                                              
LSTLTXT  DS    CL47                TEXT DESCRIPTION                             
*                                                                               
LSTLINEL EQU   LSTACT2H-LSTACTH    LIST LINE LENGTH                             
LSTLINES EQU   LSTFOOTH-LSTACTH    LIST LINES LENGTH                            
LSTLINEN EQU   LSTLINES/LSTLINEL   NO. OF LIST LINES                            
         EJECT                                                                  
WORKD    DSECT                     ** DSECT TO COVER LOCAL W/S **               
         ORG   APLOCAL                                                          
DUB      DS    D                                                                
FULL     DS    F                                                                
PARM     DS    6A                                                               
APGMLST  DS    A                                                                
INDICS   DS    XL1                 INDICATORS                                   
INDIDIC  EQU   X'80'               OVERRIDE DICTIONARY WORD(S) DEFINED          
WORK     DS    XL64                                                             
SELOVS   DS    XL1                 SELECT SYSTEM                                
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SEACS02   08/11/00'                                      
         END                                                                    
