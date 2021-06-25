*          DATA SET CTGEN10    AT LEVEL 044 AS OF 05/01/02                      
*PHASE TA0B10A                                                                  
         TITLE 'CTGEN10 - FILE MAINTENANCE - CURRENCY RECORDS'                  
GEN10    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GEN10*,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING GCURD,R2            R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     DELREC                                                           
         B     RESREC                                                           
         B     VALSEL                                                           
         B     GETSEL                                                           
         B     DISSEL                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     LSTSCR                                                           
         B     VALREQ                                                           
         B     PRTREP                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF CURRENCY RECORD                                    
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         XC    GCKEY,GCKEY                                                      
         MVI   GCKREC,GCKRECQ      RECORD TYPE                                  
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,CURCURH       VALIDATE CURRENCY                            
         BNE   VALKEYX                                                          
         MVC   GCKCURR,SPACES                                                   
         MVC   GCKCURU,FVIFLD                                                   
         MVC   APRECKEY(GCKEYL),GCKEY                                           
         LA    R1,APRECKEY                                                      
         LA    R1,IORDD+IOGENDIR+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VK010                                                            
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BNZ   *+12                                                             
         MVI   APINDS,APIOKADD     NOT DEL THEREFORE NO REC                     
         B     VALKEYY                                                          
         MVI   APINDS,APIOKDIS+APIOKRES                                         
*                                                                               
VK010    LA    R1,IOGET+IOGENFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                 GET RECORD                                   
         TM    IOERR,IOERRS-IOEDEL DELETED IS ONLY SAFE ERR                     
         BZ    *+6                                                              
         DC    H'0'                ERROR ON GET OF D/A RECORD                   
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
*                                                                               
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A CURRENCY RECORD                                    
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         MVC   GCKEY,APRECKEY                                                   
         CLI   APACTN,ACTADD                                                    
         BE    VR010               NO ELEMENT TO REMOVE ON ADD                  
         MVI   APELEM,GCRELQ                                                    
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,GCURD       DELETE EXISTING CURRENCY ELEMENT             
*                                                                               
VR010    LA    R3,APELEM                                                        
         USING GCREL,R3            R3=A(CURRENCY ELEMENT)                       
         XC    APELEM,APELEM                                                    
         MVI   GCREL,GCRELQ                                                     
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,CURFULNH      GET FULL NAME                                
         BNE   VALRECX                                                          
         MVC   GCRNAME,FVIFLD                                                   
         SR    R1,R1               CALCULATE ELEMENT LEN                        
         IC    R1,FVILEN                                                        
         LA    R1,GCRLENQ(R1)                                                   
         STC   R1,GCRLEN                                                        
         LA    R1,GCFIRST(R1)                                                   
         STCM  R1,3,GCFLEN                                                      
         CLI   APACTN,ACTADD       CLEAR N'EXCHANGE RECS ON ADD                 
         BNE   *+10                                                             
         XC    GCFSTNX,GCFSTNX                                                  
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,CURUNITH      GET UNITS                                    
         BNE   VALRECX                                                          
         TM    FVIIND,FVINUM       TEST NUMERIC                                 
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALRECX                                                          
         OC    SCFULL(2),SCFULL    TEST NOT > HALFWORD                          
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECX                                                          
         MVC   GCRUNITS,SCFULL+2                                                
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,CURDECH       GET DEC PLACES                               
         BNE   VALRECX                                                          
         TM    FVIIND,FVINUM       TEST NUMERIC                                 
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALRECX                                                          
         MVC   GCRDECP,SCFULL+3                                                 
*                                                                               
         MVI   APBYTE,0                                                         
         GOTO1 AFVAL,CURPFXH       GET PREFIX                                   
         BNE   VR020               NO PREFIX / GET SUFFIX                       
         MVC   APBYTE,FVILEN                                                    
         MVC   GCRPFIX,FVIFLD                                                   
*                                                                               
VR020    MVI   FVMINL,1                                                         
         GOTO1 AFVAL,CURSFXH       GET SUFFIX                                   
         BNE   VR030                                                            
         LA    R1,ERRPFIX          MUST BE PREFIX OR SUFFIX                     
         BAS   RE,SETERR                                                        
         CLI   APBYTE,0            TEST SUFFIX ONLY                             
         BNE   VALRECX                                                          
         MVC   APBYTE,FVILEN                                                    
         MVC   GCRPFIX,FVIFLD                                                   
         MVI   GCRPFSP,1           SET PREFIX IS SUFFIX                         
*                                                                               
VR030    CLI   APBYTE,0            TEST ANY INPUT                               
         BE    VALRECX                                                          
         MVC   GCRPFLN,APBYTE                                                   
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,CURMINH       GET MINIMUM RATE                             
         BNE   VALRECX                                                          
         GOTO1 VALNUM,5            CONVERT TO 99999.99999 FORMAT                
         BNE   VALRECX                                                          
         MVC   GCRMNEXC,APDUB+3                                                 
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,CURMAXH       GET MAXIMUM RATE                             
         BNE   VALRECX                                                          
         GOTO1 VALNUM,5            CONVERT TO 99999.99999 FORMAT                
         BNE   VALRECX                                                          
         MVC   GCRMXEXC,APDUB+3                                                 
*                                                                               
         LA    R1,ERRMXMN                                                       
         BAS   RE,SETERR                                                        
         CLC   GCRMNEXC,GCRMXEXC                                                
         BH    VALRECX                                                          
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,CURMAXBH      GET MAX BUY RATE                             
         BNE   VALRECX                                                          
         SR    RF,RF                                                            
         IC    RF,GCRDECP                                                       
         LA    R1,10                                                            
         SR    R1,RF                                                            
         GOTO1 VALNUM                                                           
         BNE   VALRECX                                                          
         STH   R1,APHALF                                                        
         IC    R1,GCRDECP                                                       
         SH    R1,APHALF                                                        
         BZ    VR040                                                            
         BNM   *+16                                                             
         LA    R1,ERRDECP          TOO MANY DEC PLACES                          
         BAS   RE,SETERR                                                        
         B     VALRECX             ERROR EXIT                                   
         L     RF,APFULL                                                        
         MH    RF,=H'10'           NOT ENOUGH SO MULTIPLY                       
         BCT   R1,*-4                                                           
         ST    RF,APFULL                                                        
         TM    APFULL,X'80'        TEST FOR NEGATIVE                            
         BNO   VR040                                                            
         LA    R1,ERRFMAX                                                       
         BAS   RE,SETERR                                                        
         B     VALRECX             ERROR EXIT                                   
VR040    MVC   GCRMXBUY,APFULL                                                  
*                                                                               
         CLI   APACTN,ACTADD                                                    
         BE    VR050                                                            
         CLC   GCRDECP,LASTDECP    DEC PLACES CAN'T CHANGE                      
         BE    VR050                                                            
         OC    GCFSTNX,GCFSTNX     UNLESS NO EXCHANGE RECORDS                   
         B     VR050                                                            
         BZ    VR050                                                            
         LA    R1,ERRXCHA          CAN'T CHANGE THIS FIELD                      
         BAS   RE,SETERR                                                        
         B     VALRECX                                                          
*                                                                               
VR050    GOTO1 AADDELS,GCURD                                                    
         DROP  R3                                                               
*                                                                               
         GOTO1 ASETACT,GCURD       DEFINE ACTIVITY ELEMENT                      
*                                                                               
         LA    R1,IOADD+IOGENFIL+IO1                                            
         CLI   APACTN,ACTADD                                                    
         BE    *+8                                                              
         LA    R1,IOPUT+IOGENFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   APACTN,ACTADD       DON'T UPDATE GENDIR ON ADD                   
         BE    VALRECX                                                          
         LA    R2,IOKEY                                                         
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF CURRENCY RECORD                                     
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         MVC   APWORK(3),GCKCURU                                                
         GOTO1 DISPFLD,CURCURH                                                  
*                                                                               
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY CURRENCY RECORD                                            
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         TWAXC CURFULNH                                                         
*                                                                               
         MVC   LASTDECP,GCRDECP                                                 
*                                                                               
         LA    R0,GCRLENQ+1                                                     
         SR    R1,R1                                                            
         IC    R1,GCRLEN                                                        
         SR    R1,R0               FIND LEN OF NAME FOR EXECUTE                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CURFULN(0),GCRNAME                                               
*                                                                               
         EDIT  (B2,GCRUNITS),(5,CURUNIT),WRK=APWORK,DUB=APDUB,ALIGN=LEFX        
               T                                                                
*                                                                               
         EDIT  (B1,GCRDECP),(1,CURDEC),WRK=APWORK,DUB=APDUB,ALIGN=LEFT,X        
               ZERO=NOBLANK                                                     
*                                                                               
         CLI   GCRPFSP,0                                                        
         BNE   DISR010                                                          
         MVC   CURPFX,GCRPFIX                                                   
         B     DISR020                                                          
DISR010  MVC   CURSFX,GCRPFIX                                                   
*                                                                               
DISR020  LA    R1,GCRMNEXC         EDIT MINIMUM RATE                            
         BAS   RE,EDITNUM                                                       
         MVC   CURMIN,FVOMSG                                                    
*                                                                               
         LA    R1,GCRMXEXC         EDIT MAXIMUM RATE                            
         BAS   RE,EDITNUM                                                       
         MVC   CURMAX,FVOMSG                                                    
*                                                                               
         ICM   R0,15,GCRMXBUY      EDIT MAXIMUM BUY RATE                        
         CVD   R0,APDUB                                                         
         MVC   APWORK(13),=X'40202020202020202020202020'                        
         LA    R1,APWORK+11                                                     
         SR    R0,R0                                                            
         ICM   R0,1,GCRDECP        LOCATE DECIMAL POINT                         
         BZ    *+12                                                             
         SR    R1,R0                                                            
         MVC   0(2,R1),=X'214B'    AND INSERT PATERN                            
         ED    APWORK(13),APDUB+2                                               
         LA    R0,13                                                            
         CLI   APWORK,C' '         SQUASH OUT SPACES                            
         BNE   *+18                                                             
         MVC   APWORK(12),APWORK+1                                              
         MVI   APWORK+12,C' '                                                   
         BCT   R0,*-18                                                          
         MVC   CURMAXB(10),APWORK                                               
*                                                                               
DISRECX  GOTO1 ADISACT,GCURD       DISPLAY ACTIVITY DATE                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A CURRENCY RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
DELREC   LA    R2,IOKEY                                                         
         OC    GCDSTNX,GCDSTNX                                                  
         BZ    *+16                                                             
         LA    R1,ERRXDEL          CAN'T DELETE RECORD                          
         BAS   RE,SETERR                                                        
         B     DELRECX                                                          
         OI    GCDSTAT,X'80'       SET DELETE FLAG IN DIRECTORY                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GCURD                                                    
         OI    GCFSTAT,X'80'       SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOPUT+IOGENFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED CURRENCY RECORD                                  
***********************************************************************         
         SPACE 1                                                                
RESREC   LA    R2,IOKEY                                                         
         NI    GCDSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GCURD                                                    
         NI    GCFSTAT,X'FF'-X'80' UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOGENFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RESRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY                                                      
         OI    APINDS,APILFLST                                                  
         XC    GCKEY,GCKEY                                                      
         MVI   GCKREC,GCKRECQ                                                   
*                                                                               
         GOTO1 AFVAL,LSTCURH       VALIDATE CURRENCY                            
         MVC   GCKCURR,FVIFLD                                                   
*                                                                               
VALSELY  MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   LA    R2,IOKEY                                                         
         MVC   GCKEY,APRECKEY                                                   
         TM    APINDS,APILFLST                                                  
         BNO   *+12                                                             
         NI    APINDS,255-APILFLST                                              
         B     GETSEL6                                                          
GETSEL2  TM    APINDS,APILRERD     TEST SEQUENCE BROKEN                         
         BZ    GETSEL4                                                          
         GOTO1 AIO,IOGENDIR+IORD+IO1                                            
         BE    GETSEL8                                                          
         B     GETSELN                                                          
GETSEL4  TM    APINDS,APILNSEQ     TEST READ OR READ HIGH                       
         BNZ   GETSEL8                                                          
GETSEL6  LA    R1,IOGENDIR+IOHI+IO1                                             
         B     *+8                                                              
GETSEL8  LA    R1,IOGENDIR+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GETSELN                                                          
         LA    R2,IOKEY                                                         
         CLI   GCKREC,GCKRECQ      CHECK STILL CURRENCY RECORD                  
         BNE   GETSELN                                                          
         SPACE 1                                                                
GETSEL14 GOTO1 AIO,IOGENFIL+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
*                                                                               
GETSELY  MVC   APRECKEY(L'GCKEY),GCKEY                                          
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
*                                                                               
         MVC   LISTCODE,GCKCURU                                                 
*                                                                               
         LA    RF,GCRLENQ+1                                                     
         SR    R1,R1                                                            
         IC    R1,GCRLEN                                                        
         SR    R1,RF                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTFULN(0),GCRNAME                                              
*                                                                               
         MVC   LISTPFIX,GCRPFIX                                                 
*                                                                               
         EDIT  (B1,GCRDECP),(3,LISTDECP),ALIGN=LEFT,ZERO=NOBLANK,DUB=APX        
               DUB,WRK=APWORK                                                   
*                                                                               
         LA    R1,GCRMNEXC         EDIT MINIMUM RATE                            
         BAS   RE,EDITNUM                                                       
         MVC   LISTMINR,FVOMSG                                                  
*                                                                               
         LA    R1,GCRMXEXC         EDIT MAXIMUM RATE                            
         BAS   RE,EDITNUM                                                       
         MVC   LISTMAXR,FVOMSG                                                  
*                                                                               
DISSELX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
VALREQ   L     R9,AREP                                                          
         USING REPD,R9             R9=A(REPORT WORK AREA)                       
         LA    R2,APRECKEY                                                      
         XC    APRECKEY,APRECKEY                                                
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPREQH       VALIDATE REQUESTOR                           
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       SET REQUESTOR                                
*                                                                               
         GOTO1 AVALWHEN,REPWHENH   VALIDATE WHEN                                
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALDEST,REPDESTH   VALIDATE DESTINATION ID                      
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AFVAL,REPCURH       VALIDATE CURRENCY IF INPUT                   
         BNE   VRQ20                                                            
         MVC   GCKCURU,FVIFLD                                                   
*                                                                               
VRQ20    MVI   GCKREC,GCKRECQ                                                   
         MVC   REPDESC,REPDESCL                                                 
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GENERATE CURRENCY REPORT                                 *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   L     R9,AREP                                                          
         L     R2,AIOAREA1                                                      
         MVC   IOKEY,APRECKEY      SET INITIAL KEY VALUE                        
*                                                                               
PR010    LA    R1,IOHI+IOGENDIR+IO1                                             
         GOTO1 AIO                                                              
         BE    PR030                                                            
         BNE   PRTREPX                                                          
PR020    CLI   REPLINE,10                                                       
         BNE   PR025                                                            
         MVC   IOKEY,GCKEY                                                      
         GOTO1 AIO,IOGENDIR+IORD+IO1                                            
         BNE   PRTREPX                                                          
PR025    LA    R1,IOSQ+IOGENDIR+IO1                                             
         GOTO1 AIO                                                              
         BNE   PRTREPX                                                          
*                                                                               
PR030    LA    R2,IOKEY                                                         
         CLI   GCKREC,GCKRECQ      TEST STILL A CURRENCY RECORD                 
         BNE   PRTREPX                                                          
*                                                                               
         GOTO1 AIO,IOGENFIL+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                RECORD MUST EXIST                            
         L     R2,AIOAREA1                                                      
*                                                                               
         CLC   GCKCURU,=C'ZZZ'                                                  
         BE    PRTEST                                                           
*                                                                               
         MVC   PRTCODE,GCKCURU                                                  
*                                                                               
         LA    RF,GCRLENQ+1                                                     
         SR    R1,R1                                                            
         IC    R1,GCRLEN                                                        
         SR    R1,RF                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRTFULN(0),GCRNAME                                               
*                                                                               
         MVC   PRTPFIX,GCRPFIX                                                  
*                                                                               
         EDIT  (B1,GCRDECP),(3,PRTDECP),ALIGN=LEFT,ZERO=NOBLANK,DUB=APDX        
               UB,WRK=APWORK                                                    
*                                                                               
         LA    R1,GCRMNEXC         EDIT MINIMUM RATE                            
         BAS   RE,EDITNUM                                                       
         MVC   PRTMINR,FVOMSG                                                   
*                                                                               
         LA    R1,GCRMXEXC         EDIT MAXIMUM RATE                            
         BAS   RE,EDITNUM                                                       
         MVC   PRTMAXR,FVOMSG                                                   
*                                                                               
         ICM   R0,15,GCRMXBUY      EDIT MAXIMUM BUY RATE                        
         CVD   R0,APDUB                                                         
         MVC   APWORK(13),=X'40202020202020202020202020'                        
         LA    R1,APWORK+11                                                     
         SR    R0,R0                                                            
         ICM   R0,1,GCRDECP        LOCATE DECIMAL POINT                         
         BZ    *+12                                                             
         SR    R1,R0                                                            
         MVC   0(2,R1),=X'214B'    AND INSERT PATERN                            
         ED    APWORK(13),APDUB+2                                               
         LA    R0,13                                                            
         CLI   APWORK,C' '         SQUASH OUT SPACES                            
         BNE   *+18                                                             
         MVC   APWORK(12),APWORK+1                                              
         MVI   APWORK+12,C' '                                                   
         BCT   R0,*-18                                                          
         MVC   PRTMAXB(10),APWORK                                               
*                                                                               
PR040    GOTO1 VREPORT,REPD                                                     
         B     PR020                                                            
*                                                                               
PRTEST   CLC   INUSER,=C'RJM'                                                   
         BNE   PRTREPX                                                          
         SPACE 1                                                                
         LA    R2,IOKEY                                                         
         OI    GCDSTAT,X'80'       SET DELETE FLAG IN DIRECTORY                 
         GOTO1 AIO,IOWRITE+IOGENDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,GCURD                                                    
         OI    GCFSTAT,X'80'       SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOPUT+IOGENFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 2                                                                
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GENERAL FIELD XMT IF CHANGED                                       *          
* R1=A(TWAHDR)                                                       *          
* APWORK MUST CONTAIN THE NEW TEXT                                   *          
**********************************************************************          
         SPACE 1                                                                
DISPFLD  ZIC   RF,FVTLEN-FVIHDR(R1)                                             
         SH    RF,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RF,=Y(L'FVIHDR)     KNOCK OFF HEADER EXTENSION                   
         BCTR  RF,0                                                             
         EX    RF,DISPFLDC         COMPARE FIELD CONTENTS                       
         BER   RE                                                               
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         EX    RF,DISPFLDM         MOVE IN NEW FIELD                            
         BR    RE                                                               
         SPACE 1                                                                
DISPFLDC CLC   L'FVIHDR(0,R1),APWORK                                            
DISPFLDM MVC   L'FVIHDR(0,R1),APWORK                                            
         SPACE 2                                                                
***********************************************************************         
* FIND NEXT ELEMENT OF SAME CODE WITHIN RECORD                        *         
* NTRY R3=A(CURRENT ELEMENT)                                          *         
*      APELEM = ELCODE TO FIND                                        *         
* EXIT CC EQ - FOUND - R3=A(NEW ELEMENT)                              *         
*      CC NE - NOT FOUND                                              *         
***********************************************************************         
         SPACE 1                                                                
NEXTEL   ZIC   RF,1(R3)            L'ELEMENT                                    
         AR    R3,RF               A(NEXT ELEMNT)                               
         ICM   RF,1,1(R3)          L'ELEMENT                                    
         BNZ   *+8                                                              
         LTR   RB,RB               FORCE CC NON ZERO                            
         BR    RE                                                               
         CLC   0(1,R3),APELEM                                                   
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*   ROUTINE TO VALIDATE NUMERIC                             *                   
*   INPUT IN THE FORM 99999.99999                           *                   
*   ENTRY R1=MAX NUMBER OF SIGNIFICANT DIGITS               *                   
*   EXIT CC=EQU OK / CC NEQ=ERROR -(MESSAGE SET FOR EXIT)   *                   
*   R1=NUMBER OF DECIMAL PLACES                             *                   
*   APFULL=ABSOLUTE BINARY VALUE DECIMAL POINT IGNORED      *                   
*   APDUB=PACKED 99999999999.99999 VALUE                    *                   
*************************************************************                   
         SPACE 1                                                                
VALNUM   NTR1                                                                   
         ST    R1,APFULL                                                        
         SR    RE,RE                                                            
         SR    R1,R1                                                            
         IC    R1,FVXLEN           GET INPUT LEN - 1                            
         LA    RF,FVIFLD                                                        
VALN01   CLI   0(RF),C'.'          SCAN FOR '.'                                 
         BE    VALN02                                                           
         CLI   0(RF),C' '          OR FIRST SPACE                               
         BE    VALN01A                                                          
         TM    0(RF),X'F0'         ALL ELSE MUST BE NUMERIC                     
         BNO   VALNOTN                                                          
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)            COUNT SIGNIFICANT DIGITS                     
         B     VALN01                                                           
*                                                                               
VALN01A  LA    R1,1(R1)            IF NO '.' USE REAL LENGTH                    
*                                                                               
VALN02   STC   R1,APBYTE           SAVE LEN IN APBYTE                           
         C     RE,APFULL           CHECK MAX SIGNIFICANT DIGITS                 
         BH    VALNTOG                                                          
         SR    R1,RE               R1=NUM OF DEC PLACES                         
         STH   R1,APHALF                                                        
         BZ    VALN02A                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),1(RF)       SQUASH OUT DEC POINT                         
         LA    R1,1(R1)                                                         
VALN02A  LA    RE,5                                                             
         SR    RE,R1               CALCULATE                                    
         BM    VALNDCP                                                          
         SLL   RE,2                SHIFT VALUE                                  
*                                                                               
         LA    RF,FVIFLD                                                        
         IC    R1,APBYTE                                                        
         BCTR  R1,0                                                             
         EX    R1,*+16                                                          
         EX    R1,*+18                                                          
         EX    R1,*+20                                                          
         B     *+22                                                             
         MVC   APWORK(0),0(RF)                                                  
         NC    APWORK(0),NUMERIC                                                
         CLC   APWORK(0),NUMERIC   CHECK NUMERIC                                
         BNE   VALNOTN                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  APDUB(8),0(0,RF)    PACK NUMBER                                  
         CP    APDUB,=P'2147483647'                                             
         BNL   VALNTOG             TEST ABSOLUTE MAX X'7FFFFFFF'                
         CVB   R1,APDUB                                                         
         ST    R1,APFULL                                                        
         LM    R0,R1,APDUB         LOAD INTO R0,R1                              
         SRDL  R0,4                LOSE SIGN BITS                               
         SLDL  R0,0(RE)            SHIFT CORRECT AMOUNT                         
         STM   R0,R1,APDUB         AND STORE BACK                               
         LH    R1,APHALF                                                        
         CR    RB,RB                                                            
         B     VALNX                                                            
*                                                                               
VALNOTN  MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALNERR                                                          
VALNTOG  LA    R1,ERRFMAX                                                       
         BAS   RE,SETERR                                                        
         B     VALNERR                                                          
VALNDCP  LA    R1,ERRDECP                                                       
         BAS   RE,SETERR                                                        
VALNERR  LTR   RB,RB                                                            
*                                                                               
VALNX    XIT1  REGS=(R1)                                                        
         SPACE 2                                                                
***************************************                                         
*  ROUTINE TO OUTPUT NUMBERS IN       *                                         
*  99999.99999 FORMAT                 *                                         
***************************************                                         
         SPACE 1                                                                
EDITNUM  ST    RE,APFULL                                                        
         XC    APDUB,APDUB                                                      
         MVC   APDUB+2(6),0(R1)                                                 
         MVC   FVOMSG(12),=X'4020202021204B2020202020'                          
         ED    FVOMSG(12),APDUB+2                                               
         LA    R1,FVOMSG+11                                                     
EDITN1   LA    RF,FVOMSG                                                        
         CR    R1,RF                                                            
         BNH   EDITN2                                                           
         CLI   0(R1),C'.'                                                       
         BE    EDITN2                                                           
         CLI   0(R1),C'0'                                                       
         BNE   EDITN3                                                           
         MVI   0(R1),C' '                                                       
         BCT   R1,EDITN1                                                        
EDITN2   MVI   1(R1),C'0'                                                       
EDITN3   LA    R0,12                                                            
         CLI   FVOMSG,C' '                                                      
         BNE   *+18                                                             
         MVC   FVOMSG(11),FVOMSG+1                                              
         MVI   FVOMSG+11,C' '                                                   
         BCT   R0,*-18                                                          
EDNMX    L     RE,APFULL                                                        
         BR    RE                                                               
         EJECT                                                                  
***************************************                                         
*    SET UP GETTXT ERROR EXIT         *                                         
*    R1=MSG NUMBER                    *                                         
***************************************                                         
         SPACE 1                                                                
SETERR   XC    APPARM,APPARM       SET UP GETTXT BLOCK                          
         LA    RF,APPARM                                                        
         MVI   8(RF),C'E'          MESSAGE TYPE                                 
         STH   R1,2(RF)            MESSAGE NUMBER                               
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
         BR    RE                                                               
***************************************                                         
*           ERROR EQUATES             *                                         
***************************************                                         
ERRPFIX  EQU   152                                                              
ERRDECP  EQU   153                                                              
ERRXDEL  EQU   156                                                              
ERRXCHA  EQU   157                                                              
ERRMXMN  EQU   158                                                              
ERRFMAX  EQU   9                                                                
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
NUMERIC  DC    X'F0F0F0F0F0F0F0F0F0F0'                                          
SPACES   DC    80C' '                                                           
         SPACE 1                                                                
REPDESCL DC    CL11'CURRENCY   '                                                
         SPACE 1                                                                
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,CT#CULST,13,L                                              
         SPEC  H2,57,CT#CULST,13,LU                                             
         SPEC  M1,1,CT#CODE,6,L                                                 
         SPEC  M1,8,CT#FULN,22,L                                                
         SPEC  M1,43,CT#PFIX,7,L                                                
         SPEC  M1,51,CT#DCPLS,3,L                                               
         SPEC  M1,57,CT#RATST,18,L                                              
         SPEC  M1,77,CT#MAXBR,16,L                                              
         SPEC  M2,1,88C'-'                                                      
         SPEC  M2,43,CT#SFIX,7,L                                                
         SPEC  M2,50,CT#PLACS,6,L                                               
         SPEC  M2,57,CT#MIN,3,L                                                 
         SPEC  M2,67,CT#MAX,3,L                                                 
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
         EJECT                                                                  
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
* GEGENCUR                                                                      
       ++INCLUDE GEGENCUR                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENCFD                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENEFD                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENAFD                                                       
         ORG                                                                    
         EJECT                                                                  
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
         DS    CL1                                                              
LISTCODE DS    CL3                                                              
         DS    CL3                                                              
LISTFULN DS    CL35                                                             
         DS    CL1                                                              
LISTPFIX DS    CL3                                                              
         DS    CL5                                                              
LISTDECP DS    CL3                                                              
         DS    CL2                                                              
LISTMINR DS    CL9                                                              
LISTMAXR DS    CL9                                                              
         DS    XL1                                                              
         SPACE 2                                                                
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
PRTLIN   DS    0CL(L'REPP1)                                                     
         DS    CL1                                                              
PRTCODE  DS    CL3                                                              
         DS    CL3                                                              
PRTFULN  DS    CL35                                                             
         DS    CL1                                                              
PRTPFIX  DS    CL3                                                              
         DS    CL5                                                              
PRTDECP  DS    CL3                                                              
         DS    CL2                                                              
PRTMINR  DS    CL9                                                              
         DS    CL1                                                              
PRTMAXR  DS    CL9                                                              
         DS    CL1                                                              
PRTMAXB  DS    CL10                                                             
         ORG   PRTLIN+L'PRTLIN                                                  
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
SELKEY   DS    0XL32                                                            
LOCALX   EQU   *                                                                
*                                                                               
TWAD     DSECT                                                                  
         ORG   TWAOVER                                                          
LASTDECP DS    C                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044CTGEN10   05/01/02'                                      
         END                                                                    
