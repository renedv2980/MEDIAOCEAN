*          DATA SET CTGEN0A    AT LEVEL 066 AS OF 05/01/02                      
*PHASE TA0B0AA                                                                  
         TITLE 'CTGEN0A - USER ID LUID CONVERT'                                 
GEN0A    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GENA**,RA,R8,RR=RE                                           
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING CTIREC,R2           R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         L     R1,ACOM                                                          
         L     R1,CXSORT-COMFACSD(R1)                                           
         ST    R1,VXSORT                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
         CLI   APMODE,APMSETT                                                   
         BE    SETTWA                                                           
         CLI   APMODE,APMVALK                                                   
         BE    VALFLDS                                                          
         CLI   APMODE,APMDISR                                                   
         BE    CNVRECS                                                          
*                                                                               
EXITN    LTR   RB,RB                                                            
         B     *+6                                                              
EXITY    CR    RB,RB                                                            
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE FILEDS - LISTS OF OLD/NEW LUIDS FOR CONVERT     *         
***********************************************************************         
         SPACE 1                                                                
VALFLDS  EQU   *                                                                
         LA    RF,LUTAB                                                         
         ST    RF,LUTPNTR                                                       
         L     R1,LUTPNTR                                                       
         XC    LUTCNT,LUTCNT       INITIALISE LUID TABLE                        
         XC    LUTDEL,LUTDEL                                                    
         LA    RF,LUTLEN*LUTMAX                                                 
         XCEF  LUTAB,(RF)                                                       
         BAS   RE,VALLUT           VALIDATE LUID FIELDS AND BUILD LUTAB         
         BNE   VFLDNO                                                           
         L     RF,=A(SRTLUT)                                                    
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         MVI   APINDS,APIOKDIS                                                  
         MVI   OKFLAG,C'N'                                                      
         CLI   SAVPROM,0                                                        
         BE    VFLDOK                                                           
         GOTO1 AFVAL,IDUOKH                                                     
         BNE   VFLDOK                                                           
         CLI   FVIFLD,C'Y'                                                      
         BNE   VFLDOK                                                           
         MVI   OKFLAG,C'Y'                                                      
         B     VFLDOK                                                           
*                                                                               
VFLDOK   B     YES                                                              
VFLDNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CONVERT RECORDS                                          *         
***********************************************************************         
         SPACE 1                                                                
CNVRECS  EQU   *                                                                
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
*                                  READ PAST PASSIVE # RECORDS                  
         MVI   CTIKID+L'CTIKID-L'CTIKNUM-1,1                                    
         GOTO1 AIO,IOCONFIL+IOHI+IO1                                            
         B     CREC200                                                          
*                                                                               
CREC100  LA    R2,IOKEY                                                         
         MVC   CTIKEY,APRECKEY                                                  
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    CREC110                                                          
         B     CREC300                                                          
*                                                                               
CREC110  EQU   *                                                                
         GOTO1 AIO,IOCONFIL+IOSQ+IO1                                            
         BNE   CREC300                                                          
*                                                                               
CREC200  EQU   *                                                                
         L     R2,AIOAREA1                                                      
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         CLC   IOKEYSAV(CTIKID-CTIKEY),CTIKEY                                   
         BNE   CREC300                                                          
         MVC   APRECKEY(L'CTIKEY),CTIKEY                                        
*                                                                               
         BAS   RE,CNVLUS           CONVERT LUIDS                                
         BNE   CRECNO                                                           
         CLI   CNVIDFLG,C'N'       TEST ANY LUIDS CONVERTED                     
         BE    CREC100                                                          
         CLI   SAVPROM,0                                                        
         BE    CREC100                                                          
         CLI   OKFLAG,C'Y'                                                      
         BNE   CREC100                                                          
         BAS   RE,UPDUID           UPDATE USER ID RECORD IF SO                  
         BNE   CRECNO                                                           
         B     CREC100                                                          
*                                                                               
CREC300  EQU   *                                                                
         CLI   SAVPROM,0                                                        
         BNE   CREC310                                                          
         BAS   RE,DISLUT           REDISPLAY LUID FIELDS FROM LUTAB             
         MVI   SAVPROM,X'FF'                                                    
         MVC   IDUMSG,=CL60'OK TO GO AHEAD?'                                    
         OI    IDUMSGH+(FVOIND-FVIHDR),FVOXMT                                   
         LA    R1,IDUOKH                                                        
         ST    R1,FVADDR                                                        
         B     EIIF                                                             
*                                                                               
CREC310  EQU   *                                                                
         CLI   OKFLAG,C'Y'                                                      
         BNE   CREC320                                                          
         BAS   RE,DISLUT           REDISPLAY LUID FIELDS FROM LUTAB             
         MVI   SAVPROM,0                                                        
         LA    R1,IDULUOAH                                                      
         ST    R1,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(CI#ACTCP)                                           
         B     CRECOK                                                           
*                                                                               
CREC320  EQU   *                                                                
         LA    RF,LUTAB                                                         
         ST    RF,LUTPNTR                                                       
         L     R1,LUTPNTR                                                       
         XC    LUTCNT,LUTCNT       INITIALISE LUID TABLE                        
         XC    LUTDEL,LUTDEL                                                    
         LA    RF,LUTLEN*LUTMAX                                                 
         XCEF  LUTAB,(RF)                                                       
         BAS   RE,DISLUT           CLEAR DISPLAY                                
         MVC   IDUMSG,SPACES                                                    
         OI    IDUMSGH+(FVOIND-FVIHDR),FVOXMT                                   
         LA    R1,IDUOKH                                                        
         ST    R1,FVADDR                                                        
         MVI   SAVPROM,0                                                        
         LA    R1,IDULUOAH                                                      
         ST    R1,FVADDR                                                        
         B     EMIF                                                             
*                                                                               
CRECNO   DC    H'0'                                                             
*                                                                               
CRECOK   B     YES                                                              
*CRECNO   B     NO                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CONVERT ANY LUIDS FOR CURRENT USER ID RECORD             *         
***********************************************************************         
         SPACE 1                                                                
CNVLUS   NTR1                                                                   
         MVI   CNVIDFLG,C'N'                                                    
         L     R2,AIOAREA1                                                      
         LA    R3,CTIDATA                                                       
         USING CTPRND,R3                                                        
*                                                                               
CLUS010  CLI   CTPRNEL,0                                                        
         BE    CLUS100                                                          
         CLI   CTPRNEL,CTPRNELQ                                                 
         BE    CLUS030                                                          
CLUS020  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     CLUS010                                                          
*                                                                               
CLUS030  EQU   *                                                                
         OC    CTPRNNUM(2),CTPRNNUM                                             
         BZ    CLUS040                                                          
         GOTO1 CHGLUID,CTPRNLIN                                                 
         BNE   CLUS020                                                          
         MVI   CNVIDFLG,C'Y'                                                    
         B     CLUS020                                                          
*                                                                               
CLUS040  EQU   *                                                                
         B     CLUS020                                                          
*                                                                               
CLUS100  EQU   *                                                                
         B     CLUSOK                                                           
*                                                                               
CLUSOK   B     YES                                                              
CLUSNO   B     NO                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHANAGE LUID IF OLD/NEW VALUE PAIR FOUND IN LUTAB        *         
* R1=A(LUID TO BE CONVERTED)                                          *         
***********************************************************************         
         SPACE 1                                                                
CHGLUID  NTR1                                                                   
         LA    R4,LUTAB                                                         
         USING LUTABD,R4                                                        
*                                                                               
CHGL010  L     RF,LUTPNTR                                                       
         CR    R4,RF                                                            
         BNL   CHGLNO                                                           
         CLC   LUTOLD,0(R1)                                                     
         BE    CHGL030                                                          
CHGL020  LA    R4,LUTLEN(R4)                                                    
         B     CHGL010                                                          
*                                                                               
CHGL030  EQU   *                                                                
         MVC   0(L'LUTNEW,R1),LUTNEW                                            
         B     CHGLOK                                                           
*                                                                               
CHGLOK   B     YES                                                              
CHGLNO   B     NO                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UPDATE CURRENT USER ID RECORD WHEN ANY LUIDS CONVERTED   *         
***********************************************************************         
         SPACE 1                                                                
UPDUID   NTR1                                                                   
*                                  RE-READ ID RECORD FOR UPDATE                 
         GOTO1 AIO,IOCONFIL+IORDUP+IO1                                          
         BNE   UUIDNO                                                           
*                                                                               
         BAS   RE,CNVLUS           RE-CONVERT LUIDS                             
         B     UUIDOK                                                           
*                                                                               
UUIDOK   B     YES                                                              
UUIDNO   B     NO                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY LUID CONVERT TABLE DATA ON SCREEN                *         
* R1 POINTS TO START POINT IN LIST                                    *         
***********************************************************************         
         SPACE 1                                                                
DISLUT   NTR1                                                                   
*                                                                               
         LA    R4,LUTAB                                                         
         TWAXC IDULUOAH            CLEAR SCREEN                                 
         USING LUTABD,R4                                                        
         LA    R3,IDULUOAH         ADDRESS FIRST POSITION ON SCREEN             
         LR    R8,R3                                                            
         LA    R0,COLNUM           # LIST COLUMNS                               
*                                                                               
DLST010  L     RF,LUTPNTR                                                       
         CR    R4,RF                                                            
         BNL   DLSTOK              EXIT IF END OF LIST                          
         CLI   0(R4),DELFLAG       BYPASS ENTRIES FLAG AS DELETED               
         BE    DLST030                                                          
         LR    R1,R4                                                            
*                                  DISPLAY ENTRY                                
         MVC   FVIFLD-FVIHDR(L'IDULUOA,R3),LUTOLD                               
         OI    FVOIND-FVIHDR(R3),FVOXMT                                         
         NI    FVATRB-FVIHDR(R3),X'FF'-FVAHIGH                                  
         LA    RF,LUINOFF                                                       
         AR    RF,R3                                                            
         MVC   FVIFLD-FVIHDR(L'IDULUNA,RF),LUTNEW                               
         OI    FVOIND-FVIHDR(RF),FVOXMT                                         
         NI    FVATRB-FVIHDR(RF),X'FF'-FVAHIGH                                  
*                                                                               
         B     DLST012                                                          
         LA    RF,LUTAB            HIGHLIGHT REPEATED ENTRY                     
         CR    R4,RF                                                            
         BNH   DLST012                                                          
         LR    R1,R4                                                            
         LA    RE,LUTLEN                                                        
         SR    R1,RE                                                            
         DROP  R4                                                               
         USING LUTABD,R1                                                        
         CLC   LUTOLD+LUTLEN,LUTOLD                                             
         BE    DLST011                                                          
         CLC   LUTNEW+LUTLEN,LUTNEW                                             
         BE    DLST011                                                          
         B     DLST012                                                          
DLST011  OI    FVATRB-FVIHDR(R3),FVAHIGH                                        
         DROP  R1                                                               
*                                                                               
DLST012  BCT   R0,DLST020          GET NEXT DISPLAY ADDRESS                     
         LA    R0,COLNUM                                                        
         LR    R3,R8                                                            
         LA    RF,LUILLEN                                                       
         AR    R3,RF                                                            
         LA    RF,IDUBBARH                                                      
         CR    R3,RF                                                            
         BNL   DLSTOK              EXIT AT END OF SCREEN                        
         LR    R8,R3                                                            
         B     DLST030                                                          
DLST020  LA    RF,LUIFLEN                                                       
         AR    R3,RF                                                            
DLST030  LA    R4,LUTLEN(R4)       GET NEXT LUTAB ENTRY                         
         B     DLST010                                                          
*                                                                               
DLSTNO   B     NO                                                               
DLSTOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE REPEATED ENTRIES IN LUTAB                         *         
***********************************************************************         
         SPACE 1                                                                
DELRPT   NTR1                                                                   
         USING LUTABD,R4                                                        
         LA    R4,LUTAB                                                         
DRPT010  L     RF,LUTPNTR          SEARCH DOWN TABLE                            
         LA    RE,LUTLEN                                                        
         SR    RF,RE                                                            
         CR    R4,RF                                                            
         BNL   DRPTX                                                            
*                                  MATCH LUIDS                                  
         CLC   LUTOLD+LUTLEN,LUTOLD                                             
         BNE   DRPT020                                                          
         CLC   LUTNEW+LUTLEN,LUTNEW                                             
         BNE   DRPT020                                                          
         XC    0(LUTLEN,R4),0(R4)                                               
         MVI   0(R4),DELFLAG       FLAG DELETED                                 
         SR    RF,RF                                                            
         LH    RF,LUTDEL                                                        
         LA    RF,1(RF)                                                         
         STH   RF,LUTDEL                                                        
*                                                                               
DRPT020  LA    R4,LUTLEN(R4)                                                    
         B     DRPT010                                                          
*                                                                               
DRPTX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE LUID FIELDS ON SCREEN AND BUILD LUTAB           *         
* R1 POINTS TO START POINT IN TABLE                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING LUTABD,R4                                                        
VALLUT   NTR1                                                                   
         LA    R4,LUTAB                                                         
         LA    R3,IDULUOAH         ADDRESS FIRST DISPLAY FIELD                  
         LR    R8,R3                                                            
         LA    R0,COLNUM           DISPLAY COLUMN #                             
VLUT010  CLI   0(R4),DELFLAG                                                    
         BE    VLUT030             BYPASS IF FLAG AS DELETED                    
         LR    R1,R3                                                            
         GOTO1 AFVAL                                                            
         BE    VLUT020                                                          
         B     VLUT050                                                          
         CLI   FVILEN,0            CHECK IF FIELD CLEARED                       
         BNE   VLUT030                                                          
         L     RF,LUTPNTR                                                       
         CR    R4,RF                                                            
         BNL   VLUT030             IGNORE IF PAST END OF LIST                   
         XC    0(LUTLEN,R4),0(R4)    ELSE DELETE ENTRY                          
         MVI   0(R4),DELFLAG                                                    
         SR    RF,RF                                                            
         LH    RF,LUTDEL                                                        
         LA    RF,1(RF)                                                         
         STH   RF,LUTDEL                                                        
         B     VLUT030                                                          
*                                                                               
VLUT020  EQU   *                                                                
         BAS   RE,VALLUID          VALIDATE OLD LUID                            
         BNE   VLUTNO                EXIT IF INVALID                            
         MVC   OLDLUID,FVIFLD                                                   
         LA    R1,LUINOFF                                                       
         AR    R1,R3                                                            
         GOTO1 AFVAL                                                            
         BNE   EMIF                                                             
         BAS   RE,VALLUID          VALIDATE NEW LUID                            
         BNE   VLUTNO                EXIT IF INVALID                            
         MVC   NEWLUID,FVIFLD                                                   
         XC    0(LUTLEN,R4),0(R4)                                               
         MVC   LUTOLD,OLDLUID                                                   
         MVC   LUTNEW,NEWLUID                                                   
         BAS   RE,CHKRPT           CHECK ENTRY NOT ALREADY IN LIST              
         BNE   EDIF                                                             
*                                                                               
VLUT030  L     RF,LUTPNTR                                                       
         CR    R4,RF                                                            
         BL    VLUT040             CHECK END OF LUTAB                           
         OC    0(LUTLEN,R4),0(R4)                                               
         BZ    VLUT050               AND NO NEW ENTRY LOADED                    
         BAS   RE,BUMPLTP          BUMP LUTAB POINTER/COUNT                     
         BNE   VLUTNO                EXIT IF INVALID                            
VLUT040  LA    R4,LUTLEN(R4)       NEXT LUTAB POINTER                           
*                                                                               
VLUT050  BCT   R0,VLUT060          GET NEXT SCREEN FIELD ADDRESS                
         LA    R0,COLNUM                                                        
         LR    R3,R8                                                            
         LA    RF,LUILLEN                                                       
         AR    R3,RF                                                            
         LA    RF,IDUBBARH                                                      
         CR    R3,RF                                                            
         BNL   VLUTOK              EXIT AT END OF SCREEN                        
         LR    R8,R3                                                            
         B     VLUT010                                                          
VLUT060  LA    RF,LUIFLEN                                                       
         AR    R3,RF                                                            
         B     VLUT010                                                          
*                                                                               
VLUTNO   B     NO                  ENTRY INVALID                                
*                                                                               
VLUTOK   B     YES                 LIST OK                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE LUID FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VALLUID  NTR1                                                                   
         B     VLUIOK                                                           
         MVC   KEYSAVE,IOKEY                                                    
         L     R2,AIOAREA2                                                      
         DROP  R2                                                               
         USING CTTREC,R2                                                        
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,CTTKTYPQ                                                 
         MVC   CTTKTID,FVIFLD                                                   
         MVC   IOKEY(L'CTTKEY),CTTKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         MVC   IOKEY,KEYSAVE                                                    
         DROP  R2                                                               
         USING CTIREC,R2                                                        
         B     VLUIOK                                                           
*                                                                               
VLUINO   B     NO                  INVALID ENTRY                                
VLUIOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK BYTE AT R1 IS ALPHA NUMERIC                        *         
***********************************************************************         
         SPACE 1                                                                
ALPNUM   NTR1                                                                   
         CLI   0(R1),C'A'                                                       
         BL    ANUM010                                                          
         CLI   0(R1),C'Z'                                                       
         BNH   ANUMOK                                                           
*                                                                               
ANUM010  CLI   0(R1),C'0'                                                       
         BL    ANUMNO                                                           
         CLI   0(R1),C'9'                                                       
         BH    ANUMNO                                                           
         B     ANUMOK                                                           
*                                                                               
ANUMNO   B     NO                  NOT ALPHA NUMERIC                            
*                                                                               
ANUMOK   B     YES                 IS ALPHA NUMERIC                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK FOR REPEATED ENTRY IN LUTAB                        *         
***********************************************************************         
         SPACE 1                                                                
CHKRPT   NTR1                                                                   
         USING LUTABD,R4                                                        
         LA    R8,LUTAB                                                         
CRPT010  L     RF,LUTPNTR                                                       
         CR    R8,RF                                                            
         BNL   CRPTOK                                                           
         CR    R8,R4                                                            
         BE    CRPT020                                                          
         CLI   0(R4),DELFLAG                                                    
         BE    CRPT020                                                          
         CLC   LUTOLD,LUTOLD-LUTABD(R8)                                         
         BE    CRPTNO                                                           
         CLC   LUTNEW,LUTNEW-LUTABD(R8)                                         
         BE    CRPTNO                                                           
         B     CRPT020                                                          
*                                                                               
CRPT020  LA    R8,LUTLEN(R8)                                                    
         B     CRPT010                                                          
*                                                                               
CRPTNO   B     NO                                                               
*                                                                               
CRPTOK   B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUMP LUTAB POINTERS/COUNTS FOR NEW ENTRY                 *         
***********************************************************************         
         SPACE 1                                                                
BUMPLTP  NTR1                                                                   
         L     R1,LUTPNTR                                                       
         LA    RF,LUTLEN                                                        
         AR    R1,RF                                                            
         ST    R1,LUTPNTR                                                       
         SR    RF,RF                                                            
         LH    RF,LUTCNT                                                        
         LA    RF,1(RF)                                                         
         STH   RF,LUTCNT                                                        
         BAS   RE,CHKCNT                                                        
         BNE   BPLPNO                                                           
         B     BPLPOK                                                           
*                                                                               
BPLPNO   B     NO                                                               
*                                                                               
BPLPOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK # ELEMENTS IN LIST WITHIN MAXIMUM                  *         
***********************************************************************         
         SPACE 1                                                                
CHKCNT   NTR1                                                                   
         SR    RF,RF                                                            
         LH    RF,LUTCNT                                                        
         SH    RF,LUTDEL                                                        
         LA    R1,LUTMAX                                                        
         CR    RF,R1                                                            
         BNH   CCNTOK                                                           
         B     ETOO                                                             
*                                                                               
CCNTOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SORT LUTAB                                               *         
***********************************************************************         
         SPACE 1                                                                
SRTLUT   NTR1                                                                   
         LA    R4,LUTAB                                                         
         SR    R0,R0                                                            
         LH    R0,LUTCNT                                                        
         GOTO1 VXSORT,APPARM,(X'00',(R4)),(R0),LUTLEN,LUTLEN,0                  
         XIT1                                                                   
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE INITIALISATION OF TWA                             *         
***********************************************************************         
         SPACE 1                                                                
SETTWA   EQU   *                                                                
         XC    LUTCNT,LUTCNT       INITIALISE LUID TABLE                        
         XC    LUTDEL,LUTDEL                                                    
         LA    RF,LUTLEN*LUTMAX                                                 
         XCEF  LUTAB,(RF)                                                       
         B     SETTWAX                                                          
*                                                                               
SETTWAX  B     EXIT                                                             
         EJECT                                                                  
*                                  GETTXT MESSAGE # ERROR EXITS                 
EIIF     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
EFTL     MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     NO                  INPUT FIELD TOO LONG                         
EFNN     MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     NO                  INPUT FIELD NOT NUMERIC                      
EFTS     MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     NO                  INPUT FIELD TOO SHORT                        
EFNH     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
EMIF     MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     NO                  MISSING FIELD                                
EIIO     MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     NO                  I/O ERROR                                    
ERNF     MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     NO                  RECORD NOT FOUND                             
EDIF     MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     NO                  DUPLICATE                                    
ERAE     MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     NO                  ALREADY EXISTS                               
EIRT     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  ?? ALREADY EXISTS                            
ERTB     MVC   FVMSGNO,=AL2(CE#RECTB)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  RECORD TO BIG                                
EFTB     MVC   FVMSGNO,=AL2(CE#FVMAX)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FIELD VALUE > MAX                            
EATO     MVC   FVMSGNO,=AL2(CE#ADTOU)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  ADV TIMEOUT FIELD ERROR                      
ENOW     MVC   FVMSGNO,=AL2(CE#NONOW)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  CANT PRINT IN NOW MODE                       
ETOO     MVC   FVMSGNO,=AL2(FVFTOOM)                                            
         B     NO                  TOO MANY INPUT FIELDS                        
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
         SPACE 1                                                                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         SPACE 1                                                                
CTFILE   DC    C'CTFILE '                                                       
SPACES   DC    80C' '                                                           
FFILL    DC    80X'FF'                                                          
CAPFILL  DC    (L'APWORK)X'40'                                                  
DELFLAG  EQU   X'FF'                                                            
LUIFLEN  EQU   IDULUOBH-IDULUOAH                                                
LUINOFF  EQU   IDULUNAH-IDULUOAH                                                
LUILLEN  EQU   IDULUODH-IDULUOAH                                                
LUITLEN  EQU   IDUBBARH-IDULUOAH                                                
COLNUM   EQU   LUILLEN/LUIFLEN                                                  
LINNUM   EQU   LUITLEN/LUILLEN                                                  
LUILNUM  EQU   COLNUM*LINNUM                                                    
LUTSLEN  EQU   LINNUM*LUTLEN                                                    
         EJECT                                                                  
***********************************************************************         
* FOLLOWING TABLES/SUBROUTINES ARE ONLY ADDRESSABLE VIA =A(.....)     *         
***********************************************************************         
         SPACE 1                                                                
         DROP  RB,RA,R8                                                         
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO READ ACCESS RECORD INTO IOAREA3                          *         
*                                                                     *         
* NTRY - AGAID=AGENCY ALPHA ID                                        *         
* EXIT - IDCTRY=COUNTRY CODE                                          *         
***********************************************************************         
         SPACE 1                                                                
GETACC   CSECT                                                                  
         NTR1  LABEL=NO                                                         
         LR    RB,RF                                                            
         USING GETACC,RB                                                        
         L     RE,4(RD)            PICK UP NEW BACKWARD POINTER                 
         MVC   0(4,RE),=C'+GAC'    INSERT NAME                                  
*                                                                               
         MVI   IDCTRY,0                                                         
         MVC   KEYSAVE,IOKEY                                                    
         L     R1,AIOAREA3                                                      
         USING CT5REC,R1                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGAID                                                   
         MVC   IOKEY(L'CT5KEY),CT5KEY                                           
         DROP  R1                                                               
         GOTO1 AIO,IOREAD+IOCONFIL+IO3                                          
         MVC   IOKEY,KEYSAVE                                                    
         BNL   *+6                                                              
         DC    H'00'                                                            
         BH    GETACCNO                                                         
         L     R1,AIOAREA3                                                      
         LA    R1,CT5DATA-CT5REC(R1)                                            
         SR    RF,RF                                                            
*                                  SAVE AGENCY COUNTRY CODE                     
         USING CTAGDD,R1                                                        
GETACC4  CLI   CTAGDEL,0                                                        
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   CTAGDEL,CTAGDELQ                                                 
         BE    *+14                                                             
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     GETACC4                                                          
         CLI   CTAGDLEN,CTAGDLNQ                                                
         BNH   GETACCOK                                                         
         MVC   IDCTRY,CTAGDCTY                                                  
         DROP  R1                                                               
         B     GETACCOK                                                         
*                                                                               
*                                                                               
GETACCOK SR    RC,RC               RETURN CC EQUAL                              
GETACCNO LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1  ,                                                                
         LTORG                                                                  
         SPACE 2                                                                
         EJECT                                                                  
* DSECT TO COVER TEMP W/S                                                       
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
* FAGETTXTD                                                                     
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENC1D                                                       
         ORG                                                                    
         EJECT                                                                  
*                                  WORKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
SAVSYS   DS    XL1                 LAST SYSTEM DISPLAYED                        
SAVKEY   DS    XL(L'CTIKEY)        SAVE LAST RECORD KEY READ FOR COPY           
SAVPROM  DS    XL1                 SAVE PROMPTED STATE                          
*                                                                               
*                                  LUTAB INFO                                   
LUTCNT   DS    H                   # ENTRIES IN LIST                            
LUTDEL   DS    H                   # DELETED ENTRIES IN LIST                    
LUTPNTR  DS    A                   A(END OF LIST)                               
LUTAB    DS    XL(LUTLEN*LUTMAX)   TABLE OF LUS FOR CONVERSION                  
LUTMAX   EQU   48                  MAXIMUM NUMBER OF LUTAB ENTRIES              
*                                                                               
SAVCLRL  EQU   *-SAVOVER                                                        
*                                  LUTAB ENTRY DSECT                            
LUTABD   DSECT                                                                  
LUTOLD   DS    CL(L'CTTKTID)       OLD LUID                                     
LUTNEW   DS    CL(L'CTTKTID)       NEW LUID                                     
LUTLEN   EQU   *-LUTABD                                                         
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
DUB      DS    D                                                                
RETURN   DS    F                                                                
ASYSEL   DS    A                                                                
ASE      DS    A                                                                
APGM     DS    A                                                                
VXSORT   DS    A                   A(XSORT)                                     
VDLFLD   DS    V                                                                
WORK     DS    CL(L'APWORK)                                                     
*                                                                               
OKFLAG   DS    CL1                                                              
AGAID    DS    CL2                                                              
IDCTRY   DS    CL1                                                              
CNVIDFLG DS    CL1                                                              
OLDLUID  DS    CL(L'CTTKTID)                                                    
NEWLUID  DS    CL(L'CTTKTID)                                                    
*                                                                               
DELKEY   DS    CL(L'IOKEY)                                                      
KEYSAVE  DS    CL(L'IOKEY)                                                      
*                                                                               
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'066CTGEN0A   05/01/02'                                      
         END                                                                    
