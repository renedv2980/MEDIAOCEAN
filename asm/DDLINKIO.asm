*          DATA SET DDLINKIO   AT LEVEL 013 AS OF 09/10/13                      
*PHASE T00AE2A                                                                  
*INCLUDE WRKIO                                                                  
LINKIO   TITLE '- APPLICATION INTERFACE TO DDLINK'                              
LINKIO   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,**LKIO**,CLEAR=YES,RR=RE                                   
         USING WORKD,RC                                                         
         LARL  R8,GLOBALS                                                       
         USING GLOBALS,R8          R8=A(GLOBAL LITERALS)                        
         ST    RE,RELO                                                          
         LR    R2,R1                                                            
         USING PARMD,R2            R2=A(PARAMETER LIST)                         
         MVC   ACTION,PARMACTN     EXTRACT CALLED ACTION                        
         MVI   PARMCOMP,LIOCGOOD   SET GOOD COMPLETION CODE                     
         L     R3,PARMABLK                                                      
         USING LIOBD,R3            R3=A(LINKIO CONTROL BLOCK)                   
         L     R4,LIOBACOM                                                      
         USING COMFACSD,R4         R4=A(COMFACS)                                
                                                                                
         CLI   ACTION,LIOAINI      TEST ONLINE INITIALIZATION                   
         JE    IOINI                                                            
         CLI   ACTION,LIOAOFI      TEST DOWNLOAD INITIALIZATION                 
         JE    IOINO                                                            
         CLI   ACTION,LIOAUPI      TEST UPLOAD INITIALIZATION                   
         JE    IOUPI                                                            
         CLI   ACTION,LIOAGET      TEST GET NEXT INPUT RECORD                   
         JE    IOGET                                                            
         CLI   ACTION,LIOAPUT      TEST PUT DATA TO RECORD                      
         JE    IOPUT                                                            
         CLI   ACTION,LIOAUPD      TEST UPDATE PREVIOUSLY PUT DATA              
         JE    IOPUT                                                            
         CLI   ACTION,LIOACLO      TEST CLOSE FILE & BUILD RETURN               
         JE    IOCLO                                                            
         CLI   ACTION,LIOABLD      TEST BUILD INPUT FIELD                       
         JE    IOBLD                                                            
         DC    H'0'                                                             
                                                                                
         USING FAWSSVRD,WORK                                                    
         USING GLVXFRSY,LIOBGXFC                                                
         EJECT                                                                  
***********************************************************************         
* Online initialization when running in an invoked program            *         
*                                                                     *         
* Test application invoked from DDLINK via GLOBBER transfer control   *         
* call.  Establish data storage method (worker file/WSSVR buffer),    *         
* Open the worker file or restore the WSSVR buffer and establish the  *         
* PC application version number                                       *         
***********************************************************************         
                                                                                
IOINI    LA    R0,LIOBVIS          CLEAR BLOCK WORK AREA                        
         LHI   R1,LIOBVISL+L'LIOBWORK                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RE,VWRKIO                                                        
         A     RE,RELO                                                          
         STCM  RE,15,LIOBAWIO      SET RELOCATED WRKIO ADDRESS                  
         GOTOR CGLOBBER,DMCB,GLGET,GLVXFRSY,GLVXLENQ,GLVXCTL                    
         CLI   8(R1),GLEGNF                                                     
         JE    NOXFER                                                           
                                                                                
         GOTOR (RF),(R1),GLGET,WRKWKEY,L'WRKWKEY,GLVDLUWF                       
         CLI   8(R1),0                                                          
         BNE   *+12                                                             
         MVI   LIOBFLG1,LIOBFWRK   SET WORKER FILE PASSED                       
         B     IOINI02                                                          
                                                                                
         GOTOR (RF),(R1),GLGET,LIOBTOKN,L'LIOBTOKN,GLVDLUST                     
         CLI   8(R1),GLEGNF                                                     
         JE    NODDLK                                                           
         MVI   LIOBFLG1,LIOBFWSS   SET WSSVR BUFFER PASSED                      
                                                                                
IOINI02  GOTOR (RF),(R1),GLGET,LIOBPCV1,L'LIOBPCV1,GLVDLUV#                     
         GOTOR (RF),(R1),GLCLR     CLEAR ALL THE GLOBALS                        
                                                                                
         TM    LIOBFLG1,LIOBFWRK   TEST WORKER FILE PASSED                      
         BZ    IOINI04                                                          
         MVC   WRKIABUF,LIOBABUF   OPEN THE WORKFER FILE                        
         MVC   WRKIAREC,LIOBAREC                                                
         MVC   WRKIACOM,LIOBACOM                                                
         MVI   WRKIFTYP,WRKIFTWF                                                
         MVI   WRKIACTN,WRKIAOPN                                                
         GOTOR LIOBAWIO,WRKIOB                                                  
         BE    *+6                                                              
         DC    H'0'                CAN'T OPEN THE INPUT WORKER FILE             
                                                                                
         GOTOR (RF),(R1)           GET FIRST WORKER FILE RECORD                 
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,WRKIABUF         AND EXTRACT NUMBER OF RECORDS                
         MVC   LIOBWREC,W_RECS-W_INDEX(RE)                                      
         J     EXITCC                                                           
                                                                                
IOINI04  XC    FAWSSVRD(FAWSSVRL),FAWSSVRD                                      
         MVC   FAWSTOKN,LIOBTVAL                                                
         MVI   FAWSACTN,FAWSURST   SET TO RESTORE RECORD BUFFER                 
         MVC   FAWSADR,LIOBABUF    SET RESTORE ADDRESS                          
         GOTOR CWSSVR,FAWSSVRD     RESTORE RECORD BUFFER                        
         CLI   FAWSRTN,0                                                        
         JE    EXITCC                                                           
         DC    H'0'                DIE IF CAN'T RESTORE BUFFER                  
         EJECT                                                                  
***********************************************************************         
* Offline initialization for download                                 *         
***********************************************************************         
                                                                                
IOINO    LA    R0,LIOBVIS          CLEAR BLOCK WORK AREA                        
         LHI   R1,LIOBVISL+L'LIOBWORK                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   LIOBFLG1,LIOBFOFF   SET RUNNING OFFLINE                          
                                                                                
         GOTOR SETADR              SET LIOBNXTA/LIOBENDA VALUES                 
                                                                                
         J     EXITCC                                                           
         EJECT                                                                  
***********************************************************************         
* Online initialization for upload when running in link program       *         
***********************************************************************         
                                                                                
IOUPI    MVC   WORK(L'LIOBIND2),LIOBIND2                                        
         LA    R0,LIOBVIS          CLEAR BLOCK WORK AREA                        
         LHI   R1,LIOBVISL+L'LIOBWORK                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   LIOBIND2,WORK                                                    
                                                                                
         L     RE,VWRKIO                                                        
         A     RE,RELO                                                          
         STCM  RE,15,LIOBAWIO      SET RELOCATED WRKIO ADDRESS                  
                                                                                
         MVI   LIOBFLG1,LIOBFWRK   SET WORKER FILE PASSED                       
         MVI   LIOBFLG2,LIOBFUPL   SET READING UPLOAD FILE                      
                                                                                
         L     R1,LIOBAWRK         COPY DDLINK WRKIO CONTROL BLOCK              
         MVC   WRKIOB(WRKIOBL),0(R1)                                            
         MVC   LIOBABUF,WRKIABUF   AND SET LIOB ADDRESSES                       
         MVC   LIOBAREC,WRKIAREC                                                
         MVC   LIOBACOM,WRKIACOM                                                
         L     R4,LIOBACOM                                                      
                                                                                
         L     R1,WRKIAREC                                                      
         LHI   R0,1                                                             
         STCM  R0,15,0(R1)         READ FIRST RECORD                            
         MVC   4(4,R1),RECS                                                     
         XC    8(4,R1),8(R1)                                                    
         GOTOR CDATAMGR,WRKIPARM,WRR                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,WRKIABUF         AND EXTRACT NUMBER OF RECORDS                
         MVC   LIOBWREC,W_RECS-W_INDEX(R1)                                      
                                                                                
         TM    LIOBIND2,LIOBIRUP   TEST RUNNER UPLOAD                           
         JZ    EXITCC                                                           
         LHI   R0,1                                                             
         STCM  R0,15,LIOBCREC      YES - START AT RECORD#2                      
         J     EXITCC                                                           
         EJECT                                                                  
***********************************************************************         
* Get next input record - write/update last record if required.  At   *         
* end of file or buffer issue transfer control back to calling        *         
* application and return to caller with condition code of not equal   *         
***********************************************************************         
                                                                                
IOGET    TM    LIOBFLG2,LIOBFEOF   TEST END OF FILE LAST TIME                   
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         NI    LIOBFLG2,FF-(LIOBFEOR)                                           
         TM    LIOBFLG2,LIOBFUPL   TEST UPLOADING                               
         BZ    *+10                                                             
         XC    LIOBAMAP,LIOBAMAP   YES - CLEAR JUST IN CASE IT IS SET           
                                                                                
         TM    LIOBINDS,LIOBINXT   TEST FORCE NEXT INPUT RECORD                 
         BNZ   IOGET04                                                          
                                                                                
         TM    LIOBFLG2,LIOBFRBK+LIOBFSBK+LIOBFDBK                              
         BZ    IOGET04                                                          
                                                                                
         ICM   R5,15,LIOBCURA      POINT TO CURRENT INPUT ELEMENT               
         TM    LIOBFLG2,LIOBFDBK   TEST DATA MAP BREAK                          
         BZ    IOGET02                                                          
         NI    LIOBFLG2,FF-(LIOBFDBK)                                           
         XC    LIOBDTA#,LIOBDTA#                                                
         B     IOGET56                                                          
                                                                                
IOGET02  ICM   R7,15,LIOBMAPA      R7=A(DATA MAP TABLE ENTRY)                   
         TM    LIOBFLG2,LIOBFRBK   TEST RECORD MAP BREAK                        
         BZ    *+12                                                             
         NI    LIOBFLG2,FF-(LIOBFRBK)                                           
         B     IOGET30                                                          
                                                                                
         NI    LIOBFLG2,FF-(LIOBFSBK)                                           
         CLM   R5,15,LIOBENDI      TEST AT END OF CURRENT RECORD                
         BNE   IOGET28                                                          
                                                                                
IOGET04  NI    LIOBINDS,FF-(LIOBINXT)                                           
         NI    LIOBFLG2,FF-(LIOBFRBK+LIOBFSBK+LIOBFDBK)                         
         TM    LIOBFLG1,LIOBFWRP+LIOBFPUT                                       
         BZ    IOGET06                                                          
         NI    LIOBFLG1,FF-(LIOBFWRP+LIOBFPUT)                                  
         TM    LIOBFLG1,LIOBFWRK   TEST WORKER FILE PASSED                      
         BZ    IOGET06                                                          
         MVI   WRKIACTN,WRKIAPUT   PUT BACK LAST WORKER FILE RECORD             
         GOTOR LIOBAWIO,WRKIOB                                                  
         BE    IOGET06                                                          
         DC    H'0'                                                             
                                                                                
IOGET06  NI    LIOBFLG1,FF-(LIOBFRME)                                           
         TM    LIOBFLG1,LIOBFWRK+LIOBFWSS                                       
         BNZ   *+6                                                              
         DC    H'0'                CALLER DIDN'T INITIALIZE                     
         NI    LIOBFLG3,FF-(LIOBFERR)                                           
         TM    LIOBFLG1,LIOBFWRK                                                
         BZ    IOGET08                                                          
         ICM   R0,15,LIOBCREC      BUMP INPUT RECORD NUMBER                     
         AHI   R0,1                                                             
         CLM   R0,15,LIOBWREC      TEST ALL RECORDS PROCESSED                   
         BH    IOGET58                                                          
         STCM  R0,15,LIOBCREC      SET CURRENT INPUT RECORD NUMBER              
         L     R5,WRKIAREC                                                      
         MVC   0(4,R5),LIOBCREC    READ NEXT INPUT RECORD                       
         MVC   4(4,R5),RECS                                                     
         XC    8(4,R5),8(R5)                                                    
         GOTOR CDATAMGR,WRKIPARM,WRR                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    LIOBRMEA,LIOBRMEA   CLEAR A(RETURN MAP ELEMENT)                  
         B     IOGET14                                                          
                                                                                
IOGET08  ICM   R5,15,LIOBACUR      BUMP TO FIRST/NEXT BUFFER RECORD             
         BNZ   IOGET10                                                          
         L     R5,LIOBABUF         POINT TO FIRST RECORD                        
         B     IOGET12                                                          
                                                                                
IOGET10  SR    R0,R0                                                            
         ICM   R0,3,0(R5)          PICK UP LENGTH OF CURRENT RECORD             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R5,R0               POINT TO NEXT RECORD                         
         OC    0(2,R5),0(R5)       TEST AT END OF INPUT BUFFER                  
         BZ    IOGET58             NO                                           
                                                                                
IOGET12  STCM  R5,15,LIOBACUR                                                   
         STCM  R5,15,LIOBAREC      PASS RECORD ADDRESS TO CALLER                
         SR    R0,R0                                                            
                                                                                
IOGET14  ICM   R0,3,0(R5)          GET LENGTH OF THIS RECORD                    
         AR    R0,R5                                                            
         STCM  R0,15,LIOBENDA      SET A(END OF CURRENT RECORD)                 
         AHI   R5,4                                                             
         USING LQ_D,R5             R5=A(CURRENT ELEMENT)                        
                                                                                
         XC    LIOBMAP#,LIOBMAP#   CLEAR RECORD MAP NUMBER                      
         XC    LIOBSUB#,LIOBSUB#   CLEAR SUB-RECORD MAP NUMBER                  
                                                                                
         LA    R1,LQ_D             LOCATE RETURN DATA ELEMENT                   
         SR    R0,R0                                                            
         BASR  RE,0                                                             
         CLI   LQ_EL-LQ_D(R1),0                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   LQ_EL-LQ_D(R1),LQ_RDATQ                                          
         BE    *+12                                                             
         ICM   R0,3,LQ_LN-LQ_D(R1)                                              
         AR    R1,R0                                                            
         BR    RE                                                               
         XC    0(LQ_LN1Q,R1),0(R1) CLEAR THE RETURN ELEMENT                     
         STCM  R1,15,LIOBENDI      SET A(END OF INPUT DATA)                     
         STCM  R1,15,LIOBRECA      SET A(FIRST OUTPUT ELEMENT)                  
         STCM  R1,15,LIOBNXTA      SET A(NEXT OUTPUT ELEMENT)                   
                                                                                
IOGET16  STCM  R5,15,LIOBCURA      SAVE CURRENT ELEMENT ADDRESS                 
         CLM   R5,15,LIOBENDI      TEST END OF INPUT RECORD                     
         BL    IOGET18             NO - PROCESS ELEMENT DATA                    
         BE    *+6                                                              
         DC    H'0'                RECORD IS BAD                                
                                                                                
         OI    LIOBFLG2,LIOBFEOR   SET END OF RECORD ENCOUNTERED                
         OC    LIOBSUB#,LIOBSUB#   TEST SUB-RECORD MAP CODE SET                 
         JZ    EXITE                                                            
         TM    LIOBINDS,LIOBISUB   TEST CALLER WANTS SUB-RECORD BREAK           
         JZ    EXITE                                                            
         OI    LIOBFLG2,LIOBFSBK   YES - RETURN WITH BREAK SET                  
         J     EXITE               (NOTE: CC=EQUAL NOT LOW HERE)                
                                                                                
IOGET18  CLI   LQ_EL,LQ_IMAPQ      TEST MAP CODE ELEMENT                        
         BNE   *+14                                                             
         MVC   LIOBMAPN,LQ_IMAPN                                                
         B     IOGET20                                                          
                                                                                
         CLI   LQ_EL,LQ_CTRLQ      TEST CONTROL ELEMENT                         
         BNE   IOGET36                                                          
         OI    LIOBFLG1,LIOBFVAL   SET REQUEST VALIDATION CALL                  
         MVC   LIOBMAPN,LQ_CVAL+(LP_QMAPN-LP_CVAL)                              
                                                                                
IOGET20  XC    LIOBDTA#,LIOBDTA#   CLEAR DATA MAP NUMBER                        
                                                                                
         ICM   R1,15,LIOBAMAP                                                   
         BZ    IOGET56                                                          
         USING LIORD,R1            R1=A(RECORD MAP TABLE)                       
IOGET22  CLC   LIORIMAP,BZEROS     TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                THIS RECORD IS NOT DEFINED                   
         CLC   LIORIMAP,LIOBMAPN   MATCH RECORD CODE TO TABLE                   
         BE    *+12                                                             
         AHI   R1,LIORL            BUMP TO NEXT TABLE ENTRY                     
         B     IOGET22                                                          
                                                                                
         SR    R7,R7                                                            
         ICM   R7,3,LIORDISP       R7=A(DISPLACEMENT TO DATA MAP)               
         BZ    *+8                                                              
         A     R7,LIOBAMAP         R7=A(DATA MAP TABLE)                         
         STCM  R7,15,LIOBMAPA      SET A(DATA MAP TABLE) OR ZERO                
                                                                                
         MVC   LIOBOMAP,LIOROMAP   SET RETURN MAP ELEMENT CODE                  
         DROP  R1                                                               
                                                                                
         TM    LIOBFLG1,LIOBFVAL   TEST REQUEST VALIDATION CALL                 
         BNZ   IOGET24                                                          
         TM    LIOBFLG2,LIOBFUPL   OR UPLOAD FILE                               
         BNZ   IOGET24                                                          
         TM    LIOBFLG1,LIOBFRME   OR ALREADY ADDED RETURN MAP ELEMENT          
         BNZ   IOGET24                                                          
         TM    LIOBINDS,LIOBINRM   TEST AUTO MAP RETURN INHIBITED               
         BNZ   IOGET24                                                          
         LR    R0,R5                                                            
         GOTOR PUTRME,0            BUILD AND ADD RETURN MAP ELEMENT             
         LR    R5,R0                                                            
         OI    LIOBFLG1,LIOBFWRP   SET WRITE PENDING                            
                                                                                
IOGET24  OC    LIOBMAP#,LIOBMAP#   TEST FIRST RECORD MAP ELEMENT                
         BNZ   IOGET26                                                          
         MVC   LIOBMAP#,LIOBMAPN   SET INPUT RECORD MAP NUMBER                  
         TM    LIOBINDS,LIOBIRET   TEST CALLER WANTS CONTROL NOW                
         BZ    IOGET30                                                          
         OI    LIOBFLG2,LIOBFRBK   SET TO RETURN AFTER BREAK                    
         J     EXITL                                                            
                                                                                
IOGET26  OC    LIOBSUB#,LIOBSUB#   TEST FIRST SUB-RECORD MAP ELEMENT            
         BZ    IOGET28                                                          
         TM    LIOBINDS,LIOBISUB   TEST CALLER WANTS SUB-RECORD BREAK           
         BZ    IOGET28                                                          
         OI    LIOBFLG2,LIOBFSBK   YES - RETURN WITH BREAK SET                  
         J     EXITE               (NOTE: CC=EQUAL NOT LOW HERE)                
                                                                                
IOGET28  MVC   LIOBSUB#,LIOBMAPN   SET SUB-RECORD MAP NUMBER                    
                                                                                
         USING LIODD,R7                                                         
IOGET30  LTR   R7,R7               TEST NO DATA IN THIS RECORD                  
         BZ    IOGET56                                                          
         CLC   LIODDMAP,BZEROS     TEST END OF DATA MAP TABLE                   
         BE    IOGET56                                                          
                                                                                
         TM    LIODIND1,LIODIIGN   TEST IGNORE THIS MAP CODE                    
         BNZ   IOGET34                                                          
                                                                                
         SR    R1,R1                                                            
         ICM   R1,1,LIODBLKN                                                    
         BNZ   *+6                                                              
         DC    H'0'                DATA BLOCK NUMBER NOT SUPPLIED               
         CHI   R1,LIOBSMAX                                                      
         BNH   *+6                                                              
         DC    H'0'                INVALID DATA BLOCK NUMBER                    
         SLL   R1,2                                                             
         LA    R1,LIOBASB1-L'LIOBASB1(R1)                                       
         ICM   R1,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                AND BLOCK ADDRESS MUST BE SET                
         SR    R0,R0                                                            
         ICM   R0,3,LIODDISP                                                    
         AR    R1,R0               R1=A(OUTPUT DATA FIELD)                      
                                                                                
         TM    LIODIND1,LIODINDX   TEST POINTS TO A DATA INDEX                  
         BZ    *+14                                                             
         XC    0(4,R1),0(R1)       YES - CLEAR INDEX                            
         B     IOGET34                                                          
                                                                                
         TM    LIODIND1,LIODITFH   TEST POINTS TO TWA FIELD HEADER              
         BNZ   IOGET32                                                          
         SR    RE,RE                                                            
         ICM   RE,1,LIODDLEN                                                    
         BNZ   *+6                                                              
         DC    H'0'                DATA LENGTH MUST BE SET                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     IOGET34                                                          
         XC    0(0,R1),0(R1)       CLEAR DATA FIELD                             
                                                                                
         USING FLDHDRD,R1                                                       
IOGET32  GOTOR TSTINP              TEST INPUT THE SAME AS LAST TIME             
         BE    IOGET34             YES - LEAVE FIELD ALONE                      
         TM    LIOBINDS,LIOBIDCT   TEST OPTION NOT TO CLEAR TWA FIELDS          
         BNZ   IOGET34             YES                                          
         SR    RE,RE               CLEAR TWA FIELD                              
         ICM   RE,1,FLDLEN                                                      
         SHI   RE,FLDDATA-FLDHDRD                                               
         TM    FLDATB,FATBXHDR     TEST EXTENDED FIELD HEADER                   
         BZ    *+8                                                              
         SHI   RE,FLDDATA-FLDHDRD                                               
         SHI   RE,1                                                             
         BNM   *+6                                                              
         DC    H'0'                INVALID TWA FIELD                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    FLDDATA(0),FLDDATA  CLEAR TWA INPUT FIELD                        
         MVI   FLDIIND,0           SET NO INPUT INDICATORS                      
         MVI   FLDILEN,0           SET ZERO INPUT LENGTH                        
                                                                                
IOGET34  AHI   R7,LIODL            BUMP TO NEXT TABLE ENTRY                     
         B     IOGET30                                                          
                                                                                
IOGET36  CLI   LQ_EL,LQ_RQSTQ      TEST INPUT DATA ELEMENT                      
         BNE   IOGET56                                                          
         OC    LIOBAMAP,LIOBAMAP   TEST RECORD MAP PASSED                       
         BZ    IOGET56                                                          
         MVC   LIOBDTA#,LIODDMAP   SET DATA MAP NUMBER                          
         ICM   R7,15,LIOBMAPA      R7=A(CURRENT DATA MAP)                       
         BNZ   IOGET38                                                          
         DC    H'0'                NO INPUT RECORD MAP CODE                     
IOGET38  CLC   LIODDMAP,BZEROS     TEST END OF DATA MAP TABLE                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   LIODDMAP,LQ_DCODE   MATCH INPUT MAP CODE TO TABLE                
         BE    *+12                                                             
         AHI   R7,LIODL            BUMP TO NEXT TABLE ENTRY                     
         B     IOGET38                                                          
                                                                                
         TM    LIODIND1,LIODIIGN   TEST IGNORE THIS MAP CODE                    
         BNZ   IOGET54                                                          
                                                                                
         LLC   R1,LIODBLKN                                                      
         SLL   R1,2                                                             
         LA    R1,LIOBASB1-L'LIOBASB1(R1)                                       
         ICM   R1,15,0(R1)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,LIODDISP                                                    
         AR    R1,R0               R1=A(OUTPUT DATA FIELD)                      
                                                                                
         TM    LIODIND1,LIODINDX   TEST POINTS TO A DATA INDEX                  
         BZ    IOGET40                                                          
         OC    1(3,R1),1(R1)       TEST INDEX ALREADY SET                       
         BNZ   IOGET54                                                          
         MVC   0(L'LQ_TYPE,R1),LQ_TYPE                                          
         LA    R0,LQ_D                                                          
         STCM  R0,B'0111',L'LQ_TYPE(R1)                                         
         B     IOGET54                                                          
                                                                                
IOGET40  TM    LIODIND1,LIODITFH   TEST POINTS TO TWA FIELD HEADER              
         BNZ   IOGET44                                                          
                                                                                
         TM    LIODIND1,LIODISFF   TEST SPACE FILLED FIELD                      
         BZ    IOGET42                                                          
         SR    RE,RE                                                            
         ICM   RE,1,LIODDLEN                                                    
         MVI   0(R1),C' '                                                       
         SHI   RE,1                                                             
         BZ    IOGET42                                                          
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     IOGET42                                                          
         MVC   1(0,R1),0(R1)                                                    
                                                                                
IOGET42  SR    RE,RE                                                            
         ICM   RE,3,LQ_LN                                                       
         SHI   RE,LQ_VALUE-LQ_D                                                 
         CLM   RE,1,LIODDLEN       TEST LONGER THAN ACTUAL DATA FIELD           
         BNH   *+6                                                              
         DC    H'0'                                                             
         SHI   RE,1                                                             
         BM    IOGET54                                                          
         EX    RE,*+8                                                           
         B     IOGET54                                                          
         MVC   0(0,R1),LQ_VALUE    MOVE DATA TO FIELD                           
                                                                                
         USING FLDHDRD,R1                                                       
IOGET44  TM    FLDIIND,FINPVAL     TEST INPUT IS THE SAME AS LAST TIME          
         BNZ   IOGET54             YES - LEAVE IT ALONE                         
         SR    RF,RF                                                            
         ICM   RF,1,FLDLEN                                                      
         SHI   RF,FLDDATA-FLDHDRD                                               
         TM    FLDATB,FATBXHDR     TEST EXTENDED FIELD HEADER                   
         BZ    *+8                                                              
         SHI   RF,FLDDATA-FLDHDRD  RF=LENGTH OF TWA FIELD                       
         SHI   RF,1                                                             
         EX    RF,*+8              CLEAR FIELD (IF LIOBIDCT IS SET IT           
         B     *+10                WON'T HAVE BEEN CLEARED EARLIER)             
         XC    FLDDATA(0),FLDDATA                                               
         AHI   RF,1                                                             
         SR    RE,RE                                                            
         ICM   RE,3,LQ_LN                                                       
         SHI   RE,LQ_VALUE-LQ_D                                                 
         CR    RE,RF                                                            
         BNH   *+6                                                              
         DC    H'0'                DATA TOO LONG FOR TWA FIELD                  
         STC   RE,FLDILEN                                                       
         MVI   FLDIIND,FINPTHIS                                                 
         SHI   RE,1                                                             
         BM    IOGET54                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),LQ_VALUE SET TWA INPUT FIELD                          
         OI    FLDIIND,FINPNUM+FINPALF+FINPHEX                                  
         TM    FLDILEN,1           NOT HEX IF ODD INPUT LENGTH                  
         BZ    *+8                                                              
         NI    FLDIIND,FF-(FINPHEX)                                             
                                                                                
         AHI   RE,1                SET TWA FIELD INPUT INDICATORS               
         LA    RF,FLDDATA                                                       
IOGET46  CLI   0(RF),C'0'                                                       
         BNL   *+8                                                              
         NI    FLDIIND,FF-(FINPNUM)                                             
         TM    FLDATB,FATBLC       TEST LOWER CASE ALLOWED                      
         BZ    IOGET48                                                          
         CLI   0(RF),C'a'                                                       
         BL    IOGET50                                                          
         CLI   0(RF),C'z'                                                       
         BH    IOGET48                                                          
         NI    FLDIIND,FF-(FINPNUM+FINPHEX)                                     
         B     IOGET52                                                          
IOGET48  CLI   0(RF),C'A'                                                       
         BNL   *+12                                                             
IOGET50  NI    FLDIIND,FF-(FINPALF+FINPHEX+FINPNUM)                             
         B     IOGET52                                                          
         CLI   0(RF),C'Z'                                                       
         BNH   *+12                                                             
         NI    FLDIIND,FF-(FINPALF)                                             
         B     IOGET52                                                          
         NI    FLDIIND,FF-(FINPNUM)                                             
         CLI   0(RF),C'F'                                                       
         BNH   IOGET52                                                          
         NI    FLDIIND,FF-(FINPHEX)                                             
IOGET52  TM    FLDIIND,FINPALF+FINPNUM+FINPHEX                                  
         BZ    IOGET54                                                          
         AHI   RF,1                BUMP TO NEXT INPUT CHARACTER                 
         BCT   RE,IOGET46                                                       
                                                                                
IOGET54  TM    LIODIND2,LIODIRET   TEST CALLER WANTS CONTROL NOW                
         BZ    IOGET56                                                          
         OI    LIOBFLG2,LIOBFDBK   SET TO RETURN AFTER BREAK                    
         J     EXITL                                                            
         DROP  R1,R7                                                            
                                                                                
IOGET56  SR    RE,RE               BUMP TO NEXT ELEMENT ON RECORD               
         ICM   RE,3,LQ_LN                                                       
         AR    R5,RE                                                            
         B     IOGET16                                                          
                                                                                
IOGET58  OI    LIOBFLG2,LIOBFEOF   SET END OF FILE ENCOUNTERED                  
         TM    LIOBINDS,LIOBIMLT   TEST MULTIPLE OUTPUT RECORDS                 
         BZ    IOGET60                                                          
         TM    LIOBFLG2,LIOBFUPL   CAN'T BE A DDLINK UPLOAD FILE                
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   WRKIACTN,WRKIAAPP   RE-OPEN WORKER FILE FOR APPEND               
         GOTOR LIOBAWIO,WRKIOB                                                  
         BE    *+6                                                              
         DC    H'0'                CAN'T OPEN THE INPUT WORKER FILE             
         OI    LIOBFLG1,LIOBFOPA   SET FILE IS OPEN FOR APPEND                  
         GOTOR SETADR              INITIALIZE FOR NEXT RECORD PUT               
         J     NOMORE              AND RETURN END OF FILE                       
                                                                                
IOGET60  TM    LIOBFLG1,LIOBFWRK   TEST WORKER FILE INPUT                       
         BZ    IOGET62                                                          
         TM    LIOBFLG2,LIOBFUPL   TEST UPLOAD FILE                             
         JNZ   NOMORE                                                           
         MVI   WRKIACTN,WRKIACLO   CLOSE THE INPUT WORKER FILE                  
         GOTOR LIOBAWIO,WRKIOB                                                  
         BE    IOGET64                                                          
         DC    H'0'                                                             
                                                                                
IOGET62  XC    FAWSSVRD(FAWSSVRL),FAWSSVRD                                      
         MVC   FAWSTOKN,LIOBTVAL   SET TOKEN                                    
         MVC   FAWSADR,LIOBABUF    SET BUFFER ADDRESS                           
         MVC   FAWSLEN,LIOBTLEN    SET BUFFER LENGTH                            
         MVI   FAWSACTN,FAWSUSVE                                                
         GOTOR CWSSVR,FAWSSVRD     SAVE THE RECORD BUFFER                       
         CLI   FAWSRTN,0                                                        
         BE    IOGET64                                                          
         DC    H'0'                DIE IF CAN'T SAVE RECORD BUFFER              
                                                                                
IOGET64  TM    GLVXFLG1,GLV1SEPD   TEST DIALOGUE MODE ENABLED                   
         BZ    *+12                                                             
         MVI   GLVXFLG1,GLV1RETN+GLV1SEPD+GLV1SEPS+GLV1SIDR                     
         B     *+8                                                              
         MVI   GLVXFLG1,GLV1RETN+GLV1SEPS                                       
         GOTOR CGLOBBER,DMCB,GLPUT,GLVXFRSY,GLVXLENQ,GLVXCTL                    
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'0'                                                             
         TM    LIOBINDS,LIOBIMLT   TEST MULTIPLE OUTPUT RECORDS                 
         JNZ   EXITCC              YES - RETURN GOOD CONDITION CODE             
         J     NOMORE              ELSE RETURN NO MORE INPUT                    
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* Routine to check if input string for a twa field is the same as the *         
* current value of the field.  The application must set the validated *         
* bit in the twa field header for this to take effect. If the input   *         
* this time is the same the field will remain unchanged - this is so  *         
* that the application doesn't have to re-validate the data.  Note    *         
* that if the data isn't input this time the field will be cleared    *         
*                                                                     *         
* Ntry:- R1=A(TWA field header)                                       *         
*        R5=A(Map header element)                                     *         
*        R7=A(Map table entry)                                        *         
* Exit:- CC=Equal if input is the same, not equal if different        *         
***********************************************************************         
                                                                                
         USING FLDHDRD,R1                                                       
         USING LIODD,R7                                                         
TSTINP   ST    RE,SAVERE                                                        
         TM    FLDIIND,FINPVAL     TEST CALLER SET PREVIOUSLY VALIDATED         
         BZ    TSTINPN                                                          
                                                                                
         LR    RF,R5                                                            
         USING LQ_D,RF             RF=A(CURRENT INPUT ELEMENT)                  
         SR    RE,RE                                                            
TSTINP02 ICM   RE,3,LQ_LN          BUMP TO NEXT INPUT ELEMENT                   
         AR    RF,RE                                                            
         CLM   RF,15,LIOBENDI      TEST END OF INPUT RECORD                     
         BE    TSTINPN1                                                         
         CLI   LQ_EL,LQ_IMAPQ      TEST START OF NEW RECORD MAP                 
         BE    TSTINPN1                                                         
         CLI   LQ_EL,LQ_RQSTQ      TEST REQUEST DATA ELEMENT                    
         BNE   TSTINP02                                                         
         CLC   LQ_DCODE,LIODDMAP   MATCH DATA MAP CODE                          
         BNE   TSTINP02                                                         
                                                                                
         ICM   RE,3,LQ_LN          FIELD IS INPUT THIS TIME                     
         SHI   RE,LQ_VALUE-LQ_D                                                 
         BNP   TSTINPN                                                          
         CLM   RE,1,FLDILEN        TEST INPUT STRING LENGTH THE SAME            
         BNE   TSTINPN                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8              TEST INPUT STRING THE SAME                   
         BNE   TSTINPN                                                          
         CLC   LQ_VALUE(0),FLDDATA                                              
                                                                                
TSTINPY  L     RE,SAVERE           RETURN CC=EQUAL IF THE SAME                  
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
TSTINPN1 TM    4(R1),X'20'         PREVIOUSLY VALIDATED?                        
         BZ    TSTINPN                                                          
         CLI   5(R1),0             NOTHING INPUT LAST TIME?                     
         BNE   TSTINPN                                                          
         B     TSTINPY                                                          
                                                                                
TSTINPN  L     RE,SAVERE           RETURN CC=NOT EQUAL IF DIFFERENT             
         LTR   RE,RE                                                            
         BR    RE                                                               
         DROP  R1,R7,RF                                                         
         EJECT                                                                  
***********************************************************************         
* Put data to record                                                  *         
***********************************************************************         
                                                                                
IOPUT    LA    R5,ELEM             BUILD PUT ELEMENT IN WORK AREA               
         USING LQ_D,R5                                                          
                                                                                
IOPUTRUN CLI   PARMTYPE,LIOTSRU    TEST ADD RUN PROCESS                         
         BE    *+12                                                             
         CLI   PARMTYPE,LIOTRUN    TEST PUT RUN PROCESS (CHANGE/NEW)            
         BNE   IOPUTERU                                                         
         CLI   ACTION,LIOAPUT      CAN'T USE LIOAUPD FOR RUN PROCESS            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   PARMTYPE,LIOTSRU    TEST ADD RUN PROCESS                         
         BE    IOPUTRU2                                                         
         ICM   RE,15,LIOBRMEA      POINT TO RETURN MAP ELEMENT                  
         BZ    IOPUTRU2                                                         
         CLI   LQ_EL-LQ_D(RE),LQ_RUNXQ                                          
         BE    IOPUTRU2                                                         
         LR    R5,RE                                                            
         CLI   LQ_EL,LQ_DLDDQ      ELSE ENSURE IS CORRECT ELEMENT               
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    LIOBFLG2,LIOBFRUN   SET RUN ELEMENT ADDED                        
         MVC   LIOBSAVE,LQ_DCODE   SAVE CURRENT MAP CODE                        
         OI    LIOBFLG3,LIOBFSAV   SET MAP CODE SAVED                           
         MVI   LQ_EL,LQ_RUNXQ      AND CONVERT IT TO RUN PROCESS                
         MVC   LQ_DCODE,PARMMAP#   SET RUN PROCESS MAP CODE                     
         NI    LIOBFLG1,FF-(LIOBFRME)                                           
         J     EXITCC                                                           
                                                                                
IOPUTRU2 TM    LIOBFLG2,LIOBFRUN   TEST ADDING WITHOUT END RUN                  
         BZ    *+6                                                              
         DC    H'0'                YES - INVALID CALLING SEQUENCE               
         OI    LIOBFLG2,LIOBFRUN   SET RUN ELEMENT ADDED                        
         MVI   LQ_EL,LQ_RUNXQ      ADD A NEW RUN ELEMENT                        
         LHI   R0,LQ_LNCQ                                                       
         STCM  R0,3,LQ_LN                                                       
         MVC   LQ_DCODE,PARMMAP#                                                
         GOTOR PUTDAT,PDMSPLIT     ADD ELEMENT TO RECORD                        
         OI    LIOBFLG1,LIOBFWRP   SET DATA ADDED TO OUTPUT RECORD              
         J     EXITCC                                                           
                                                                                
IOPUTERU CLI   PARMTYPE,LIOTERU    TEST ADD END RUN ELEMENT                     
         BNE   IOPUTMAP                                                         
         TM    LIOBFLG2,LIOBFRUN   TEST RUN ELEMENT ADDED                       
         BNZ   *+6                                                              
         DC    H'0'                NO - INVALID CALLING SEQUENCE                
         NI    LIOBFLG2,FF-(LIOBFRUN)                                           
         MVI   LQ_EL,LQ_ERUNQ                                                   
         LHI   R0,LQ_LN1Q                                                       
         STCM  R0,3,LQ_LN                                                       
         GOTOR PUTDAT,PDMOEXIT     ADD ELEMENT TO RECORD                        
         J     EXITCC                                                           
                                                                                
IOPUTMAP CLI   PARMTYPE,LIOTMAP    TEST PUT RECORD MAP                          
         BNE   IOPUTBAD                                                         
         CLI   ACTION,LIOAPUT      CAN'T USE LIOAUPD FOR MAP                    
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    LIOBINDS,LIOBIMRA   TEST MULTIPLE RECORDS ALLOWED                
         BNZ   IOPUTMA2                                                         
         TM    LIOBFLG2,LIOBFUPL   TEST UPLOADING                               
         BNZ   IOPUTMA2                                                         
         GOTOR PUTREC              YES - PUT LAST RECORD                        
IOPUTMA2 GOTOR PUTRME,PARMMAP#     BUILD RECORD MAP CODE ELEMENT                
         OI    LIOBFLG1,LIOBFWRP   SET WRITE PENDING FOR THIS RECORD            
         J     EXITCC                                                           
                                                                                
IOPUTBAD CLI   PARMTYPE,LIOTBAD    TEST PUT ABORT ERROR                         
         BNE   IOPUTERR                                                         
         CLI   ACTION,LIOAPUT      CAN'T USE LIOAUPD FOR ABORT                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   LQ_EL,LQ_ERRRQ                                                   
         LHI   R0,LQ_LNEQ                                                       
         STCM  R0,3,LQ_LN                                                       
         SR    R1,R1                                                            
         ICM   R1,7,PARMAERR                                                    
         MVC   LQ_ERRMN,0(R1)                                                   
         MVC   LQ_ERRMS,LIOBMSYS                                                
         MVI   LQ_D+LQ_LNEQ,0                                                   
         SR    R1,R1                                                            
         ICM   R1,1,PARMLXTX       TEST ANY EXTRA TEST TO BE APPENDED           
         BZ    IOPUTBA2                                                         
         ICM   RE,7,PARMAXTX       RE=A(EXTRA TEXT)                             
         BZ    IOPUTBA2                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LQ_ERRXT(0),0(RE)                                                
         AHI   R1,LQ_LNEQ                                                       
         STCM  R1,3,LQ_LN          SET ACTUAL ELEMENT LENGTH                    
         LA    R1,LQ_D(R1)                                                      
         MVI   0(R1),0                                                          
IOPUTBA2 GOTOR PUTDAT,PDMSPLIT     ADD ELEMENT TO RECORD                        
         OI    LIOBFLG3,LIOBFERR   SET ERROR PUT TO RECORD                      
         TM    LIOBFLG1,LIOBFOFF   DONE IF RUNNING OFFLINE                      
         JNZ   EXITCC                                                           
         TM    LIOBFLG1,LIOBFWRK   TEST WORKER FILE PASSED                      
         BZ    IOGET60                                                          
         MVI   WRKIACTN,WRKIAPUT   PUT BACK LAST WORKER FILE RECORD             
         GOTOR LIOBAWIO,WRKIOB                                                  
         BE    IOGET60                                                          
         DC    H'0'                                                             
                                                                                
IOPUTERR CLI   PARMTYPE,LIOTERR    TEST NORMAL ERROR                            
         BNE   IOPUTRAW                                                         
         TM    LIOBFLG1,LIOBFRME   TEST RETURN MAP ELEMENT ADDED                
         BNZ   IOPUTER2                                                         
         TM    LIOBFLG2,LIOBFRUN   TEST IN RUN SEQUENCE                         
         BNZ   IOPUTER2                                                         
         CLI   ACTION,LIOAUPD                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTOR PUTRME,0            BUILD & ADD RETURN MAP ELEMENT               
IOPUTER2 MVI   LQ_EL,LQ_RAWDQ                                                   
         SR    R1,R1                                                            
         ICM   R1,7,PARMAERR                                                    
         STCM  R1,3,LQ_DCODE                                                    
         MVI   LQ_DTYPE,LD_CHARQ                                                
         USING GETTXTD,WORK        BUILD GETTXT BLOCK                           
         XC    GETTXTD(GETTXTL),GETTXTD                                         
         ICM   R1,7,PARMADAT                                                    
         MVC   GTMSGNO,0(R1)                                                    
         MVC   GTMSYS,LIOBMSYS                                                  
         MVI   GTMAXL,GTMAXLEN                                                  
         LA    R0,LQ_DDATA                                                      
         STCM  R0,7,GTAOUT                                                      
         MVI   GTMTYP,GTMERR                                                    
         MVC   GTLTXT,PARMLXTX                                                  
         MVC   GTATXT,PARMAXTX                                                  
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         TM    LIOBINDS,LIOBIGTR   TEST GETTXT MESSAGE REFS REQUIRED            
         BZ    *+8                                                              
         MVI   GT1INDS,GT1REF+GT1OWRK                                           
         GOTOR CGETTXT,GETTXTD                                                  
         LA    R1,LQ_DDATA+GTMAXLEN-1                                           
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         MVI   0(R1),0                                                          
         BCT   R1,*-12                                                          
         AHI   R1,1                                                             
         SR    R1,R5               R1=LENGTH OF ELEMENT                         
         STCM  R1,3,LQ_LN                                                       
         GOTOR PUTDAT,PDMSPLIT     ADD ELEMENT TO RECORD                        
         OI    LIOBFLG3,LIOBFERR   SET ERROR PUT TO RECORD                      
         J     EXITCC                                                           
                                                                                
IOPUTRAW CLI   PARMTYPE,LIOTRAW    TEST RAW (UNEDITED) DATA                     
         BNE   IOPUTEDT                                                         
         TM    LIOBFLG1,LIOBFRME   TEST RETURN MAP ELEMENT ADDED                
         BNZ   IOPUTRA2                                                         
         TM    LIOBFLG2,LIOBFRUN   TEST IN RUN SEQUENCE                         
         BNZ   IOPUTRA2                                                         
         CLI   ACTION,LIOAUPD                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTOR PUTRME,0            BUILD AND ADD RETURN MAP ELEMENT             
IOPUTRA2 MVI   LQ_EL,LQ_RAWDQ                                                   
         MVC   LQ_DCODE,PARMMAP#                                                
         MVC   LQ_DTYPE,PARMDTYP                                                
         ICM   R1,7,PARMADAT                                                    
         SR    RE,RE                                                            
         ICM   RE,1,PARMLDAT                                                    
         BZ    IOPUTRA4                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   LQ_DDATA(0),0(R1)                                                
         AHI   RE,1                                                             
IOPUTRA4 AHI   RE,LQ_LNDQ                                                       
         STCM  RE,3,LQ_LN                                                       
         GOTOR PUTDAT,PDMSPLIT     ADD ELEMENT TO RECORD                        
         J     EXITCC                                                           
                                                                                
IOPUTEDT CLI   PARMTYPE,LIOTEDT    TEST PRE-EDITED DATA                         
         BNE   IOPUTVAL                                                         
         TM    LIOBFLG1,LIOBFRME   TEST RETURN MAP ELEMENT ADDED                
         BNZ   IOPUTED2                                                         
         TM    LIOBFLG2,LIOBFRUN   TEST IN RUN SEQUENCE                         
         BNZ   IOPUTED2                                                         
         CLI   ACTION,LIOAUPD                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTOR PUTRME,0            BUILD AND ADD RETURN MAP ELEMENT             
IOPUTED2 MVI   LQ_EL,LQ_DLDDQ                                                   
         MVC   LQ_DCODE,PARMMAP#                                                
         MVC   LQ_DTYPE,PARMDTYP                                                
         SR    R1,R1                                                            
         ICM   R1,7,PARMADAT                                                    
         SR    RE,RE                                                            
         ICM   RE,1,PARMLDAT                                                    
         BZ    IOPUTED4                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   LQ_DDATA(0),0(R1)                                                
         AHI   RE,1                                                             
IOPUTED4 AHI   RE,LQ_LNDQ                                                       
         STCM  RE,3,LQ_LN                                                       
         GOTOR PUTDAT,PDMSPLIT     ADD ELEMENT TO RECORD                        
         J     EXITCC                                                           
                                                                                
IOPUTVAL CLI   PARMTYPE,LIOTVAL    TEST DATA VALUE                              
         BNE   IOPUTREQ                                                         
         TM    LIOBFLG1,LIOBFRME   TEST RETURN MAP ELEMENT ADDED                
         BNZ   IOPUTVA2                                                         
         CLI   ACTION,LIOAUPD                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTOR PUTRME,0            BUILD AND ADD RETURN MAP ELEMENT             
IOPUTVA2 MVI   LQ_EL,LQ_VALUQ                                                   
         MVC   LQ_DCODE,PARMMAP#                                                
         SR    R1,R1                                                            
         ICM   R1,7,PARMADAT                                                    
         SR    RE,RE                                                            
         ICM   RE,1,PARMLDAT                                                    
         BZ    IOPUTVA4                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   LQ_VVALU(0),0(R1)                                                
         AHI   RE,1                                                             
IOPUTVA4 AHI   RE,LQ_LNCQ                                                       
         STCM  RE,3,LQ_LN                                                       
         GOTOR PUTDAT,PDMSPLIT     ADD ELEMENT TO RECORD                        
         J     EXITCC                                                           
                                                                                
IOPUTREQ CLI   PARMTYPE,LIOTREQ    TEST REQUEST DATA                            
         BE    *+12                                                             
         CLI   PARMTYPE,LIOTLRQ    TEST LONG REQUEST DATA                       
         BNE   IOPUTEND                                                         
         CLI   ACTION,LIOAPUT      CAN'T USE LIOAUPD FOR REQUEST                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   LQ_EL,LQ_RQSTQ                                                   
         MVC   LQ_DCODE,PARMMAP#                                                
         MVC   LQ_TYPE,PARMDTYP                                                 
         SR    RE,RE                                                            
         ICM   RE,7,PARMADAT                                                    
         LLC   RF,PARMLDAT                                                      
         CLI   PARMTYPE,LIOTLRQ                                                 
         BNE   *+8                                                              
         ICM   RF,3,PARMLLDT                                                    
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R0,LQ_VALUE-LQ_D(RF)                                             
         STCM  R0,3,LQ_LN                                                       
         LA    R0,LQ_VALUE                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR PUTDAT,PDMFORCE     ADD ELEMENT TO RECORD                        
         J     EXITCC                                                           
                                                                                
IOPUTEND CLI   PARMTYPE,LIOTEND    TEST PUT LAST RECORD                         
         BNE   IOPUTLRD                                                         
         CLI   ACTION,LIOAPUT      CAN'T USE LIOAUPD FOR END                    
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    LIOBFLG1,LIOBFOFF   TEST OFFLINE DOWNLOADING                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR PUTREC              PUT LAST RECORD                              
         J     EXITCC                                                           
                                                                                
IOPUTLRD CLI   PARMTYPE,LIOTLRD    TEST PUT LONG REQUEST DATA                   
         BNE   IOPUTGEL                                                         
         CLI   ACTION,LIOAPUT      CAN'T USE LIOAUPD FOR REQUEST                
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         ICM   RE,7,PARMADAT                                                    
         SR    R1,R1                                                            
         ICM   R1,1,PARMLDAT       GET DISPLACEMENT TO ACTUAL LENGTH            
         AR    R1,RE                                                            
         SR    R0,R0                                                            
         ICM   R0,3,0(R1)          R0=ACTUAL DATA LENGTH                        
         LR    R1,R0                                                            
         AHI   R0,LQ_VALUE-LQ_D                                                 
         ICM   RE,15,LIOBNXTA                                                   
         AR    RE,R0                                                            
         CLM   RE,15,LIOBENDA      TEST RECORD WILL OVERFLOW                    
         BNH   *+6                                                              
         DC    H'0'                YES - MAKE RECORD BIGGER                     
         STCM  RE,15,LIOBNXTA      UPDATE ELEMENT POINTER                       
         XC    0(3,RE),0(RE)                                                    
         ICM   R5,15,LIOBENDA      BUILD ELEMENT IN RECORD                      
         MVI   LQ_EL,LQ_RQSTQ                                                   
         STCM  R0,3,LQ_LN                                                       
         MVC   LQ_DCODE,PARMMAP#                                                
         MVC   LQ_TYPE,PARMDTYP                                                 
         LA    R0,LQ_VALUE                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         OI    LIOBFLG1,LIOBFPUT   SET DATA ADDED TO OUTPUT RECORD              
         J     EXITCC                                                           
                                                                                
IOPUTGEL CLI   PARMTYPE,LIOTGEL    GENERAL TYPE DATA ELEMENT                    
         BNE   IOPUTXXX                                                         
         CLI   ACTION,LIOAPUT                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    PARMMAP#,PARMMAP#   MAP CODE FOR RECORD ?                        
         BZ    IOPUTGE2                                                         
         TM    LIOBFLG2,LIOBFUPL   TEST UPLOADING                               
         BNZ   *+8                                                              
         GOTOR PUTREC              YES - PUT LAST RECORD                        
         GOTOR PUTRME,PARMMAP#     BUILD RECORD MAP CODE ELEMENT                
         OI    LIOBFLG1,LIOBFWRP   SET WRITE PENDING FOR THIS RECORD            
IOPUTGE2 ICM   RE,7,PARMADAT                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         ICM   RF,3,LQ_LN-LQ_D(RE)                                              
         CHI   RF,ELEMLNQ          ENSURE WE DON'T CLOBBER OUR                  
         BNH   *+6                 WORKING STORAGE CHAIN                        
         DC    H'0'                                                             
         LA    R0,LQ_EL                                                         
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR PUTDAT,PDMSPLIT     ADD ELEMENT TO RECORD                        
         J     EXITCC                                                           
                                                                                
IOPUTXXX DC    H'0'                UNKNOWN PUT TYPE                             
         EJECT                                                                  
***********************************************************************         
* Close file (multiple output records mode)                           *         
***********************************************************************         
                                                                                
IOCLO    TM    LIOBINDS,LIOBIMLT   MUST BE MULTIPLE OUTPUT RECORDS              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    LIOBFLG2,LIOBFEOF   AND EOF MUST HAVE BEEN ENCOUNTERED           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR PUTREC              PUT LAST RECORD                              
         B     IOGET60             CLOSE AND RETURN CONTROL                     
         EJECT                                                                  
***********************************************************************         
* Build & add return map element                                      *         
***********************************************************************         
                                                                                
PUTRME   ST    RE,SAVERE                                                        
         LA    R5,ELEM                                                          
         USING LQ_D,R5                                                          
         MVI   LQ_EL,LQ_DLDDQ      BUILD DOWNLOAD MAP ELEMENT                   
         LHI   RE,LQ_LNCQ                                                       
         STCM  RE,3,LQ_LN                                                       
                                                                                
         LTR   R1,R1               TEST USE RESPONSE MAP                        
         JZ    *+14                                                             
         MVC   LQ_DCODE,0(R1)      NO - SET FROM CALLER                         
         J     PUTRME02                                                         
                                                                                
         MVC   LQ_DCODE,LIOBOMAP   SET RESPONSE RECORD MAP CODE                 
         TM    LIOBFLG3,LIOBFSAV   TEST MAP CODE SAVED                          
         JZ    PUTRME02                                                         
         MVC   LQ_DCODE,LIOBSAVE   YES - USE SAVED VALUE                        
                                                                                
PUTRME02 OC    LQ_DCODE,LQ_DCODE   TEST OUTPUT MAP NON-ZERO                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR PUTDAT,PDMSPLIT                                                  
         MVC   LIOBRMEA,APUTELEM   SET A(RETURN MAP ELEMENT)                    
         OI    LIOBFLG1,LIOBFRME                                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
***********************************************************************         
* Add data element to output record                                   *         
***********************************************************************         
                                                                                
PDMSPLIT EQU   0                   SPLIT RECORD ON OVERFLOW                     
PDMOEXIT EQU   1                   EXIT ON OVERFLOW (IGNORE)                    
PDMFORCE EQU   2                   FORCE ELEMENT TO SAME RECORD                 
                                                                                
PUTDAT   NTR1  LABEL=NO                                                         
         STC   R1,PDMODE           SAVE CALLING MODE                            
                                                                                
         CLI   ACTION,LIOAUPD      TEST UPDATE EXISTING PUT ELEMENT             
         BE    PUTDAT08                                                         
                                                                                
PUTDAT02 SR    RF,RF               ADD DATA ELEMENT TO OUTPUT RECORD            
         ICM   RF,3,LQ_LN          RE=L'ELEMENT TO BE INSERTED                  
         ICM   RE,15,LIOBNXTA                                                   
         ST    RE,APUTELEM         SAVE A(THIS ELEMENT)                         
         AR    RE,RF                                                            
         CLM   RE,15,LIOBENDA      TEST RECORD WILL OVERFLOW                    
         BNH   PUTDAT06                                                         
         CLI   PDMODE,PDMOEXIT     TEST EXIT IF RECORD OVERFLOW                 
         BE    PUTDAT04                                                         
         CLI   PDMODE,PDMSPLIT     TEST OK TO SPLIT RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    LIOBFLG1,LIOBFOFF   TEST OFFLINE                                 
         BZ    *+6                                                              
         DC    H'0'                RECORD IS 14K LONG - MUST BE A BUG           
         TM    LIOBFLG2,LIOBFUPL   TEST UPLOADING IN LINK PROGRAM               
         BZ    *+6                                                              
         DC    H'0'                NOT GOOD                                     
         TM    LIOBINDS,LIOBIMLT   TEST MULTIPLE OUTPUT RECORDS                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    LIOBFLG1,LIOBFOPA   TEST FILE OPEN FOR APPEND                    
         BNZ   PUTDAT04                                                         
         DC    H'0'                                                             
                                                                                
PUTDAT04 GOTOR PUTREC              ADD NEW RECORD                               
         B     PUTDAT02            AND TRY ADDING DATA AGAIN                    
                                                                                
PUTDAT06 STCM  RE,15,LIOBNXTA      SET ADDRESS OF NEXT ELEMENT                  
         XC    0(3,RE),0(RE)       CLEAR SPACE (JUST IN CASE)                   
         SR    RE,RF                                                            
         B     PUTDAT12                                                         
                                                                                
PUTDAT08 ICM   RE,15,LIOBRECA      POINT TO FIRST OUTPUT ELEMENT                
         SR    RF,RF                                                            
PUTDAT10 CLI   0(RE),0             TEST END OF RECORD                           
         BNE   *+6                                                              
         DC    H'0'                ELEMENT MUST EXIST                           
         ICM   RF,3,LQ_LN-LQ_D(RE)                                              
         CLC   LQ_D(LQ_LNDQ-1),0(RE)                                            
         BE    PUTDAT12                                                         
         AR    RE,RF               BUMP TO NEXT ELEMENT ON RECORD               
         B     PUTDAT10                                                         
                                                                                
PUTDAT12 LA    R0,LQ_EL            MOVE ELEMENT TO OUTPUT RECORD                
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         OI    LIOBFLG1,LIOBFPUT   SET DATA ADDED TO OUTPUT RECORD              
                                                                                
PUTDATX  J     EXIT                                                             
         DROP  R5,RB                                                            
         EJECT                                                                  
***********************************************************************         
* Routine to build an input string and optionally move it to one or   *         
* more twa input fields                                               *         
*                                                                     *         
* Exit:- CC set to equal if string built (and optionally moved)       *         
*           PARMASTR points to 2 byte field length followed by text   *         
*        CC set to low if no string built or no input                 *         
*        CC set to high on input errors (missing, too long etc.)      *         
*           LIOBERR# contains map number of missing required field    *         
***********************************************************************         
                                                                                
IOBLD    BASE  ,                                                                
         LA    R0,FIELDX           GET ADDRESS OF END OF FIELD                  
         ST    R0,AFIELDX                                                       
         XC    LIOBDTA#,LIOBDTA#   CLEAR DATA FIELD MAP NUMBER                  
         XC    LIOBERR#,LIOBERR#   CLEAR FIELD IN ERROR MAP NUMBER              
         MVC   FIELDNUM,PARMNFLD   SET NUMBER OF USABLE TWA FIELDS              
         CLI   FIELDNUM,0          IF N'TWA FIELDS NOT SET                      
         BNE   *+8                                                              
         MVI   FIELDNUM,1          SET TO ONE                                   
                                                                                
         SR    R1,R1               PRE-CLEAR TWA FIELDS                         
         ICM   R1,7,PARMAFLD       R1=A(FIRST TWA OUTPUT FIELD HEADER)          
         BZ    IOBLD10                                                          
         USING FLDHDRD,R1                                                       
         LLC   R6,FIELDNUM         R6=NUMBER OF USABLE TWA FIELDS               
         SR    R0,R0                                                            
IOBLD02  ICM   R0,1,FLDLEN                                                      
         BZ    IOBLD10                                                          
         TM    FLDATB,FATBPROT                                                  
         BNZ   IOBLD06                                                          
         LR    RF,R0                                                            
         SHI   RF,FLDDATA+1-FLDHDRD                                             
         TM    FLDATB,FATBXHDR                                                  
         BZ    *+8                                                              
         SHI   RF,FLDDATA-FLDHDRD                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    FLDDATA(0),FLDDATA                                               
         MVI   FLDIIND,0                                                        
         MVI   FLDILEN,0                                                        
         BCT   R6,IOBLD06                                                       
         B     IOBLD10                                                          
IOBLD06  AR    R1,R0               BUMP TO NEXT TWA FIELD                       
         B     IOBLD02                                                          
         DROP  R1                                                               
                                                                                
IOBLD10  L     R6,PARMATAB                                                      
         USING LIOFD,R6            R6=A(FIELD TABLE)                            
         LA    R7,FIELD            R7=A(FIELD BUILD AREA)                       
                                                                                
IOBLD12  CLI   LIOFIND1,LIOFIEOT   TEST END OF FIELD TABLE                      
         BE    IOBLD46                                                          
                                                                                
         MVC   HALF,LIOFDISP                                                    
         LH    RE,HALF             RE=DISPLACEMENT TO VALUE (+ OR -)            
         LA    R5,LIOFDISP                                                      
         AR    R5,RE               R5=A(LITERAL OR MAP)                         
                                                                                
         TM    LIOFIND2,LIOFIFST   TEST FIRST ARRAY COLUMN                      
         BZ    IOBLD28                                                          
         ST    R6,AARRAY           SAVE A(START OF ARRAY)                       
                                                                                
         LR    RE,R6                                                            
ARYF     USING LIOFD,RE                                                         
         XC    ROWWIDTH,ROWWIDTH   CLEAR ROW WIDTH                              
         XC    ALQ_EL,ALQ_EL       SET WE HAVE NO DATA                          
                                                                                
IOBLD14  TM    ARYF.LIOFIND1,LIOFIMAP                                           
         BZ    IOBLD20                                                          
                                                                                
         MVC   HALF,ARYF.LIOFDISP                                               
         LH    R1,HALF             R1=DISPLACEMENT TO VALUE (+ OR -)            
         LA    RF,ARYF.LIOFDISP                                                 
         AR    RF,R1               RF=A(LITERAL OR MAP)                         
ARYD     USING LIODD,RF                                                         
         OC    ALQ_EL,ALQ_EL       TEST FIRST ARRAY COLUMN                      
         BNZ   IOBLD18                                                          
         SR    R1,R1                                                            
         ICM   R1,1,ARYD.LIODBLKN  YES - IT POINTS TO THE INDEX                 
         SLL   R1,2                                                             
         LA    R1,LIOBASB1-L'LIOBASB1(R1)                                       
         ICM   R1,15,0(R1)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,ARYD.LIODDISP                                               
         AR    R1,R0               R1=A(INDEX)                                  
         ICM   R1,15,0(R1)         R1=A(LQ_EL)                                  
         BZ    *+12                                                             
         ST    R1,ALQ_EL           SET A(ELEMENT)                               
         B     IOBLD18                                                          
                                                                                
         TM    ARYF.LIOFIND1,LIOFIREQ TEST REQUIRED FIELD                       
         BZ    IOBLD16             NO - GO TO END OF ARRAY                      
         LA    R5,ARYD.LIODD       YES - RETURN MISSING INPUT                   
         B     IOBLDMI                                                          
                                                                                
IOBLD16  TM    ARYF.LIOFIND2,LIOFILST TEST END OF ARRAY                         
         BZ    *+12                                                             
         LA    R6,ARYF.LIOFD+LIOFL YES - POINT TO NEXT ENTRY                    
         B     IOBLD12             AND GO PROCESS                               
                                                                                
         AHI   RE,LIOFL            BUMP TO NEXT FIELD ENTRY                     
         CLI   ARYF.LIOFIND1,LIOFIEOT                                           
         BNE   IOBLD16                                                          
         DC    H'0'                END ARRAY FIELD NOT SET                      
                                                                                
IOBLD18  SR    R0,R0               INCREMENT ROW WIDTH                          
         ICM   R0,1,ARYD.LIODDLEN                                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AH    R0,ROWWIDTH                                                      
         STH   R0,ROWWIDTH                                                      
                                                                                
IOBLD20  TM    ARYF.LIOFIND2,LIOFILST TEST END OF ARRAY                         
         BNZ   IOBLD22                                                          
         AHI   RE,LIOFL                                                         
         CLI   ARYF.LIOFIND1,LIOFIEOT                                           
         BNE   IOBLD14                                                          
         DC    H'0'                HIT END OF TABLE BEFORE END OF ARRAY         
                                                                                
IOBLD22  OC    ROWWIDTH,ROWWIDTH   R2=ROW WIDTH                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     RE,ALQ_EL                                                        
         USING LQ_D,RE                                                          
         CLI   LQ_TYPE,LQ_TLSTQ    ARRAYS ARE ALWAYS LISTS                      
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,LQ_VALUE       R1=N'ENTRIES IN THE LIST                     
         MH    R0,ROWWIDTH         R0=ROW WIDTH                                 
         AHI   R0,LQ_VALUE+2-LQ_D  R0=L'ELEMENT                                 
         CLM   R0,3,LQ_LN                                                       
         BE    *+6                                                              
         DC    H'0'                ARRAY DATA DOESN'T MATCH FIELD TABLE         
         MVC   ROWCOUNT,LQ_VALUE   SET ROW COUNT                                
         LHI   R0,1                                                             
         STH   R0,ROWDOING         SET PROCESSING ROW 1                         
         LA    RE,LQ_VALUE+2                                                    
         ST    RE,AROW             SET A(CURRENT INPUT ROW)                     
                                                                                
IOBLD24  L     R6,AARRAY           POINT TO FIRST ARRAY FIELD                   
         GOTOR SAVDLM,ARRAYDLM     SET FIELD DELIMITER                          
         XC    ROWDDISP,ROWDDISP   CLEAR DATA DISPLACEMENT                      
                                                                                
IOBLD26  MVC   HALF,LIOFDISP                                                    
         LH    RE,HALF             RE=DISPLACEMENT TO VALUE (+ OR -)            
         LA    R5,LIOFDISP                                                      
         AR    R5,RE               R5=A(LITERAL OR MAP)                         
         DROP  RE                                                               
                                                                                
IOBLD28  TM    LIOFIND1,LIOFILIT   TEST LITERAL VALUE                           
         BZ    IOBLD32                                                          
         TM    LIOFIND1,LIOFIPFX   TEST IF A PREFIX FOR NEXT FIELD              
         BZ    IOBLD30                                                          
         STCM  R5,15,FIELDALP      YES - SAVE ADDRESS OF LITERAL                
         MVC   FIELDLLP,LIOFLLEN   AND LENGTH                                   
         NI    FIELDLLP,LIOFILEN                                                
         B     IOBLD40                                                          
                                                                                
IOBLD30  GOTOR PUTDLM,FIELDDLM     INSERT DELIMITER/PREFIX IF NECESSARY         
         SR    RF,RF                                                            
         ICM   RF,1,LIOFLLEN                                                    
         SHI   RF,1                                                             
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    R0,1(RF,R7)                                                      
         C     R0,AFIELDX          CHECK WITHIN BOUNDS                          
         BH    IOBLDTL                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R5)                                                    
         LA    R7,1(RF,R7)         POINT TO NEXT OUTPUT POSITION                
         CLI   0(R7),C' '                                                       
         BH    *+8                                                              
         BCT   R7,*-8                                                           
         AHI   R7,1                                                             
         C     R6,AARRAY                                                        
         BE    *+12                                                             
         GOTOR SAVDLM,FIELDDLM     SET PENDING DELIMITER                        
         B     IOBLD40                                                          
                                                                                
IOBLD32  TM    LIOFIND1,LIOFIMAP   TEST MAP                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING LIODD,R5            R5=A(MAP ENTRY)                              
         OC    AROW,AROW           TEST PROCESSING AN ARRAY                     
         BZ    *+16                                                             
         L     R1,AROW             GET CURRENT ROW ADDRESS                      
         AH    R1,ROWDDISP         ADD CURRENT DISPLACEMENT                     
         B     IOBLD34             PROCESS AS REGULAR DATA                      
                                                                                
         SR    R1,R1                                                            
         ICM   R1,1,LIODBLKN                                                    
         SLL   R1,2                                                             
         LA    R1,LIOBASB1-L'LIOBASB1(R1)                                       
         ICM   R1,15,0(R1)                                                      
         SR    R0,R0                                                            
         ICM   R0,3,LIODDISP                                                    
         AR    R1,R0               R1=A(DATA)                                   
                                                                                
         TM    LIODIND1,LIODINDX   TEST POINTING TO A DATA INDEX                
         BZ    IOBLD34                                                          
         ICM   RE,7,1(R1)          POINT TO DATA ELEMENT VIA INDEX              
         BZ    IOBLD36             NO INDEX - TEST REQUIRED                     
                                                                                
         USING LQ_D,RE             RE=A(DATA ELEMENT IN RECORD)                 
         SR    RF,RF                                                            
         ICM   RF,3,LQ_LN                                                       
         SHI   RF,LQ_VALUE+1-LQ_D                                               
         BM    IOBLD36             NO DATA - TEST REQUIRED                      
         LA    R1,LQ_VALUE         POINT TO DATA IN ELEMENT                     
         B     IOBLD38                                                          
         DROP  RE                                                               
                                                                                
IOBLD34  SR    RF,RF               POINTING TO DATA                             
         ICM   RF,1,LIODDLEN                                                    
         SHI   RF,1                                                             
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    RF,*+8              TEST FOR SPACES                              
         BE    IOBLD36             YES - NO INPUT                               
         CLC   0(0,R1),SPACES                                                   
         EX    RF,*+8              TEST FOR BINARY ZEROES                       
         BNZ   IOBLD38             NO - THERE IS INPUT                          
         OC    0(0,R1),0(R1)                                                    
                                                                                
IOBLD36  TM    LIOFIND1,LIOFIREQ   NO INPUT - TEST REQUIRED                     
         BNZ   IOBLDMI             YES - MISSING INPUT FIELD ERROR              
         MVI   FIELDLLP,0          ELSE TURN PREFIX OFF                         
         B     IOBLD40             AND CARRY ON                                 
                                                                                
IOBLD38  OI    FIELDFLG,1          SET INPUT PRESENT                            
         OC    LIOBDTA#,LIOBDTA#   TEST FIRST DATA ITEM                         
         BNZ   *+10                                                             
         MVC   LIOBDTA#,LIODDMAP   YES - SET DATA MAP NUMBER                    
         LR    R0,R1                                                            
         GOTOR PUTDLM,FIELDDLM     INSERT DELIMITER IF NECESSARY                
         LR    R1,R0                                                            
         LA    R0,0(R7,RF)                                                      
         C     R0,AFIELDX          CHECK WITHIN BOUNDS                          
         BH    IOBLDTL                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R1)                                                    
         AR    R7,RF                                                            
         CLI   0(R7),C' '                                                       
         BH    *+8                                                              
         BCT   R7,*-8                                                           
         AHI   R7,1                                                             
         C     R6,AARRAY                                                        
         BE    IOBLD40                                                          
         GOTOR SAVDLM,FIELDDLM     SET PENDING DELIMITER                        
                                                                                
IOBLD40  OC    AROW,AROW           TEST PROCESSING AN ARRAY                     
         BZ    IOBLD44                                                          
                                                                                
         TM    LIOFIND2,LIOFILST   ARE WE POINTING TO END OF ARRAY              
         BNZ   IOBLD42                                                          
         SR    RF,RF                                                            
         TM    LIOFIND1,LIOFILIT   TEST JUST PROCESSED A LITERAL                
         BNZ   *+8                                                              
         IC    RF,LIODDLEN         NO - SET DATA LENGTH                         
         LH    RE,ROWDDISP                                                      
         AR    RE,RF               ADD DATA LENGTH TO SET DISPLACEMENT          
         STH   RE,ROWDDISP         TO NEXT DATA IN ROW                          
         AHI   R6,LIOFL            BUMP TO NEXT ARRAY ENTRY                     
         B     IOBLD26                                                          
                                                                                
IOBLD42  LH    R1,ROWDOING         DECREMENT ROW COUNT                          
         AHI   R1,1                                                             
         CH    R1,ROWCOUNT                                                      
         BNH   *+14                                                             
         XC    AROW,AROW           ALL ROWS PROCESSED - CLEAR AROW              
         B     IOBLD44             AND PROCESS NEXT FIELD TABLE ENTRY           
                                                                                
         STH   R1,ROWDOING         SET CURRENT ROW NUMBER                       
         GOTOR PUTDLM,FIELDDLM     INSERT DELIMITER/PREFIX IF NECESSARY         
         GOTOR PUTDLM,ARRAYDLM     INSERT ARRAY DELIMITER IF NECESSARY          
         L     RE,AROW                                                          
         AH    RE,ROWWIDTH                                                      
         ST    RE,AROW             SET A(NEXT ARRAY ROW)                        
         B     IOBLD24             POINT TO FIRST ARRAY FIELD                   
                                                                                
IOBLD44  AHI   R6,LIOFL            BUMP TO NEXT TABLE ENTRY                     
         B     IOBLD12                                                          
                                                                                
IOBLD46  LA    R0,FIELD            MOVE TO TWA FIELD IF NECESSARY               
         SR    R7,R0                                                            
         JZ    EXITL               EXIT WITH CC=LOW IF NOTHING BUILT            
         CHI   R7,L'FIELD          TEST EXCEEDS MAXIMUM LENGTH                  
         BNH   *+6                                                              
         DC    H'0'                                                             
         STCM  R7,3,FIELDL         SET FIELD LENGTH                             
         LA    R0,FIELDL           RETRUN A(BUILT STRING) TO CALLER             
         ST    R0,PARMASTR         SET A(BUILT STRING) FOR CALLER               
                                                                                
         ICM   R1,7,PARMAFLD       MOVE TO TWA FIELD IF NECESSARY               
         BZ    IOBLDX                                                           
         USING FLDHDRD,R1          R1=A(TWA FIELD HEADER)                       
         LA    R5,FIELD            R5=A(BUILT STRING)                           
IOBLD48  SR    RF,RF                                                            
         ICM   RF,1,FLDLEN         RF=LENGTH OF CURRENT TWA FIELD               
         BZ    IOBLDTL             EXIT IF END OF TWA                           
         SHI   RF,FLDDATA-FLDHDRD                                               
         TM    FLDATB,FATBXHDR                                                  
         JZ    *+8                                                              
         SHI   RF,FLDDATA-FLDHDRD  RF=ACTUAL LENGTH OF TWA FIELD                
         LR    RE,R7                                                            
         BCTR  RE,0                RE=DATA LENGTH-1                             
         CR    R7,RF               DOES INPUT STRING FIT INTO FIELD             
         BNH   IOBLD52             YES - MOVE IT ALL IN                         
         LA    RE,0(R5,RF)         NO - SPLIT THE FIELD                         
                                                                                
IOBLD50  BCTR  RE,0                LOOK BACKWARDS FOR A COMMA                   
         BCT   RF,*+8                                                           
         B     IOBLDTL             NO COMMA - FIELD TOO LONG                    
         CLI   0(RE),SLASHQ                                                     
         BE    *+12                                                             
         CLI   0(RE),COMMAQ                                                     
         BNE   IOBLD50                                                          
         SR    RE,R5               RE=LENGTH OF THIS CHUNK                      
                                                                                
IOBLD52  EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),0(R5)    MOVE DATA TO TWA FIELD                       
         AHI   RE,1                                                             
         STC   RE,FLDILEN          SET INPUT FIELD LENGTH                       
         AR    R5,RE               POINT TO NEXT FIELD DATA                     
         SR    R7,RE               DECREMENT MOVED LENGTH                       
         BZ    IOBLDX              EXIT IF ALL DONE                             
                                                                                
         IC    RE,FIELDNUM                                                      
         SHI   RE,1                DECREMENT TWA FIELD USED COUNT               
         BZ    IOBLDTL                                                          
         STC   RE,FIELDNUM                                                      
         IC    RE,FLDLEN           BUMP TO NEXT INPUT FIELD                     
         AR    R1,RE                                                            
         B     IOBLD48                                                          
                                                                                
IOBLDX   TM    FIELDFLG,1          TEST ANY INPUT PROCESSED                     
         JZ    EXITL               EXIT WITH CC LOW IF NO INPUT                 
         J     EXITE               EXIT WITH CC EQUAL IF FIELD(S) BUILT         
                                                                                
IOBLDMI  MVI   PARMCOMP,LIOCMISS   SET MISSING INPUT FIELD                      
         MVC   LIOBERR#,LIODDMAP   SET MAP NUMBER OF MISSING FIELD              
         J     EXITH                                                            
                                                                                
IOBLDTL  MVI   PARMCOMP,LIOCTOOL   SET INPUT STRING TOO LONG                    
         J     EXITH                                                            
         DROP  R5,RB                                                            
         EJECT                                                                  
***********************************************************************         
* Save delimiter value for insertion into string                      *         
***********************************************************************         
                                                                                
SAVDLM   TM    LIOFIND1,LIOFICOM                                                
         JZ    *+10                                                             
         MVI   0(R1),COMMAQ                                                     
         BR    RE                                                               
                                                                                
         TM    LIOFIND1,LIOFIEQU                                                
         JZ    *+10                                                             
         MVI   0(R1),EQUALQ                                                     
         BR    RE                                                               
                                                                                
         TM    LIOFIND1,LIOFISLS                                                
         JZ    *+10                                                             
         MVI   0(R1),SLASHQ                                                     
         BR    RE                                                               
                                                                                
         TM    LIOFIND2,LIOFIDSH                                                
         JZ    *+10                                                             
         MVI   0(R1),DASHQ                                                      
         BR    RE                                                               
                                                                                
         TM    LIOFIND2,LIOFIPLS                                                
         BZR   RE                                                               
         MVI   0(R1),PLUSQ                                                      
         BR    RE                                                               
                                                                                
***********************************************************************         
* Insert delimter/prefix into string                                  *         
***********************************************************************         
                                                                                
PUTDLM   CLI   0(R1),0             DELIMITER FIRST                              
         JE    PUTDLM02                                                         
         C     R7,AFIELDX          TEST WITHIN BOUNDS                           
         JNL   IOBLDTL                                                          
         MVC   0(1,R7),0(R1)                                                    
         AHI   R7,L'FIELDDLM                                                    
         MVI   0(R1),0                                                          
                                                                                
PUTDLM02 CLI   FIELDLLP,0          TEST PREFIX LENGTH SET                       
         BER   RE                                                               
                                                                                
         STM   RE,R1,12(RD)        INSERT FIELD PREFIX INTO STRING              
         SR    RF,RF                                                            
         ICM   RF,1,FIELDLLP                                                    
         BCTR  RF,0                                                             
         ICM   R1,15,FIELDALP                                                   
         LA    RE,1(RF,R7)                                                      
         C     RE,AFIELDX          TEST WITHIN BOUNDS                           
         JH    IOBLDTL                                                          
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R7),0(R1)                                                    
         LA    R7,1(RF,R7)                                                      
         MVI   FIELDLLP,0          TURN PREFIX OFF                              
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
COMMAQ   EQU   C','                                                             
EQUALQ   EQU   C'='                                                             
SLASHQ   EQU   C'/'                                                             
DASHQ    EQU   C'-'                                                             
PLUSQ    EQU   C'+'                                                             
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERAL VALUES **                  
                                                                                
         LTORG                                                                  
                                                                                
GLCLR    DC    C'CLEAR'                                                         
GLPUT    DC    C'PUTD'                                                          
GLGET    DC    C'GETD'                                                          
GLDEL    DC    C'DELE'                                                          
                                                                                
WRR      DC    C'RANDOM  '                                                      
RECS     DC    C'REC#'                                                          
SPACES   DC    CL80' '                                                          
                                                                                
VWRKIO   DC    V(WRKIO)                                                         
BZEROS   DC    AL2(0)                                                           
         EJECT                                                                  
***********************************************************************         
* Errors and exits                                                    *         
***********************************************************************         
                                                                                
NOXFER   MVI   PARMCOMP,LIOCNOXC   NO TRANSFER CONTROL ELEMENT FOUND            
         J     EXITCC                                                           
                                                                                
NODDLK   MVI   PARMCOMP,LIOCNOTL   INVOKER IS NOT DDLINK                        
         J     EXITCC                                                           
                                                                                
NOMORE   MVI   PARMCOMP,LIOCDONE   NO MORE INPUT RECORDS                        
                                                                                
EXITCC   CLI   PARMCOMP,LIOCGOOD   SET CONDITION CODE FOR CALLER                
         J     EXIT                                                             
                                                                                
EXITL    LHI   RE,0                EXIT WITH CC LOW                             
         J     EXITLEH                                                          
EXITE    LHI   RE,1                EXIT WITH CC EQUAL                           
         J     EXITLEH                                                          
EXITH    LHI   RE,2                EXIT WITH CC HIGH                            
                                                                                
EXITLEH  CHI   RE,1                                                             
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* Put record to worker file or call DDLINK to do it (offline)         *         
*                                                                     *         
* The reason that LINKIO does not do I/O directly to the worker file  *         
* offline is because DDLINK may segement the output and switch worker *         
* file buffers (see SETADR routine below)                             *         
***********************************************************************         
                                                                                
PUTREC   ST    RE,SAVERE                                                        
         TM    LIOBFLG1,LIOBFWRP+LIOBFPUT                                       
         JZ    PUTRECX                                                          
         TM    LIOBFLG1,LIOBFOFF   TEST OFFLINE                                 
         JNZ   PUTREC02                                                         
         ICM   RE,15,LIOBNXTA                                                   
         MVI   0(RE),0             SET END OF CURRENT RECORD                    
         AHI   RE,1                                                             
         S     RE,LIOBAREC         RE=RECORD LENGTH                             
         SLL   RE,16                                                            
         L     RF,LIOBAREC                                                      
         STCM  RE,15,0(RF)         SET RECORD LENGTH                            
         MVI   WRKIACTN,WRKIAADD                                                
         GOTOR LIOBAWIO,WRKIOB                                                  
         JE    PUTREC04                                                         
         DC    H'0'                CAN'T ADD RECORD TO WORKER FILE              
                                                                                
PUTREC02 L     R1,LIOBALPD         CALL DDLINK'S RECPUT ROUTINE                 
         L     RF,LP_ARECP-LP_D(R1)                                             
         GOTOR (RF),(R1)                                                        
                                                                                
PUTREC04 GOTOR SETADR              RESET LIOBNXTA/LIOBENDA VALUES               
         NI    LIOBFLG1,FF-(LIOBFWRP+LIOBFPUT)                                  
         NI    LIOBFLG2,FF-(LIOBFRUN)                                           
         NI    LIOBFLG3,FF-(LIOBFERR)                                           
                                                                                
PUTRECX  L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
***********************************************************************         
* Set LIOBNXTA/LIOBENDA                                               *         
***********************************************************************         
                                                                                
SETADR   STM   RE,R1,12(RD)                                                     
         TM    LIOBFLG1,LIOBFOFF   TEST OFFLINE                                 
         JNZ   *+12                                                             
         L     RE,LIOBAREC         YES - POINT TO RECORD DIRECTLY               
         J     SETADR02                                                         
         L     R1,LIOBALPD         ELSE DIG IT OUT                              
         ICM   RE,15,LP_ARUNP-LP_D(R1)                                          
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         ICM   RF,7,RUNPARUN-RUNPARMD(RE)                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   RE,15,RWRKBLK-RUNFACSD(RF)                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   RE,15,WRKIAREC-WRKIOB(RE)                                        
SETADR02 AHI   RE,4                                                             
         STCM  RE,15,LIOBRECA      SET A(FIRST DATA ELEMENT)                    
         STCM  RE,15,LIOBNXTA      SET A(NEXT DATA ELEMENT)                     
         XC    0(256,RE),0(RE)                                                  
         SR    RF,RF                                                            
         ICM   RF,3,LIOBRECL       TEST CALLER SUPPLIED RECORD LENGTH           
         JNZ   *+8                                                              
         LHI   RF,14*ONEK          NO - ASSUME IT'S AS LONG AS A BUFFER         
         AR    RE,RF                                                            
         SHI   RE,10               ALLOW A SMALL AMOUNT OF SLACK                
         STCM  RE,15,LIOBENDA      SET A(END OF RECORD)                         
SETADRX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
FF       EQU   X'FF'                                                            
ONEK     EQU   1024                                                             
                                                                                
PARMD    DSECT                     ** PARAMETER LIST **                         
PARMACTN DS    0X                  ACTION CODE                                  
PARMCOMP DS    0X                  COMPLETION CODE                              
PARMABLK DS    A                   A(LINKIO CONTROL BLOCK)                      
PARMATAB DS    0A                  A(FIELD BUILD TABLE)                         
PARMTYPE DS    X                   PUT DATA TYPE                                
PARMAERR DS    AL3                 A(ERROR NUMBER)                              
         ORG   *-L'PARMMAP#                                                     
PARMMAP# DS    AL2                 DATA MAP NUMBER                              
PARMNFLD DS    X                   NUMBER OF TWA FIELDS                         
PARMAFLD DS    AL3                 A(FIRST TWA OUTPUT FIELD)                    
         ORG   PARMNFLD                                                         
PARMDTYP DS    X                   DATA TYPE (IF RAW)                           
PARMADAT DS    AL3                 A(DATA TO BE PUT)                            
PARMASTR DS    0A                  A(BUILT STRING) (LIOABLD ONLY)               
PARMLDAT DS    0AL1                L'DATA TO BE PUT                             
PARMLXTX DS    AL1                 L'EXTRA ERROR TEXT                           
PARMAXTX DS    AL3                 A(EXTRA ERROR TEXT)                          
         ORG   *-L'PARMLLDT                                                     
PARMLLDT DS    AL2                                                              
                                                                                
WORKD    DSECT                     ** WORKING STORAGE **                        
DUB      DS    D                                                                
RELO     DS    F                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
SAVERE   DS    A                                                                
HALF     DS    H                                                                
WORK     DS    XL64                                                             
                                                                                
ACTION   DS    XL(L'PARMACTN)      CALLING ACTION                               
PDMODE   DS    X                   PUTDAT MODE (SEE PDMXXXXX EQUATES)           
                                                                                
ROWWIDTH DS    H                   WIDTH OF ARRAY ROW                           
ROWCOUNT DS    H                   NUMBER OF ARRAY ROWS                         
ROWDOING DS    H                   CURRENT ROW NUMBER                           
ROWDDISP DS    H                   DISPLACEMENT TO CURRENT DATA IN ROW          
                                                                                
AARRAY   DS    A                   A(START OF ARRAY FIELD ENTRY)                
ALQ_EL   DS    A                   A(LQ_EL) CONTAINING ARRAY                    
AROW     DS    A                   A(CURRENT INPUT ROW)                         
AFIELDX  DS    A                   A(END OF FIELD)                              
APUTELEM DS    A                   A(ELEMENT JUST PUT BY PUTDAT S/R)            
                                                                                
ELEM     DS    0X                                                               
FIELDNUM DS    X                   NUMBER OF USABLE TWA FIELDS                  
FIELDFLG DS    X                   FLAG BYTE                                    
FIELDDLM DS    C                   PENDING FIELD DELIMITER                      
ARRAYDLM DS    C                   PENDING ARRAY DELIMITER                      
FIELDALP DS    AL4                 A(LITERAL PREFIX)                            
FIELDLLP DS    AL1                 LENGTH OF LITERAL PREFIX                     
FIELDL   DS    AL2                 LENGTH OF DATA IN FIELD                      
FIELD    DS    CL1024              FIELD BUILD AREA                             
FIELDX   EQU   *                                                                
ELEMLNQ  EQU   *-ELEM                                                           
                                                                                
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
LIOBD    DSECT                                                                  
       ++INCLUDE DDLINKIOD                                                      
LIOBD    DSECT                                                                  
         ORG   LIOBWORK                                                         
LIOBSAVE DS    AL2                 SAVED MAP NUMBER                             
LIOBMAPN DS    AL2                 CURRENT ELEMENT MAP NUMBER                   
                                                                                
LIOBTOKN DS    0XL6                ** WSSVR TOKEN/LENGTH **                     
LIOBTVAL DS    CL4                 DDLINK TOKEN VALUE                           
LIOBTLEN DS    AL2                 BUFFER LENGTH                                
                                                                                
LIOBWREC DS    XL(L'W_RECS)        NUMBER OF RECORDS ON WORKER FILE             
LIOBACUR DS    0AL4                A(CURRENT RECORD IN BUFFER)                  
LIOBCREC DS    XL(L'LIOBWREC)      CURRENT RECORD NUMBER                        
LIOBRMEA DS    AL4                 A(RETURN MAP ELEMENT)                        
LIOBENDI DS    AL4                 A(END OF INPUT DATA)                         
LIOBRECA DS    AL4                 A(FIRST DATA ELEMENT)                        
LIOBNXTA DS    AL4                 A(NEXT RETURN DATA ELEMENT)                  
LIOBENDA DS    AL4                 A(END OF CURRENT RECORD)                     
LIOBCURA DS    AL4                 A(CURRENT ELEMENT)                           
LIOBMAPA DS    AL4                 A(DATA MAP TABLE)                            
LIOBOMAP DS    XL(L'LIOROMAP)      RETURN MAP CODE                              
                                                                                
       ++INCLUDE DDWRKIOD                                                       
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
* DDGLVXCTLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLVXCTLD                                                     
GLVXCTLX DS    0X                                                               
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
* DDLINKD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLINKD                                                        
         PRINT ON                                                               
* DDRUNNERD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDRUNNERD                                                      
         PRINT ON                                                               
* DMWRKFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFD                                                        
         PRINT ON                                                               
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
GETTXTL  EQU   *-GETTXTD                                                        
         PRINT ON                                                               
* FAWSSVRD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAWSSVRD                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013DDLINKIO  09/10/13'                                      
         END                                                                    
