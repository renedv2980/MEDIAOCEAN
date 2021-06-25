*          DATA SET DDXTRPER   AT LEVEL 002 AS OF 05/05/05                      
*PHASE XTRPERA                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
         TITLE 'XTRPER - EXTRACT AGENCY, USERID AND PERSON INFO'                
XTRPER   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,*XTRPER*,RA,R8,WORK=A(WORKC),CLEAR=YES               
         USING WORKD,RC            LOCAL W/S                                    
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
*                                                                               
MAIN     BAS   RE,GENINIT          GENERAL INTIALISATION                        
         BAS   RE,VALCARDS         VALIDATE JCL CARD DATA LINE                  
*                                                                               
         OPEN  (TAPEOUT,OUTPUT)                                                 
*                                                                               
         BRAS  RE,OPENCTFL                                                      
         BRAS  RE,READAGY                                                       
         BRAS  RE,READPER                                                       
         BRAS  RE,READUID                                                       
*                                                                               
         CLOSE (TAPEOUT)                                                        
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
GENINIT  NTR1                                                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(20,TODAY)      TODAY (YYYYMMDD)              
*                                                                               
         TIME  DEC                                                              
         ST    R0,DUB                                                           
         MVI   DUB+4,X'0F'                                                      
         UNPK  DUB2,DUB(5)                                                      
         MVC   TIME,DUB2                                                        
*                                                                               
         MVI   ACTION,C'L'         ACTION=LOAD                                  
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE JCL CARD DATA LINES AS DEFINED IN CARDTBL                  *         
***********************************************************************         
VALCARDS NTR1                                                                   
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* OPEN CTFILE                                                         *         
***********************************************************************         
OPENCTFL NTR1                                                                   
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,FLISTCTF                            
         XIT1                                                                   
***********************************************************************         
* READ USERID                                                         *         
***********************************************************************         
READUID  NTR1                                                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ    ID RECORDS                                   
         MVI   CTIKID,X'01'        ONLY LOOK AT ALPHA-KEYED RECORDS             
         DROP  R4                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,IOKEY,A(IO)                          
         B     RU20                                                             
*                                                                               
RU10     GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,IOKEY,A(IO)                          
*                                                                               
RU20     CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IO                                                            
         USING CTIREC,R4                                                        
         CLI   CTIKTYP,CTIKTYPQ    ID RECORD?                                   
         BNE   RUX                                                              
*                                                                               
         MVC   WORK,SPACES                                                      
         LA    R5,WORK                                                          
         USING CXSUID,R5                                                        
*                                                                               
         XC    CXSUILEN,CXSUILEN   RECORD LENGTH/TYPE/ENDOFREC                  
         MVC   CXSUILEN(2),=AL2(CXSUIDL)                                        
         MVC   CXSUITYP,CXSUIDQ                                                 
*                                                                               
         MVC   CXSUIACT,ACTION                                                  
         MVC   CXSUIDAT,TODAY                                                   
         MVC   CXSUITIM,TIME                                                    
         MVC   CXSUICOD,CTIKID                                                  
*                                                                               
         LA    R3,CTIDATA                                                       
RU50     CLI   0(R3),0                                                          
         BE    RU200                                                            
*                                                                               
         CLI   0(R3),CTDSCELQ      DESCRIPTION ELEMENT  X'02'                   
         BNE   RU60                                                             
         USING CTDSCD,R3                                                        
         SR    RF,RF                                                            
         ICM   RF,3,CTDSC          USERID NUMBER                                
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CXSUINUM,DUB+5(3)                                                
         B     RU100                                                            
         DROP  R3                                                               
*                                                                               
RU60     CLI   0(R3),CTAGYELQ      AGENCY ELEMENT       X'06'                   
         BNE   RU70                                                             
         USING CTAGYD,R3                                                        
         MVC   CXSUIAID,CTAGYID    AGENCY ALPHA                                 
         B     RU100                                                            
         DROP  R3                                                               
*                                                                               
RU70     CLI   0(R3),CTDSTELQ      DESTINATION DETAIL ELEMENT                   
         BNE   RU80                                                             
         USING CTDSTD,R3                                                        
         MVC   CXSUINAM,CTDSTNAM   DESTINATION NAME                             
         TR    CXSUIADD,MXTRT                                                   
         MVC   CXSUIADD,CTDSTADD   DESTINATION ADDRESS LINE 1                   
         TR    CXSUIADD,MXTRT                                                   
         MVC   CXSUIAD2,CTDSTAD2   DESTINATION ADDRESS LINE 2                   
         TR    CXSUIAD2,MXTRT                                                   
         MVC   CXSUIAD3,CTDSTAD3   DESTINATION ADDRESS LINE 3                   
         TR    CXSUIAD3,MXTRT                                                   
         B     RU100                                                            
         DROP  R3                                                               
*                                                                               
RU80     EQU   *                                                                
         CLI   0(R3),CTACTELQ      ACTIVITY ELEMENT (X'01')                     
         BNE   RU100                                                            
         USING CTACTD,R3                                                        
         GOTO1 VDATCON,DMCB,(3,CTACTDT),(20,CXSUILUD)                           
         B     RU100                                                            
         DROP  R3                                                               
*                                                                               
RU100    SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     RU50                                                             
*                                                                               
*                                                                               
RU200    EQU   *                                                                
*                                                                               
         MVI   CXSUITYP+L'CXSUITYP,MXTRTQ                                       
         MVI   CXSUIACT+L'CXSUIACT,MXTRTQ                                       
         MVI   CXSUIDAT+L'CXSUIDAT,MXTRTQ                                       
         MVI   CXSUITIM+L'CXSUITIM,MXTRTQ                                       
         MVI   CXSUICOD+L'CXSUICOD,MXTRTQ                                       
         MVI   CXSUINUM+L'CXSUINUM,MXTRTQ                                       
         MVI   CXSUIAID+L'CXSUIAID,MXTRTQ                                       
         MVI   CXSUINAM+L'CXSUINAM,MXTRTQ                                       
         MVI   CXSUIADD+L'CXSUIADD,MXTRTQ                                       
         MVI   CXSUIAD2+L'CXSUIAD2,MXTRTQ                                       
         MVI   CXSUIAD3+L'CXSUIAD3,MXTRTQ                                       
         MVI   CXSUILUD+L'CXSUILUD,MXTRTQ                                       
*                                                                               
         BRAS  RE,PUTOUT                                                        
*                                                                               
         B     RU10                                                             
         DROP  R4,R5                                                            
*                                                                               
RUX      XIT1                                                                   
*                                                                               
***********************************************************************         
* READ AGENCY (SYSTEM ACCESS)                                         *         
***********************************************************************         
READAGY  NTR1                                                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING CT5REC,R4                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         DROP  R4                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,IOKEY,A(IO)                          
         B     RA20                                                             
*                                                                               
RA10     GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,IOKEY,A(IO)                          
*                                                                               
RA20     CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IO                                                            
         USING CT5REC,R4                                                        
         CLI   CT5KTYP,CT5KTYPQ    SYSTEM ACCESS RECORD?                        
         BNE   RAX                                                              
*                                                                               
         MVC   WORK,SPACES                                                      
         LA    R5,WORK                                                          
         USING CXSAGD,R5                                                        
*                                                                               
         XC    CXSAGLEN,CXSAGLEN   RECORD LENGTH/TYPE/ENDOFREC                  
         MVC   CXSAGLEN(2),=AL2(CXSAGDL)                                        
         MVC   CXSAGTYP,CXSAGDQ                                                 
*                                                                               
         MVC   CXSAGACT,ACTION                                                  
         MVC   CXSAGDAT,TODAY                                                   
         MVC   CXSAGTIM,TIME                                                    
         MVC   CXSAGAID,CT5KALPH                                                
*                                                                               
         LA    R3,CT5DATA                                                       
RA50     CLI   0(R3),0                                                          
         BE    RA200                                                            
*                                                                               
         CLI   0(R3),CTSEAELQ      SECURITY AGENCY ELEMENT                      
         BNE   RA60                                                             
         USING CTSEAD,R3                                                        
         MVC   CXSAGSEC,CTSEAAID   SECURITY AGENCY                              
         B     RA100                                                            
         DROP  R3                                                               
*                                                                               
RA60     EQU   *                                                                
         CLI   0(R3),CTAADELQ      AGENCY ACCESS DETAILS ELEM                   
         BNE   RA80                                                             
*                                                                               
         USING CTAADD,R3                                                        
         MVI   CXSAGIPP,C'N'                                                    
         TM    CTAADFLG,CTAADPRQ   TEST FOR PERSON ID REQUIRED                  
         BNO   *+8                 NO                                           
         MVI   CXSAGIPP,C'Y'                                                    
*                                                                               
         ZIC   RF,CTAADPTO         N'DAYS TO EXPIRATION                         
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CXSAGPWE,DUB+6(2)                                                
*                                                                               
         ZIC   RF,CTAADPTW         N'DAYS TO WARN IN ADVANCE                    
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CXSAGPWW,DUB+6(2)                                                
*                                                                               
         ZIC   RF,CTAADPRU         N'TIMES BEFORE REUSE ALLOWED                 
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CXSAGPWR,DUB+6(2)                                                
*                                                                               
         ZIC   RF,CTAADPML         MINIMUM LENGTH OF PASSWORD                   
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CXSAGPWM,DUB+6(2)                                                
         B     RA100                                                            
         DROP  R3                                                               
*                                                                               
RA80     EQU   *                                                                
         CLI   0(R3),CTACTELQ      ACTIVITY ELEMENT (X'01')                     
         BNE   RA100                                                            
         USING CTACTD,R3                                                        
         GOTO1 VDATCON,DMCB,(3,CTACTDT),(20,CXSAGLUD)                           
         B     RA100                                                            
         DROP  R3                                                               
*                                                                               
RA100    SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     RA50                                                             
*                                                                               
*                                                                               
RA200    EQU   *                                                                
*                                                                               
         MVI   CXSAGTYP+L'CXSAGTYP,MXTRTQ                                       
         MVI   CXSAGACT+L'CXSAGACT,MXTRTQ                                       
         MVI   CXSAGDAT+L'CXSAGDAT,MXTRTQ                                       
         MVI   CXSAGTIM+L'CXSAGTIM,MXTRTQ                                       
         MVI   CXSAGAID+L'CXSAGAID,MXTRTQ                                       
         MVI   CXSAGSEC+L'CXSAGSEC,MXTRTQ                                       
         MVI   CXSAGIPP+L'CXSAGIPP,MXTRTQ                                       
         MVI   CXSAGPWE+L'CXSAGPWE,MXTRTQ                                       
         MVI   CXSAGPWW+L'CXSAGPWW,MXTRTQ                                       
         MVI   CXSAGPWR+L'CXSAGPWR,MXTRTQ                                       
         MVI   CXSAGPWM+L'CXSAGPWM,MXTRTQ                                       
         MVI   CXSAGLUD+L'CXSAGLUD,MXTRTQ                                       
*                                                                               
         BRAS  RE,PUTOUT                                                        
*                                                                               
         B     RA10                                                             
         DROP  R4,R5                                                            
*                                                                               
RAX      XIT1                                                                   
*                                                                               
***********************************************************************         
* READ PERSON                                                         *         
***********************************************************************         
READPER  NTR1                                                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING SAPEREC,R4                                                       
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         DROP  R4                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,IOKEY,A(IO)                          
         B     RP20                                                             
*                                                                               
RP10     GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,IOKEY,A(IO)                          
*                                                                               
RP20     CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,IO                                                            
         USING SAPEREC,R4                                                       
         CLI   SAPETYP,SAPETYPQ                                                 
         BNE   RPX                                                              
         CLI   SAPESUB,SAPESUBQ                                                 
         BNE   RPX                                                              
*                                                                               
*                                                                               
*                                                                               
         MVC   WORK,SPACES                                                      
         LA    R5,WORK                                                          
         USING CXSPED,R5                                                        
*                                                                               
         XC    CXSPELEN,CXSPELEN   RECORD LENGTH/TYPE/ENDOFREC                  
         MVC   CXSPELEN(2),=AL2(CXSPEDL)                                        
         MVC   CXSPETYP,CXSPEDQ                                                 
*                                                                               
*                                                                               
         MVC   CXSPEACT,ACTION                                                  
         MVC   CXSPEDAT,TODAY                                                   
         MVC   CXSPETIM,TIME                                                    
         MVC   CXSPEAID,SAPEAGY    AGENCY                                       
         MVC   CXSPEPID,SAPEPID    PERSON ID                                    
*                                                                               
         MVC   EFFDATE,SAPEDEF                                                  
         XC    EFFDATE,FFILL                                                    
         GOTO1 VDATCON,DMCB,(2,EFFDATE),(20,CXSPEEFD)                           
*                                                                               
         LA    R3,SAPEDATA                                                      
RP50     CLI   0(R3),0                                                          
         BE    RP200                                                            
*                                                                               
         CLI   0(R3),SAPWDELQ      PERSON PASSWORD ELEMENT X'C4'                
         BNE   RP60A                                                            
         USING SAPWDD,R3                                                        
*DON'T NEED THE PASSWORD INFO                                                   
*        MVC   CXSPEPWD,SAPWDCOD   PASSWORD CODE                                
         SR    RF,RF                                                            
         ICM   RF,3,SAPWDNUM                                                    
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CXSPENUM,DUB+5(3)   PASSWORD NUMBER                              
         B     RP180                                                            
         DROP  R3                                                               
*                                                                               
RP60A    CLI   0(R3),SANAMELQ      PERSON NAME ELEMENT  X'C5'                   
         BNE   RP70                                                             
         USING SANAMD,R3                                                        
         LA    RE,SANAMELN                                                      
         SR    RF,RF                                                            
         TM    SANAMIND,SANAMIFN                                                
         BZ    RP60B                                                            
         IC    RF,0(RE)                                                         
         LA    RE,1(RE)                                                         
*                                                                               
         CHI   RF,L'CXSPEFNA       LONGER THAN OUTPUT FIELD?                    
         BNH   *+8                 NO - OKAY                                    
         LHI   RF,L'CXSPEFNA       YES - USE OUTPUT FIELD LENGTH                
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CXSPEFNA(0),0(RE)                                                
         LA    RF,1(RF)                                                         
         AR    RE,RF                                                            
*                                                                               
RP60B    TM    SANAMIND,SANAMIMN                                                
         BZ    RP60C                                                            
         IC    RF,0(RE)                                                         
         LA    RE,1(RE)                                                         
*                                                                               
         CHI   RF,L'CXSPEMNA       LONGER THAN OUTPUT FIELD?                    
         BNH   *+8                 NO - OKAY                                    
         LHI   RF,L'CXSPEMNA       YES - USE OUTPUT FIELD LENGTH                
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CXSPEMNA(0),0(RE)                                                
         LA    RF,1(RF)                                                         
         AR    RE,RF                                                            
*                                                                               
RP60C    TM    SANAMIND,SANAMILN                                                
         BZ    RP180                                                            
         IC    RF,0(RE)                                                         
         LA    RE,1(RE)                                                         
*                                                                               
         CHI   RF,L'CXSPELNA       LONGER THAN OUTPUT FIELD?                    
         BNH   *+8                 NO - OKAY                                    
         LHI   RF,L'CXSPELNA       YES - USE OUTPUT FIELD LENGTH                
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CXSPELNA(0),0(RE)                                                
         B     RP180                                                            
         DROP  R3                                                               
*                                                                               
RP70     CLI   0(R3),SAAGCELQ      ACCESS GROUP ELEMENT                         
         BNE   RP80                                                             
         USING SAAGCD,R3                                                        
         GOTO1 VHEXOUT,DMCB,SAAGCNUM,CXSPEAGC,2,=C'TOG'                         
         B     RP180                                                            
         DROP  R3                                                               
*                                                                               
RP80     CLI   0(R3),SAPERELQ      PERSON DETAILS ELEMENT                       
         BNE   RP90                                                             
         USING SAPERD,R3                                                        
         MVC   CXSPEOFF,SAPEROFF                                                
         MVC   CXSPEDEP,SAPERDID                                                
         GOTO1 VDATCON,DMCB,(2,SAPERDHI),(20,CXSPEDHI)                          
         GOTO1 VDATCON,DMCB,(2,SAPERDTE),(20,CXSPEDTE)                          
         B     RP180                                                            
         DROP  R3                                                               
*                                                                               
RP90     EQU   *                                                                
         CLI   0(R3),SAACVELQ      ACTIVITY ELEMENT (X'01')                     
         BNE   RP100                                                            
         USING SAACVD,R3                                                        
         GOTO1 VDATCON,DMCB,(3,SAACVDT),(20,CXSPELUD)                           
         B     RP180                                                            
         DROP  R3                                                               
*                                                                               
RP100    EQU   *                                                                
* DON'T NEED THE VALUE OF                                                       
*  1) CXSPEIPE                                                                  
*  2) CXSPE1RL                                                                  
*                                                                               
RP180    SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     RP50                                                             
*                                                                               
*                                                                               
RP200    EQU   *                                                                
*                                                                               
         MVI   CXSPETYP+L'CXSPETYP,MXTRTQ                                       
         MVI   CXSPEACT+L'CXSPEACT,MXTRTQ                                       
         MVI   CXSPEDAT+L'CXSPEDAT,MXTRTQ                                       
         MVI   CXSPETIM+L'CXSPETIM,MXTRTQ                                       
         MVI   CXSPEAID+L'CXSPEAID,MXTRTQ                                       
         MVI   CXSPENUM+L'CXSPENUM,MXTRTQ                                       
         MVI   CXSPEEFD+L'CXSPEEFD,MXTRTQ                                       
         MVI   CXSPEPID+L'CXSPEPID,MXTRTQ                                       
         MVI   CXSPEPWD+L'CXSPEPWD,MXTRTQ                                       
         MVI   CXSPEAGC+L'CXSPEAGC,MXTRTQ                                       
         MVI   CXSPE1RL+L'CXSPE1RL,MXTRTQ                                       
         MVI   CXSPEFNA+L'CXSPEFNA,MXTRTQ                                       
         MVI   CXSPEMNA+L'CXSPEMNA,MXTRTQ                                       
         MVI   CXSPELNA+L'CXSPELNA,MXTRTQ                                       
         MVI   CXSPEOFF+L'CXSPEOFF,MXTRTQ                                       
         MVI   CXSPEDEP+L'CXSPEDEP,MXTRTQ                                       
         MVI   CXSPEDHI+L'CXSPEDHI,MXTRTQ                                       
         MVI   CXSPEDTE+L'CXSPEDTE,MXTRTQ                                       
         MVI   CXSPEIPE+L'CXSPEIPE,MXTRTQ                                       
         MVI   CXSPELUD+L'CXSPELUD,MXTRTQ                                       
*                                                                               
         BRAS  RE,PUTOUT                                                        
         B     RP10                                                             
         DROP  R4,R5                                                            
*                                                                               
RPX      XIT1                                                                   
*                                                                               
***********************************************************************         
* WRITE A RECORD TO THE OUTPUT FILE                                   *         
***********************************************************************         
PUTOUT   NTR1  ,                                                                
         PUT   TAPEOUT,WORK                                                     
*                                                                               
         MVC   P,WORK                                                           
         GOTO1 VPRINTER                                                         
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
VDATAMGR DC    V(DATAMGR)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHELLO   DC    V(HELLO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VGETFACT DC    V(GETFACT)                                                       
VCARDS   DC    V(CARDS)                                                         
VDATCON  DC    V(DATCON)                                                        
*                                                                               
         SPACE 1                                                                
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               BLKSIZE=8200,LRECL=2048,BUFNO=2                                  
         SPACE 1                                                                
*                                                                               
FLISTCTF DC    CL8'NCTFILE '                                                    
         DC    C'X'                                                             
*                                                                               
DMOPEN   DC    C'OPEN   '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
*                                                                               
FFILL    DC    80X'FF'                                                          
*                                                                               
         DS    0D                                                               
         DC    C'***IO***'                                                      
IO       DC    2000X'00'                                                        
*                                                                               
* CXSRECID                                                                      
* MXTRT                                                                         
         PRINT OFF                                                              
       ++INCLUDE MXTRT                                                          
       ++INCLUDE CXSRECID                                                       
         PRINT ON                                                               
         SPACE 1                                                                
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINTL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         PRINT ON                                                               
         SPACE 1                                                                
*                                                                               
* COPY DSECT FROM 1) CXSAGD, 2) CXSUID AND 3) CXSPE2D                           
* THEN, ADD 1 MORE FIELD (LAST UPATED DATE) AT THE END OF EACH RECORD           
*                                                                               
*          DATA SET CXSAGD     AT LEVEL 006 AS OF 01/16/04                      
***********************************************************************         
*CONTROL SYSTEM SECURITY EXTRACT - AGENCY RECORD                      *         
***********************************************************************         
         SPACE 1                                                                
CXSAGD   DSECT                     AGENCY RECORD                                
CXSAGLEN DS    CL4                 AGENCY RECORD LENGTH                         
CXSAGTYP DS    CL5     R CHAR      AGENCY RECORD TYPE                           
         DS    C                                                                
CXSAGACT DS    CL1     R CHAR      ACTION (A/C/D/L)                             
         DS    C                                                                
CXSAGDAT DS    CL8     R DATE      ACTION DATE YYYYMMDD                         
         DS    C                                                                
CXSAGTIM DS    CL6     R TIME      ACTION TIME HHMMSS                           
*                                                                               
         DS    C                                                                
CXSAGAID DS    CL2     K CHAR      AGENCY ALPHA ID                              
*                                                                               
         DS    C                                                                
CXSAGSEC DS    CL2     R CHAR      SECURITY AGENCY ALPHA ID                     
*                                                                               
         DS    C                                                                
CXSAGIPP DS    CL1     R CHAR      IS PPS (Y/N)                                 
*                                                                               
         DS    C                                                                
CXSAGPWE DS    CL3     R SMALLINT  PASSWORD EXPIRATION (N'DAYS)                 
*                                                                               
         DS    C                                                                
CXSAGPWW DS    CL3     R SMALLINT  PASSWORD WARNING (N'DAYS)                    
*                                                                               
         DS    C                                                                
CXSAGPWR DS    CL3     R SMALLINT  PASSWORD REUSE (N'TIMES)                     
*                                                                               
         DS    C                                                                
CXSAGPWM DS    CL3     R SMALLINT  PASSWORD MINIMUM LENGTH                      
*                                                                               
         DS    C                                                                
CXSAGLUD DS    CL8     R DATE      LAST UPDATED DATE                            
*                                                                               
CXSAGDX  DS    CL2                 RECORD END                                   
CXSAGDL  EQU   *-CXSAGD            RECORD LENGTH                                
*                                                                               
*          DATA SET CXSUID     AT LEVEL 003 AS OF 07/06/94                      
***********************************************************************         
*CONTROL SYSTEM SECURITY EXTRACT - USER ID RECORD                     *         
***********************************************************************         
         SPACE 1                                                                
CXSUID   DSECT                     USER ID RECORD                               
CXSUILEN DS    CL4                 USER ID RECORD LENGTH                        
CXSUITYP DS    CL5     R CHAR      USER ID RECORD TYPE                          
         DS    C                                                                
CXSUIACT DS    CL1     R CHAR      ACTION (A/C/D/L)                             
         DS    C                                                                
CXSUIDAT DS    CL8     R DATE      ACTION DATE YYYYMMDD                         
         DS    C                                                                
CXSUITIM DS    CL6     R TIME      ACTION TIME HHMMSS                           
*                                                                               
         DS    C                                                                
CXSUICOD DS    CL10    K CHAR      USER ID CODE                                 
*                                                                               
         DS    C                                                                
CXSUINUM DS    CL6     R INT       USER ID NUMBER                               
         DS    C                                                                
CXSUIAID DS    CL2     R CHAR      AGENCY ALPHA ID                              
         DS    C                                                                
CXSUINAM DS    CL33    O CHAR      DESTINATION NAME                             
         DS    C                                                                
CXSUIADD DS    CL33    O CHAR      DESTINATION ADDRESS LINE 1                   
         DS    C                                                                
CXSUIAD2 DS    CL33    O CHAR      DESTINATION ADDRESS LINE 2                   
         DS    C                                                                
CXSUIAD3 DS    CL33    O CHAR      DESTINATION ADDRESS LINE 3                   
*                                                                               
         DS    C                                                                
CXSUILUD DS    CL8     R DATE      LAST UPDATED DATE                            
*                                                                               
CXSUIDX  DS    CL2                 RECORD END                                   
CXSUIDL  EQU   *-CXSUID            RECORD LENGTH                                
*                                                                               
*          DATA SET CXSPE2D    AT LEVEL 007 AS OF 01/20/04                      
***********************************************************************         
*CONTROL SYSTEM SECURITY EXTRACT - PERSON PASSWORD RECORD VERSION 2   *         
***********************************************************************         
         SPACE 1                                                                
CXSPED   DSECT                     PERSON PASSWORD RECORD                       
CXSPELEN DS    CL4                 PERSON PASSWORD RECORD LENGTH                
CXSPETYP DS    CL5     R CHAR      PERSON PASSWORD RECORD TYPE                  
         DS    C                                                                
CXSPEACT DS    CL1     R CHAR      ACTION (A/C/D/L)                             
         DS    C                                                                
CXSPEDAT DS    CL8     R DATE      ACTION DATE YYYYMMDD                         
         DS    C                                                                
CXSPETIM DS    CL6     R TIME      ACTION TIME HHMMSS                           
*                                                                               
         DS    C                                                                
CXSPEAID DS    CL2     K CHAR      AGENCY ALPHA ID                              
         DS    C                                                                
CXSPENUM DS    CL6     R INT       PERSON NUMBER                                
         DS    C                                                                
CXSPEEFD DS    CL8     K CHAR      PERSON RECORD EFFECTIVE DATE                 
*                                                                               
         DS    C                                                                
CXSPEPID DS    CL8     R CHAR      PERSON PUBLIC-ID                             
         DS    C                                                                
CXSPEPWD DS    CL10    K CHAR      PASSWORD CODE                                
         DS    C                                                                
CXSPEAGC DS    CL8     R CHAR      ACCESS GROUP CODE                            
         DS    C                                                                
CXSPE1RL DS    CL8     R CHAR      1R LEDGER GROUP CODE                         
         DS    C                                                                
CXSPEFNA DS    CL20    R CHAR      PERSON FIRST NAME                            
         DS    C                                                                
CXSPEMNA DS    CL20    O CHAR      PERSON MIDDLE NAME                           
         DS    C                                                                
CXSPELNA DS    CL20    R CHAR      PERSON LAST NAME                             
         DS    C                                                                
CXSPEOFF DS    CL2     R CHAR      PERSON OFFICE CODE                           
         DS    C                                                                
CXSPEDEP DS    CL3     R CHAR      PERSON DEPARTMENT CODE                       
         DS    C                                                                
CXSPEDHI DS    CL8     R CHAR      PERSON HIRE DATE                             
         DS    C                                                                
CXSPEDTE DS    CL8     R CHAR      PERSON TERMINATION DATE                      
         DS    C                                                                
CXSPEIPE DS    C       R CHAR      IS PASSWORD EXPIRES (Y/N)                    
*                                                                               
         DS    C                                                                
CXSPELUD DS    CL8     R DATE      LAST UPDATED DATE                            
*                                                                               
CXSPEDX  DS    CL2                 RECORD END                                   
CXSPEDL  EQU   *-CXSPED            RECORD LENGTH                                
*                                                                               
         SPACE 1                                                                
WORKD    DSECT                     ** GLOBAL WORKING STORAGE **                 
*                                                                               
DUB      DS    D                                                                
DUB2     DS    D                                                                
DMCB     DS    6F                                                               
*                                                                               
TODAY    DS    CL8                 YYYYMMDD                                     
TIME     DS    CL6                 HHMMSS                                       
ACTION   DS    C                   A/C/D/L                                      
*                                                                               
EFFDATE  DS    XL2                 EFF DATE IN BINARY                           
*                                                                               
WORK     DS    XL256                                                            
IOKEY    DS    XL(L'CT5KEY)                                                     
*                                                                               
*                                                                               
WORKX    DS    0D                                                               
         SPACE 1                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL 1 **                 
         DS    (64*1024)X                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DDXTRPER  05/05/05'                                      
         END                                                                    
