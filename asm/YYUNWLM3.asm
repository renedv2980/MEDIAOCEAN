*          DATA SET YYUNWLM3   AT LEVEL 107 AS OF 08/16/00                      
*PHASE YYUNWLMA                                                                 
WLMSAS   CSECT                                                                  
WLMSAS   AMODE 31                                                               
WLMSAS   RMODE 24                                                               
         REQUS                 REGISTER EQU'S                                   
*                                                                               
         USING *,RB,R7                                                          
         STM   RE,RC,12(RD)    SAVE REGS IN CALLER SAVE AREA                    
         BASR  RB,0                                                             
         AHI   RB,-6           SET RB AS BASE REGISTER                          
         LR    R7,RB                                                            
         AHI   R7,X'FFF'       SET R7 AS 2ND BASE REGISTER                      
*                                                                               
*        GETMAIN RU,LV=WORKSIZE,LOC=ANY  MY WORK AREA ABOVE 16M-LINE            
         STORAGE OBTAIN,LENGTH=WORKSIZE,LOC=ANY                                 
         LR    RC,R1           SET RC TO MY WORK AREA                           
         USING WORKD,RC                                                         
*                                                                               
*        GETMAIN RU,LV=SAVESIZE,LOC=ANY  MY SAVE AREA ABOVE 16M-LINE            
         STORAGE OBTAIN,LENGTH=SAVESIZE,LOC=ANY                                 
         MVC   0(4,R1),PROGID            PROGRAM EYE-CATCHER                    
         ST    RD,4(R1)        SAVE A(CALLERS SAVE AREA) IN MINE                
         ST    R1,8(RD)        STORE A(MY SAVE AREA) IN HIS                     
         LR    RD,R1           SET RD TO MY SAVE AREA                           
*                                                                               
*    THIS A SINGLE ADDRESS SPACE TRANSACTION MANAGER WLM SERVICES               
*                                                                               
         MODESET  MF=(E,SUPSTATE)                                               
         LA    R5,WLMCONN                                                       
         IWMCONN  CONNTKN=(R4),       RETURN TOKEN IN R4               X        
               CONNTKNKEYP=PSWKEY,                                     X        
               SUBSYS=#DDS,                                            X        
               SUBSYSNM=FRED,                                          X        
               MF=(E,(R5))                                                      
         LTR   RF,RF                                                            
         BZ    CLASSFY                                                          
         DC    H'0'                                                             
*                                                                               
*                                                                               
CLASSFY  DS    0H                                                               
         LA    R5,WLMCLSFY                                                      
         IWMCLSFY CONNTKN=(R4),            CLASSIFY                    X        
               SERVCLS=CLASS_TOKEN,        GET ASSIGNED SC TOKEN       X        
               SRVCLSNM=CLASS_ASSIGNED,    GET NAME OF SC              X        
               TRXCLASS=MONSOON_SYSTEM,    CLASSIFY ON MONSOON'S SYS   X        
               TRXNAME=MONSOON_PROGRAM,    AND SUBSYSTEM BELOW         X        
               ACCTINFO=NO_ACCTINFO,ACCTINFL=ZERO,                     X        
               COLLECTION=NO_COLLECTION,COLLECTION_LEN=ZERO,           X        
               CONNECTION=NO_CONNECTION,                               X        
               CORRELATION=NO_CORRELATION,CORR_LEN=ZERO,               X        
               PACKAGE=NO_PACKAGE,                                     X        
               PLAN=NO_PLAN,                                           X        
               PRCNAME=NO_PRCNAME,PRCNAME_LEN=ZERO,                    X        
               SOURCELU=NO_SOURCELU,                                   X        
               SUBSYSPM=NO_SUBSYSPM,SSPMLEN=ZERO,                      X        
               USERID=NO_USERID,                                       X        
               MF=(E,(R5))                                                      
         LTR   RF,RF                                                            
         BZ    RUNIT                                                            
         DC    H'0'                                                             
*                                                                               
*                                                                               
RUNIT    DS    0H                                                               
         LA    R5,WLMRPT                                                        
         IWMRPT   CONNTKN=(R4),       REPORT COMPLETION                X        
               MONTKNI=NO,                                             X        
               ARRIVALTIME=ARRIVAL_TIME,                               X        
               SERVCLS=CLASS_TOKEN,                                    X        
               EXSTARTTIMEP=NO,                                        X        
               TRAXFRPT=YES,                                           X        
               SYSEVPL=SYSEVPL,                                        X        
               MF=(E,(R5))                                                      
         LTR   RF,RF                                                            
         BZ    DISC                                                             
         DC    H'0'                                                             
*                                                                               
*                                                                               
DISC     LA    R5,WLMDISC                                                       
         IWMDISC  CONNTKN=(R4),       TERMINATE THE CONNECTION         X        
               MF=(E,(R5))                                                      
         LTR   RF,RF                                                            
         BZ    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
*                                                                               
EXIT     STORAGE RELEASE,LENGTH=WORKSIZE,ADDR=(12)                              
*XIT     FREEMAIN RU,LV=SAVESIZE,A=(13)     FREE MY SAVE AREA                   
*        FREEMAIN RU,LV=WORKSIZE,A=(12)     FREE MY WORK AREA                   
         XIT                                                                    
*                                                                               
*                                                                               
*                                                                               
*                                                                               
SYSEVPL  DC    XL40'0'                                                          
PROGID   DC    C'WLMS'                                                          
SAVESIZE EQU   5000       REMEMBER THE DOUBLE WORD ALIGNMENT OF GETMAIN         
ZERO     DC    F'0'                                                             
#DDS     DC    CL4'#DDS'                                                        
FRED     DC    CL4'FRED'                                                        
SUPSTATE MODESET KEY=ZERO,MODE=SUP,MF=L                                         
PROBSTAT MODESET KEY=NZERO,MODE=PROB,MF=L                                       
MONSOON_SYSTEM  DC  CL8'ADV1'                                                   
MONSOON_PROGRAM DC  CL8'SPT'                                                    
         EJECT                                                                  
         IWMCONN  PLISTVER=MAX,MF=(L,WLMCONN,0D)                                
         IWMDISC  PLISTVER=MAX,MF=(L,WLMDISC,0D)                                
         IWMCLSFY PLISTVER=MAX,MF=(L,WLMCLSFY,0D)                               
         IWMRPT   PLISTVER=MAX,MF=(L,WLMRPT,0D)                                 
         EJECT                                                                  
         CVT  DSECT=YES,LIST=NO                                                 
         IHATRBPL                                                               
WORKD    DSECT                                                                  
CLASS_ASSIGNED  DS  CL8                                                         
CLASS_TOKEN     DS  1F                                                          
ARRIVAL_TIME    DS  XL8                                                         
WORKSIZE EQU   *-WORKD                                                          
*                                                                               
         IWMYCON                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'107YYUNWLM3  08/16/00'                                      
         END                                                                    
